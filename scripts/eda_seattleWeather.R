library(data.table)
library(dplyr)
library(ggplot2)
library(lubridate)
library(magrittr)

raw_seattleWeather <- fread('data/city-of-seattle/seattle-road-weather-information-stations/road-weather-information-stations.csv',
                            nrows = 1E7,header = TRUE, drop = c("StationLocation"))

# write two versions of a helper function to grab a 
## (1) metric field for 
## all occurrences of a given filter based on (2) filter fields and values, 
## (3) indexed by a time series variable coerced into a standard datetime format
## one version for data.table, and one for data.frame

fn_get_ts_subset = function(dt,metric_field,ts_field,filter_fields,filter_vals) {
  if(length(filter_fields)>0){
    for (i in seq(1:length(filter_fields))){
      dt = dt[get(filter_fields[i])==filter_vals[i]]
    }
  }
  dt[,ts_field:=as_datetime(get(ts_field))]
  dt = dt[,.(ts_field,get(metric_field))]
  setnames(dt,c("ts_field","V2"),c(ts_field,metric_field))
  return(head(dt))
}

# fn_get_ts_subset(dt=raw_seattleWeather,metric_field = "AirTemperature",
# filter_fields = c("StationName","RoadSurfaceTemperature"),
# filter_vals = c("35thAveSW_SWMyrtleSt",103.00),ts_field = "DateTime")

###### distribution of the number of observations per StationName value ######
num_master_timestamps = 
  fread('data/city-of-seattle/seattle-road-weather-information-stations/road-weather-information-stations.csv',
        header = TRUE, select = c("DateTime"))[,.N,by=DateTime] %>% nrow()
obs_per_station = 
  fread('data/city-of-seattle/seattle-road-weather-information-stations/road-weather-information-stations.csv',
        header = TRUE, drop = c("StationLocation"))[,.(num_obs=.N),by=.(StationName,DateTime)][,.(num_obs_timestamps=.N),by=.(StationName)]
obs_per_station$pct_obs_filled=obs_per_station$num_obs_timestamps/num_master_timestamps
ggplot(data = obs_per_station,aes(pct_obs_filled)) +
  # geom_histogram(bins = 5) +
  # geom_density() +
  stat_ecdf(geom="step",fill="navy")
# there are 10 stations, they all have measurements at least 84% of the time, and 
# most have measurements between 84% and 90%

###### how often and why do stations have more rows than possible timestamps? ######
obs_per_station_timestamp = 
  fread('data/city-of-seattle/seattle-road-weather-information-stations/road-weather-information-stations.csv',
        header = TRUE, drop = c("StationLocation"))[,.(num_obs=.N),by=.(StationName,DateTime)]
ggplot(data=obs_per_station_timestamp %>% mutate(num_obs_log=log10(num_obs)),aes(x=StationName,y=num_obs_log)) +
  geom_violin(fill="navy") 
# it looks like the vast majority of the timestamps appear just once per StationName, 
# with exceptions at specific frequencies that are common across the StationNames
# check these out by grouping on DateTime - most should have 10, maybe 9, but some 
# probably have much more. We need to identify those ones
obs_per_timestamp = obs_per_station_timestamp[,.(num_obs=sum(num_obs)),by=.(DateTime)][,num_obs_log:=log10(num_obs)]
ggplot(data = obs_per_timestamp,aes(num_obs_log)) + stat_ecdf(geom="step")
# looks like a slow drop-off after 10 observations...go back to raw data to rummage
overused_timestamps = obs_per_timestamp[num_obs > 10][,num_obs_log:=NULL][,DateTime:=as.character(DateTime)]
setkey(overused_timestamps,DateTime)
setkey(raw_seattleWeather,StationName)
# high_cardinality_epochs = overused_timestamps[raw_seattleWeather,nomatch=0]
high_cardinality_epochs = inner_join(overused_timestamps,raw_seattleWeather,by="DateTime")
head(high_cardinality_epochs,n = 30)
# it looks like there are true duplicate records in here
num_dupes= (raw_seattleWeather %>% nrow()) - (raw_seattleWeather %>% unique() %>% nrow())
# about 25% of the records are dupes. So we should do a simple dedupe in our original fread
# clean up exploratory workspace - save memory
rm(high_cardinality_epochs,obs_per_station,obs_per_station_timestamp,
   obs_per_timestamp,overused_timestamps)
gc()
###### revised read pipeline to avoid dupes ######
raw_seattleWeather <- 
  fread('data/city-of-seattle/seattle-road-weather-information-stations/road-weather-information-stations.csv',
        nrows = 1E7,header = TRUE, drop = c("StationLocation")) %>%
  unique()

###### prep data for modeling ######
# make sure no non-numeric observations or nonsensical timestamps get passed in
# compute average sensor readings for each StationName per hour (day?)
fn_convert_datetime_to_datehour = function(date_time) {
  return(paste0(substr(date_time,1,14),"00:00"))
}
hourly_seattleWeather = raw_seattleWeather %>%
  mutate(date_hour = fn_convert_datetime_to_datehour(DateTime)) %>%
  mutate(date_hour=as_datetime(date_hour)) %>%
  group_by(StationName,date_hour) %>%
  summarise(air_temp_mean=mean(AirTemperature),air_temp_median=median(AirTemperature),num_obs=n()) %>%
  arrange(StationName,date_hour)

###### distribution of the time between observations per StationName value ######

ggplot(hourly_seattleWeather,aes(StationName,num_obs)) +
  geom_violin(fill="navy") +
  theme_bw() +
  theme(axis.text = element_text(angle = 0)) 
# the vast majority of hours for all sites have 60 measurements; a few have 55, 
# and there are some scattered other counts as well. So in general, the 
# measurements are taken every minute
# THIS MEANS WE COULD ALSO USE THIS DATA FOR MINUTE-LEVEL FORECASTS
ggplot(data = hourly_seattleWeather,aes(date_hour,num_obs)) +
  geom_line() +
  facet_grid(StationName ~ ., scales = "free")
# Here we see that two stations did not report anything for months in 2014
# we should probably throw them out of the modeling process if we want to 
# treat all the stations equally. 
top_stations = hourly_seattleWeather %>% 
  group_by(StationName) %>% 
  summarise(total_obs = sum(num_obs)) %>%
  arrange(-total_obs)
top_stations
# these two are HarborAveUpperNorthBridge and AlaskanWayViaduct_KingSt
exclude_stations = c("HarborAveUpperNorthBridge","AlaskanWayViaduct_KingSt")
# there were also some correlated dips in the observation counts in Jun 2015

###### what patterns do we see in the hourly data? any cyclical? ######
ggplot(data = hourly_seattleWeather,aes(date_hour,air_temp_mean)) +
  geom_line() +
  facet_grid(StationName ~ ., scales = "free")
ggplot(data = hourly_seattleWeather,aes(date_hour,air_temp_median)) +
  geom_line() +
  facet_grid(StationName ~ ., scales = "free")
# definitely some long-term trends, but we have less than two years of data,
# so not enough to model annual patterns
# let's dive deeper to see time-of-day patterns
monthly_deep_dive = hourly_seattleWeather %>%
  mutate(date_year=year(date_hour),date_month=month(date_hour)) %>%
  filter(date_year==2015,date_month==07) %>%
  select(-date_year,-date_month)
ggplot(data = monthly_deep_dive,aes(date_hour,air_temp_mean)) +
  geom_line() +
  facet_grid(StationName ~ ., scales = "free")
# yep, time-of-day patterns for sure. but the daily magnitudes differ quite a bit
# how strong is the acf?
top_stations = hourly_seattleWeather %>%
  
fn_plot_acf = function(station_name) {
  df = hourly_seattleWeather %>% 
    filter(StationName==station_name)
  station_acf = acf(df$air_temp_mean,plot = FALSE)
  return(plot(station_acf,main=paste0("ACF for ",station_name)))
}
for (station in top_stations$StationName[1:5]){
  fn_plot_acf(station)
}
# strong 24-hour patterns for all, with varying decay rates

# start off modeling with auto.arima as a baseline
library(forecast)
library(zoo)
library(forecastHybrid)
library(prophet)
split_date = as_datetime("2015-07-01")
train_data = hourly_seattleWeather %>% filter(date_hour<split_date)
test_data = hourly_seattleWeather %>% filter(date_hour>=split_date)
fn_form_ts_vec = function(df,station_name) {
  subset_df = df %>% 
    filter(StationName==station_name) %>%
    select(date_hour,air_temp_mean)
  #ts_df = data.frame(subset_df$air_temp_mean,row.names = "date_hour")
  ts_vec = zoo(subset_df$air_temp_mean,subset_df$date_hour)
  # return(as.ts(subset_df))
  return(ts_vec)
}

fn_generate_train_test_vecs = function(ts_vec,train_periods,test_periods){
  total_periods = length(ts_vec)
  lb = train_periods
  ub = total_periods - test_periods + 1
  split_period = ceiling(runif(1,lb,ub))
  train = ts_vec[(split_period-train_periods+1):split_period]
  test = ts_vec[(split_period+1):(split_period+test_periods)]
  return(list(train,test))
}

fn_generate_fcst_df = function(station_name,train_periods,test_periods) {
  sample = fn_form_ts_vec(df=train_data,station_name = station_name)
  vecs = fn_generate_train_test_vecs(sample,train_periods,test_periods)
  train = vecs[[1]]
  test = vecs[[2]]
  model_auto.arima = auto.arima(input)
  pred_auto.arima = predict(model_auto.arima,test_periods)$pred %>% 
    zoo(.,seq(end(train)+hours(1),end(train)+hours(test_periods),by="hour"))
  train_fh = merge.zoo(train,zoo(,seq(start(train),end(train),by="hour")), all=TRUE) %>%
    na.approx(.)
  model_forecastHybrid = forecastHybrid::hybridModel(train_fh,"aef")
  pred_forecastHybrid = predict(model_forecastHybrid,test_periods)$mean %>%
    zoo(.,seq(end(train)+hours(1),end(train)+hours(test_periods),by="hour"))
  input_prophet = data.frame(ds=index(train),y=train,row.names = NULL)
  model_prophet = prophet(input_prophet)
  pred_prophet = (make_future_dataframe(model_prophet,test_periods,freq = "hour",
                                        include_history = FALSE) %>%
                    predict(model_prophet,.))$yhat %>%
    zoo(.,seq(end(train)+hours(1),end(train)+hours(test_periods),by="hour"))
  pred_merged = cbind.zoo(pred_auto.arima,pred_forecastHybrid,pred_prophet,test) %>%
    data.frame(.,dt=seq(end(train)+hours(1),end(train)+hours(test_periods),by="hour"),
               row.names=NULL)
  return(pred_merged)
}

input = fn_form_ts_vec(train_data,station_name = "35thAveSW_SWMyrtleSt")
# fill in missing timestamps with NAs
# need to wrap this function call in a training & test time selection step
model_auto.arima = auto.arima(input)
pred_auto.arima = predict(model_auto.arima,24)$pred %>% 
  zoo(.,seq(end(input)+hours(1),end(input)+hours(24),by="hour"))
# forecastHybrid can't accommodate missing values, so we'll create
# placeholders for missing timestamps and do local interpolation for the values
input_fh = merge.zoo(input,zoo(,seq(start(input),end(input),by="hour")), all=TRUE) %>%
  na.approx(.)
model_forecastHybrid = forecastHybrid::hybridModel(input_fh,"aef")
pred_forecastHybrid = predict(model_forecastHybrid,24)$mean %>%
  zoo(.,seq(end(input)+hours(1),end(input)+hours(24),by="hour"))
# omitting s because I don't feel like specifying a seasonal frequency
# omitting n and t because they're too slow
input_prophet = data.frame(ds=index(input),y=input,row.names = NULL)
model_prophet = prophet(input_prophet)
pred_prophet = (make_future_dataframe(model_prophet,24,freq = "hour",
                                     include_history = FALSE) %>%
  predict(model_prophet,.))$yhat %>%
  zoo(.,seq(end(input)+hours(1),end(input)+hours(24),by="hour"))
# use cbind.zoo to merge the forecasts into a dataframe to inner_join with 
# the test data
pred_merged = cbind.zoo(pred_auto.arima,pred_forecastHybrid,pred_prophet) %>%
  data.frame(.,dt=seq(end(input)+hours(1),end(input)+hours(24),by="hour"),
             row.names=NULL)#.,row.names=index(input_prophet))
# make variables for num_steps_predicted and num_training_obs
# wrap this all up in a function
ex1 = fn_generate_fcst_df("35thAveSW_SWMyrtleSt",90*24,168)
ggplot(data = ex1,aes(dt,test)) +
  geom_line(color='black') +
  geom_line(aes(dt,pred_forecastHybrid),color="blue") +
  geom_line(aes(dt,pred_prophet),color="green") +
  geom_line(aes(dt,pred_auto.arima),color="green")

# add in the sampling parameters: 

# take train_size and test_size objects
# randomly choose a row number in the raw data that leaves enough room before and after for train and test
# form the train set and pass it to the forecasting function
# form the test set and join it (on timestamp) with the forecast output


fn_fit_auto_arima = function(station_name) {
  
}

# evaluate the fit for each of the predictions, versus the test data
# use MAPE as the error
library(MLmetrics)
MAPE(ex1$pred_auto.arima,ex1$test)
MAPE(ex1$pred_forecastHybrid,ex1$test)
MAPE(ex1$pred_prophet,ex1$test)
