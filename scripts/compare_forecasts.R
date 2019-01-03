library(data.table)
library(dplyr)
library(ggplot2)
library(lubridate)
library(magrittr)
library(forecast)
library(zoo)
library(forecastHybrid)
library(prophet)
library(MLmetrics)
library(xts)

############## HELPER FUNCTIONS ##############
fn_convert_datetime_to_datehour = function(date_time) {
  return(paste0(substr(date_time,1,14),"00:00"))
}

fn_form_ts_vec = function(df,station_name) {
  subset_df = df %>% 
    filter(StationName==station_name) %>%
    select(date_hour,air_temp_mean)
  #ts_df = data.frame(subset_df$air_temp_mean,row.names = "date_hour")
  ts_vec = zoo(subset_df$air_temp_mean,subset_df$date_hour)
  # return(as.ts(subset_df))
  return(ts_vec)
}

fn_generate_train_test_vecs = function(ts_vec,train_periods,test_periods,logging=TRUE){
  total_periods = length(ts_vec)
  lb = train_periods
  ub = total_periods - test_periods + 1
  split_period = ceiling(runif(1,lb,ub))
  train = ts_vec[(split_period-train_periods+1):split_period]
  test = ts_vec[(split_period+1):(split_period+test_periods)]
  if(logging){
    print(paste0("raw training data range: ",min(index(train))," ",max(index(train))))
    print(paste0("raw test range: ",min(index(test))," ",max(index(test))))
  }
  return(list(train,test))
}

fn_generate_fcst_df = function(train_data,station_name,train_periods,test_periods,logging=TRUE) {
  sample = fn_form_ts_vec(df=train_data,station_name = station_name)
  vecs = fn_generate_train_test_vecs(sample,train_periods,test_periods)
  train = vecs[[1]]
  test = vecs[[2]]
  model_auto.arima = auto.arima(train)
  # pred_auto.arima = predict(model_auto.arima,test_periods)$pred %>% 
  pred_auto.arima = forecast(model_auto.arima,test_periods)$fitted %>% 
    zoo(.,seq(end(train)+hours(1),end(train)+hours(test_periods),by="hour"))
  # train_fh = merge.zoo(train,zoo(,seq(start(train),end(train),by="hour")), all=TRUE) %>%
  #   na.approx(.)
  train_fh = as.zoo(merge(
    as.xts(train),
    as.xts(zoo(,seq(start(train),end(train),by="hour"))))) %>%
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
  if(logging){
    print(paste0("training data range: ",min(index(train_fh))," ",max(index(train_fh))))
    print(paste0("prophet forecast range: ",min(index(pred_prophet))," ",max(index(pred_prophet))))
    print(paste0("hybrid forecast range: ",min(index(pred_forecastHybrid))," ",max(index(pred_forecastHybrid))))
    print(paste0("arima forecast range: ",min(index(pred_auto.arima))," ",max(index(pred_auto.arima))))
    print(paste0("test range: ",min(index(test))," ",max(index(test))))
    print(paste0("number of periods to forecast: ",test_periods))
    print(paste0("full bound forecasts length: ",length(unique(do.call(c,list(index(pred_prophet),index(pred_forecastHybrid),index(pred_auto.arima),index(test)))))))
  }
  pred_merged = cbind.zoo(pred_auto.arima,pred_forecastHybrid,pred_prophet,test,all = FALSE) %>%
    data.frame(.,row.names=NULL)
  pred_merged$dt = index(pred_merged)
  return(pred_merged)
}

fn_get_raw_data <- function(num_raw_rows=1E7){
  return(fread('data/city-of-seattle/seattle-road-weather-information-stations/road-weather-information-stations.csv',
               nrows = num_raw_rows,header = TRUE, drop = c("StationLocation")) %>%
           unique())
}

############## MAIN METHOD ##############
fn_compare_forecasts_main <- function(read_raw_data=FALSE,raw_data=data.frame(),num_raw_rows=1E7,
                                      arg_train_lengths=c(24*7,24*7*4,24*7*16),
                                      arg_test_lengths=c(1,4,24,24*7),
                                      arg_num_reps=20){
  if(read_raw_data){
    raw_seattleWeather = fread('data/city-of-seattle/seattle-road-weather-information-stations/road-weather-information-stations.csv',
          nrows = num_raw_rows,header = TRUE, drop = c("StationLocation")) %>%
      unique()
  } else{
    raw_seattleWeather = raw_data
  } 
  
  hourly_seattleWeather = raw_seattleWeather %>%
    mutate(date_hour = fn_convert_datetime_to_datehour(DateTime)) %>%
    mutate(date_hour=as_datetime(date_hour)) %>%
    group_by(StationName,date_hour) %>%
    summarise(air_temp_mean=mean(AirTemperature),air_temp_median=median(AirTemperature),num_obs=n()) %>%
    arrange(StationName,date_hour)
  
  train_data = hourly_seattleWeather
  
  # create parameter lists to use to sample from
  station_names = train_data %>% select(StationName) %>% unique()
  station_names = as.vector(station_names[['StationName']])
  train_lengths = arg_train_lengths
  test_lengths = arg_test_lengths
  num_reps = arg_num_reps
  
  # prepare containers to hold results
  vec_station_names = c()
  vec_train_lengths = c()
  vec_test_lengths = c()
  vec_split_times = c()
  vec_methods = c()
  method_list = c("pred_auto.arima","pred_forecastHybrid","pred_prophet")
  vec_mape = c()
  # generate results
  for(sn in station_names){
    for(trl in train_lengths){
      for(tel in test_lengths){
        for(j in seq(1:num_reps)){
          fits = fn_generate_fcst_df(train_data,sn,trl,tel)
          cutoff_time = min(fits$dt)
          for(m in method_list){
            vec_mape = append(vec_mape,MAPE(fits[,m],fits$test))
            vec_methods = append(vec_methods,m)
            vec_station_names = append(vec_station_names,sn)
            vec_train_lengths = append(vec_train_lengths,trl)
            vec_test_lengths = append(vec_test_lengths,tel)
            vec_split_times = append(vec_split_times,cutoff_time)
          }
        }
      }
    }
  }
  # capture results in a data frame
  raw_results = data.frame(station_name=vec_station_names,train_length=vec_train_lengths,
                           test_length=vec_test_lengths,split_time=vec_split_times,
                           forecast_method=vec_methods,mape=vec_mape,stringsAsFactors = FALSE) 
  return(raw_results)
}
