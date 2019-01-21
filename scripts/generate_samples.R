source('scripts/compare_forecasts.R')
library(data.table)

raw_data = fn_get_raw_data()

clean_data = fn_fill_ts_na(raw_data)

rm(raw_data)
gc()

station_names = clean_data %>% select(StationName) %>% unique()
station_names = as.vector(station_names[['StationName']])
model_results_list = list()
for (s in station_names){
  set.seed(as.integer(Sys.time()))
  model_results_list[[s]] = 
    fn_compare_forecasts_main(raw_data = clean_data[clean_data$StationName==s,],
                              arg_train_lengths = c(24,168,168*4,168*4*4),
                              arg_test_lengths = c(4,24,168,168*4),
                              arg_num_reps = 3)
  gc()
}

# had to limit the number of reps per batch to 3-5; otherwise, Prophet would almost certainly 
# throw a c++ sampler error before finishing the execution. Even at the 3-5 rep range, it would
# still throw the error, but much less often. This is apparently a known issue per
# https://github.com/facebook/prophet/issues/93

model_results = do.call(rbind,model_results_list)

# fwrite(model_results,file = "data/output_01.csv")
# fwrite(model_results,file = "data/output_02.csv")
# fwrite(model_results,file = "data/output_03.csv")
# fwrite(model_results,file = "data/output_04.csv")
# fwrite(model_results,file = "data/output_05.csv")
fwrite(model_results,file = "data/output_06.csv")

# model_results = fn_compare_forecasts_main(raw_data = clean_data,
#                                           arg_train_lengths = c(24,168,168*4,168*4*4),
#                                           arg_test_lengths = c(4,24,168,168*4),
#                                           arg_num_reps = 10)

# model_results_2 = fn_compare_forecasts_main(raw_data = clean_data,
#                                           arg_train_lengths = c(168),
#                                           arg_test_lengths = c(24),
#                                           arg_num_reps = 1)
