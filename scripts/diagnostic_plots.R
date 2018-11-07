library(dplyr)
library(ggplot2)

source('scripts/compare_forecasts.R')

raw_data = fn_get_raw_data()

model_results = fn_compare_forecasts_main(raw_data = raw_data,
                                          arg_train_lengths = c(168),
                                          arg_test_lengths = c(500),
                                          arg_num_reps = 1)