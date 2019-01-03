library(dplyr)
library(ggplot2)

source('scripts/compare_forecasts.R')

raw_data = fn_get_raw_data()

model_results = fn_compare_forecasts_main(raw_data = raw_data,
                                          arg_train_lengths = c(24,168,168*4,168*4*6),
                                          arg_test_lengths = c(4,24,168,168*4),
                                          arg_num_reps = 5)

# plot overall
ggplot(data = model_results,aes(x=forecast_method,y=mape)) +
  geom_boxplot()

# plot for each train_length and test_length: 
# does the amount of historical data matter?
# does the forecast length matter?

ggplot(data = model_results,aes(x=forecast_method,y=mape)) +
  geom_violin(aes(color=forecast_method)) +
  facet_grid(train_length ~ test_length,scales = "free",
             labeller = label_both) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# need to either rotate teh x-axis labels or remove them

# future improvements:
# - pre-clean the data and fill in missing values via interpolation, so that we don't keep crashing
# - build an xgb or lightgbm model with station as a categorical variable, and with decomposed time series componenets
## note that this would likely require a longer training set...or would it?
# - also try an LSTM model