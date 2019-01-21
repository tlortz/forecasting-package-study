library(dplyr)
library(ggplot2)
library(data.table)

output_files = c()
for (f in list.files("output/",pattern = "output_*")){
  output_files = append(output_files,paste0("output/",f))
}

model_results = do.call(rbind, lapply(output_files, fread))

# plot overall
ggplot(data = model_results,aes(x=forecast_method,y=mape)) +
  geom_boxplot()

# plot for each train_length and test_length: 
# does the amount of historical data matter?
# does the forecast length matter?

############## plot for point accuracy ##############
ggplot(data = model_results,aes(x=forecast_method,y=mape)) +
  geom_violin(aes(color=forecast_method)) +
  facet_grid(train_length ~ test_length,scales = "free",
             labeller = label_both) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("MAPE vs. Method by Training and Test Lengths")

############## marginal accuracy over training length ##############
ggplot(data = model_results,aes(x=train_length,y=mape)) +
  geom_quantile() +
  facet_grid(~forecast_method) +
  ggtitle("MAPE vs. Training Length by Method")

ggplot(data = model_results,aes(x=test_length,y=mape)) +
  geom_quantile() +
  facet_grid(~forecast_method) +
  ggtitle("MAPE vs. Test Length by Method")

############## plot for interval accuracy ##############
ggplot(data = model_results,aes(x=forecast_method,y=interval_accuracy)) +
  geom_violin(aes(color=forecast_method)) +
  facet_grid(train_length ~ test_length,scales = "free",
             labeller = label_both) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Interval Accuracy vs. Method by Training and Test Lengths")

ggplot(data = model_results,aes(x=train_length,y=interval_accuracy)) +
  geom_quantile(quantile=c(.05,.5,.95)) +
  facet_grid(~forecast_method) +
  ggtitle("Interval Accuracy vs. Training Length by Method")

ggplot(data = model_results,aes(x=test_length,y=interval_accuracy)) +
  geom_quantile(quantile=c(.05,.5,.95)) +
  facet_grid(~forecast_method) +
  ggtitle("Interval Accuracy vs. Test Length by Method")

############## model-based assessment of MAPE accuracy ##############
library(caret)
lm_mape = lm(mape~forecast_method+train_length+test_length,data=model_results)
summary(lm_mape)
lm_mape$coefficients
anova(lm_mape)
# try a cross-validated model
lm_mape_cv = train(mape~forecast_method+train_length+test_length,model_results,
                   method="lm",trControl=trainControl(method = "boot",
                                                      number = 10))
summary(lm_mape_cv$finalModel)
lm_mape_cv$finalModel$coefficients

lm_interval = glm(interval_accuracy~forecast_method+train_length+test_length,
                  data=model_results)
summary(lm_interval)
lm_interval$coefficients

############## examine any artifacts over time that might cause errors ##############
fn_convert_datetime_to_datehour = Vectorize(fn_convert_datetime_to_datehour)
model_results$date_hour  = fn_convert_datetime_to_datehour(model_results$split_time)
model_results$split_date = as_date(model_results$date_hour)
model_summaries_by_date = model_results %>%
  group_by(forecast_method,split_date) %>%
  summarise(mape_avg = mean(mape))
ggplot(data = model_summaries_by_date,aes(split_date,mape_avg)) +
  geom_col() +
  facet_grid(forecast_method~.) +
  ggtitle("Average MAPE by Date of Train/Test Split")