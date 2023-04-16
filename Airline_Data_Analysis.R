library(tidyverse)
library(data.table)
library(lubridate)
library(timetk)
library(skimr)
library(highcharter)
library(h2o)
library(tidymodels)
library(modeltime)

path <- dirname(getSourceEditorContext()$path)
setwd(path)

raw <- fread('AirPassengers (3).csv')
raw %>% glimpse()
raw %>% skim()

#Changing the column names
colnames(raw) <- c('Date','Count')

#Changing the data type of the date column
raw$Date <- as.Date(paste0(raw$Date, "-01"), format = "%Y-%m-%d")

#Plotting the data. The seasonality increases with time.
raw %>% 
  plot_time_series(
    Date, Count, 
    .interactive = T,
    .plotly_slider = T,
    .smooth = T)

#Seasonality plots. We can see that the number of passengers is maximum in July and August. There is an obvious increasing trend through years.
raw %>%
  plot_seasonal_diagnostics(
    Date, Count, .interactive = T)

#Now we will create new features from the Date column
all_time_arg <- raw %>% tk_augment_timeseries_signature()

df <- all_time_arg %>%
  select(-contains("hour"),
         -contains("day"),
         -minute,-second,-am.pm) %>% 
  mutate_if(is.ordered, as.character) %>% 
  mutate_if(is.character,as_factor)

df %>% skim()

#Splitting into train and test data:
train <- raw %>% filter(Date < "1959-01-01")
test <- raw %>% filter(Date >= "1959-01-01")

#Q1. Use arima_boost(), exp_smoothing(), prophet_reg() models;

#Building the arima_boost() model
model_fit_arima_boosted <- arima_boost(
  min_n = 2,
  learn_rate = 0.015
) %>%
  set_engine(engine = "auto_arima_xgboost") %>%
  fit(Count ~ Date, train)

#Building the exp_smoothing() model
model_fit_ets <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(Count ~ Date, train)

#Building the prophet_reg() model
model_fit_prophet <- prophet_reg() %>%
  set_engine(engine = "prophet") %>%
  fit(Count ~ Date, train)

# calibration
calibration <- modeltime_table(
  model_fit_arima_boosted,
  model_fit_ets,
  model_fit_prophet) %>%
  modeltime_calibrate(test)

# Plotting the predictions of each model. We can see that Prophet is giving the best results.
calibration %>% 
  modeltime_forecast(actual_data = df) %>%
  plot_modeltime_forecast(.interactive = T,
                          .plotly_slider = T)

#Q2. Compare RMSE scores on test set; 
#Prophet has led to the lowest RMSE of 40.34.
calibration %>% modeltime_accuracy() %>% 
  table_modeltime_accuracy(.interactive = F)

#Q3. Make forecast on lowest RMSE score model; 
#We choose .model_id == 3 because this corresponds to the Prophet model
#We are making a forecast for the next 2 years.
forecast <- calibration %>%
  filter(.model_id %in% 3) %>% # best model
  modeltime_refit(df) %>%
  modeltime_forecast(h = "2 year", 
                     actual_data = df) %>%
  select(-contains("conf"))

forecast

#Q4. Visualize past data and forecast values on one plot; make separation with two different colors.
forecast %>% plot_modeltime_forecast(.interactive = T,
                                     .plotly_slider = T,
                                     .legend_show = T)
