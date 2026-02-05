# ==============================================================================
# Script Name: 05_figure4_polar.R
# Description: arim_model
# Author: Yuan-Hai Zhou & Yu-Hong Qin
# ==============================================================================


# 1. Environment Setup ----------------------------------------------------
rm(list = ls())

# Load necessary libraries

library(forecast)
library(ggtext)
library(patchwork)
library(scales)
library(tidyverse)
library(ggspatial)
library(RColorBrewer)
library(sjmisc)
library(readxl)
library(cowplot)
library(ggbreak)
library(grid)
library(ggsci)

colnames(all_data)

set.seed(52445443)

all_data <- read.csv("/Users/zhouyuanhai/Desktop/桌面 - Mac/投稿/01-我的期刊投稿/乙类传染病负担及新冠/03-nature comunication/data/mold_data.csv")

# Data frame for ARIMA validation set accuracy
validation_accuracy <- data.frame()

# Data frame for ARIMA model information
model_info <- data.frame()

# Store residuals for each disease on the [Training Set]
all_residuals_list <- list()

# Store validation set results
validation_forecasts_list <- list()

# Store final forecast results
final_forecasts_list <- list()

#Initialize list for fitted data
fitted_data_list <- list()

# Get all unique disease names for the loop
unique_metrics <- unique(all_data$metric2)

# 2. Begin loop for modeling and forecasting each disease
for (disease in unique_metrics) {
  
  cat("Processing disease: ", disease, "\n")
  
  tryCatch({
    
    disease_data <- all_data %>% 
      filter(metric2 == disease) %>% 
      arrange(time)
    
    training_data <- disease_data %>% filter(time >= 2004 & time <= 2017)
    validation_data <- disease_data %>% filter(time >= 2018 & time <= 2019)
    
    if(nrow(training_data) < 10) {
      cat(">> Insufficient data, skipping disease: ", disease, "\n")
      next
    }
    
    ts_train <- ts(training_data$incidence_rate, start = 2004, frequency = 1)
    arima_model <- auto.arima(ts_train, stepwise = FALSE, approximation = FALSE)
    
    train_residuals <- residuals(arima_model)
    
    residuals_df <- data.frame(
      metric = disease,
      time = training_data$time, 
      residual = as.numeric(train_residuals)
    )
    
    all_residuals_list[[disease]] <- residuals_df
    
    validation_forecast <- forecast(arima_model, h = 2)
    ts_validation_actual <- validation_data$incidence_rate
    acc_metrics <- accuracy(validation_forecast, ts_validation_actual)
    
    validation_accuracy <- rbind(validation_accuracy, data.frame(
      metric = disease,
      RMSE = acc_metrics[2, "RMSE"],
      MAE = acc_metrics[2, "MAE"],
      MAPE = acc_metrics[2, "MAPE"]
    ))
    
    validation_forecast_df <- data.frame(
      metric = disease,
      time = 2018:2019,
      predicted_incidence_rate = as.numeric(validation_forecast$mean)
    )
    validation_forecasts_list[[disease]] <- validation_forecast_df
    
    full_history_data <- disease_data %>% filter(time >= 2004 & time <= 2019)
    ts_full <- ts(full_history_data$incidence_rate, start = 2004, frequency = 1)
    final_model <- auto.arima(ts_full, stepwise = FALSE, approximation = FALSE)
    
    fitted_values <- fitted(final_model)
    
    fitted_df <- data.frame(
      metric = disease,
      time = 2004:2019,
      fitted_incidence_rate = as.numeric(fitted_values),
      observed_incidence_rate = as.numeric(ts_full) 
    )
    
    fitted_data_list[[disease]] <- fitted_df
    
    model_info <- rbind(model_info, data.frame(
      metric = disease,
      model = as.character(final_model),
      AICc = final_model$aicc
    ))
    
    final_forecast <- forecast(final_model, h = 4)
    
    forecast_df <- data.frame(
      metric = disease,
      time = 2020:2023,
      predicted_incidence_rate = as.numeric(final_forecast$mean),
      Lower_95 = as.numeric(final_forecast$lower[, "95%"]),
      Upper_95 = as.numeric(final_forecast$upper[, "95%"])
    )
  
    forecast_df$predicted_incidence_rate[forecast_df$predicted_incidence_rate < 0] <- 0
    # forecast_df$Lower_95[forecast_df$Lower_95 < 0] <- 0
    final_forecasts_list[[disease]] <- forecast_df
    
  }, error = function(e) {
    cat(" >> Error processing: ", disease, " - Error message:", e$message, "\n")
  })
}

# Combined residuals
final_residuals_df <- bind_rows(all_residuals_list)

# Combined validation set forecasts
validation_forecasts_df <- bind_rows(validation_forecasts_list)

# Combined final forecast results
final_forecasts_df <- bind_rows(final_forecasts_list)

# Combined fitted data
fitted_df <-  bind_rows(fitted_data_list)

# Validation set performance metrics
validation_accuracy_sen2 <- validation_accuracy



write.csv(final_residuals_df,"*****/training_set_residuals.csv")
write.csv(validation_forecasts_df,"*****/validation_set_predictions.csv")
write.csv(final_forecasts_df,"*****/final_forecast_results.csv")
write.csv(all_data,"******/original_data.csv")



