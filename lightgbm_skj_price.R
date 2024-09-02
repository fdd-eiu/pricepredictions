#Clear the workspace
rm(list=ls())

# Set a random seed for reproducibility
set.seed(123)

# Load libraries
library(lightgbm)
library(lubridate)
library(data.table)
library(dplyr)
library(ggplot2)
library(readxl) # we have used this
library(writexl)
#set working directory 
setwd("C:/Users/thomas.ruaia/Documents/FFA/Data/Member request/PNG/Prediction model/data")

# Load the data
data <- read_excel("lightgbm_skj_price_data.xlsx")
data
tail(data)
# Convert 'yr' and 'month' into a Date format
data$date <- as.Date(paste(data$yr, data$month, "01", sep = "-"), format = "%Y-%b-%d")

# Sort the data by date
data <- data[order(data$date), ]
data

# Drop unnecessary columns (yr and month)
#data <- data[, c("yr", "month") := NULL]
data <- data %>% 
  select(-yr, -month)
data

# Set target variable and exogenous variables
target <- "skj_thai_price_mt"
exogenous <- c("fuel_data_mt", "crude_oil", "covid_19")

# Create training and testing sets
train_data <- data[1:(nrow(data)-11), ]  # Use all but the last 12 months for training
train_data
tail(train_data)

test_data <- data[(nrow(data)-294):nrow(data), ]  # Use all data for testing
test_data
tail(test_data)

# Prepare the data for LightGBM
train_matrix <- lgb.Dataset(data = as.matrix(train_data[, exogenous]), label = train_data[[target]])

# Set the parameters for LightGBM
params <- list(
  objective = "regression",
  metric = "rmse",
  num_leaves = 31,
  learning_rate = 0.05,
  feature_fraction = 0.9,
  seed = 123 # Se the seed to ensure reproducibility of LighGBM moel and other random processes # reproducible refers to the consistently obtain the same results when running the same code/analysis
)

# Train the model
lgb_model <- lgb.train(params, train_matrix, nrounds = 100)


# Forecast the next 12 months using the trained model
forecast_matrix <- as.matrix(test_data[, exogenous])
predictions <- predict(lgb_model, forecast_matrix)

# Combine predictions with actual dates
forecast_results <- data.frame(
  date = test_data$date,
  actual = test_data[[target]],
  predicted = predictions
)

# Define plotting theme for consistency
gg.theme <- theme(axis.line = element_line(color="black"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  text = element_text(size=12),
                  axis.title = element_text(size=12),
                  strip.background =element_rect(fill="white"),
                  strip.text = element_text(size=12),
                  legend.box.background = element_rect(colour = "black"),
                  legend.background = element_blank(),
                  legend.text = element_text(size=8),
                  legend.title=element_blank(),
                  legend.position = 'top',
                  panel.background = element_rect(fill = NA, color = "black")) 


print(forecast_results)


# Plot the actual vs. predicted values
ggplot(forecast_results, aes(x = date)) +
  geom_line(aes(y = actual, color = "Actual")) +
  geom_line(aes(y = predicted, color = "Predicted")) +
  labs(title = "Skipjack Price: Actual vs Predicted Data", x = "Date", y = "Price in Metric Tons") +
  scale_color_manual(name = "Legend", values = c("Actual" = "blue", "Predicted" = "red")) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +  # Add year labels
  gg.theme +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


###########################################
##########################################
##Extend the dataset with future dates###
###########################################
##########################################

# Determine the last date in the dataset
last_date <- max(data$date)

# Create a sequence of the next 12 months
future_dates <- seq(last_date %m+% months(1), by = "month", length.out = 12)
future_dates

# Calculate the mean of the recent 12 periods for crude_oil and covid_19
recent_12_mean_crude_oil <- mean(tail(data$crude_oil, 12), na.rm = TRUE)
recent_12_mean_fuel_data_mt <- mean(tail(data$fuel_data_mt, 12), na.rm = TRUE)
recent_12_mean_covid_19 <- mean(tail(data$covid_19, 12), na.rm = TRUE)

# Prepare a new data frame for the future dates
future_data <- data.frame(
  date = future_dates,
  crude_oil = NA,  # Initially set to NA
  covid_19 = NA,   # Initially set to NA
  fuel_data_mt = NA, # Initially set to NA
  skj_thai_price_mt = NA # Add this column to match the test_data structure
  )

# Use the mean of the recent 12 periods as the basis for future data
future_data$crude_oil <- recent_12_mean_crude_oil * (1 + rnorm(12, mean = 0, sd = 0.05))  # 5% variability
future_data$crude_oil
future_data$fuel_data_mt <- recent_12_mean_fuel_data_mt * (1 + rnorm(12, mean = 0, sd = 0.05))   # 5% variability
future_data$fuel_data_mt
future_data$covid_19 <- recent_12_mean_covid_19 * (1 + rnorm(12, mean = 0, sd = 0.05))   # 5% variability
future_data$covid_19


# Print the updated future_data to verify
print(future_data)

# Combine the future data with the test data
combined_data <- rbind(test_data, future_data)

# Prepare the data for forecasting the next 12 months
forecast_matrix_future <- as.matrix(combined_data[(nrow(test_data)+1):nrow(combined_data), exogenous])
forecast_matrix_future

# Use the trained model to forecast the skj price for the next 12 months
future_predictions <- predict(lgb_model, forecast_matrix_future)
future_predictions

# Add the predictions to the future_data
combined_data$fuel_data_mt[(nrow(test_data)+1):nrow(combined_data)] <- future_predictions

# Extract the relevant forecast results
forecast_results_future <- data.frame(
  date = combined_data$date[(nrow(test_data)+1):nrow(combined_data)],
  predicted = future_predictions
)

# Print the forecast results
print(forecast_results_future)


# Plot the future predicted values
ggplot(forecast_results_future, aes(x = date, y = predicted)) +
  geom_line(color = "gold3") +
  labs(title = "Forecasted skipjack price for the next 12 Months", x = "Date", y = "Price in Metric Tons") +
  gg.theme +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))



####################################################################

# Add the predictions to the future_data
combined_data$fuel_data_mt[(nrow(test_data)+1):nrow(combined_data)] <- future_predictions

# Extract the relevant forecast results
forecast_results_future <- data.frame(
  date = combined_data$date[(nrow(test_data)+1):nrow(combined_data)],
  forecasted = future_predictions
)

# Combine actual, predicted, and forecasted data into one data frame for plotting
all_results <- rbind(
  data.frame(date = forecast_results$date, value = forecast_results$actual, type = "Actual"),
  data.frame(date = forecast_results$date, value = forecast_results$predicted, type = "Predicted"),
  data.frame(date = forecast_results_future$date, value = forecast_results_future$forecasted, type = "Forecasted")
)

# Plot the actual, predicted, and forecasted values
ggplot(all_results, aes(x = date, y = value, color = type)) +
  geom_line(size = 1) + #Increase line size
  labs(title = "Skipjack Price Forecast using LightGBM Model", x = "", y = "US$ per metric tonne") +
  scale_color_manual(name = "Legend", values = c("Actual" = "blue", "Predicted" = "red", "Forecasted" = "gold3")) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +  # Add year labels
  gg.theme +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

#dev.off()

# export data to excel

# Define the file path
path <- "C:/Users/thomas.ruaia/Documents/FFA/Data/Member request/PNG/Prediction model/data/skj_price_forecast_lightgbm.xlsx"

# Write the forecast values to an Excel file
write_xlsx(as.data.frame(all_results), path)

