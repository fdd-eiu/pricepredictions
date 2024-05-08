#ARIMA Price Analysis 2024
# See https://otexts.com/fpp2/ Hyndman's Forecasting textbook
rm(list=ls())

# Call libraries

library(tidyverse)
library(forecast)
library(zoo)
library(tseries) # now we use this
library(readxl) # we have used this
library(writexl)
library(CADFtest)
library(ggplot2)
library(scales) # For date formatting
library(openxlsx)

#Set your directory where you stored the data
setwd("C:/Users/thomas.ruaia/Documents/FFA/PNA/Les/Peter Terewasi/Price Predictions/data")

# Read in data

#df <- read_excel("tunaprice_data.xlsx")
df <- read.csv("tunaprice_data.csv")
# Clean data 

#############################################
################# skj_bkk ###################
#############################################
dim(df)
head(df)
tail(df)
data.skj_bkk <- df[,c("yr","month","skj_bkk")] # filtering and selecting only skj_bkk
df2 <- data.skj_bkk
head(df2)
tail(df2)
# Convert to time series object
skjbkk_ts <- ts(df2[,3],start=c(1984,1),end=c(2024,3),frequency=12)
skjbkk_ts
plot(skjbkk_ts)
dev.off()
# Stationarity tests- Augmented Dickey Fuller test(adf)

#If the p-value is less than 0.05, reject the null hypothesis that a unit root is present, indicating the series is stationary.
adf <- adf.test(skjbkk_ts,alternative="stationary")
adf # p-value = 0.1081, hence not stationary, so takes the first difference

#First difference
d1_skjbkk <- diff(skjbkk_ts)
adf <- adf.test(d1_skjbkk, alternative = "stationary")
adf # p-value = 0.01, hence less than 0.05, so now stationary

#plot ACF and PACF
par(mfrow=c(1,2))
Acf(skjbkk_ts) # need to difference more
Pacf(skjbkk_ts)
dev.off()
# From both ACF and PACF plots, the data may follow an ARIMA(0,d,q)model: The reasons as follows

#The data may follow an ARIMA(p,d,0) model if the ACF and PACF plots of the differenced data show the following patterns:

#the PACF is exponentially decaying or sinusoidal
#there is a significant spike at lag p in the ACF, but none beyond lag p

###Hence PACF suggest AR(6), ACF suggest of q=1 i.e 6 spikes and d=1(No of differencing needed for stationarity) = ARIMA(4,1,1)
#####NONSEASONAL ARIMA MODEL ARIMA(p,d,q)
##NOTES##
###ARIMA notes: ARIMA(p,d,q)" model where 
#p is the number of autoregressive terms, 
#d is the number of nonseasonal differences needed for stationarity, 
#q is the number of lagged forecast errors in the prediction equation.

###OR###
###ARIMA(AR,I,MA) where 
#p= AR(Auto Regressive)- look at PACF
#d= I (Intergrative part of the model)
#q= MA Moving Average - look at ACF

Box.test(diff(skjbkk_ts),lag=1,type="Ljung-Box") # 1 lag ok but first difference
#Ljung-Box test Ljung-Box test examines whether there is significant evidence for non-zero correlation at lags 1-20. Small p-values (i.e., less than 0.05) suggest that the series is stationary.
# with the box test with first differencing, p-value is 7.283e-11, hence stationary

auto.arima(skjbkk_ts) # this says order  (0,1,4)(0,0,1)[12] but above analaysis of ACf and Pacf suggests 

# Fit ARIMA model
fit_skjbkk <- arima(skjbkk_ts, order = c(6, 1, 3), seasonal = list(order = c(0, 1, 1), period = 12))

#fit <- arima(skj_ts,order=c(3,1,0))

#  Plot ARIMA model
skjbkk_forecast <-plot(forecast(fit_skjbkk), xlab = "", ylab ="US$ per metric tonne")
#grid(lty=1, col=gray(.8)) # add a grid
#lines(skjbkk_ts, col=4)# add blue colour line 
#lines(skjbkk_ts,col="red")

###USE AIC Akaike's An Information Criterion to select the best model- the lower the AIC the more reliable is the model
AIC(fit_skjbkk)
#Arima (0,1,4)(0,0,1)[12] AIC 5833.669# suggested by autoarima
#Arima (3,1,1)(0,0,1)[12 AIC 5830.856
#Arima (3,1,1) AIC 5837.973
#Arima (3,1,3)(0,1,1)[12] AIC 5719.045

#Arima (4,1,1) AIC 5838.163
#Arima (4,1,3) AIC 5842.502
#Arima (4,1,3)(0,0,1)[12] AIC 5832.954
#Arima (4,1,3)(0,1,1)[12] AIC 5715.974
#Arima (4,1,3)(0,1,2)[12] AIC 5716.613
#Arima (4,1,3)(0,1,3)[12] AIC 5718.602

#Arima (4,1,3)(1,1,1)[12] AIC 5716.637
#Arima (4,1,3)(1,1,2)[12] AIC 5718.78
#Arima (4,1,3)(1,1,3)[12] AIC 5720.535
#Arima (4,1,3)(4,1,1)[12] AIC 5721.161
#Arima (4,1,3)(2,1,1)[12] AIC 5715.713
#Arima (4,1,3)(2,1,2)[12] AIC 5720.616

#Arima (4,1,1)(0,0,1)[12] AIC 5835.207
#Arima (4,1,26)(0,0,1)[12] AIC 5832.967
#Arima (4,1,26)(0,0,2)[12] AIC 5834.957

#Arima (5,1,3)(0,1,1)[12] AIC 5714.456
#Arima (5,1,3)(0,1,2)[12] AIC 5716.369

#Arima (6,1,1)(0,1,1)[12] AIC 5719.443
#Arima (6,1,2)(0,1,1)[12] AIC 5723.056
#Arima (6,1,3)(0,1,1)[12] AIC 5711.616 lowest AIC- best model
#Arima (6,1,4)(0,1,1)[12] AIC 5718.484

#Arima (7,1,3)(0,1,1)[12] AIC 5717.92

##view all forecasted values with their lower and upper limits
skjbkk_forecast

# Extract forecasted values
forecasted_values <- skjbkk_forecast$mean
print(forecasted_values)


#############################################
################# skj_thai ###################
#############################################

dim(df)
head(df)
tail(df)
data.skj_thai <- df[193:483,c("yr","month","skj_thai")] #filtering and selecting only skj_thai data
df3 <- data.skj_thai
head(df3)
tail(df3)
# Convert to time series object
skjthai_ts <- ts(df3[,3],start=c(2000,1),end=c(2024,3),frequency=12)
skjthai_ts
plot(skjthai_ts, main = "Skipjack Thai Import Prices", xlab = "", ylab ="US$ per metric tonne")
dev.off()
# Stationarity tests- Augmented Dickey Fuller test(adf)

#If the p-value is less than 0.05, reject the null hypothesis that a unit root is present, indicating the series is stationary.
adf <- adf.test(skjthai_ts,alternative="stationary")
adf # p-value = 0.3476( not stationary, hence take the first differencing)

#First difference
d1_skjthai <- diff(skjthai_ts)
adf1 <- adf.test(d1_skjthai, alternative = "stationary")
adf1 # p-value = 0.01 hence less than 0.05, so now stationary

#plot ACF and PACF
par(mfrow=c(1,2))
Acf(skjthai_ts) # need to difference more
Pacf(skjthai_ts)
dev.off()
# From both ACF and PACF plots, the data may follow an ARIMA(0,d,q)model: The reasons as follows

#The data may follow an ARIMA(p,d,0) model if the ACF and PACF plots of the differenced data show the following patterns:

#the PACF is exponentially decaying or sinusoidal
#there is a significant spike at lag p in the ACF, but none beyond lag p

#####NONSEASONAL ARIMA MODEL ARIMA(p,d,q)
##NOTES##
###ARIMA notes: ARIMA(p,d,q)" model where 
#p is the number of autoregressive terms, 
#d is the number of nonseasonal differences needed for stationarity, 
#q is the number of lagged forecast errors in the prediction equation.

###OR###
###ARIMA(AR,I,MA) where 
#p= AR(Auto Regressive)- look at PACF (2 spikes-Arima(2))
#d= I (Intergrative part of the model)
#q= MA Moving Average - look at ACF

Box.test(diff(skjthai_ts),lag=12,type="Ljung-Box") # 1 lag ok but first difference
#Ljung-Box test Ljung-Box test examines whether there is significant evidence for non-zero correlation at lags 1-20. Small p-values (i.e., less than 0.05) suggest that the series is stationary.
# with the box test with first differencing, p-value is 0.0181, hence stationary

auto.arima(skjthai_ts) # this says order  (0,1,0)(0,0,1)[12] but above analaysis of ACf and Pacf suggests Arima (2,1,3)

# Fit ARIMA model
fit_skjthai <- arima(skjthai_ts, order = c(2, 1, 5), seasonal = list(order = c(0, 1, 2), period = 12))

#  Plot ARIMA model
skjthai_forecast <-plot(forecast(fit_skjthai), xlab = "", ylab ="US$ per metric tonne")
#grid(lty=1, col=gray(.8)) # add a grid
#lines(skjbkk_ts, col=4)# add blue colour line 
#lines(skjbkk_ts,col="red")

###USE AIC Akaike's An Information Criterion to select the best model- the lower the AIC the more reliable is the model
AIC(fit_skjthai)
#Arima (0,1,0)(0,0,1)[12] AIC 3584.464# suggested by autoarima
#Arima (2,1,1)(0,1,1)[12] AIC 3470.374
#Arima (2,1,2)(0,1,1)[12] AIC 3470.958
#Arima (2,1,3)(0,1,1)[12] AIC 3468.944
#Arima (2,1,3)(0,1,2)[12] AIC 3466.893 
#Arima (2,1,3)(0,1,3)[12] AIC 3468.89
#Arima (2,1,5)(0,1,1)[12] AIC 3466.353 
#Arima (2,1,5)(0,1,2)[12] AIC 3465.868#### low AIC best model
#Arima (2,1,5)(0,1,3)[12] AIC 3467.618

##view all forecasted values with their lower and upper limits
skjthai_forecast

# Extract forecasted values
forecasted1_values <- skjthai_forecast$mean
print(forecasted1_values)



#############################################
################# yft_thai ###################
#############################################

dim(df)
head(df)
tail(df)
data.yft_thai <- df[193:483,c("yr","month","yft_thai")] # filtering and selecting only yft_thai data
df4 <- data.yft_thai
head(df4)
tail(df4)
# Convert to time series object
yftthai_ts <- ts(df4[,3],start=c(2000,1),end=c(2024,3),frequency=12)
yftthai_ts
plot(skjthai_ts, main = "Skipjack Thai Import Prices", xlab = "", ylab ="US$ per metric tonne")
dev.off()
# Stationarity tests- Augmented Dickey Fuller test(adf)

#If the p-value is less than 0.05, reject the null hypothesis that a unit root is present, indicating the series is stationary.
adf <- adf.test(yftthai_ts,alternative="stationary")
adf # p-value = 0.3648( not stationary, hence take the first differencing)

#First difference
d1_yftthai <- diff(yftthai_ts)
adf1 <- adf.test(d1_yftthai, alternative = "stationary")
adf1 # p-value = 0.01 hence less than 0.05, so now stationary

#plot ACF and PACF
par(mfrow=c(1,2))
Acf(yftthai_ts) # need to difference more
Pacf(yftthai_ts)
dev.off()
# From both ACF and PACF plots, the data may follow an ARIMA(0,d,q)model: The reasons as follows

#The data may follow an ARIMA(p,d,0) model if the ACF and PACF plots of the differenced data show the following patterns:

#the PACF is exponentially decaying or sinusoidal
#there is a significant spike at lag p in the ACF, but none beyond lag p

#####NONSEASONAL ARIMA MODEL ARIMA(p,d,q)
##NOTES##
###ARIMA notes: ARIMA(p,d,q)" model where 
#p is the number of autoregressive terms, 
#d is the number of nonseasonal differences needed for stationarity, 
#q is the number of lagged forecast errors in the prediction equation.

###OR###
###ARIMA(AR,I,MA) where 
#p= AR(Auto Regressive)- look at PACF (1 spikes-Arima(1))
#d= I (Intergrative part of the model)
#q= MA Moving Average - look at ACF

Box.test(diff(yftthai_ts),lag=1,type="Ljung-Box") # 1 lag ok but first difference
#Ljung-Box test Ljung-Box test examines whether there is significant evidence for non-zero correlation at lags 1-20. Small p-values (i.e., less than 0.05) suggest that the series is stationary.
# with the box test with first differencing, p-value is 0.01637, hence stationary

auto.arima(yftthai_ts) # this says order  (1,1,1) but above analaysis of ACf and Pacf suggests Arima (1,1,3)

# Fit ARIMA model
fit_yftthai <- arima(skjthai_ts, order = c(1, 1, 3), seasonal = list(order = c(1, 1, 1), period = 12))

#  Plot ARIMA model
yftthai_forecast <-plot(forecast(fit_yftthai), xlab = "", ylab ="US$ per metric tonne")
#grid(lty=1, col=gray(.8)) # add a grid
#lines(skjbkk_ts, col=4)# add blue colour line 
#lines(skjbkk_ts,col="red")

###USE AIC Akaike's An Information Criterion to select the best model- the lower the AIC the more reliable is the model
AIC(fit_yftthai)
#Arima (1,1,1)# AIC 3592 suggested by autoarima
#Arima (1,1,1)(0,1,0)[12] AIC 3470.374
#Arima (1,1,1)(1,1,0)[12] AIC 3533.832
#Arima (1,1,1)(1,1,1)[12] AIC 3467.047 ###
#Arima (1,1,1)(1,1,2)[12] AIC 3469.054 
#Arima (1,1,1)(1,1,3)[12] AIC 3470.571

#Arima (1,1,2)(1,1,0)[12] AIC 3530.932
#Arima (1,1,2)(1,1,1)[12] AIC 3468.524 ###
#Arima (1,1,2)(1,1,2)[12] AIC 3469.246
#Arima (1,1,2)(1,1,3)[12] AIC 3471.47

#Arima (1,1,3)(1,1,0)[12] AIC 3531.358
#Arima (1,1,3)(1,1,1)[12] AIC 3466.093 ### Low AIC-better model
#Arima (1,1,3)(1,1,2)[12] AIC 3468.052
#Arima (1,1,3)(1,1,3)[12] AIC 3469.696

#Arima (1,1,4)(1,1,0)[12] AIC 3533.004
#Arima (1,1,4)(1,1,1)[12] AIC 3467.328 ###
#Arima (1,1,4)(1,1,2)[12] AIC 3469.295
#Arima (1,1,4)(1,1,3)[12] AIC 3470.873


##view all forecasted values with their lower and upper limits
yftthai_forecast

# Extract forecasted values
forecasted2_values <- yftthai_forecast$mean
print(forecasted2_values)

dev.off()# remove the plot

#Export forecasted data into one excel spreadsheet
wb <- createWorkbook()
addWorksheet(wb, "skjbkk")
addWorksheet(wb, "skjthai")
addWorksheet(wb, "yftthai")

writeData(wb, 1,skjbkk_forecast)
writeData(wb, 2,skjthai_forecast)
writeData(wb, 3,yftthai_forecast)

saveWorkbook(wb, file = "C:/Users/thomas.ruaia/Documents/FFA/PNA/Les/Peter Terewasi/Price Predictions/data//arima_tuna_price_forecast_2024.xlsx", overwrite = TRUE) 

#plot skjbkk, skjthai & yftthai

 # Indivdual plots
skjbkk_forecast <-plot(forecast(fit_skjbkk), main="Skipjack Bangkok prices forecast 2024", xlab = "", ylab ="US$ per metric tonne")
skjthai_forecast <-plot(forecast(fit_skjthai), main="Skijack Thai prices forecast 2024", xlab = "", ylab ="US$ per metric tonne")
yftthai_forecast <-plot(forecast(fit_yftthai), main="Yellowfin Thai prices forecast 2024", xlab = "", ylab ="US$ per metric tonne")

# Combine plots
par(mfrow=c(1,3))
skjbkk_forecast <-plot(forecast(fit_skjbkk), main="Skipjack Bangkok prices forecast 2024", xlab = "", ylab ="US$ per metric tonne")
skjthai_forecast <-plot(forecast(fit_skjthai), main="Skijack Thai prices forecast 2024", xlab = "", ylab ="US$ per metric tonne")
yftthai_forecast <-plot(forecast(fit_yftthai), main="Yellowfin Thai prices forecast 2024", xlab = "", ylab ="US$ per metric tonne")

