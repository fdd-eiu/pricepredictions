#Thai monthly import data skipjack and yellowfin predictions

# Data preparation
# First import the Exchange rate data DEXTHUS from FRED
rm(list=ls())
library(fredr) #US Federal Reserve Database
library("RPostgreSQL") #To connect to our Database PostgreSQL drivers
library("forecast") #Rob Hyndman's Forecast packge
library("lubridate")
library(xlsx)    

#Quandl.api_key("xxx")

fredr_set_key("xxx") #set the API key like a password

#data <- Quandl('DEXTHUS', collapse = "monthly", type = "ts", limit = 450) An alternative source of data

# The following reads in the exchange rate data

data <- fredr(
  series_id = "DEXTHUS",
  observation_start = as.Date("2007-01-01"),
  observation_end = as.Date("2022-03-01"),
  frequency ="m"
)

data["value"]

data2 <- fredr(
  series_id = "DDFUELUSGULF",
  observation_start = as.Date("2007-01-01"),
  observation_end = as.Date("2022-03-01"),
  frequency ="m"
)  
  
# Read in the Skipjack import values and quantities from our database


pw <- {
  "xxx"}

drv <- dbDriver("PostgreSQL")

# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database

con2 = dbConnect(drv = drv, dbname = "xxx", host = "xxx", port = 5432,
                 user = "xxx", password = pw)

print(con2)

# Read annual prices and ace catch and days 

#df <- dbGetQuery(con2, "SELECT year, month, cif_baht, quantity FROM 'markets'.'bkkmonthlylprices' limit = 100 ")

df <- dbGetQuery(con2, "SELECT * FROM markets.bkkmonthlyprices WHERE species = 'yft' ")

#df.ace <- dbGetQuery(con2, "SELECT * from ace")
#df.fees <- dbGetQuery(con2,"SELECT lafr from edis.*")


rm(pw)
allcons <- dbListConnections(PostgreSQL())
for (con in allcons) {
  dbDisconnect(con)
}

dbDisconnect(con2)
dbUnloadDriver(drv)

df #return the data frame

# Calculate the prices and convert them to US dollars

# Baht per prices per tonne 

price_baht <- as.numeric(unlist(df["cif_baht"]))/as.numeric(unlist(df["quantity"]))*1000 
# Calculates the baht price per metric ton

xrate <- as.data.frame(data["value"]) #Extract the exchange rates obtained from FRED

yft_p <- price_baht/xrate # Calculates skipjack price in USD

# Do some graphs

ts.prices <- ts(yft_p, frequency = 12, start = c(2007,1))

ts.plot(ts.prices)

brent  <- as.data.frame(data2[13:length(data2),"value"])

yft_p_ts <- ts(yft_p, frequency = 12, start = c(2007,1))

# Read in enso data
# source(here::here('subdirectory', 'functions.R'))
source(here::here('/Users/rbeard/documents/bioeconomics/climate/',"enso.R"))

# Monthly data in df.soi From enso.R

soi <- as.data.frame(df.soi)


# Time series analysis

#acf

#pcf

#arima

fitauto<- auto.arima(yft_p_ts,seasonal=TRUE)
#dates <- seq(as.Date("2007-01-01"),by="month",length.out=192)

fc <- forecast(fitauto,h=9)

#dates <- format(date_decimal(as.numeric(row.names(as.data.frame(fc)))),"%Y-%m-%d")

#dates

#fitdata <- cbind(dates[1:183],as.data.frame(fc$fitted))
#fc2 <- cbind(dates[184:192],as.data.frame(fc))

plot(fc, main="Monthly yft forecast (Seasonal Arima)")


# Use ARIMAX

# arimax(x, order = c(0, 0, 0), seasonal = list(order = c(0, 0, 0), period = NA),
#xreg = NULL, include.mean = TRUE, transform.pars = TRUE, fixed = NULL, 
#init = NULL, method = c("CSS-ML", "ML", "CSS"), n.cond, optim.control = list(),
#kappa = 1e+06, io = NULL, xtransf, transfer = NULL)

# First need to create a dataframe

yft_p_ts_short <- ts(yft_p, frequency = 12, start = c(2007,1),end = c(2021,11))


x0 <- cbind(data2[1:179,"value"],xrate[1:179,1])

colnames(x0) <- c("DDFUELUSGULF","xrate")

y <- yft_p_ts_short
x <-  ts(x0,frequency = 12, start = c(2007,1))

fitx <- Arima(y, xreg=x, order=c(1,1,1)) #should include enso and exchange rate

plotl <- function(){
plot(y,col='red',main='Fitted model yellowfin')
lines(fitted(fitx),col="Blue")
}
plotl()
legend('topleft',inset=0.05,c("Data","Fit"),lty=c(1,1),col=c("red","blue"),lwd=2)

# Run arima forecast oil price

fit.oil <- auto.arima(x[,1],seasonal=TRUE)

fc_oil <- forecast(fit.oil,h=13)

plot(fc_oil)


# Run arima forecast xrate

fit.xrate <- auto.arima(x[,2],seasonal=TRUE)

fc_xrate <- forecast(fit.xrate,h=13)

plot(fc_xrate)


#forecast skipjack based on oil price forecasts

x.predict <- cbind(fc_oil$mean,fc_xrate$mean)

colnames(x.predict) <- c("DDFUELUSGULF","xrate")

fcx <- forecast(fitx,h = 13, xreg=x.predict)

plot(fcx,main="Arimax Yellowfin with US Gulf oil  and Exchange rate")
df.pred_yft <- as.data.frame(fcx)
write.xlsx(df.pred_yft, 'yftpred.xlsx')