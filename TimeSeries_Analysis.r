##################################################
### PROG8430                                    ##
### Time Series Assignment                      ## 
##################################################
#                                               ##
##################################################
# Written by Manthan Ravi Pillai
# ID: 8744053
#
##################################################
### Basic Set Up                                ##
##################################################

# Clear plots
if(!is.null(dev.list())) dev.off()

# Clear console
cat("\014") 

# Clean workspace
rm(list=ls())

#Set work directory
setwd("C:/Users/Manthan/Documents/R Programs")

options(scipen=9)

##################################################
### Install Libraries                           ##
##################################################

#If the library is not already downloaded, download it

if(!require(tseries)){install.packages("tseries")}
library("tseries")

if(!require(TTR)){install.packages("TTR")}
library("TTR")

if(!require(smooth)){install.packages("smooth")}
library("smooth")


####################################################
## TIME SERIES  - Quarterly Data                  ##
####################################################

#1.1 Loading the woodstock data

load("Woodstock.Rdata")
head(Woodstock)

#changing the name of data set
Woodstock_MRP<-Woodstock

#changing it to initals
colnames(Woodstock_MRP) <- paste0('Temperature_MRP')
head(Woodstock_MRP)


# Convert woodstock to a Time Series datatype
Temprature_WS_MRP <- ts(Woodstock_MRP, frequency = 12, start=c(1988,1))
head(Temprature_WS_MRP)


#2.1 Summarizing the Temprature over years of woodstock
summary(Temprature_WS_MRP)
sd(Temprature_WS_MRP)


###2.2 PLOT THE TIME SERIES ####

plot.ts(Temprature_WS_MRP, main="Average Temperature - Woodstock",
        ylim = c(-12, 22)) 

###2.3 Decompose and check for Autocorrelation of the woodstock data

decompTemp_WD_MRP <- decompose(Temprature_WS_MRP, type="additive")  #it is a additive
decompTemp_WD_MRP

plot(decompTemp_WD_MRP)

#2.4 Checking the data is stationary or not

adf_WD_MRP<-adf.test(Temprature_WS_MRP)
adf_WD_MRP

#2.5 Deseasonalize the woodstock data

Temprature_WD_Seas_Adj_MRP <- Temprature_WS_MRP - decompTemp_WD_MRP$seasonal

plot.ts(Temprature_WD_Seas_Adj_MRP, main="Deseasonalized - Average Temperature
        - Woodstock", ylim = c(-13, 23))

#####################################################
## TIME SERIES  - Annual  for Ayr                  ##
#####################################################

#Loading the Ayr Data 
load("Ayr.Rdata")
head(Ayr)

#changing the dataset name
Ary_MRP<-Ayr

#1.1 Converting Ary_MRP to time series data
TempAry_Ts_MRP <- ts(Ary_MRP, frequency = 1, 
                     start=c(1968,1))  #Converts to Time Series
head(TempAry_Ts_MRP)

#1.1 Summarize the data
summary(TempAry_Ts_MRP)
sd(TempAry_Ts_MRP)
range(TempAry_Ts_MRP)


#2.1 plotting ts data for Ary
plot.ts(TempAry_Ts_MRP, main="Average Temperature - Ary", ylim=c(11,13.5))


#2.2 Spot trends by smoothing

#Trying different n-values for smoothing

TempAry_MRPSMA10 <- SMA(TempAry_Ts_MRP,n=9)#9 best fits the data 
plot.ts(TempAry_MRPSMA10)

TempAry_MRPSMA10 <- SMA(TempAry_Ts_MRP,n=6)
plot.ts(TempAry_MRPSMA10)

TempAry_MRPSMA10 <- SMA(TempAry_Ts_MRP,n=12)
plot.ts(TempAry_MRPSMA10)

#2.4 adf test to find the data is statinory or not
adf_Ary_MRP<-adf.test(TempAry_Ts_MRP)
adf_Ary_MRP

#2.5 Autocorrelations and lags for Ary 
acf_Ary_MRP<-acf(TempAry_Ts_MRP)   
acf_Ary_MRP

#3.1 Moving Average Forecast

move_avg_Ayr_MRP <- sma(TempAry_Ts_MRP)
move_avg_Ayr_MRP
move_avg_Ayr_MRP <- forecast(move_avg_Ayr_MRP, h=5,
                                level=0.75)   
move_avg_Ayr_MRP
plot(move_avg_Ayr_MRP)

#3.2 Exponential Smoothing Forecast

ES_avg_Ayr_MRP <- es(TempAry_Ts_MRP)
ES_avg_Ayr_MRP
ES_avg_Ayr_MRP <- forecast(ES_avg_Ayr_MRP, h=5,level=0.75)
ES_avg_Ayr_MRP
plot(ES_avg_Ayr_MRP)