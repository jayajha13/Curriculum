############################
##                        ##
## Forecasting: Chapter 2 ##
##                        ##
############################

rm(list=ls())   #Clean workspace
getwd()         #Get current working directory
setwd("C:/Users/jajha/Dropbox (Davidson College)/Teaching/1_Davidson College/ECO385_S23/R") #Set working directory

#install.packages(c("tidyverse", "zoo", "ggplot2", "qcc"))   #Install necessary packages
library(ggplot2)
library(tidyverse)  #Load packages: tidyverse
library(zoo)        #Load packages: zoo

#------------------------------------------------------------------------------------------------
# Moving Average and Moving Median Exercises
#------------------------------------------------------------------------------------------------
# Data: Table B.6  Global Mean Surface Air Temperature Anomaly and Global CO2 Concentration
# Converted original data to .csv; Saved as "SurfaceTemp"

airTemp <- read_csv("SurfaceTemp.csv")  #Use underscore when working with tidyverse
airTemp <- airTemp[,1:3]                #Remove extra variables
names(airTemp) <- c("year","tempAnomaly","co2") # Rename all variables

plot(airTemp$year, airTemp$tempAnomaly, type="l", xlab='Year', ylab='Average Amount of Anomaly, °C')  # Line plot of data
points(airTemp[,1:2], pch=16, cex=.5)  # Add ovservations dots, i.e., make it a 'connected' line plot

#------------------------------------------------------------------------------------------------
# Moving Average: https://www.rdocumentation.org/packages/zoo/versions/1.8-7/topics/rollmean
#------------------------------------------------------------------------------------------------
# To create a centered moving average of span 5
x <- airTemp[5:125,1]        #Define 'x' to be the first column; skip first four observations
y <- rollmean(airTemp[,2],5)
z <- cbind(x,y)              #Column bind
lines(z, col="red")
points(z,col="red", pch=15, cex=.5)

#------------------------------------------------------------------------------------------------
# Moving Median: https://www.rdocumentation.org/packages/zoo/versions/1.8-7/topics/rollmean
#------------------------------------------------------------------------------------------------
a <- rollmedian(airTemp[,2], 5) #To create a moving median of span 3
b <- cbind(x,a)
lines(b, col="blue")
points(b, col="blue", pch=17, cex=.5)

# To create a legend of moving average and moving median
legend(1990,-0.2, 
       c("Actual","Moving average","Moving median"), 
       pch=c(16,15,17), lwd=c(.5,.5,.5), cex=.55, 
       col=c("black","red","blue"))

#------------------------------------------------------------------------------------------------
# Plotting "t" against "t+1"
#------------------------------------------------------------------------------------------------
nr <- dim(airTemp)[1]  #Calculate dimension of data. [1] gives number of rows; [2] would give number of columns
nr1 <- nr-1
plot(airTemp$tempAnomaly[1:(nr-1)], airTemp$tempAnomaly[2:nr], 
     xlab='Average Amount of Anomaly in year t', 
     ylab='Average Amount of Anomaly in year t+1',
     type="p", pch=20, cex=1)

#------------------------------------------------------------------------------------------------
# Autocorrelation Function: https://www.rdocumentation.org/packages/forecast/versions/8.10/topics/Acf
#------------------------------------------------------------------------------------------------
acf(airTemp[,2], lag.max=25, type="correlation", main="ACF of Temperature Anomaly Data")
acf(airTemp[,2], lag.max=25, plot=FALSE) #To calculate table of ACF values (as in Figure 2.13)

#------------------------------------------------------------------------------------------------
# Variogram Function: https://www.rdocumentation.org/packages/gstat/versions/2.0-4/topics/variogram
#------------------------------------------------------------------------------------------------
##install.packages(gstat) #Need to install the "gstat" package to use Variogram function
#library(gstat)
#library(sp)
## To Be Completed ##

#------------------------------------------------------------------------------------------------
# Data transformations: Log transform
#------------------------------------------------------------------------------------------------
# Because of negative values in the "Anomaly" series, I will plot the "CO2" series.
par(mfrow=c(1,2))  #Two figures, arranged in 1 row and two columns
plot(airTemp$year, airTemp$co2, main="Untransformed data")
plot(airTemp$year, log(airTemp$co2), main="Log transformed data")

par(mfrow=c(1,1))  #Reset plotting area

#------------------------------------------------------------------------------------------------
# Trend Adjustment: Linear trend
#------------------------------------------------------------------------------------------------
fit.anomaly <- lm(airTemp$tempAnomaly ~ airTemp$year)  # LM = Linear model, includes constant by default
plot(airTemp$year, airTemp$tempAnomaly, type="l", 
     xlab='Year', 
     ylab='Average Amount of Anomaly, °C')
points(airTemp[,1:2], pch=16, cex=.5)
lines(airTemp$year, fit.anomaly$fit, col="red", lty=2)
legend(1990,-0.2,
       c("Actual","Fits"),
       pch=c(16,NA), lwd=c(.5,.5), lty=c(1,2), cex=.55, 
       col=c("black","red"))

# To see whether detrending the data gives stationary residuals, I construct residual plots
# For "qqnorm" see https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/qqnorm
par(mfrow=c(2,2), oma=c(0,0,0,0))  # OMA = Outer Margin Area
qqnorm(fit.anomaly$res, datax=TRUE, pch=16, xlab='Residual', main='')
qqline(fit.anomaly$res, datax=TRUE)
plot(fit.anomaly$fit, fit.anomaly$res, pch=16, 
     xlab='Fitted Value', ylab='Residual')
abline(h=0)
hist(fit.anomaly$res, col="gray", xlab='Residual', main='')
plot(fit.anomaly$res, type="l", xlab='Observation Order', ylab='Residual')
points(fit.anomaly$res, pch=16, cex=0.5)
abline(h=0)

par(mfrow=c(1,1))  #Reset plotting area

#------------------------------------------------------------------------------------------------
# Trend Adjustment: Detrending vs. Differencing the data
#------------------------------------------------------------------------------------------------
# Data: Table B.4  US Production of Blue and Gorgonzola Cheeses
# Converted original data to .csv; Saved as "Cheese"

cheese.data <- read_csv("Cheese.csv")  #Use underscore when working with tidyverse
#attach(cheese.data)
names(cheese.data) <- c("year","production") #Rename all variables

fit.cheese<-lm(cheese.data$production ~ cheese.data$year)
plot(cheese.data,type="l",xlab='Year',ylab='Production, 10000lb')
points(cheese.data,pch=16,cex=.5)
lines(cheese.data$year, fit.cheese$fit,col="red",lty=2)
legend(1990,12000,c("Actual","Fits"),
       pch=c(16,NA),lwd=c(.5,.5),lty=c(1,2),cex=.55,col=c("black","red"))

# To see whether detrending the data gives stationary residuals, construct residual plots
par(mfrow=c(2,2),oma=c(0,0,0,0))
qqnorm(fit.cheese$res,datax=TRUE,pch=16,xlab='Residual',main='')
qqline(fit.cheese$res,datax=TRUE)
plot(fit.cheese$fit,fit.cheese$res,pch=16, xlab='Fitted Value',
     ylab='Residual')
abline(h=0)
hist(fit.cheese$res,col="gray",xlab='Residual',main='')
plot(fit.cheese$res,type="l",xlab='Observation Order',
     ylab='Residual')
points(fit.cheese$res,pch=16,cex=.5)
abline(h=0)

par(mfrow=c(1,1))  #Reset plotting area to 1X1

# To see whether differencing the data gives stationary residuals, regress Diff(Prod) on Year
# Remember that differencing causes the first observation to be dropped, so use only T-1 observations
nr<-dim(cheese.data)[1]
dcheese.data<-cbind(cheese.data$year[2:nr],diff(cheese.data$production))
fit.dcheese<-lm(dcheese.data[,2]~dcheese.data[,1])
plot(dcheese.data,type="l",xlab='',ylab='Production, d=1')
points(dcheese.data,pch=16,cex=.5)
lines(dcheese.data[,1], fit.dcheese$fit,col="red",lty=2)
legend(1952,-2200,c("Actual","Fits"),
       pch=c(16,NA),lwd=c(.5,.5),lty=c(1,2),
       cex=.75,col=c("black","red"))

par(mfrow=c(2,2),oma=c(0,0,0,0))
qqnorm(fit.dcheese$res,datax=TRUE,pch=16,xlab='Residual',main='')
qqline(fit.dcheese$res,datax=TRUE)
plot(fit.dcheese$fit,fit.dcheese$res,pch=16, xlab='Fitted Value',
     ylab='Residual')
abline(h=0)
hist(fit.dcheese$res,col="gray",xlab='Residual',main='')
plot(fit.dcheese$res,type="l",xlab='Observation Order',
     ylab='Residual')
points(fit.dcheese$res,pch=16,cex=.5)
abline(h=0)

par(mfrow=c(1,1))  #Reset plotting area to 1X1


#------------------------------------------------------------------------------------------------
# Seasonal Adjustment
#------------------------------------------------------------------------------------------------
# Data: Table B.5  US Beverage Manufacturer Product Shipments, Unadjusted
# Converted original data to .csv; Saved as "Beverage"

bev.data <- read_csv("Beverage.csv")  #Use underscore when working with tidyverse
names(bev.data) <- c("month","amount") #Rename all variables

#--------------------
# Plotting level series
nr<-dim(bev.data)[1] #No. of rows
tt<-1:nr #Need this for x-axis variables, which is non-numeric in the data

plot(tt, bev.data$amount, type="l", 
     xlab='Original series', 
     ylab='Beverage Shipments, Millions of Dollars', 
     xaxt='n') #xaxt='n' leaves the x-axis labels blank
axis(1,seq(1,nr,24),labels=bev.data$month[seq(1,nr,24)])
points(tt,bev.data$amount,pch=16,cex=.75)

#--------------------
#Plotting seasonally differenced series

dsbev.data<-bev.data
dsbev.data[,2]<- c(array(NA,dim=c(12,1)),diff(bev.data$amount,12))
plot(tt,dsbev.data$amount,type="l", 
     xlab='Seasonally differenced series', 
     ylab='Seasonal, d=12', 
     xaxt='n')
axis(1,seq(1,nr,12),labels=dsbev.data$month[seq(1,nr,12)])
points(tt,dsbev.data$amount,pch=16,cex=.5)

#--------------------
#Plotting detrended seasonally differenced series

dstbev.data<-dsbev.data
dstbev.data[,2]<- c(NA,diff(dstbev.data$amount,1))
fit.dstbev<-lm(dstbev.data$amount ~ tt) #Regressing first difference of seasonally differenced series on time
plot(tt,dstbev.data$amount, type="l", 
     xlab='Detrended and Seasonally differenced series', 
     ylab='Seasonal d=12 with Trend d=1',
     xaxt='n')
axis(1,seq(1,nr,24),labels=dsbev.data$month[seq(1,nr,24)])
points(tt,dstbev.data$amount, pch=16, cex=.5)
lines(c(array(NA,dim=c(12,1)),fit.dstbev$fit),col="red",lty=2) #Adding line for "fitted value" from regression
legend(2,-300,c("Actual","Fits"),
       pch=c(16,NA),lwd=c(.5,.5),lty=c(1,2),cex=.75,col=c("black","red"))

#--------------------
#Residual diagnostics

par(mfrow=c(2,2),oma=c(0,0,0,0))
qqnorm(fit.dstbev$res,datax=TRUE,pch=16,xlab='Residual',main='')
qqline(fit.dstbev$res,datax=TRUE)
plot(fit.dstbev$fit,fit.dstbev$res,pch=16, xlab='Fitted Value',
     ylab='Residual')
abline(h=0)
hist(fit.dstbev$res,col="gray",xlab='Residual',main='')
plot(fit.dstbev$res,type="l",xlab='Observation Order',
     ylab='Residual')
points(fit.dstbev$res,pch=16,cex=.5)
abline(h=0)

#------------------------------------------------------------------------------------------------
# Additive decomposition approach (pp 55-56)
#------------------------------------------------------------------------------------------------
## The following code reproduces figure 2.29 in chapter 2

#--------------------
# De-trend the data
tt<-1:nr
fit.tbev<-lm(bev.data$amount~tt)
bev.data.dt<-fit.tbev$res

#--------------------
# Obtain seasonal medians for each month, seasonal period is sp=12
sp<-12
smed<-apply(matrix(bev.data.dt,nrow=sp),1,median)

#--------------------
# Adjust the medians so that their sum is zero
smed<-smed-mean(smed)

#--------------------
# Data without the trend and seasonal components
bev.data.dts<-bev.data.dt-rep(smed,nr/sp)
# Note that we can also reverse the order, i.e. first take the seasonality out
smed2<-apply(matrix(bev.data$amount,nrow=sp),1,median)
smed2<-smed2-mean(smed2)
bev.data.ds<-bev.data$amount-rep(smed2,nr/sp)

#--------------------
# To reproduce Figure 2.29
par(mfrow=c(2,2),oma=c(0,0,0,0))

plot(tt,bev.data$amount,type="l",xlab='(a) Original Data',ylab='Data',xaxt='n')
axis(1,seq(1,nr,24),labels=bev.data$month[seq(1,nr,24)])
points(tt,bev.data$amount,pch=16,cex=.75)

plot(tt, bev.data.dt,type="l",xlab='(b) Detrended Data',ylab='Detr. Data',xaxt='n')
axis(1,seq(1,nr,24),labels=bev.data$month[seq(1,nr,24)])
points(tt, bev.data.dt,pch=16,cex=.75)

plot(tt, bev.data.ds,type="l",xlab='(c) Seasonally Adjusted Data', ylab='Seas. Adj. Data',xaxt='n')
axis(1,seq(1,nr,24),labels=bev.data$month[seq(1,nr,24)])
points(tt, bev.data.ds,pch=16,cex=.75)

plot(tt, bev.data.dts,type="l",xlab='(c) Seasonally Adj. and Detrended Data',ylab='Seas. Adj. and Detr. Data',xaxt='n')
axis(1,seq(1,nr,24),labels=bev.data$month[seq(1,nr,24)]) 
points(tt, bev.data.dts, pch=16,cex=.75)

#--------------------
#Residual diagnostics: Figure 2.30

fit.bev.dts<-lm(bev.data.dts ~ tt) #Linear regression of detrended and seasonally adjusted series on time
par(mfrow=c(2,2),oma=c(0,0,0,0))
qqnorm(fit.bev.dts$res,datax=TRUE,pch=16,xlab='Residual',main='')
qqline(fit.bev.dts$res,datax=TRUE)
plot(fit.bev.dts$fit,fit.bev.dts$res,pch=16, xlab='Fitted Value', ylab='Residual')
abline(h=0)
hist(fit.bev.dts$res,col="gray",xlab='Residual',main='')
plot(fit.bev.dts$res,type="l",xlab='Observation Order', ylab='Residual')
points(fit.bev.dts$res,pch=16,cex=.5)
abline(h=0)

par(mfrow=c(1,1))  #Reset plotting area to 1X1

#------------------------------------------------------------------------------------------------
# Another way to decompose your time series into trend, seasonal, and random components
#------------------------------------------------------------------------------------------------
bev.ts <- ts(bev.data[,2],  start = 1992, freq = 12) #Declare your data to be time series
plot(decompose(bev.ts))
bev.decom <- decompose(bev.ts, type = "mult")
plot(bev.decom)
Trend <- bev.decom$trend
Seasonal <- bev.decom$seasonal
ts.plot(cbind(bev.ts, Trend, Trend * Seasonal), lty = 1:3)


#------------------------------------------------------------------------------------------------
# Forecast Model Evaluation: Table 2.2 (p.68)
#------------------------------------------------------------------------------------------------
# original data and forecast errors
yt <- c(47,46,51,44,54,47,52,45,50,51,49,41,48,50,51,55,52,53,48,52)
fe <- c(-4.1,-6.9,2.2,-4.1,4.3,-.5,.8,-8.1,-4.4,-.2,-4.3,-5.5,-5.1,-2.1,4.2,7.3,6.6,5.9,-3.8,6.2)
ME <- mean(fe)           #Mean Error
MAD <- mean(abs(fe))     #Mean Absolute Deviation
MSE <- mean(fe^2)        #Mean Squared Error
ret1 <- (fe/yt)*100      #Relative Forecast Error
MPE <- mean(ret1)        #Mean Percent Forecast Error
MAPE <- mean(abs(ret1))  #Mean Absolute Percent Forecast Error
ME
MAD
MSE
ret1
MPE
MAPE
#--------------------
#Autocorrelation Function of Forecast Errors
acf.fe <- acf(fe, main='ACF of Forecast Error')
#--------------------
#Ljung-Box "goodness-of-fit" test statistic for small smaples
K <- 13  #Define lag K
T <- length(fe)
QLB <- T*(T+2)*sum((1/(T-1:K))*(acf.fe$acf[2:(K+1)]^2))
qchisq(0.95,K) # Upper 5% of chi-squared distribution with K degrees of freedom

#------------------------------------------------------------------------------------------------
#Quality control charts: https://www.rdocumentation.org/packages/qcc/versions/2.6/topics/qcc
#------------------------------------------------------------------------------------------------
#install.packages("qcc") #qcc = Quality Control Charts
library(qcc)

#--------------------
#Shewhart control chart("xbar.one" means data are continuous, one-at-atime)
qcc(fe, type="xbar.one", title="Individuals Chart for the Forecast Error")

#--------------------
#CUSUM chart
cusum(fe, title='Cusum Chart for the Forecast Error', sizes=1)

#--------------------
#EWMA (Exponentially-Weighted Moving Average) chart
ewma(fe, title='EWMA Chart for the Forecast Error', lambda=.1,sizes=1)

#================================================================================================

