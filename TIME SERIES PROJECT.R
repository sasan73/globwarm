library(zoo) # package zoo ra farakhani mikonim ta az function "na.approx()" estefade konim va taghribi az dade haye gomshodeye data set ra bedast avarim.
library(ggfortify) # ggfortify  be onvane mokamele package ggplot estefade mishavad va baraye rasme nemudar haye time series monaseb ast.
library(fpp2) # note: ba farakhanie package fpp2 package forcast niz be surate automatic farakhani mishavad 


setwd("C:\\Workshop\\Data")
globt <- read.csv("GlobalLandTemperatures_GlobalTemperatures.csv") # data marbut be garmayeshe zamin
globt <- globt[,1:2]
head(globt)
tail(globt)
names(globt) <- c("Date","GlobTemp")
globt_ts <- ts(globt$GlobTemp, frequency = 12, start = 1800 , end = 2015)

ggtsdisplay(globt_ts) # forecast package

n <- nrow(globt)
m <- nrow(globt)-200

globt_selected <- globt[m:n,]
globt_ts_1 <- ts(globt_selected$GlobTemp
               , frequency = 12)


globt_ts <- na.approx(globt_ts) # zoo package
globt_ts_1 <- na.approx(globt_ts_1) # zoo package

autoplot(globt_ts)+ # ggfortify package
  ggtitle("Global Temperature over the last 215 years")+
  xlab("Date")+
  ylab("Temperature")
autoplot(globt_ts_1)


# seasonal plot 
ggseasonplot(globt_ts_1, year.labels = TRUE, year.labels.left = TRUE)+ # forecast package
  ylab('temperature')+
  ggtitle('seasonal plot for global temperature')

ggAcf(globt_ts , lag=50) # forecast package 
ggPacf(globt_ts) # forecast package

###########################
#### decomposition #####
# classical decomposition  
# decomposition using multiplicative decomposition

globt_ts_1 %>% decompose(type = 'multiplicative') %>% autoplot() 

# decomposition using additive decomposition

autoplot(decompose(globt_ts_1, type = 'additive'))

# stl decomposition

globt_ts_1 %>% 
  stl(t.window= 13, s.window='periodic', robust= TRUE) %>% 
  autoplot()
###########################

# differencing 

globt_diff_seas<-diff(globt_ts_1, lag = 12)
ggtsdisplay(globt_diff_seas) # forecast package

globt_diff2 <- diff(globt_diff_seas)
ggtsdisplay(globt_diff2)

# transformation

lambda1 <- BoxCox.lambda(globt_diff2) # forecast package
lambda1 

globt_transformed <- BoxCox(globt_diff2, lambda = lambda1) # forecast package

ggtsdisplay(globt_transformed)

# Arima model 

# automatic model selection
fit_auto<-auto.arima(globt_transformed, approximation = FALSE, stepwise = FALSE) # forecast package
fit_auto
checkresiduals(fit_auto)



# manual model selection
fit<-Arima(globt_transformed , order = c(3,0,0) , seasonal = c(1,1,1)) # forecast package
fit
checkresiduals(fit, lag=36) # forecast package

predictions1 <- globt_ts_1 %>% 
  Arima(order = c(0,0,2), seasonal = c(1,0,1), lambda = lambda1) %>%
  forecast()

autoplot(predictions1)+
  ylab('Temperature') + xlab('Month')
  
predictions2 <- globt_ts_1 %>%
  Arima(order = c(3,0,0), seasonal = c(1,1,1)) %>%
  forecast() 

autoplot(predictions2) + ylab('Temperatue') + xlab('Month')


