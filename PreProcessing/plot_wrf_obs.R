## This script is to check wrf with field data (temp and precept)

rm(list = ls())
setwd("~/wrf/Holl")
library('openxlsx')
############### ploting the wrf downloaded data vs stations

n = 1 # This is the sheet in the file

Station.Data <- read.xlsx("Holl_2011.xlsx", sheet = n,colNames = T)
# Read the temprature data [c] at 3m and 30 mins measurments
tmp <- as.vector(Station.Data$`IRT.2.(N)`)



tmp_k <- tmp+273.15 

# now we take the 3 hour average of 30 mins data
tmp_k_reshape <- matrix(tmp_k, nrow = 6)
obs <- colMeans(tmp_k_reshape)  
t_obs <- seq(1,length(obs),1)
plot(t_obs,obs)

var <- "T2_2010_2013.xlsx"
wrf.data <- as.matrix(read.xlsx(var))
wrf.2011 <- wrf.data[,366:(366+365-1)]
wrf<-as.vector(wrf.2011)
t_wrf <- seq(1,length(wrf),1)

plot(t_obs,obs,ty="l")
lines(t_wrf,wrf,col="red")

## ploting the precipitation-------------------------------------------------------------
rm(list = ls())
setwd("~/wrf/Holl")
library('openxlsx')
n = 1 # This is the sheet in the file
Station.Data <- read.xlsx("Holl_2011.xlsx", sheet = n,colNames = T)
# Read the temprature data [c] at 3m and 30 mins measurments
tmp <- Station.Data$Rain_mm_Tot
# now we take the 3 hour SUM of 30 mins data
tmp_reshape <- matrix(tmp, nrow = 6)
obs <- colSums(tmp_reshape)
t_obs <- seq(1,length(obs),1)
obs_kg <- (1/10800)*obs

var <- "RAINNC_2010_2013.xlsx"
wrf.data <- as.matrix(read.xlsx(var))
wrf.2011 <- wrf.data[,366:(366+365-1)]
wrf<-as.vector(wrf.2011)
wrf<-diff(wrf)   # convert it from cumulative to mm
wrf <- replace(wrf, wrf<0, 0)
wrf_kg<-(1/10800)*wrf
t_wrf= seq(1,length(wrf),1)
plot(t_obs,obs,ty="l",ylim = c(0,15),col='blue')
lines(t_wrf,wrf,col='red')
legend('topright', legend=c("Observed", "wrf Simulated"),
       col=c("blue", "red"), lty=1:2, cex=1.2)

plot.new()
plot(t_obs,obs_kg,ylim = c(0,max(wrf_kg)),col='blue',ty='l', xlab="time (3h)", ylab="Rainfall [ kg/m2/s ]" )
title(main="WRF vs observed 2011")
lines(t_wrf,wrf_kg,col='red')

legend('topright', legend=c("Observed", "WRF Simulated"),
       col=c("blue", "red"), lty=1:2, cex=1.2)










