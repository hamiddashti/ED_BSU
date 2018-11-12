## This script is to check wrf with field data (temp and precept)

rm(list = ls())
setwd("~/wrf")
library('openxlsx')
############### ploting the wrf downloaded data vs stations

n = 2 # This is the sheet in the file

Station.Data <- read.xlsx("Data_wb_2015.xlsx", sheet = n,colNames = T)
# Read the temprature data [c] at 3m and 30 mins measurments
tmp <- Station.Data$tmp3
tmp_k <- tmp+273.15 

# now we take the 3 hour average of 30 mins data
tmp_k_reshape <- matrix(tmp_k, nrow = 6)
obs <- colMeans(tmp_k_reshape)  

t= seq(1,length(obs),1)


var <- "T2_2015.xlsx"
wrf.data <- as.matrix(read.xlsx(var))
wrf<-as.vector(wrf.data)

plot(t,obs,ty="l")
lines(t,wrf,col="red")

## ploting the precipitation-------------------------------------------------------------
rm(list = ls())
setwd("~/wrf")
library('openxlsx')
n = 2 # This is the sheet in the file
Station.Data <- read.xlsx("Data_wb_2015.xlsx", sheet = n,colNames = T)
# Read the temprature data [c] at 3m and 30 mins measurments
tmp <- Station.Data$ppta
# now we take the 3 hour SUM of 30 mins data
tmp_reshape <- matrix(tmp, nrow = 6)
obs <- colSums(tmp_reshape)  
var <- "RAINNC_2015.xlsx"
wrf.data <- as.matrix(read.xlsx(var))
wrf<-as.vector(wrf.data)
wrf<-diff(wrf)   # convert it from cumulative to mm
t= seq(1,length(wrf),1)
plot(t,obs)
lines(t,wrf,col='red')