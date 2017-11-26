## This script is to check wrf with field data (temp and precept)

rm(list = ls())
setwd("~/wrf/Holl")
library('openxlsx')
source("extract_wrf.R")

#-----------------------Ploting temprature--------------------------

JAN <- extract_wrf(2011,01,"tmp","HOL","FALSE")
FEB <- extract_wrf(2011,02,"tmp","HOL","FALSE")
MAR <- extract_wrf(2011,03,"tmp","HOL","FALSE")
APR <- extract_wrf(2011,04,"tmp","HOL","FALSE")
MAY <- extract_wrf(2011,05,"tmp","HOL","FALSE")
JUN <- extract_wrf(2011,06,"tmp","HOL","FALSE")
JUL <- extract_wrf(2011,07,"tmp","HOL","FALSE")
AUG <- extract_wrf(2011,08,"tmp","HOL","FALSE")
SEP <- extract_wrf(2011,09,"tmp","HOL","FALSE")
OCT <- extract_wrf(2011,10,"tmp","HOL","FALSE")
NOV <- extract_wrf(2011,11,"tmp","HOL","FALSE")
DEC <- extract_wrf(2011,12,"tmp","HOL","FALSE")

y_2011<-c(JAN,FEB,MAR,APR,MAY,JUN,JUL,AUG,SEP,OCT,NOV,DEC)


n = 1 # This is the sheet in the file

Station.Data <- read.xlsx("Holl_2011.xlsx", sheet = n,colNames = T)
# Read the temprature data [c] at 3m and 30 mins measurments
tmp <- as.vector(Station.Data$`IRT.2.(N)`)
tmp_k <- tmp+273.15 

# now we take the 3 hour average of 30 mins data
tmp_k_reshape <- matrix(tmp_k, nrow = 6)
obs <- colMeans(tmp_k_reshape)  
t <- seq(1,length(obs),1)
plot(t_obs,obs,ty="l")
lines(t,y_2011,col="red")

title(main="WRF vs observed 2011")


#--------------------Ploting rainfall [kg/m2/s]---------------------------

JAN <- extract_wrf(2011,01,"prate","HOL","FALSE")
FEB <- extract_wrf(2011,02,"prate","HOL","FALSE")
MAR <- extract_wrf(2011,03,"prate","HOL","FALSE")
APR <- extract_wrf(2011,04,"prate","HOL","FALSE")
MAY <- extract_wrf(2011,05,"prate","HOL","FALSE")
JUN <- extract_wrf(2011,06,"prate","HOL","FALSE")
JUL <- extract_wrf(2011,07,"prate","HOL","FALSE")
AUG <- extract_wrf(2011,08,"prate","HOL","FALSE")
SEP <- extract_wrf(2011,09,"prate","HOL","FALSE")
OCT <- extract_wrf(2011,10,"prate","HOL","FALSE")
NOV <- extract_wrf(2011,11,"prate","HOL","FALSE")
DEC <- extract_wrf(2011,12,"prate","HOL","FALSE")
y_2011<-c(JAN,FEB,MAR,APR,MAY,JUN,JUL,AUG,SEP,OCT,NOV,DEC)
prate_mont_vec_diff<-diff(y_2011)
prate_mont_vec_diff <- replace(prate_mont_vec_diff, prate_mont_vec_diff<0, 0)
prate_mont_vec_diff_kg<-(1/10800)*prate_mont_vec_diff
n_prate<-as.numeric(length(prate_mont_vec_diff_kg))
t_prate<-1:n_prate

# importing the observed data 
n = 1 # This is the sheet in the file
Station.Data <- read.xlsx("Holl_2011.xlsx", sheet = n,colNames = T)
# Read the temprature data [c] at 3m and 30 mins measurments
tmp <- Station.Data$Rain_mm_Tot
# now we take the 3 hour SUM of 30 mins data
tmp_reshape <- matrix(tmp, nrow = 6)
obs <- colSums(tmp_reshape)
t_obs <- seq(1,length(obs),1)
obs_kg <- (1/10800)*obs

max_obs<-max(obs_kg)
max_wrf<-max(prate_mont_vec_diff_kg)

par(mar=c(5.1,4.5,4.1,2.1))
plot(t_obs,obs_kg,col='red',ty="l",ylim = c(0,max(c(max_obs,max_wrf))),
     xlab="time (3h)", ylab="Rainfall [ kg/m2/s ]",cex.lab=1.5)
title(main="WRF vs observed 2011")
lines(t_prate,prate_mont_vec_diff_kg,col='blue')
legend('topright', legend=c("Observed", "WRF Simulated"),
       col=c("red", "blue"), lty=1:2, cex=1.2)

#-----------------------Ploting presure--------------------------


JAN <- extract_wrf(2011,01,"pres","HOL","FALSE")
FEB <- extract_wrf(2011,02,"pres","HOL","FALSE")
MAR <- extract_wrf(2011,03,"pres","HOL","FALSE")
APR <- extract_wrf(2011,04,"pres","HOL","FALSE")
MAY <- extract_wrf(2011,05,"pres","HOL","FALSE")
JUN <- extract_wrf(2011,06,"pres","HOL","FALSE")
JUL <- extract_wrf(2011,07,"pres","HOL","FALSE")
AUG <- extract_wrf(2011,08,"pres","HOL","FALSE")
SEP <- extract_wrf(2011,09,"pres","HOL","FALSE")
OCT <- extract_wrf(2011,10,"pres","HOL","FALSE")
NOV <- extract_wrf(2011,11,"pres","HOL","FALSE")
DEC <- extract_wrf(2011,12,"pres","HOL","FALSE")
y_2011<-c(JAN,FEB,MAR,APR,MAY,JUN,JUL,AUG,SEP,OCT,NOV,DEC)
t_pres<-seq(1,length(y_2011),1)

# importing the observed data 
n = 1 # This is the sheet in the file
Station.Data <- read.xlsx("Holl_2011.xlsx", sheet = n,colNames = T)
# Read the temprature data [c] at 3m and 30 mins measurments
tmp <- Station.Data$PTB110_kP_Avg
# now we take the 3 hour SUM of 30 mins data
tmp_reshape <- matrix(tmp, nrow = 6)
obs <- colMeans(tmp_reshape)
obs <-obs*1000
pos <- which( obs<80000)
obs[pos[1]]<-(obs[pos[1]-1]+obs[pos[1]+1])/2
obs[pos[2]]<-(obs[pos[2]-1]+obs[pos[2]+1])/2

t_obs <- seq(1,length(obs),1)


max_obs<-max(obs)
max_wrf<-max(y_2011)
min_obs<-min(obs)
min_wrf<-min(y_2011)
par(mar=c(5.1,4.5,4.1,2.1))
plot(t_obs,obs,col='red',ty="l",ylim = c(min(c(min_obs,min_wrf)),max(c(max_obs,max_wrf))),
     xlab="time (3h)", ylab="Rressure [ Pa ]",cex.lab=1.5)

title(main="WRF vs observed 2011")
lines(t_pres,y_2011,col='blue')
legend('topright', legend=c("Observed", "WRF Simulated"),
       col=c("red", "blue"), lty=1:2, cex=1.2)










