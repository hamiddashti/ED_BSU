rm(list = ls())
require(bfast)
require(quantmod)
#install.packages("greenbrown", repos="http://R-Forge.R-project.org")
library(greenbrown)
library(lubridate)
library(ggplot2)

setwd("N:/Data02/bcal/Personal/hamid/ED_opt/working/Timeseries")

#------------ Load the datasets ----------------------------

ws5_opt <- read.csv("ws5_opt.csv", header = TRUE, as.is = TRUE)
ws5_reg <- read.csv("ws5_reg.csv", header = TRUE, as.is = TRUE)
ws_obs_tmp <- read.csv("ws_obs.csv", header = TRUE, as.is = TRUE)

#------------ show the first and end date of each dataset-----

print(paste0("The first date for ED simulation is:", ws5_opt$dates[1]))
print(paste0("The end date for ED simulation is:", ws5_opt$dates[length(ws5_opt$dates)]))

print(paste0("The first date for observation:", ws_obs_tmp$dates[1]))
print(paste0("The End date for observation:", ws_obs_tmp$dates[length(ws_obs_tmp$dates)]))

#------------ Prepare data for RRMSE and one-to-one plots-----

dates <- seq(as.Date("2014/10/1"), as.Date("2016/9/30"), "days")
ws5_opt_gpp <- ws5_opt$GPP[which(ws5_opt$dates==dates[1]):which(ws5_opt$dates==dates[length(dates)])]
ws5_reg_gpp <- ws5_reg$GPP[which(ws5_reg$dates==dates[1]):which(ws5_reg$dates==dates[length(dates)])]
ws_obs_gpp <- ws_obs_tmp$GPP[which(ws_obs_tmp$dates=="10/1/2014"):which(ws_obs_tmp$dates=="9/30/2016")]

ws5_opt_df <- data.frame(dates,ws5_opt_gpp)
colnames(ws5_opt_df) <- c("Date","GPP")

ws5_reg_df <- data.frame(dates,ws5_reg_gpp)
colnames(ws5_reg_df) <- c("Date","GPP")

ws_obs_df <- data.frame(dates,ws_obs_gpp)
colnames(ws_obs_df) <- c("Date","GPP")

ws5_opt_monthly <- aggregate(ws5_opt_df$GPP, list(format(ws5_opt_df$Date, "%Y-%m")), max)
colnames(ws5_opt_monthly) <- c('Date', 'GPP')

ws5_reg_monthly <- aggregate(ws5_reg_df$GPP, list(format(ws5_reg_df$Date, "%Y-%m")), max)
colnames(ws5_reg_monthly) <- c('Date', 'GPP')

ws_obs_monthly <- aggregate(ws_obs_df$GPP, list(format(ws_obs_df$Date, "%Y-%m")), max)
colnames(ws_obs_monthly) <- c('Date', 'GPP')

#-------------some simple ploting--------------------------------------------

graphics.off()
ggplot() + 
  geom_line(data = ws_obs_df, aes(x =ws_obs_df$Date, y = ws_obs_df$GPP,color="black"),size=1) +
  geom_line(data = ws5_opt_df, aes(x =Date, y = ws5_opt_df$GPP,color="red"),size=1) +
  geom_line(data = ws5_reg_df, aes(x =Date, y = ws5_reg_df$GPP,color="blue"),size=1)+
  labs(color = '') + theme_bw() + xlab('data_date') + ylab('percent.change')
  
theme(legend.text = element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  scale_color_discrete(breaks=c("Observation","No regulation","Regulation"))
  








plot(ws_obs_df$GPP,ty="l",col="red")
lines(ws5_opt_df$GPP,ty="l",col="blue")
lines(ws5_reg_df$GPP,ty="l",col="black")









obs <- read.csv("ws_obs.csv",header = TRUE, as.is = TRUE)
obs_date <-as.Date(as.character(obs$dates),"%m/%d/%Y")
obs_gpp <- obs$GPP
ss <- smooth.spline(obs_gpp)
obs_gpp <- ss$y
df_obs = data.frame(date = obs_date, gpp = obs_gpp)
df_obs <- df_obs[which(obs_date>=as.Date(startdate) & obs_date<=as.Date(enddate)),]
plot(df_obs$gpp ,ty="l")






startdate <- "2001-10-01"
enddate <- "2017-09-30"

landsat <- read.csv("ws_landsat.csv", header = TRUE, as.is = TRUE)
landsat_date <-as.Date(as.character(landsat$dates),"%m/%d/%Y")
landsat_gpp <- (landsat$GPP)/1000
ss <- smooth.spline(landsat_gpp)
landsat_gpp <- ss$y
df_landsat <- data.frame(date = landsat_date, gpp = landsat_gpp)
df_landsat <- df_landsat[which(landsat_date>=as.Date(startdate) & landsat_date<=as.Date(enddate)),]



modis <- read.csv("ws_modis.csv",header = TRUE, as.is = TRUE)
modis_date <-as.Date(as.character(modis$dates),"%m/%d/%Y")
modis_gpp <- (modis$GPP)/1000
ss <- smooth.spline(modis_gpp)
modis_gpp <- ss$y
df_modis = data.frame(date = modis_date, gpp = modis_gpp)
df_modis <- df_modis[which(modis_date>=as.Date(startdate) & modis_date<=as.Date(enddate)),]
plot(df_modis$gpp ,ty="l")

 sim <- read.csv("ws_sim.csv", header = TRUE, as.is = TRUE)
 sim_date <-as.Date(sim$dates)
 sim_gpp <- sim$GPP
 ss <- smooth.spline(sim_gpp)
 sim_gpp <- ss$y
 df_sim = data.frame(date = sim_date, gpp = sim_gpp)
 df_sim <- df_sim[which(sim_date>=as.Date(startdate) & sim_date<=as.Date(enddate)),]
 plot(df_sim$gpp ,ty="l")



############## Phenology part##################################
head(df_landsat$date)
landsat_ts <- ts(df_landsat$gpp,     # random data
               start = c(2001, 10),
               frequency =23 )
print(landsat_ts)

landsat_phen <- Phenology(landsat_ts, tsgf="TSGFspline", approach="White") 
length(landsat_phen$sos)
landsat_phen$sos


head(df_modis$date)
modis_ts <- ts(df_modis$gpp,     # random data
                 start = c(2001, 10),
                 frequency = 46)
print(modis_ts)

modis_phen <- Phenology(modis_ts, tsgf="TSGFspline", approach="White") 
length(modis_phen$sos)
modis_phen$sos

head(df_sim$date)
sim_ts <- ts(df_sim$gpp,     # random data
               start = c(2001, 274),
               frequency = 365)
print(sim_ts)
sim_phen <- Phenology(sim_ts, tsgf="TSGFssa", approach="Deriv") 
length(sim_phen$sos)
sim_phen$sos

obs_ts <- ts(df_obs$gpp,     # random data
             start = c(2001, 274),
             frequency = 365)
Seasonality(obs_ts)
obs_phen <- Phenology(obs_ts, tsgf="TSGFspline", approach="White") 
length(obs_phen$sos)
obs_phen$sos



landsat.stl <- stl(landsat_ts,s.window="periodic")
plot(landsat.stl,main="Landsat")
landsat.bfast <- bfast(landsat_ts,h=0.15,season = "dummy",max.iter =1)
plot(
  landsat.bfast,
  type="trend",
  main="Trend and breakpoints"
)


modis.stl <- stl(modis_ts,s.window="periodic")
plot(modis.stl,main="MODIS")
modis.bfast <- bfast(modis_ts,h=0.15,season = "dummy",max.iter =1)
plot(
  modis.bfast,
  type="trend",
  main="Trend and breakpoints"
)


sim.stl <- stl(sim_ts,s.window="periodic")
plot(sim.stl,main="ED simulation")
sim.bfast <- bfast(sim_ts,h=0.15,season = "dummy",max.iter =1)
plot(
  sim.bfast,
  type="trend",
  main="Trend and breakpoints"
)

modis.tren <- modis.stl$time.series[,2]
landsat.tren <- landsat.stl$time.series[,2]
sim.tren <- sim.stl$time.series[,2]
mmax <-max(sim.tren)
mmin <-min(sim.tren)
modis.tren.norm <- (modis.tren-mmin)/(mmax-mmin)
landsat.tren.norm <- (landsat.tren-mmin)/(mmax-mmin)

library(ppls)
modis.tren.norm <-normalize.vector(modis.tren)
landsat.tren.norm <-normalize.vector(landsat.tren)
sim.tren.norm <-normalize.vector(sim.tren)
plot(sim.tren)
lines(modis.tren.norm)
lines(landsat.tren.norm)



par(mfrow=c(3,1))
plot(modis.tren,ty="l",col="red")

plot(landsat.tren,ty="l",col="blue")

plot(sim.tren,ty="l",col="black")





landsat_phen <- TSGFspline(landsat_ts,interpolate = FALSE)
plot(landsat_phen)
I <- which(is.na(landsat_phen))
phen_obs[I] <- (phen_obs[I-1]+phen_obs[I+1])/2
plot(phen_obs)
PhenoDeriv(phen_obs, plot=TRUE)
PhenoTrs(phen_obs,approach = c("White"))
PhenoTrs(phen_obs,approach = c("Trs"))



landsat_phen <- Phenology(landsat_ts, tsgf="TSGFspline", approach="White") 
landsat_phen$sos













myts_ed <- ts(GPP_ed,     # random data
              start = c(2015, as.numeric(format(dates[1], "%j"))),
              frequency = 366)
Seasonality(myts_ed)


















##############Phenology change detection#########################################
#install.packages("greenbrown", repos="http://R-Forge.R-project.org")

#### trend analysis
#trd_obs <- Trend(myts_obs,method = "STM", breaks = NULL, mosum.pval = 0.05)
#plot(trd_obs)

#trd_ed <- Trend(myts_ed,method = "STM", breaks = NULL, mosum.pval = 0.05)
#plot(trd_ed)


### Phenometric estimation ..... 

#--------------- Linear smoothing----------------------
phen_obs <- TSGFlinear(myts_obs,interpolate = FALSE)
plot(phen_obs)
I <- which(is.na(phen_obs))
plot(phen_obs)
PhenoDeriv(phen_obs, plot=TRUE)
PhenoTrs(phen_obs,approach = c("White"))
PhenoTrs(phen_obs,approach = c("Trs"))


phen_ed <- TSGFlinear(myts_ed,interpolate = FALSE)
PhenoDeriv(phen_ed, plot=TRUE)
PhenoTrs(phen_ed,approach = c("White"))
PhenoTrs(phen_ed,approach = c("Trs"))

#-------------- spline smoothing ----------------------
phen_obs <- TSGFspline(myts_obs,interpolate = TRUE)
plot(phen_obs)
I <- which(is.na(phen_obs))
phen_obs[I] <- (phen_obs[I-1]+phen_obs[I+1])/2
plot(phen_obs)
PhenoDeriv(phen_obs, plot=TRUE)
PhenoTrs(phen_obs,approach = c("White"))
PhenoTrs(phen_obs,approach = c("Trs"))


phen_ed <- TSGFspline(myts_ed,interpolate = TRUE)
PhenoDeriv(phen_ed, plot=TRUE)
PhenoTrs(phen_ed,approach = c("White"))
PhenoTrs(phen_ed,approach = c("Trs"))

# ------------singular spectrum analysis smoothing --------
phen_obs <- TSGFssa(myts_obs,interpolate = TRUE)
plot(phen_obs)
I <- which(is.na(phen_obs))
#phen_obs[I] <- (phen_obs[I-1]+phen_obs[I+1])/2
plot(phen_obs)
PhenoDeriv(phen_obs, plot=TRUE)
PhenoTrs(phen_obs,approach = c("White"))
PhenoTrs(phen_obs,approach = c("Trs"))


phen_ed <- TSGFssa(myts_ed,interpolate = TRUE)
PhenoDeriv(phen_ed, plot=TRUE)
PhenoTrs(phen_ed,approach = c("White"))
PhenoTrs(phen_ed,approach = c("Trs"))

# ------------double-logistic smoothing (Elmore) -----------
phen_obs <- TSGFdoublelog(myts_obs,interpolate = TRUE, method = c("Elmore"), check.seasonality = 1)
plot(phen_obs)
I <- which(is.na(phen_obs))
#phen_obs[I] <- (phen_obs[I-1]+phen_obs[I+1])/2
PhenoDeriv(phen_obs, plot=TRUE)
PhenoTrs(phen_obs,approach = c("White"))
PhenoTrs(phen_obs,approach = c("Trs"))

phen_ed <- TSGFdoublelog(myts_ed,interpolate = TRUE, method = c("Elmore"), check.seasonality = 1)
plot(phen_ed)
PhenoDeriv(phen_ed, plot=TRUE)
PhenoTrs(phen_ed,approach = c("White"))
PhenoTrs(phen_ed,approach = c("Trs"))

# ------------double-logistic smoothing (Beck) -----------
phen_obs <- TSGFdoublelog(myts_obs,interpolate = TRUE, method = c("Beck"), check.seasonality = 1)
plot(phen_obs)
I <- which(is.na(phen_obs))
#phen_obs[I] <- (phen_obs[I-1]+phen_obs[I+1])/2
PhenoDeriv(phen_obs, plot=TRUE)
PhenoTrs(phen_obs,approach = c("White"))
PhenoTrs(phen_obs,approach = c("Trs"))


phen_ed <- TSGFdoublelog(myts_ed,interpolate = TRUE, method = c("Beck"), check.seasonality = 1)
plot(phen_ed)
PhenoDeriv(phen_ed, plot=TRUE)
PhenoTrs(phen_ed,approach = c("White"))
PhenoTrs(phen_ed,approach = c("Trs"))






##################






gpp_obs.stl <- stl(landsat_ts,s.window="periodic")
plot(gpp_obs.stl,main="STL Decomposition")


gpp_ed.stl <- stl(myts_ed,s.window="periodic")
plot(gpp_ed.stl,main="STL Decomposition")



gpp_obs.bfast <- bfast(myts_obs,h=0.15,season = "dummy",max.iter =1)
gpp_ed.bfast <- bfast(myts_ed,h=0.15,season = "dummy",max.iter =1)

plot(
  gpp_obs.bfast,
  type="all",
  main="Trend and breakpoints"
)

plot(
  gpp_ed.bfast,
  type="all",
  main="Trend and breakpoints"
)




