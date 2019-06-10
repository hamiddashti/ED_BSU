rm(list = ls())
require(bfast)
require(quantmod)
library(greenbrown)
library(lubridate)
library(ggplot2)
library(reshape2)
library(Fgmutils)
library(xts)
setwd("N:/Data02/bcal/Personal/hamid/ED_opt/working/Timeseries")

#------------ Load the datasets ----------------------------

ws_opt <- read.csv("ws_opt.csv", header = TRUE, as.is = TRUE)
ws_reg <- read.csv("ws_reg.csv", header = TRUE, as.is = TRUE)
ws_obs <- read.csv("ws_obs.csv", header = TRUE, as.is = TRUE)
dates <- seq(as.Date("2014/10/1"), as.Date("2017/9/30"), "days") # calibration period
ws_obs$dates <- dates

#------------ Prepare data for RRMSE and one-to-one plots-----

cal_dates <- seq(as.Date("2014/10/1"), as.Date("2016/9/30"), "days") # calibration period
val_dates <- seq(as.Date("2016/10/1"), as.Date("2017/9/30"), "days") # validation period
opt.cal.daily <- ws_opt$GPP[which(ws_opt$dates==cal_dates[1]):which(ws_opt$dates==cal_dates[length(cal_dates)])]
opt.val.daily <- ws_opt$GPP[which(ws_opt$dates==val_dates[1]):which(ws_opt$dates==val_dates[length(val_dates)])]
reg.cal.daily <- ws_reg$GPP[which(ws_reg$dates==cal_dates[1]):which(ws_reg$dates==cal_dates[length(cal_dates)])]
reg.val.daily <- ws_reg$GPP[which(ws_reg$dates==val_dates[1]):which(ws_reg$dates==val_dates[length(val_dates)])]
obs.cal.daily <- ws_obs$GPP[which(ws_obs$dates==cal_dates[1]):which(ws_obs$dates==cal_dates[length(cal_dates)])]
obs.val.daily <- ws_obs$GPP[which(ws_obs$dates==val_dates[1]):which(ws_obs$dates==val_dates[length(val_dates)])]

opt.cal.monthly <- aggregate(opt.cal.daily, list(format(cal_dates, "%Y-%m")), mean)
colnames(opt.cal.monthly) <- c('Date', 'GPP')
opt.val.monthly <- aggregate(opt.val.daily, list(format(val_dates, "%Y-%m")), mean)
colnames(opt.val.monthly) <- c('Date', 'GPP')
reg.cal.monthly <- aggregate(reg.cal.daily, list(format(cal_dates, "%Y-%m")), mean)
colnames(reg.cal.monthly) <- c('Date', 'GPP')
reg.val.monthly <- aggregate(reg.val.daily, list(format(val_dates, "%Y-%m")), mean)
colnames(reg.val.monthly) <- c('Date', 'GPP')
obs.cal.monthly <- aggregate(obs.cal.daily, list(format(cal_dates, "%Y-%m")), mean)
colnames(obs.cal.monthly) <- c('Date', 'GPP')
obs.val.monthly <- aggregate(obs.val.daily, list(format(val_dates, "%Y-%m")), mean)
colnames(obs.val.monthly) <- c('Date', 'GPP')


#ws5_reg_gpp <- ws5_reg$GPP[which(ws5_reg$dates==dates[1]):which(ws5_reg$dates==dates[length(dates)])]
#ws_obs_gpp <- ws_obs_tmp$GPP[which(ws_obs_tmp$dates=="10/1/2014"):which(ws_obs_tmp$dates=="9/30/2016")]

#ws5_opt_df <- data.frame(dates,ws5_opt_gpp)
#colnames(ws5_opt_df) <- c("Date","GPP")

#ws5_reg_df <- data.frame(dates,ws5_reg_gpp)
#colnames(ws5_reg_df) <- c("Date","GPP")

#ws_obs_df <- data.frame(dates,ws_obs_gpp)
#colnames(ws_obs_df) <- c("Date","GPP")

#ws5_opt_monthly <- aggregate(ws5_opt_df$GPP, list(format(ws5_opt_df$Date, "%Y-%m")), max)
#colnames(ws5_opt_monthly) <- c('Date', 'GPP')

#ws5_reg_monthly <- aggregate(ws5_reg_df$GPP, list(format(ws5_reg_df$Date, "%Y-%m")), max)
#colnames(ws5_reg_monthly) <- c('Date', 'GPP')

#ws_obs_monthly <- aggregate(ws_obs_df$GPP, list(format(ws_obs_df$Date, "%Y-%m")), max)
#colnames(ws_obs_monthly) <- c('Date', 'GPP')

# tmp_opt <- zoo(ws5_opt_df$GPP,ws5_opt_df$Date)  # weekly interpolation
# tmp_opt_week <- apply.weekly(tmp_opt, max)
# ws5_opt_weekly <- as.data.frame(tmp_opt_week)
# ws5_opt_weekly$Date <- as.Date(rownames(ws5_opt_weekly))
# colnames(ws5_opt_weekly) <- c('GPP','Date')
# 
# tmp_reg <- zoo(ws5_reg_df$GPP,ws5_reg_df$Date)
# tmp_reg_week <- apply.weekly(tmp_reg, max)
# ws5_reg_weekly <- as.data.frame(tmp_reg_week)
# ws5_reg_weekly$Date <- as.Date(rownames(ws5_reg_weekly))
# colnames(ws5_reg_weekly) <- c('GPP','Date')
# 
# tmp_obs <- zoo(ws_obs_df$GPP,ws_obs_df$Date)
# tmp_obs_week <- apply.weekly(tmp_obs, max)
# ws_obs_weekly <- as.data.frame(tmp_obs_week)
# ws_obs_weekly$Date <- as.Date(rownames(ws_obs_weekly))
# colnames(ws_obs_weekly) <- c('GPP','Date')
# 

# df_all_daily <- data.frame(dates,ws_obs_gpp,ws5_opt_gpp,ws5_reg_gpp)
# colnames(df_all_daily) <- c('Date', 'Obs', "Opt","Reg")
# 
# df_all_monthly <- data.frame(ws5_opt_monthly$Date,ws_obs_monthly$GPP,ws5_opt_monthly$GPP,ws5_reg_monthly$GPP)
# colnames(df_all_monthly) <- c('Date', 'Obs', "Opt","Reg")
# 
# df_all_weekly <- data.frame(ws5_opt_weekly$Date,ws_obs_weekly$GPP,ws5_opt_weekly$GPP,ws5_reg_weekly$GPP)
# colnames(df_all_weekly) <- c('Date', 'Obs', "Opt","Reg")



#------------Calculate the RMSEs -------------------------------------------
MAE.cal.obs.opt.daily <- mae(obs.cal.daily,opt.cal.daily)
R2.cal.obs.opt.daily <- caret::R2(obs.cal.daily,opt.cal.daily)
MAE.cal.obs.reg.daily <- mae(obs.cal.daily,reg.cal.daily)
R2.cal.obs.reg.daily <- caret::R2(obs.cal.daily,reg.cal.daily)
MAE.val.obs.opt.daily <- mae(obs.val.daily,opt.val.daily)
R2.val.obs.opt.daily <- caret::R2(obs.val.daily,opt.val.daily)
MAE.val.obs.reg.daily <- mae(obs.val.daily,reg.val.daily)
R2.val.obs.reg.daily <- caret::R2(obs.val.daily,reg.val.daily)

MAE.cal.obs.opt.monthly <- mae(obs.cal.monthly$GPP,opt.cal.monthly$GPP)
R2.cal.obs.opt.monthly <- caret::R2(obs.cal.monthly$GPP,opt.cal.monthly$GPP)
MAE.cal.obs.reg.monthly <- mae(obs.cal.monthly$GPP,reg.cal.monthly$GPP)
R2.cal.obs.reg.monthly <- caret::R2(obs.cal.monthly$GPP,reg.cal.monthly$GPP)
MAE.val.obs.opt.monthly <- mae(obs.val.monthly$GPP,opt.val.monthly$GPP)
R2.val.obs.opt.monthly <- caret::R2(obs.val.monthly$GPP,opt.val.monthly$GPP)
MAE.val.obs.reg.monthly <- mae(obs.val.monthly$GPP,reg.val.monthly$GPP)
R2.val.obs.reg.monthly <- caret::R2(obs.val.monthly$GPP,reg.val.monthly$GPP)

# MAE_obs_opt_monthly <- mae(ws_obs_monthly$GPP,ws5_opt_monthly$GPP)
# MAE_obs_reg_monthly <- mae(ws_obs_monthly$GPP,ws5_reg_monthly$GPP)
# R2_obs_opt_monthly <- caret::R2(ws_obs_monthly$GPP,ws5_opt_monthly$GPP)
# R2_obs_reg_monthly <- caret::R2(ws_obs_monthly$GPP,ws5_reg_monthly$GPP)

# MAE_obs_opt_weekly <- mae(ws_obs_weekly$GPP,ws5_opt_weekly$GPP)
# MAE_obs_reg_weekly <- mae(ws_obs_weekly$GPP,ws5_reg_weekly$GPP)
# R2_obs_opt_weekly <- caret::R2(ws_obs_weekly$GPP,ws5_opt_weekly$GPP)
# R2_obs_reg_weekly <- caret::R2(ws_obs_weekly$GPP,ws5_reg_weekly$GPP)


#-------------some simple ploting--------------------------------------------

df_all_daily <- data.frame(dates,ws_obs$GPP,ws_opt$GPP,ws_reg$GPP)
colnames(df_all_daily) <- c('Date', 'Obs', "Opt","Reg")

datm <- melt(df_all_daily,id.vars = "Date")
datm$variable <- factor(datm$variable,levels = c('Obs', "Opt","Reg"),
                        labels=c("Observation", "No regularization", "Regularized"))
graphics.off()
ggplot(datm) + 
  geom_line(aes(x = Date,y = value,colour=variable ,linetype = variable),size=1)+
  scale_fill_manual(values=c("black","blues","red"))+
  theme_bw()+xlab('Time') + ylab('GPP [kgC/m2/yr')+
  theme(legend.title=element_blank(),legend.text = element_text(size=12),legend.position="top",
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))



#-----------trend analysis monthly ---------------------------------------

obs_ts_monthly <- ts(df_all_monthly$Obs,     # random data
               start = c(2014, 10),
               frequency =12 )
print(obs_ts_monthly)
Seasonality(obs_ts_monthly)
obs_trd_monthly <- Trend(obs_ts_monthly,method ="STM")
obs_trd_monthly
plot(obs_trd_monthly) 

opt_ts_monthly <- ts(df_all_monthly$Opt,     # random data
             start = c(2014, 10),
             frequency =12 )
print(opt_ts_monthly)
Seasonality(opt_ts_monthly)
opt_trd_monthly <- Trend(opt_ts_monthly,method ="STM")
opt_trd_monthly
plot(opt_trd_monthly) 

reg_ts_monthly <- ts(df_all_monthly$Reg,     # random data
             start = c(2014, 10),
             frequency =12 )
print(reg_ts_monthly)
Seasonality(reg_ts_monthly)
reg_trd_monthly <- Trend(reg_ts_monthly,method ="STM")
reg_trd_monthly
plot(reg_trd_monthly)


#-----------trend analysis weekly ---------------------------------------

obs_ts_weekly <- ts(df_all_weekly$Obs,     # random data
                    start = decimal_date(ymd("2014-10-05")),
                     frequency =52 )
print(obs_ts_weekly)
Seasonality(obs_ts_weekly)
obs_trd_weekly <- Trend(obs_ts_weekly,method ="STM")
obs_trd_weekly
plot(obs_trd_weekly) 

opt_ts_weekly <- ts(df_all_weekly$Opt,     # random data
                    decimal_date(ymd("2014-10-05")),
                     frequency =52 )
print(opt_ts_weekly)
Seasonality(opt_ts_weekly)
opt_trd_weekly <- Trend(opt_ts_weekly,method ="STM")
opt_trd_weekly
plot(opt_trd_weekly) 

reg_ts_weekly <- ts(df_all_weekly$Reg,     # random data
                    decimal_date(ymd("2014-10-05")),
                     frequency =52 )
print(reg_ts_weekly)
Seasonality(reg_ts_weekly)
reg_trd_weekly <- Trend(reg_ts_weekly,method ="STM")
reg_trd_weekly
plot(reg_trd_weekly)



#------------Phenology analysis monthly------------------------------------
var.tsgf="TSGFssa"
var.approach = "Trs"

obs.phen_monthly <- Phenology(obs_ts_monthly, tsgf=var.tsgf, approach=var.approach)
obs.phen_monthly
obs.phen_monthly$pop
plot(obs.phen_monthly)

opt.phen_monthly <- Phenology(opt_ts_monthly, tsgf=var.tsgf, approach=var.approach)
opt.phen_monthly
opt.phen_monthly$pop
plot(opt.phen_monthly)

reg.phen_monthly <- Phenology(reg_ts_monthly,tsgf=var.tsgf, approach=var.approach)
reg.phen_monthly
reg.phen_monthly$pop
plot(reg.phen_monthly)

#------------Phenology analysis weekly ------------------------------------
var.tsgf="TSGFssa"
var.approach = "Trs"

obs.phen_weekly <- Phenology(obs_ts_weekly, tsgf=var.tsgf, approach=var.approach)
obs.phen_weekly
obs.phen_weekly$pop
plot(obs.phen_weekly)

opt.phen_weekly <- Phenology(opt_ts_weekly, tsgf=var.tsgf, approach=var.approach)
opt.phen_weekly
opt.phen_weekly$pop
plot(opt.phen_weekly)

reg.phen_weekly <- Phenology(reg_ts_weekly,tsgf=var.tsgf, approach=var.approach)
reg.phen_weekly
reg.phen_weekly$pop
plot(reg.phen_weekly)

# BFAST analysis ---------------
obs.bfast <- bfast(obs_ts_weekly,h=0.15,season = "dummy",max.iter =1)
plot(
  obs.bfast,
  type="all",
  main="Trend and breakpoints"
)


opt.bfast <- bfast(opt_ts_weekly,h=0.15,season = "dummy",max.iter =1)
plot(
  opt.bfast,
  type="all",
  main="Trend and breakpoints"
)

reg.bfast <- bfast(reg_ts_weekly,h=0.15,season = "dummy",max.iter =1)
plot(
  reg.bfast ,
  type="all",
  main="Trend and breakpoints"
)

obs.stl <- stl(obs_ts_weekly,s.window="periodic")
plot(obs.stl,main="Observatio")

opt.stl <- stl(opt_ts_weekly,s.window="periodic")
plot(opt.stl,main="No regularization")
obs.stl <- stl(obs_ts_weekly,s.window="periodic")
plot(obs.stl,main="Regularaized")


#-----------trend analysis daily ---------------------------------------
a=runif(731)

#obs_ts_daily <- ts(df_all_daily$Obs,     # random data
 #            start=decimal_date(as.Date("2014-10-01")),
  #           frequency =365 )

obs_ts_daily <- ts(a,     # random data
              start=decimal_date(ymd("2006-12-27")),
               frequency =365)
head(obs_ts_daily)
              
obs_ts_daily <- ts(a,     # random data
                   start=decimal_date(as.Date("2014-10-01")),
                   frequency =365 )


head(obs_ts_daily)
Seasonality(obs_ts_daily)
obs_trd_daily <- Trend(obs_ts_daily,method ="STM")
obs_trd_daily
plot(obs_trd_daily) 

opt_ts_daily <- ts(df_all_daily$Opt,     # random data
             start = c(2014, 274),
             frequency =365 )
head(opt_ts_daily)
Seasonality(opt_ts_daily)
opt_trd_daily <- Trend(opt_ts_daily,method ="STM")
opt_trd_daily
plot(opt_trd_daily) 

reg_ts_daily <- ts(df_all_daily$Reg,     # random data
             start = c(2014, 274),
             frequency =365)
print(reg_ts_daily)
Seasonality(reg_ts_daily)
reg_trd_daily <- Trend(reg_ts_daily,method ="STM")
reg_trd_daily
plot(reg_trd_daily)

#------------Phenology analysis daily------------------------------------
var.tsgf="TSGFssa"
var.approach = "Trs"

obs.phen_daily <- Phenology(obs_ts_daily, tsgf=var.tsgf, approach=var.approach)
obs.phen_daily
obs.phen_daily$pop
plot(obs.phen_daily)

opt.phen_daily <- Phenology(opt_ts_daily , tsgf=var.tsgf, approach=var.approach)
opt.phen_daily 
opt.phen_daily$pop
plot(opt.phen_daily)

reg.phen_monthly <- Phenology(reg_ts_monthly,tsgf=var.tsgf, approach=var.approach)
reg.phen_monthly
reg.phen_monthly$pop
plot(reg.phen_monthly)

#-------------- 30 years trend analysis ----------------













landsat.stl <- stl(landsat_ts,s.window="periodic")
plot(landsat.stl,main="Landsat")
landsat.bfast <- bfast(landsat_ts,h=0.15,season = "dummy",max.iter =1)
plot(
  landsat.bfast,
  type="trend",
  main="Trend and breakpoints"
)



#-------------GRAVE YARD ----------------------
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



par(mfrow=c(3,1))
plot(modis.tren,ty="l",col="red")

plot(landsat.tren,ty="l",col="blue")

plot(sim.tren,ty="l",col="black")










