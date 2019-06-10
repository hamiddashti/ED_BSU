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

ws_reg <- read.csv("ws_reg15.csv", header = TRUE, as.is = TRUE)
ws_obs <- read.csv("ws_obs.csv", header = TRUE, as.is = TRUE)
dates <- seq(as.Date("2014/10/1"), as.Date("2017/9/30"), "days") # calibration period
ws_obs$dates <- dates

#------------ Prepare data for RRMSE and one-to-one plots-----

cal_dates <- seq(as.Date("2014/10/1"), as.Date("2016/9/30"), "days") # calibration period
val_dates <- seq(as.Date("2016/10/1"), as.Date("2017/9/30"), "days") # validation period
reg.cal.daily <- ws_reg$GPP[which(ws_reg$dates==cal_dates[1]):which(ws_reg$dates==cal_dates[length(cal_dates)])]
reg.val.daily <- ws_reg$GPP[which(ws_reg$dates==val_dates[1]):which(ws_reg$dates==val_dates[length(val_dates)])]
obs.cal.daily <- ws_obs$GPP[which(ws_obs$dates==cal_dates[1]):which(ws_obs$dates==cal_dates[length(cal_dates)])]
obs.val.daily <- ws_obs$GPP[which(ws_obs$dates==val_dates[1]):which(ws_obs$dates==val_dates[length(val_dates)])]

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
MAE.cal.obs.reg.daily <- mae(obs.cal.daily,reg.cal.daily)
R2.cal.obs.reg.daily <- caret::R2(obs.cal.daily,reg.cal.daily)
MAE.val.obs.reg.daily <- mae(obs.val.daily,reg.val.daily)
R2.val.obs.reg.daily <- caret::R2(obs.val.daily,reg.val.daily)

MAE.cal.obs.reg.monthly <- mae(obs.cal.monthly$GPP,reg.cal.monthly$GPP)
R2.cal.obs.reg.monthly <- caret::R2(obs.cal.monthly$GPP,reg.cal.monthly$GPP)
MAE.val.obs.reg.monthly <- mae(obs.val.monthly$GPP,reg.val.monthly$GPP)
R2.val.obs.reg.monthly <- caret::R2(obs.val.monthly$GPP,reg.val.monthly$GPP)

#-------------some simple ploting--------------------------------------------

df_all_daily <- data.frame(dates,ws_obs$GPP,ws_reg$GPP)
colnames(df_all_daily) <- c('Date', 'Obs',"Reg")

datm <- melt(df_all_daily,id.vars = "Date")
datm$variable <- factor(datm$variable,levels = c('Obs',"Reg"),
                        labels=c("Observation", "Regularized"))
graphics.off()
ggplot(datm) + 
  geom_line(aes(x = Date,y = value,colour=variable ,linetype = variable),size=1)+
  scale_fill_manual(values=c("black","red"))+
  theme_bw()+xlab('Time') + ylab('GPP [kgC/m2/yr')+
  theme(legend.title=element_blank(),legend.text = element_text(size=12),legend.position="top",
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))


