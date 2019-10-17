rm(list = ls())
#require(bfast)
#require(quantmod)
#library(greenbrown)
#library(lubridate)
library(ggplot2)
library(reshape2)
library(Fgmutils)
#library(xts)
library(anytime)

library(zoo)
library(signal)
setwd("N:/Data02/bcal/Personal/hamid/ED_opt/working/Timeseries")


#------------ Load the datasets ----------------------------

ws_opt <- read.csv("ws_trend.csv", header = TRUE, as.is = TRUE)
ws_obs <- read.csv("WS_obs.csv", header = TRUE, as.is = TRUE)

obs_dates <- seq(as.Date(ws_obs$Date[1],"%m/%d/%Y"), as.Date(ws_obs$Date[length(ws_obs$Date)],"%m/%d/%Y"), "days") # Observation period
opt_dates <- seq(as.Date(ws_opt$dates[1],"%m/%d/%Y"), as.Date(ws_opt$dates[length(ws_opt$dates)],"%m/%d/%Y"), "days")
ws_obs$dates <- obs_dates
ws_opt$dates <- opt_dates

ws_obs_df <- data.frame(ws_obs$dates,ws_obs$GPP)
tmp <- ws_opt$GPP[which(ws_opt$dates==obs_dates[1]):which(ws_opt$dates==obs_dates[length(obs_dates)])]
ws_opt_df <- data.frame(obs_dates,tmp)
colnames(ws_opt_df) <- c("Date","GPP") 
colnames(ws_obs_df) <- c("Date","GPP") 

#######################################################################################

#--------------------Get the daily mean of the data ---------------------------------
mean(ws_opt_df$GPP)
mean(ws_obs_df$GPP)


#######################################################################################
####################### Prepare data ###################################################
########################################################################################

#------------ Daily data ----------

cal_dates <- seq(as.Date("2014/10/1"), as.Date("2016/9/30"), "days") # calibration period
val_dates <- seq(as.Date("2016/10/1"), as.Date("2017/9/30"), "days") # validation period
opt.cal.daily <- ws_opt$GPP[which(ws_opt$dates==cal_dates[1]):which(ws_opt$dates==cal_dates[length(cal_dates)])]
opt.val.daily <- ws_opt$GPP[which(ws_opt$dates==val_dates[1]):which(ws_opt$dates==val_dates[length(val_dates)])]
obs.cal.daily <- ws_obs$GPP[which(ws_obs$dates==cal_dates[1]):which(ws_obs$dates==cal_dates[length(cal_dates)])]
obs.val.daily <- ws_obs$GPP[which(ws_obs$dates==val_dates[1]):which(ws_obs$dates==val_dates[length(val_dates)])]
df_all_daily <- data.frame(obs_dates,ws_obs_df$GPP,ws_opt_df$GPP)
colnames(df_all_daily) <- c('Date', 'Observations', "Simulations")



#------------ Montly data ------------
opt.cal.monthly <- aggregate(opt.cal.daily, list(format(cal_dates, "%Y-%m")), mean)
colnames(opt.cal.monthly) <- c('Date', 'GPP')
opt.val.monthly <- aggregate(opt.val.daily, list(format(val_dates, "%Y-%m")), mean)
colnames(opt.val.monthly) <- c('Date', 'GPP')
obs.cal.monthly <- aggregate(obs.cal.daily, list(format(cal_dates, "%Y-%m")), mean)
colnames(obs.cal.monthly) <- c('Date', 'GPP')
obs.val.monthly <- aggregate(obs.val.daily, list(format(val_dates, "%Y-%m")), mean)
colnames(obs.val.monthly) <- c('Date', 'GPP')
obs.all.monthly <- aggregate(ws_obs_df$GPP,list(format(obs_dates,"%Y-%m")),mean)
colnames(obs.all.monthly) <- c('Date', 'GPP')
opt.all.monthly <- aggregate(ws_opt_df$GPP,list(format(obs_dates,"%Y-%m")),mean)
colnames(opt.all.monthly) <- c('Date', 'GPP')
df.all.monthly <- data.frame(obs.all.monthly$Date,obs.all.monthly$GPP,opt.all.monthly$GPP)
colnames(df.all.monthly) <- c('Date', 'Observations', 'Simulations')


# ----------- Weekly data ------------
opt.cal.weekly <- aggregate(opt.cal.daily, list(format(cal_dates, "%Y-%W")), mean)
colnames(opt.cal.weekly) <- c('Date', 'GPP')
opt.val.weekly <- aggregate(opt.val.daily, list(format(val_dates, "%Y-%W")), mean)
colnames(opt.val.weekly) <- c('Date', 'GPP')
obs.cal.weekly <- aggregate(obs.cal.daily, list(format(cal_dates, "%Y-%W")), mean)
colnames(obs.cal.weekly) <- c('Date', 'GPP')
obs.val.weekly <- aggregate(obs.val.daily, list(format(val_dates, "%Y-%W")), mean)
colnames(obs.val.weekly) <- c('Date', 'GPP')
obs.all.weekly <- aggregate(ws_obs_df$GPP,list(format(obs_dates,"%Y-%W")),mean)
colnames(obs.all.weekly) <- c('Date', 'GPP')
opt.all.weekly <- aggregate(ws_opt_df$GPP,list(format(obs_dates,"%Y-%W")),mean)
colnames(opt.all.weekly) <- c('Date', 'GPP')
df.all.weekly <- data.frame(obs.all.weekly$Date,obs.all.weekly$GPP,opt.all.weekly$GPP)
colnames(df.all.weekly) <- c('Date', 'Observations', 'Simulations')



#------------Calculate the MAEs -------------------------------------------
RMSE.cal.obs.opt.daily <- rmse(obs.cal.daily,opt.cal.daily)
R2.cal.obs.opt.daily <- caret::R2(obs.cal.daily,opt.cal.daily)
NSE.cal.obs.opt.daily <- hydroGOF::NSE(obs.cal.daily,opt.cal.daily)
CV.cal.obs.opt.daily <-  (sqrt((sum((obs.cal.daily - opt.cal.daily)^2))/length(opt.cal.daily))*100)/mean(obs.cal.daily)


RMSE.val.obs.opt.daily <- rmse(obs.val.daily,opt.val.daily)
R2.val.obs.opt.daily <- caret::R2(obs.val.daily,opt.val.daily)
NSE.val.obs.opt.daily <- hydroGOF::NSE(obs.val.daily,opt.val.daily)
CV.val.obs.opt.daily <-  (sqrt((sum((obs.val.daily - opt.val.daily)^2))/length(opt.val.daily))*100)/mean(obs.val.daily)

RMSE.cal.obs.opt.monthly <- rmse(opt.cal.weekly$GPP,obs.cal.weekly$GPP)
R2.cal.obs.opt.monthly <- caret::R2(obs.cal.monthly$GPP,opt.cal.monthly$GPP)
NSE.cal.obs.opt.monthly <- hydroGOF::NSE(obs.cal.monthly$GPP,opt.cal.monthly$GPP)
CV.cal.obs.opt.monthly <-  (sqrt((sum((obs.cal.monthly$GPP - opt.cal.monthly$GPP)^2))/length(opt.cal.monthly$GPP))*100)/mean(obs.cal.monthly$GPP)

RMSE.val.obs.opt.monthly <- rmse(obs.val.monthly$GPP,opt.val.monthly$GPP)
R2.val.obs.opt.monthly <- caret::R2(obs.val.monthly$GPP,opt.val.monthly$GPP)
NSE.val.obs.opt.monthly <- hydroGOF::NSE(obs.val.monthly$GPP,opt.val.monthly$GPP)
CV.val.obs.opt.monthly <-  (sqrt((sum((obs.val.monthly$GPP - opt.val.monthly$GPP)^2))/length(opt.val.monthly$GPP))*100)/mean(obs.val.monthly$GPP)

RMSE.cal.obs.opt.weekly <- rmse(opt.cal.weekly$GPP,obs.cal.weekly$GPP)
RMSE.val.obs.opt.weekly <- rmse(opt.val.weekly$GPP,obs.val.weekly$GPP)

R2.cal.obs.opt.weekly <- caret::R2(obs.cal.weekly$GPP,opt.cal.weekly$GPP)
R2.val.obs.opt.weekly <- caret::R2(obs.val.weekly$GPP,opt.val.weekly$GPP)


#############################################################################
########################## some simple ploting ##############################
#############################################################################


#-------------------------- Daily plot -------------------------------------- 

graphics.off()
ylabel_p="GPP ";unit="[kgC/m2/yr]"
#tiff("N:/Data02/bcal/Personal/hamid/ED_opt/working/Manuscript/Figures/ws_calibration.tiff", units="in", width=2.7, height=2.3, res=300)
ggplot() + 
  geom_line(data = ws_obs_df, aes(x =as.Date(ws_obs_df$Date), y = ws_obs_df$GPP,color="EC tower GPP"),size=0.5) +
  geom_line(data = ws_opt_df, aes(x =as.Date(ws_opt_df$Date), y = ws_opt_df$GPP,color="Simulated GPP"),size=0.5)+
  labs(color = '') + theme_bw() +labs(x="Time", y=paste(ylabel_p,unit))+ 
  theme(legend.title = element_blank(),legend.text = element_text(size=6),
        axis.text=element_text(size=6),
        axis.title=element_text(size=6,face="bold"),legend.position = "top",legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10)) +ylim(0,2.3)
#dev.off()
#-------------------------- Monthly plot -------------------------------------- 

df.all.monthly$Date <- as.Date(anytime(df.all.monthly$Date))
datm2 <- melt(df.all.monthly,id = "Date")
graphics.off()
ggplot(datm2) + 
  geom_line(aes(x = Date,y = value,colour=variable ,linetype = variable),size=1)+
  scale_fill_manual(values=c("black","blues"))+
  theme_bw()+xlab('Time') + ylab('GPP [kgC/m2/yr')+
  theme(legend.title=element_blank(),legend.text = element_text(size=12),legend.position="top",
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))
############################################################################################

