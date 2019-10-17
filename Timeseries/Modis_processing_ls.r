
####### Modis time series processing ###############################




#--------------Modis quality control for GPP --------------
# References: https://www.nceas.ucsb.edu/~pau/StephanieSite/Home_files/MODIS_LP_QA_Tutorial-1.pdf 
# And https://www.r-bloggers.com/modis-qc-bits/

# Fist step is to convert integer to bit
#intbit <- as.integer(intToBits(1075576832))[1:8]       # the number [1:8] is to select the  bit (MODIS GPP file specification)
# Then since HDF files are written in endian format we have to reverese this order

#intbit_rev <- rev(intbit)

# Then we flag the data based on the bit number (page 9  https://www.nceas.ucsb.edu/~pau/StephanieSite/Home_files/MODIS_LP_QA_Tutorial-1.pdf)
# NOTE WE SHOULD READ THE BITS FROM RIGHT TO LEFT

# Everything is SAVED in "N:/Data02/bcal/Personal/hamid/ED_opt/working/Timeseries/Modis/Trend_analysis.RData"


####################################################################

rm(list = ls())
setwd("N:/Data02/bcal/Personal/hamid/ED_opt/working/Timeseries/Modis")
library(zoo)
library(signal)
library(lubridate)
library(anytime)
require(grid)
library(ggplot2)
library(Fgmutils)
library(Rbeast)

source('N:/Data02/bcal/Personal/hamid/ED_BSU/Timeseries/My_SOS.R')
source('N:/Data02/bcal/Personal/hamid/ED_BSU/Timeseries/My_EOS.R')
source('N:/Data02/bcal/Personal/hamid/ED_BSU/Timeseries/multiplot.R')
#####################################################################

# ----------- Load the simulated and Observed ----------------------------------

ls_opt <- read.csv("ls_trend.csv", header = TRUE, as.is = TRUE)
ls_obs <- read.csv("ls_obs.csv", header = TRUE, as.is = TRUE)
obs_dates <- seq(as.Date("2015/10/1"), as.Date("2017/9/30"), "days") # Observation period
opt_dates <- seq(as.Date("2000/02/11"), as.Date("2017/9/30"), "days")
ls_obs$dates <- obs_dates
ls_opt$dates <- opt_dates
#smooth_opt <- sgolayfilt(ws_opt$GPP,3,11)

df_obs_gpp <- data.frame(ls_obs$dates,ls_obs$GPP)
df_opt_gpp <- data.frame(ls_opt$dates,ls_opt$GPP)
colnames(df_obs_gpp) <- c("Date","GPP") 
colnames(df_opt_gpp) <- c("Date","GPP") 


# --------------- Plot Simulated vs Observed ---------------


tmp_df_opt <- df_opt_gpp[df_opt_gpp$Date>"2015/10/1",]
ylabel_p="GPP ";unit="[kgC/m2/yr]"
graphics.off()
#tiff("N:/Data02/bcal/Personal/hamid/ED_opt/working/Manuscript/Figures/ws_calibration.tiff", units="in", width=2.7, height=2.3, res=300)
ggplot() + 
  geom_line(data = df_obs_gpp, aes(x =as.Date(df_obs_gpp$Date), y = df_obs_gpp$GPP,color="Measured GPP"),size=0.5) +
  geom_line(data = tmp_df_opt, aes(x =as.Date(tmp_df_opt$Date), y = tmp_df_opt$GPP,color="Simulated GPP"),size=0.5)+
  labs(color = '') + theme_bw() +labs(x="Time", y=paste(ylabel_p,unit))+ 
  theme(legend.title = element_blank(),legend.text = element_text(size=6),
        axis.text=element_text(size=6),
        axis.title=element_text(size=6,face="bold"),legend.position = "top",legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10)) #+
#scale_color_manual(values = c(
#  'Measured GPP' = 'black',
 # 'Simulated GPP' = 'red')) +
#dev.off()

# --------------- Lets do the maximum 8-days composite for simulations --------------------------------
# We do this step to exclude low-biased simulated GPP 
#date_range <- seq(as.Date("2000/02/11"), as.Date("2017/9/30"), "days") # This is the range that simulated and observed and measured overlap

# max <- 8  # every 8 days
# x <- seq_along(df_opt_gpp$GPP)
# gpp_splits <- split(df_opt_gpp$GPP, ceiling(x/max))
# gpp_date_splits <- split(df_opt_gpp$Date, ceiling(x/max))
# sim_gpp <- rep(NA, length(gpp_splits))
# sim_gpp_date <- rep(NA, length(gpp_splits))
# 
#     for (k in 1:length(gpp_splits)){
#       sim_gpp[k] <- max(gpp_splits[[k]])
#       sim_gpp_date[k] <- max(gpp_date_splits[[k]])
#     }
# 
# sim_gpp_date <- as.Date(sim_gpp_date)
# smooth_sim <- sgolayfilt(sim_gpp,3,11)
# df_sim <- data.frame(sim_gpp_date,smooth_sim)
# colnames(df_sim) <- c('dates', 'GPP')
# plot(df_sim,ty='l')

# --------------- Lets do the maximum Monthly composite for simulations --------------------------------
# We do this step to exclude low-biased simulated GPP 
#date_range <- seq(as.Date("2000/02/11"), as.Date("2017/9/30"), "days") # This is the range that simulated and observed and measured overlap

sim_mon_tmp <- aggregate(df_opt_gpp$GPP, list(format(df_opt_gpp$Date, "%Y-%m")), max)
sim_mon_gpp <- data.frame(as.Date(anytime(sim_mon_tmp$Group.1,"%Y-%m")),sim_mon_tmp$x)   # The anytime() convert incomplete date to complete if not using it as.Date returns NA
colnames(sim_mon_gpp) <- c("Date", "GPP")
plot(sim_mon_gpp,ty='l')

# --------------- Lets do the maximum 8-day composite for observations --------------------------------
# We do this step to exclude low-biased observed GPP 

# 
# max <- 8  # every 8 days
# x <- seq_along(df_obs_gpp$GPP)
# gpp_splits <- split(df_obs_gpp$GPP, ceiling(x/max))
# gpp_date_splits <- split(df_obs_gpp$Date, ceiling(x/max))
# obs_gpp <- rep(NA, length(gpp_splits))
# obs_gpp_date <- rep(NA, length(gpp_splits))
# 
# for (k in 1:length(gpp_splits)){
#   obs_gpp[k] <- max(gpp_splits[[k]])
#   obs_gpp_date[k] <- max(gpp_date_splits[[k]])
# }
# obs_gpp_date <- as.Date(obs_gpp_date)
# smooth_obs <- sgolayfilt(obs_gpp,3,11)
# df_obs <- data.frame(obs_gpp_date,smooth_obs)
# colnames(df_obs) <- c('dates', 'GPP')
# plot(df_obs,ty='l')
# new_df_obs <- df_obs[df_obs >= "2015-01-01",] 
# 

# --------------- Lets do the maximum Monthly composite for observation --------------------------------
# We do this step to exclude low-biased simulated GPP 
#date_range <- seq(as.Date("2000/02/11"), as.Date("2017/9/30"), "days") # This is the range that simulated and observed and measured overlap

obs_mon_tmp <- aggregate(df_obs_gpp$GPP, list(format(df_obs_gpp$Date, "%Y-%m")), max)
obs_mon <- data.frame(as.Date(anytime(obs_mon_tmp$Group.1,"%Y-%m")),obs_mon_tmp$x)   # The anytime() convert incomplete date to complete if not using it as.Date returns NA
colnames(obs_mon) <- c("Date", "GPP")
plot(obs_mon,ty='l')


# ------------Load csv files (SR and GPP) downloaded from GEE ---------------
tmp1 <- read.csv("EC_ls_MOD09A1006_SR8d_t_2000_2018.csv", header = TRUE, as.is = TRUE)
tmp2 <- as.matrix(tmp1[2:(length(tmp1)-1)])  # remove the first and last columns
# This is 8days products so for this date range we have 821 observations. 
# We also have 8 variables downloaded --> 8*821 = 6568
data <- matrix(tmp2, nrow = 821, byrow = TRUE)
data <- as.data.frame(data[,2:8]*0.0001)
colnames(data) <- c("B1","B2","B3","B4","B5","B6","B7")  # RED; NIR; BLUE; GREEN; SWIR; SWIR; SWIR  

# Creating the date range 
tmp3 <- substr(names(tmp1[2:(length(tmp1)-1)]),2,11)
tmp4 <- matrix(tmp3,nrow = 821, byrow = TRUE)
modis_dates <- as.Date(tmp4[,1],"%Y.%m.%d" )
ls_sr <- cbind(modis_dates,data)
I_modis <- which(ls_sr$modis_dates<"2017-10-01")
NDVI <- (ls_sr$B2[I_modis] - ls_sr$B1[I_modis]) /(ls_sr$B2[I_modis] + ls_sr$B1[I_modis])
NIRv <- NDVI *ls_sr$B2[I_modis]
NIRv <- na.approx(NIRv)
I_nirv <- which(!is.na(NIRv))
tmp_modis_date <- modis_dates[I_modis]
df_modis_nirv <-  data.frame(modis_dates[I_nirv],NIRv[I_nirv])
colnames(df_modis_nirv) <- c("Date","NIRv")

plot(df_modis_nirv,ty='l')

tmp1 <- read.csv("EC_lS_MYD17A2H006_GPP8d_t_2000_2018.csv", header = TRUE, as.is = TRUE)
tmp2 <- as.matrix(tmp1[2:(length(tmp1)-1)])  # remove the first and last columns
# This is 8days products so for this date range we have 821 observations. 
# We also have 2 variables [QC and GPP] downloaded --> 2*821 = 6568
data <- matrix(tmp2, nrow = 821, byrow = TRUE)
data <- (data[,1]*0.0001)*(365/8) # make units [kgC/m2/year]
data <- na.approx(data)   # Fill NA values

# Creating the date range 
tmp3 <- substr(names(tmp1[2:(length(tmp1)-1)]),2,11)
tmp4 <- matrix(tmp3,nrow = 821, byrow = TRUE)
modis_dates <- as.Date(tmp4[,1],"%Y.%m.%d" )
df_modis_gpp <-  data.frame(modis_dates[I_modis],data[I_modis])
colnames(df_modis_gpp) <- c("Date","GPP")
plot(df_modis_gpp,ty='l')  # Just a simple plot to make sure everything is OK


# --------------- Lets do the maximum Monthly composite for Modis --------------------------------
# We do this step to exclude low-biased simulated GPP 
#date_range <- seq(as.Date("2000/02/11"), as.Date("2017/9/30"), "days") # This is the range that simulated and observed and measured overlap

modis_mon_gpp_tmp <- aggregate(df_modis_gpp$GPP, list(format(df_modis_gpp$Date, "%Y-%m")), max)
modis_mon_gpp <- data.frame(as.Date(anytime(modis_mon_gpp_tmp$Group.1,"%Y-%m")),modis_mon_gpp_tmp$x)   # The anytime() convert incomplete date to complete if not using it as.Date returns NA
colnames(modis_mon_gpp) <- c("Date", "GPP")
plot(modis_mon_gpp,ty='l')

modis_mon_nirv_tmp <- aggregate(df_modis_nirv$NIRv, list(format(df_modis_nirv$Date, "%Y-%m")), max)
modis_mon_nirv <- data.frame(as.Date(anytime(modis_mon_nirv_tmp$Group.1,"%Y-%m")),modis_mon_nirv_tmp$x)   # The anytime() convert incomplete date to complete if not using it as.Date returns NA
colnames(modis_mon_nirv) <- c("Date", "NIRv")
plot(modis_mon_nirv,ty='l')



# ------------ Apply SG filter to get higher quality data ----------------------------

smooth_sim_gpp <- sgolayfilt(sim_mon_gpp$GPP,3,11)
smooth_modis_gpp <- sgolayfilt(modis_mon_gpp$GPP,3,11)
smooth_modis_nirv <- sgolayfilt(modis_mon_nirv$NIRv,3,11)

df_sim_gpp <- data.frame(as.Date(sim_mon_gpp$Date),smooth_sim_gpp)
df_modis_gpp <- data.frame(as.Date(modis_mon_gpp$Date),smooth_modis_gpp)
df_modis_nirv <- data.frame(as.Date(modis_mon_nirv$Date),smooth_modis_nirv)
colnames(df_sim_gpp) <- c("Date","GPP")
colnames(df_modis_gpp) <- c("Date","GPP")
colnames(df_modis_nirv) <- c("Date","NIRv")

# df_modis_nirv <- data.frame(as.Date(ws_sr$modis_dates),smooth_nirv)
# df_modis_scaled_nirv <- data.frame(as.Date(ws_sr$modis_dates),scaled_nirv)
# smooth_gpp[smooth_gpp<0] <- 0
# df_modis_gpp <- data.frame(as.Date(ws_gpp$modis_dates),smooth_gpp)
# names(df_modis_nirv) <- c("dates", "NIRv")
# names(df_modis_gpp) <- c("dates", "GPP")
# names(df_modis_scaled_nirv) <- c("dates", "GPP")
# 
# df_modis_nirv_new <- data.frame(df_modis_nirv$dates[df_modis_nirv$NIRv>0],df_modis_nirv$NIRv[df_modis_nirv$NIRv>0]) # No NIRv <0
# names(df_modis_nirv_new) <- c("dates", "NIRv")


# ------------ Let find the SOS, EOS of simulated, observed,modis gpp and modis nirv -------------------------

modis_sos <- My_SOS(df_modis_gpp,"GPP")
modis_sos_daily <- modis_sos$Daily[2:366,] 
modis_sos_date <- as.Date(modis_sos$Date)
modis_sos_gpp <- rep(NA,dim(modis_sos$Daily)[2])
for (j in 1: dim(modis_sos$Daily)[2]) {
  
  modis_sos_gpp[j] <- modis_sos_daily[modis_sos$DOY[j],j]
}
modis_sos_point<- data.frame(as.Date(modis_sos$Date),modis_sos_gpp)


modisnir_sos <- My_SOS(df_modis_nirv,"NIRv")
modisnir_sos_daily <- modisnir_sos$Daily[2:366,] 
modisnir_sos_date <- as.Date(modisnir_sos$Date)
modisnir_sos_nir <- rep(NA,dim(modisnir_sos$Daily)[2])
for (j in 1: dim(modisnir_sos$Daily)[2]) {

  modisnir_sos_nir[j] <- modisnir_sos_daily[modisnir_sos$DOY[j],j]
}
modisnir_sos_point<- data.frame(as.Date(modisnir_sos$Date),modisnir_sos_nir)

sim_sos <- My_SOS(df_sim_gpp,"GPP")
sim_sos_daily <- sim_sos$Daily[2:366,] 
sim_sos_date <- as.Date(sim_sos$Date)
sim_sos_gpp <- rep(NA,dim(sim_sos$Daily)[2])
for (j in 1: dim(sim_sos$Daily)[2]) {
   
  sim_sos_gpp[j] <- sim_sos_daily[sim_sos$DOY[j],j]
}
sim_sos_point<- data.frame(as.Date(sim_sos$Date),sim_sos_gpp)



# 
# obs_sos <- My_SOS(new_df_obs,"GPP")
# obs_sos_daily <- obs_sos$Daily[2:366,] 
# obs_sos_date <- as.Date(obs_sos$Date)
# obs_sos_gpp <- rep(NA,dim(sim_sos$Daily)[2])
# for (j in 1: dim(obs_sos$Daily)[2]) {
#   
#   obs_sos_gpp[j] <- obs_sos_daily[obs_sos$DOY[j],j]
# }
# obs_sos_point<- data.frame(as.Date(obs_sos$Date),obs_sos_gpp)

modis_eos <- My_EOS(df_modis_gpp,"GPP")
modis_eos_daily <- modis_eos$Daily[2:366,] 
modis_eos_date <- as.Date(modis_eos$Date1)
modis_eos_gpp <- rep(NA,dim(modis_eos$Daily)[2])
for (j in 1: dim(modis_eos$Daily)[2]) {
  
  modis_eos_gpp[j] <- modis_eos_daily[modis_eos$DOY1[j],j]
}
modis_eos_point<- data.frame(as.Date(modis_eos$Date1),modis_eos_gpp)


modisnir_eos <- My_EOS(df_modis_nirv,"NIRv")
modisnir_eos_daily <- modisnir_eos$Daily[2:366,] 
modisnir_eos_date <- as.Date(modisnir_eos$Date1)
modisnir_eos_nir <- rep(NA,dim(modisnir_eos$Daily)[2])
for (j in 1: dim(modisnir_eos$Daily)[2]) {
  
  modisnir_eos_nir[j] <- modisnir_eos_daily[modisnir_eos$DOY1[j],j]
}
modisnir_eos_point<- data.frame(as.Date(modisnir_eos$Date1),modisnir_eos_nir)


sim_eos <- My_EOS(df_sim_gpp,"GPP")
sim_eos_daily <- sim_eos$Daily[2:366,] 
sim_eos_date <- as.Date(sim_eos$Date1)
sim_eos_gpp <- rep(NA,dim(sim_eos$Daily)[2])
for (j in 1: dim(sim_eos$Daily)[2]) {
  
  sim_eos_gpp[j] <- sim_eos_daily[sim_eos$DOY1[j],j]
}
sim_eos_point<- data.frame(as.Date(sim_eos$Date1),sim_eos_gpp)




# --------------------------- Plot the estimated SOS and EOS -------------------------
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
cols = gg_color_hue(2)

graphics.off()
ball_size = 1.5
#tiff("N:/Data02/bcal/Personal/hamid/ED_opt/working/Manuscript/Figures/Phenometrics.tiff", units="in", width=6.5, height=4, res=300)
ggplot() + 
  geom_line(data = df_sim_gpp, aes(x =as.Date(df_sim_gpp$Date), y = df_sim_gpp$GPP,color="Simulated GPP"),size=0.5) +
  geom_line(data = df_modis_gpp, aes(x =as.Date(df_modis_gpp$Date), y = df_modis_gpp$GPP,color="Modis GPP"),size=0.5)+
  geom_point(data=sim_sos_point, aes(x=as.Date(sim_sos_point$as.Date.sim_sos.Date.), 
                                     y=sim_sos_point$sim_sos_gpp, color='Simulated SOS'), size=ball_size, alpha=1,shape=19) +
  geom_point(data=modis_sos_point, aes(x=as.Date(modis_sos_point$as.Date.modis_sos.Date.), 
                                       y=modis_sos_point$modis_sos_gpp, color='Modis SOS'), size=ball_size, alpha=1,shape = 19)+
  geom_point(data=sim_eos_point, aes(x=as.Date(sim_eos_point$as.Date.sim_eos.Date1.),
                                     y=sim_eos_point$sim_eos_gpp, color='Simulated EOS'), size=ball_size, alpha=1,shape=17)+
  geom_point(data=modis_eos_point, aes(x=as.Date(modis_eos_point$as.Date.modis_eos.Date1.),
                                       y=modis_eos_point$modis_eos_gpp, color='Modis EOS'), size=ball_size, alpha=1,shape=17)+
  theme(legend.title = element_blank(),legend.text = element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12,face="bold")) + scale_colour_manual(values = c(cols[1] ,cols[1],cols[1],cols[2],cols[2],cols[2]),
                                                          guide = guide_legend(override.aes = list(
                                                          linetype = c("solid", "solid"), shape = c(17, NA, 19, 17,NA,19)
                                                                )))+
  labs(x="Time", y=paste(ylabel_p,unit)) +
  theme_bw()+theme(legend.title = element_blank(),legend.position="top")
#dev.off()

#--------------- Calculate the MAD (mean absolute difference) between differen SOS and EOS ------------------------------


n <- length(sim_sos$DOY)
MAD_sim_modisgpp_sos <- (sum(abs(sim_sos$DOY - modis_sos$DOY)))/n
MAD_sim_modisnirv_sos <- (sum(abs(sim_sos$DOY-modisnir_sos$DOY)))/n
MAD_modisnir_modisgpp_sos <- (sum(abs(modisnir_sos$DOY - modis_sos$DOY)))/n

MAD_sim_modisgpp_eos <- (sum(abs(sim_eos$DOY1 - modis_eos$DOY1)))/n
MAD_sim_modisnirv_eos <- (sum(abs(sim_eos$DOY1-modisnir_eos$DOY1)))/n
MAD_modisnir_modisgpp_eos <- (sum(abs(modisnir_eos$DOY1 - modis_eos$DOY1)))/n




# ------------ Let find the SOS, EOS of observations data -------------------------

#new_df <- df_obs[df_obs >= "2015-01-01",]   # remove days before 2015

#Obs_SOS <- My_SOS(new_df,"GPP")
#Obs_EOS <- My_EOS(new_df,"GPP")


####################### END of Phenology ############################################


#####################################################################################

####################### Bayesian averaging trend analysis ###########################

# modis_tmp <- subset(df_modis_gpp, df_modis_gpp$dates < "2017-10-01")
# sim_tmp <- subset(df_sim, df_sim$dates < "2017-10-01")
# 
# 
# modis.all.monthly <- aggregate(modis_tmp$GPP,list(format(modis_tmp$dates,"%Y-%m")),mean)
# modis.months <- as.Date(anytime(modis.all.monthly$Group.1))
# modis.gpp.month <- data.frame(modis.months,modis.all.monthly$x)
# colnames(modis.gpp.month) <- c('Date', 'GPP')
# 
# sim.all.monthly <- aggregate(sim_tmp$GPP,list(format(sim_tmp$dates,"%Y-%m")),mean)
# sim.months <- as.Date(anytime(sim.all.monthly$Group.1))
# sim.gpp.month <- data.frame(sim.months,sim.all.monthly$x)
# colnames(sim.gpp.month) <- c('Date', 'GPP')
# 
# 
# 
# modis_tmp <- subset(df_modis_gpp, df_modis_gpp$dates < "2017-10-01")
# sim_tmp <- subset(df_sim, df_sim$dates < "2017-10-01")
# 
# 
# modis.all.yearly <- aggregate(modis.gpp.month$GPP,list(format(modis.gpp.month$Date,"%Y")),sum)
# modis.year <- as.Date(anytime(modis.all.yearly$Group.1))
# modis.gpp.year <- data.frame(modis.year,modis.all.yearly$x)
# colnames(modis.gpp.year) <- c('Date', 'GPP')
# 
# sim.all.yearly <- aggregate(sim.gpp.month$GPP,list(format(sim.gpp.month$Date,"%Y")),sum)
# sim.year <- as.Date(anytime(sim.all.yearly$Group.1))
# sim.gpp.year <- data.frame(sim.year,sim.all.yearly$x)
# colnames(sim.gpp.year) <- c('Date', 'GPP')
# 


# ------------ Running rBeast algorithm ---------------------------------------------

opt=list() 
opt$period=12           #period of the cyclic/seasonal component of the time series
opt$seed=004   
#opt$minSeasonOrder=4    #min harmonic order allowed in fitting season component
#opt$maxSeasonOrder=8    #max harmonic order allowed in fititing season component
#opt$minTrendOrder=0     #min polynomial order allowed to fit trend (0 for constant)
#opt$maxTrendOrder=1     #max polynomial order allowed to fit trend (1 for linear term)
#opt$minSepDist_Season=1#min seperation distance btw neighboring season changepoints(must be >=0)
#opt$minSepDist_Trend=3 #min seperation distance btw neighboring trend changepoints(must be >=0)
#opt$maxKnotNum_Season=32 #max number of season changepoints allowed 
#opt$maxKnotNum_Trend=8 #max number of trend changepoints allowed 
opt$computeCredible=1
#opt$sample = 2000

modis.gpp.trend <- beast(df_modis_gpp$GPP,opt)
modis.nirv.trend <- beast(df_modis_nirv$NIRv,opt)
sim.trend <- beast(df_sim_gpp$GPP,opt)

par(mfrow=c(2,1)) 
plot(modis.gpp.trend)
plot(sim.trend)

modis.tcp <- modis.gpp.trend$tcp
modis.scp <- modis.gpp.trend$scp
sim.tcp <- sim.trend$tcp
sim.scp <- sim.trend$scp

modisnirv.tcp <- modis.nirv.trend$tcp
modisnirv.scp <- modis.nirv.trend$scp


intersect(modisnirv.tcp,sim.tcp)
intersect(modisnirv.scp,sim.scp)
    

modis.trend.df <- data.frame(cbind(df_modis_gpp$Date,modis.gpp.trend$s,modis.gpp.trend$t
                                       ,modis.gpp.trend$tProb,modis.gpp.trend$sCI[,1,1],modis.gpp.trend$sCI[,2,1],
                                       modis.gpp.trend$tCI[,1,1],modis.gpp.trend$tCI[,2,1],modis.gpp.trend$sProb))
    colnames(modis.trend.df) <- c("Date","s","t","tProb","LsCI","UsCI","LtCI","UtCI","sProb")

modisnirv.trend.df <- data.frame(cbind(df_modis_nirv$Date,modis.nirv.trend$s,modis.nirv.trend$t
                                       ,modis.nirv.trend$tProb,modis.nirv.trend$sCI[,1,1],modis.nirv.trend$sCI[,2,1],
                                       modis.nirv.trend$tCI[,1,1],modis.nirv.trend$tCI[,2,1],modis.nirv.trend$sProb))
    colnames(modisnirv.trend.df) <- c("Date","s","t","tProb","LsCI","UsCI","LtCI","UtCI","sProb")
    
    
sim.trend.df <- data.frame(cbind(df_sim_gpp$Date,sim.trend$s,sim.trend$t
                                       ,sim.trend$tProb,sim.trend$sCI[,1,1],sim.trend$sCI[,2,1],
                                     sim.trend$tCI[,1,1],sim.trend$tCI[,2,1],sim.trend$sProb))
    colnames(sim.trend.df) <- c("Date","s","t","tProb","LsCI","UsCI","LtCI","UtCI","sProb")
   

    
    
graphics.off()
#tiff("N:/Data02/bcal/Personal/hamid/ED_opt/working/Manuscript/Figures/ws_trend.tiff", units="in", width=10, height=7.5, res=300)
     
    df1 <- data.frame(cbind(df_modis_gpp$Date, rep(c("Season component"),212),modis.trend.df$s)) 
    df2 <- data.frame(cbind(rep(c("Changepoint probability"),212),modis.trend.df$sProb))
    df<-cbind(df1, df2)
    colnames(df) <- c("Date", "Group1", "Value1","Group2","Value2")
    
    p1 <- ggplot() + 
      geom_line(data =df , aes(x =as.Date(modis.trend.df$Date), y = modis.trend.df$s,
                                           colour=Group1,linetype=Group1),size=0.5)+ 
      geom_line(data =df , aes(x =as.Date(modis.trend.df$Date), y = modis.trend.df$sProb,
                                           colour=Group2,linetype=Group2),size=0.8)+
      geom_ribbon(aes(x=as.Date(modis.trend.df$Date), ymin = modis.trend.df$LsCI, ymax = modis.trend.df$UsCI),
                  alpha = 0.2) + 
      scale_linetype_manual(values=c("solid", "dashed"))+ 
      scale_fill_manual("",values="grey12") +
      theme_bw() + 
      theme(legend.title = element_blank(),
            axis.text=element_text(size=8),
            axis.title=element_text(size=8,face="bold"),legend.position = "top",legend.text=element_text(size=12))+
      xlab("Time") + ylab("Seasonal") 
    
      
      
    df1 <- data.frame(cbind(df_modis_gpp$Date, rep(c("Trend component"),212),modis.trend.df$t)) 
    df2 <- data.frame(cbind(rep(c("Changepoint probability"),212),modis.trend.df$tProb))
    df<-cbind(df1, df2)
    colnames(df) <- c("Date", "Group1", "Value1","Group2","Value2")
    
    p2 <- ggplot() + 
      geom_line(data =df , aes(x =as.Date(modis.trend.df$Date), y = modis.trend.df$t,
                               colour=Group1,linetype=Group1),size=0.5)+ 
      geom_line(data =df , aes(x =as.Date(modis.trend.df$Date), y = modis.trend.df$tProb,
                               colour=Group2,linetype=Group2),size=0.8)+
      geom_ribbon(aes(x=as.Date(modis.trend.df$Date), ymin = modis.trend.df$LtCI, ymax = modis.trend.df$UtCI),
                  alpha = 0.2) + 
      scale_linetype_manual(values=c("solid", "dashed"))+ 
      scale_fill_manual("",values="grey12") +
      theme_bw() + 
      theme(legend.title = element_blank(),
            axis.text=element_text(size=8),
            axis.title=element_text(size=8,face="bold"),legend.position = "top",legend.text=element_text(size=12))+
      xlab("Time") + ylab("Trend") 
    
    
    df1 <- data.frame(cbind(df_sim_gpp$Date, rep(c("Season component"),212),sim.trend.df$s)) 
    df2 <- data.frame(cbind(rep(c("Changepoint probability"),212),sim.trend.df$sProb))
    df<-cbind(df1, df2)
    colnames(df) <- c("Date", "Group1", "Value1","Group2","Value2")
    
    p3 <- ggplot() + 
      geom_line(data =df , aes(x =as.Date(sim.trend.df$Date), y = sim.trend.df$s,
                               colour=Group1,linetype=Group1),size=0.5)+ 
      geom_line(data =df , aes(x =as.Date(sim.trend.df$Date), y = sim.trend.df$sProb,
                               colour=Group2,linetype=Group2),size=0.8)+
      geom_ribbon(aes(x=as.Date(sim.trend.df$Date), ymin = sim.trend.df$LsCI, ymax = sim.trend.df$UsCI),
                  alpha = 0.2) + 
      scale_linetype_manual(values=c("solid", "dashed"))+ 
      scale_fill_manual("",values="grey12") +
      theme_bw() + 
      theme(legend.title = element_blank(),
            axis.text=element_text(size=8),
            axis.title=element_text(size=8,face="bold"),legend.position = "top",legend.text=element_text(size=12))+
      xlab("Time") + ylab("Seasonal") 
    
    
    df1 <- data.frame(cbind(df_sim_gpp$Date, rep(c("Trend component"),212),sim.trend.df$t)) 
    df2 <- data.frame(cbind(rep(c("Changepoint probability"),212),sim.trend.df$tProb))
    df<-cbind(df1, df2)
    colnames(df) <- c("Date", "Group1", "Value1","Group2","Value2")
    
    p4 <- ggplot() + 
      geom_line(data =df , aes(x =as.Date(sim.trend.df$Date), y = sim.trend.df$t,
                               colour=Group1,linetype=Group1),size=0.5)+ 
      geom_line(data =df , aes(x =as.Date(sim.trend.df$Date), y = sim.trend.df$tProb,
                               colour=Group2,linetype=Group2),size=0.8)+
      geom_ribbon(aes(x=as.Date(sim.trend.df$Date), ymin = sim.trend.df$LtCI, ymax = sim.trend.df$UtCI),
                  alpha = 0.2) + 
      scale_linetype_manual(values=c("solid", "dashed"))+ 
      scale_fill_manual("",values="grey12") +
      theme_bw() + 
      theme(legend.title = element_blank(),
            axis.text=element_text(size=8),
            axis.title=element_text(size=8,face="bold"),legend.position = "top",legend.text=element_text(size=12))+
      xlab("Time") + ylab("Trend") 
    
    
    df1 <- data.frame(cbind(df_modis_nirv$Date, rep(c("Season component"),212),modisnirv.trend.df$s)) 
    df2 <- data.frame(cbind(rep(c("Changepoint probability"),212),modisnirv.trend.df$sProb))
    df<-cbind(df1, df2)
    colnames(df) <- c("Date", "Group1", "Value1","Group2","Value2")
    
    # p5 <- ggplot() + 
    #   geom_line(data =df , aes(x =as.Date(modisnirv.trend.df$Date), y = modisnirv.trend.df$s,
    #                            colour=Group1,linetype=Group1),size=0.5)+ 
    #   geom_line(data =df , aes(x =as.Date(modisnirv.trend.df$Date), y = modisnirv.trend.df$sProb,
    #                            colour=Group2,linetype=Group2),size=0.8)+
    #   geom_ribbon(aes(x=as.Date(modisnirv.trend.df$Date), ymin = modisnirv.trend.df$LsCI, ymax = modisnirv.trend.df$UsCI),
    #               alpha = 0.2) + 
    #   scale_linetype_manual(values=c("dashed", "solid"))+ 
    #   scale_fill_manual("",values="grey12") +
    #   theme_bw() + 
    #   theme(legend.title = element_blank(),
    #         axis.text=element_text(size=8),
    #         axis.title=element_text(size=8,face="bold"),legend.position = "top",legend.text=element_text(size=12))+
    #   xlab("Time") + ylab("Seasonal") 
    # 
    # 
    # 
    # df1 <- data.frame(cbind(df_modis_nirv$Date, rep(c("Trend component"),212),modisnirv.trend.df$t)) 
    # df2 <- data.frame(cbind(rep(c("Changepoint probability"),212),modisnirv.trend.df$tProb))
    # df<-cbind(df1, df2)
    # colnames(df) <- c("Date", "Group1", "Value1","Group2","Value2")
    # 
    # p6 <- ggplot() + 
    #   geom_line(data =df , aes(x =as.Date(modisnirv.trend.df$Date), y = modisnirv.trend.df$t,
    #                            colour=Group1,linetype=Group1),size=0.5)+ 
    #   geom_line(data =df , aes(x =as.Date(modisnirv.trend.df$Date), y = modisnirv.trend.df$tProb,
    #                            colour=Group2,linetype=Group2),size=0.8)+
    #   geom_ribbon(aes(x=as.Date(modisnirv.trend.df$Date), ymin = modisnirv.trend.df$LtCI, ymax = modisnirv.trend.df$UtCI),
    #               alpha = 0.2) + 
    #   scale_linetype_manual(values=c("dashed", "solid"))+ 
    #   scale_fill_manual("",values="grey12") +
    #   theme_bw() + 
    #   theme(legend.title = element_blank(),
    #         axis.text=element_text(size=8),
    #         axis.title=element_text(size=8,face="bold"),legend.position = "top",legend.text=element_text(size=12))+
    #   xlab("Time") + ylab("Trend") 
    # 
    # 
    
    multiplot(p1,p3,p2,p4,cols = 2)
#dev.off()


##################################################################################

#                                 Precipitation time series analysis

##################################################################################
    
#-----------------------loading WS pricept--------------------------
    setwd("N:/Data02/bcal/Personal/hamid/ED_opt/wrf_data/wrf_d02_hourly_30years_LS")
    start <- as.Date("2000-02-01")
    end <- as.Date("2017-09-30")
    
    forced_start <- as.Date(paste0(format(start, "%Y-%m"), "-01"))
    forced_end <- as.Date(paste0(format(end, "%Y-%m"), "-01"))
    
    seq_dates <- seq.Date(forced_start, forced_end, by = "month")
    dates <- toupper(format(seq_dates, "%Y%b"))
    
    dates2 <- seq.Date(start, end, by = "month")
    
    n <- length(dates)
    var_ls <- rep(0, n)
    
    for (i in 1:n ){
      print(i)
      fName <- paste0("LS_",dates[i],".h5")
      var_tmp1 <-drop(h5read(fName,"/prate"))
      #var[i] <- mean(var_tmp1)*length(var_tmp1)*60*60   
      var_ls[i] <- sum(var_tmp1)*(3600)
    }
    
    
    precept_ws <- data.frame(dates2,var_ws)
    colnames(precept_ws) <- c("Date","Precipitation")
# ------------------------- Trend analysis for precipitation -------------------
    
    precep.trend <- beast(precept_ws$Precipitation,opt)
    
    
    par(mfrow=c(2,1)) 
    plot(precep.trend)
    plot(sim.trend)
    
    
    
    precep.trend.df <- data.frame(cbind(precept_ws$Date,precep.trend$s,precep.trend$t
                                     ,precep.trend$tProb,precep.trend$sCI[,1,1],precep.trend$sCI[,2,1],
                                     precep.trend$tCI[,1,1],precep.trend$tCI[,2,1],precep.trend$sProb))
    colnames(precep.trend.df) <- c("Date","s","t","tProb","LsCI","UsCI","LtCI","UtCI","sProb")
    
graphics.off()
tiff("N:/Data02/bcal/Personal/hamid/ED_opt/working/Manuscript/Figures/precept_trend.tiff", units="in", width=6, height=3, res=300)
    
    df1 <- data.frame(cbind(precept_ws$Date, rep(c("Season component"),212),precep.trend.df$s)) 
    df2 <- data.frame(cbind(rep(c("Changepoint probability"),212),precep.trend.df$sProb))
    df<-cbind(df1, df2)
    colnames(df) <- c("Date", "Group1", "Value1","Group2","Value2")
    
    p7 <-ggplot() + 
      geom_line(data =df , aes(x =as.Date(precep.trend.df$Date), y = precep.trend.df$s,
                               colour=Group1,linetype=Group1),size=0.5)+ 
      geom_line(data =df , aes(x =as.Date(precep.trend.df$Date), y = precep.trend.df$sProb,
                               colour=Group2,linetype=Group2),size=0.8)+
      geom_ribbon(aes(x=as.Date(precep.trend.df$Date), ymin = precep.trend.df$LsCI, ymax = precep.trend.df$UsCI),
                  alpha = 0.2) + 
      scale_linetype_manual(values=c("solid", "dashed"))+ 
      scale_fill_manual("",values="grey12") +
      theme_bw() + 
      theme(legend.title = element_blank(),
            axis.text=element_text(size=8),
            axis.title=element_text(size=8,face="bold"),legend.position = "top",legend.text=element_text(size=8))+
      xlab("Time") + ylab("Seasonal") 
    
    
    df1 <- data.frame(cbind(precept_ws$Date, rep(c("Trend component"),212),precep.trend$t)) 
    df2 <- data.frame(cbind(rep(c("Changepoint probability"),212),precep.trend$tProb))
    df<-cbind(df1, df2)
    colnames(df) <- c("Date", "Group1", "Value1","Group2","Value2")
    
    p8 <- ggplot() + 
      geom_line(data =df , aes(x =as.Date(precep.trend.df$Date), y = precep.trend.df$t,
                               colour=Group1,linetype=Group1),size=0.5)+ 
      geom_line(data =df , aes(x =as.Date(precep.trend.df$Date), y = precep.trend.df$tProb,
                               colour=Group2,linetype=Group2),size=0.8)+
      geom_ribbon(aes(x=as.Date(precep.trend.df$Date), ymin = precep.trend.df$LtCI, ymax = precep.trend.df$UtCI),
                  alpha = 0.2) + 
      scale_linetype_manual(values=c("solid", "dashed"))+ 
      scale_fill_manual("",values="grey12") +
      theme_bw() + 
      theme(legend.title = element_blank(),
            axis.text=element_text(size=8),
            axis.title=element_text(size=8,face="bold"),legend.position = "top",legend.text=element_text(size=8))+
      xlab("Time") + ylab("Trend") 
    
    multiplot(p7,p8,cols = 2)
dev.off()    
#########################################################################################
    
setwd("N:/Data02/bcal/Personal/hamid/ED_opt/working/Timeseries/Modis")
save.image("Trend_analysis.RData")

#########################################################################################
    
    
    
    
    
