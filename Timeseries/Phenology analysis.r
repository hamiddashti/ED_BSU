
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




####################################################################

rm(list = ls())
setwd("N:/Data02/bcal/Personal/hamid/ED_opt/working/Timeseries/Modis")
library(zoo)
library(signal)
source('N:/Data02/bcal/Personal/hamid/ED_BSU/Timeseries/My_SOS.R')
source('N:/Data02/bcal/Personal/hamid/ED_BSU/Timeseries/My_EOS.R')

#####################################################################

# ----------- Load the simulated and Observed ----------------------------------

ws_opt <- read.csv("ws_trend.csv", header = TRUE, as.is = TRUE)
ws_obs <- read.csv("WS_obs.csv", header = TRUE, as.is = TRUE)
obs_dates <- seq(as.Date("2014/10/1"), as.Date("2017/9/30"), "days") # Observation period
opt_dates <- seq(as.Date("2000/02/11"), as.Date("2017/9/30"), "days")
ws_obs$dates <- obs_dates
ws_opt$dates <- opt_dates
#smooth_opt <- sgolayfilt(ws_opt$GPP,3,11)

df_obs_gpp <- data.frame(ws_obs$dates,ws_obs$GPP)
df_opt_gpp <- data.frame(ws_opt$dates,ws_opt$GPP)
colnames(df_obs_gpp) <- c("Date","GPP") 
colnames(df_opt_gpp) <- c("Date","GPP") 


# --------------- Plot Simulated vs Observed ---------------


tmp_df_opt <- df_opt_gpp[df_opt_gpp$Date>"2014/10/1",]
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

# --------------- Lets do the maximum composite for simulations --------------------------------
# We do this step to exclude low-biased simulated GPP 
#date_range <- seq(as.Date("2000/02/11"), as.Date("2017/9/30"), "days") # This is the range that simulated and observed and measured overlap

max <- 8  # every 8 days
x <- seq_along(df_opt_gpp$GPP)
gpp_splits <- split(df_opt_gpp$GPP, ceiling(x/max))
gpp_date_splits <- split(df_opt_gpp$Date, ceiling(x/max))
sim_gpp <- rep(NA, length(gpp_splits))
sim_gpp_date <- rep(NA, length(gpp_splits))

    for (k in 1:length(gpp_splits)){
      sim_gpp[k] <- max(gpp_splits[[k]])
      sim_gpp_date[k] <- max(gpp_date_splits[[k]])
    }

sim_gpp_date <- as.Date(sim_gpp_date)
smooth_sim <- sgolayfilt(sim_gpp,3,11)
df_sim <- data.frame(sim_gpp_date,smooth_sim)
colnames(df_sim) <- c('dates', 'GPP')
plot(df_sim,ty='l')

# --------------- Lets do the maximum composite for observations --------------------------------
# We do this step to exclude low-biased observed GPP 


max <- 8  # every 8 days
x <- seq_along(df_obs_gpp$GPP)
gpp_splits <- split(df_obs_gpp$GPP, ceiling(x/max))
gpp_date_splits <- split(df_obs_gpp$Date, ceiling(x/max))
obs_gpp <- rep(NA, length(gpp_splits))
obs_gpp_date <- rep(NA, length(gpp_splits))

for (k in 1:length(gpp_splits)){
  obs_gpp[k] <- max(gpp_splits[[k]])
  obs_gpp_date[k] <- max(gpp_date_splits[[k]])
}
obs_gpp_date <- as.Date(obs_gpp_date)
smooth_obs <- sgolayfilt(obs_gpp,3,11)
df_obs <- data.frame(obs_gpp_date,smooth_obs)
colnames(df_obs) <- c('dates', 'GPP')
plot(df_obs,ty='l')
new_df_obs <- df_obs[df_obs >= "2015-01-01",] 



# ------------Load csv files (SR and GPP) downloaded from GEE ---------------
tmp1 <- read.csv("EC_WS_MOD09A1006_SR8d_t_2000_2018.csv", header = TRUE, as.is = TRUE)
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
ws_sr <- cbind(modis_dates,data)
tmp5 <- which(ws_sr$modis_dates<"2017-10-01")

plot(ws_sr$modis_dates,ws_sr$B2,ty="l")  # Just a simple plot to make sure everything is OK

tmp1 <- read.csv("EC_WS_MYD17A2H006_GPP8d_t_2000_2018.csv", header = TRUE, as.is = TRUE)
tmp2 <- as.matrix(tmp1[2:(length(tmp1)-1)])  # remove the first and last columns
# This is 8days products so for this date range we have 821 observations. 
# We also have 2 variables [QC and GPP] downloaded --> 2*821 = 6568
data <- matrix(tmp2, nrow = 821, byrow = TRUE)
data <- as.data.frame((data[,1]*0.0001)*(365/8)) # make units [kgC/m2/year]
colnames(data) <- c("GPP")
# Creating the date range 
tmp3 <- substr(names(tmp1[2:(length(tmp1)-1)]),2,11)
tmp4 <- matrix(tmp3,nrow = 821, byrow = TRUE)
modis_dates <- as.Date(tmp4[,1],"%Y.%m.%d" )
ws_gpp <- cbind(modis_dates,data)
plot(ws_gpp$modis_dates,ws_gpp$GPP,ty="l")  # Just a simple plot to make sure everything is OK

# ------------- Calculate the NIRv index from surface reflectance ----------------------

NDVI <- (ws_sr$B2 - ws_sr$B1) /(ws_sr$B2 + ws_sr$B1)
NIRv <- NDVI *ws_sr$B2


# ------------ fill the NA values ------------------------------------------------------
nirv <- na.approx(NIRv) 
gpp <- na.approx(ws_gpp$GPP)

# ------------ Apply SG filter to get higher quality data ----------------------------

smooth_nirv <- sgolayfilt(nirv,3,11)
smooth_gpp <- sgolayfilt(gpp,3,11)
scaled_nirv <- max(df_opt_gpp$GPP) *(nirv-min(nirv))/(max(nirv)-min(nirv)) # This is just for plotting

df_modis_nirv <- data.frame(as.Date(ws_sr$modis_dates),smooth_nirv)
df_modis_scaled_nirv <- data.frame(as.Date(ws_sr$modis_dates),scaled_nirv)
smooth_gpp[smooth_gpp<0] <- 0
df_modis_gpp <- data.frame(as.Date(ws_gpp$modis_dates),smooth_gpp)
names(df_modis_nirv) <- c("dates", "NIRv")
names(df_modis_gpp) <- c("dates", "GPP")
names(df_modis_scaled_nirv) <- c("dates", "GPP")

df_modis_nirv_new <- data.frame(df_modis_nirv$dates[df_modis_nirv$NIRv>0],df_modis_nirv$NIRv[df_modis_nirv$NIRv>0]) # No NIRv <0
names(df_modis_nirv_new) <- c("dates", "NIRv")

#Modis_GPP_SOS <- My_SOS(df_modis_gpp)
#MODIS_GPP_EOS <- My_EOS(df_modis_gpp)


# ------------ Let find the SOS, EOS of simulated, observed,modis gpp and modis nirv -------------------------


sim_sos <- My_SOS(df_sim,"GPP")
sim_sos_daily <- sim_sos$Daily[2:366,] 
sim_sos_date <- as.Date(sim_sos$Date)
sim_sos_gpp <- rep(NA,dim(sim_sos$Daily)[2])
for (j in 1: dim(sim_sos$Daily)[2]) {
   
  sim_sos_gpp[j] <- sim_sos_daily[sim_sos$DOY[j],j]
}
sim_sos_point<- data.frame(as.Date(sim_sos$Date),sim_sos_gpp)




obs_sos <- My_SOS(new_df_obs,"GPP")
obs_sos_daily <- obs_sos$Daily[2:366,] 
obs_sos_date <- as.Date(obs_sos$Date)
obs_sos_gpp <- rep(NA,dim(sim_sos$Daily)[2])
for (j in 1: dim(obs_sos$Daily)[2]) {
  
  obs_sos_gpp[j] <- obs_sos_daily[obs_sos$DOY[j],j]
}
obs_sos_point<- data.frame(as.Date(obs_sos$Date),obs_sos_gpp)



modis_sos <- My_SOS(df_modis_gpp,"GPP")
modis_sos_daily <- modis_sos$Daily[2:366,] 
modis_sos_date <- as.Date(modis_sos$Date)
modis_sos_gpp <- rep(NA,dim(modis_sos$Daily)[2])
for (j in 1: dim(modis_sos$Daily)[2]) {
  
  modis_sos_gpp[j] <- modis_sos_daily[modis_sos$DOY[j],j]
}
modis_sos_point<- data.frame(as.Date(modis_sos$Date),modis_sos_gpp)


#modisnir_sos <- My_SOS(df_modis_nirv_new,"NIRv")
#modisnir_sos_daily <- modisnir_sos$Daily[2:366,] 
#modisnir_sos_date <- as.Date(modisnir_sos$Date)
#modisnir_sos_gpp <- rep(NA,dim(modisnir_sos$Daily)[2])
#for (j in 1: dim(modisnir_sos$Daily)[2]) {
  
#  modisnir_sos_gpp[j] <- modisnir_sos_daily[modisnir_sos$DOY[j],j]
#}
#modisnir_sos_point<- data.frame(as.Date(modisnir_sos$Date),modisnir_sos_gpp)



sim_eos <- My_EOS(df_sim,"GPP")
sim_eos_daily <- sim_eos$Daily[2:366,] 
sim_eos_date <- as.Date(sim_eos$Date1)
sim_eos_gpp <- rep(NA,dim(sim_eos$Daily)[2])
for (j in 1: dim(sim_eos$Daily)[2]) {
  
  sim_eos_gpp[j] <- sim_eos_daily[sim_eos$DOY1[j],j]
}
sim_eos_point<- data.frame(as.Date(sim_eos$Date1),sim_eos_gpp)


modis_eos <- My_EOS(df_modis_gpp,"GPP")
modis_eos_daily <- modis_eos$Daily[2:366,] 
modis_eos_date <- as.Date(modis_eos$Date1)
modis_eos_gpp <- rep(NA,dim(modis_eos$Daily)[2])
for (j in 1: dim(modis_eos$Daily)[2]) {
  
  modis_eos_gpp[j] <- modis_eos_daily[modis_eos$DOY1[j],j]
}
modis_eos_point<- data.frame(as.Date(modis_eos$Date1),modis_eos_gpp)

graphics.off()
ball_size = 1.5
#tiff("N:/Data02/bcal/Personal/hamid/ED_opt/working/Manuscript/Figures/Phenometrics.tiff", units="in", width=6.5, height=4, res=300)
ggplot() + 
  geom_line(data = df_sim, aes(x =as.Date(df_sim$dates), y = df_sim$GPP,color="Simulated GPP"),size=0.5) +
  geom_line(data = df_modis_gpp, aes(x =as.Date(df_modis_gpp$dates), y = df_modis_gpp$GPP,color="Modis GPP"),size=0.5)+
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
        axis.title=element_text(size=12,face="bold")) + scale_colour_manual(values = c("black", "black","black", "red","red","red"),
                                                          guide = guide_legend(override.aes = list(
                                                          linetype = c("solid", "solid"), shape = c(17, NA, 19, 17,NA,19)
                                                                )))+
  labs(x="Time", y=paste(ylabel_p,unit)) +
  theme_bw()+theme(legend.title = element_blank(),legend.position="top")
#dev.off()

#--------------- Calculate the RMSE between differen SOS and EOS ------------------------------



rmse_sim_modis_sos <- rmse(sim_sos$DOY,modis_sos$DOY)
rmse_sim_modis_eos <- rmse(sim_eos$DOY1,modis_eos$DOY1)


# ------------ Let find the SOS, EOS of observations data (in case needed) -------------------------

new_df <- df_obs[df_obs >= "2015-01-01",]   # remove days before 2015

Obs_SOS <- My_SOS(new_df,"GPP")
Obs_EOS <- My_EOS(new_df,"GPP")


####################### END of Phenology ############################################


#####################################################################################

####################### Bayesian averaging trend analysis ###########################



