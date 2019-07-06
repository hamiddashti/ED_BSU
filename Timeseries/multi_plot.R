rm(list = ls())
library(Fgmutils)

library(Metrics)
library(birk)
library(ggplot2)

rm(list = ls())
setwd("N:/Data02/bcal/Personal/hamid/ED_opt/working/Timeseries")

startdate <- "2002-01-01"
enddate <- "2017-09-30"

landsat <- read.csv("ws_landsat_gpp.csv", header = TRUE, as.is = TRUE)
landsat_date <-as.Date(as.character(landsat$dates),"%m/%d/%Y")
landsat_gpp <- ((landsat$GPP)/10000)*(365/16)
ss <- smooth.spline(landsat_gpp)
landsat_gpp <- ss$y
df_landsat <- data.frame(date = landsat_date, gpp = landsat_gpp)
df_landsat <- df_landsat[which(landsat_date>=as.Date(startdate) & landsat_date<=as.Date(enddate)),]



modis <- read.csv("ws_modis_gpp.csv",header = TRUE, as.is = TRUE)
modis_date <-as.Date(as.character(modis$dates),"%m/%d/%Y")
modis_gpp <- ((modis$GPP)/10000)*(365/8)
ss <- smooth.spline(modis_gpp)
modis_gpp <- ss$y
df_modis = data.frame(date = modis_date, gpp = modis_gpp)
df_modis <- df_modis[which(modis_date>=as.Date(startdate) & modis_date<=as.Date(enddate)),]

sim <- read.csv("ws_karun_best.csv", header = TRUE, as.is = TRUE)
sim_date <-as.Date(sim$dates)
sim_gpp <- sim$GPP
ss <- smooth.spline(sim_gpp)
sim_gpp <- ss$y
df_sim = data.frame(date = sim_date, gpp = sim_gpp)
df_sim <- df_sim[which(sim_date>=as.Date(startdate) & sim_date<=as.Date(enddate)),]

obs <- read.csv("ws_obs.csv",header = TRUE, as.is = TRUE)
obs_date <-as.Date(as.character(obs$dates),"%m/%d/%Y")
obs_gpp <- obs$GPP
ss <- smooth.spline(obs_gpp)
obs_gpp <- ss$y
df_obs = data.frame(date = obs_date, gpp = obs_gpp)
df_obs <- df_obs[which(obs_date>=as.Date(startdate) & obs_date<=as.Date(enddate)),]


ylabel_p="GPP ";unit="[kgC/m2/yr]"

graphics.off()
ggplot() + 
  geom_line(data = df_obs, aes(x =as.Date(df_obs$date), y = df_obs[["gpp"]],color="Observation"),size=1) +
  geom_line(data = df_sim, aes(x =as.Date(df_sim$date), y = df_sim[["gpp"]],color="Simulation"),size=1) +
  geom_line(data = df_landsat, aes(x =as.Date(df_landsat$date), y = df_landsat[["gpp"]],color="LANDSAT"),size=1) +
  geom_line(data = df_modis, aes(x =as.Date(df_modis$date), y = df_modis[["gpp"]],color="MODIS"),size=1)+
  scale_color_manual(values = c(
    'Observation' = 'brown',
    'Simulation' = 'green',
    'LANDSAT' = 'red',
    'MODIS' = 'black')) +
  labs(color = '') + theme_bw() +labs(x="Time", y=paste(ylabel_p,unit))+ 
theme(legend.text = element_text(size=12),
      axis.text=element_text(size=12),
      axis.title=element_text(size=14,face="bold"))

##################### Calculating RMSE


I_landsat <- vector(length = length(landsat_date))
I_modis <- vector(length = length(modis_date))

for (i in 1:length(landsat_date)){

  I_landsat[i] <- which.closest(sim_date, landsat_date[i])
}
sim_landsat <- sim_gpp[I_landsat] 

for (i in 1:length(modis_date)){
  I_modis[i] <- which.closest(sim_date, modis_date[i])
}
sim_modis <- sim_gpp[I_modis]

plot(sim_modis,ty="l")
lines(modis_gpp,ty='l',col="red")


rrmse(sim_modis,modis_gpp)
rrmse(sim_landsat,landsat_gpp)


