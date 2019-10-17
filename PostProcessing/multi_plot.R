

rm(list = ls())
#setwd("N:/Data02/bcal/Personal/hamid/ED_opt/working/Timeseries")
setwd("N:/Data02/bcal/Personal/hamid/ED_opt/CleanData")


data <- read.csv ("Data_2015_2018.csv", header = TRUE, as.is = TRUE)
data_ws <- data[2:dim(data)[1],]
startdate <-as.Date("2014-10-01")
enddate <- as.Date("2017-09-30")
dates<- seq.Date(startdate, enddate, by = "day")
ws <- data.frame(dates,as.numeric(data_ws$GPPwbsec.1))
colnames(ws) <- c("Date","GPP")


#data_ls <- read.csv ("Data_2015_2018.csv", header = TRUE, as.is = TRUE)

data_ls <- data[2:dim(data)[1],]
startdate <-as.Date("2014-10-01")
enddate <- as.Date("2017-09-30")
dates<- seq.Date(startdate, enddate, by = "day")
ls <- data.frame(dates,as.numeric(data_ls$GPPlosec.1))
colnames(ls) <- c("Date","GPP")

data_mbs <- data[2:dim(data)[1],]
startdate <-as.Date("2014-10-01")
enddate <- as.Date("2017-09-30")
dates<- seq.Date(startdate, enddate, by = "day")
mbs <- data.frame(dates,as.numeric(data_mbs$GPPmbsec.1))
colnames(mbs) <- c("Date","GPP")


plot(ls)
ylabel_p="GPP ";unit="[kgC/m2/yr]"
graphics.off()
tiff("N:/Data02/bcal/Personal/hamid/ED_opt/working/Manuscript/Figures/sites_gpp.tiff", units="in", width=2.7, height=2.3, res=300)
ggplot() + 
  geom_line(data = ws, aes(x =Date, y = ws$GPP,color = "WS"),size=0.5) +
  geom_line(data = ls, aes(x =Date, y = ls$GPP,color = "LS"),size=0.5) +
  geom_line(data = mbs, aes(x =Date, y = mbs$GPP,color = "MBS"),size=0.5) +
  labs(color = '') + theme_bw() +labs(x="Time", y=paste(ylabel_p,unit))+ 
  theme(legend.title = element_blank(),legend.text = element_text(size=6),
        axis.text=element_text(size=6),
        axis.title=element_text(size=6,face="bold"),legend.position = "top",legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10)) #+


dev.off()
modis <- read.csv("ws_modis.csv",header = TRUE, as.is = TRUE)
modis_date <-as.Date(as.character(modis$dates),"%m/%d/%Y")
modis_gpp <- (modis$GPP)/1000
ss <- smooth.spline(modis_gpp)
modis_gpp <- ss$y
df_modis = data.frame(date = modis_date, gpp = modis_gpp)
df_modis <- df_modis[which(modis_date>=as.Date(startdate) & modis_date<=as.Date(enddate)),]

sim <- read.csv("ws_sim.csv", header = TRUE, as.is = TRUE)
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


ylabel_p="Vegetation productivity (GPP) ";unit="[kgC/m2/yr]"

graphics.off()
ggplot() + 
  geom_line(data = df_obs, aes(x =as.Date(df_obs$date), y = df_obs[["gpp"]],color="Observation"),size=1) +
  geom_line(data = df_sim, aes(x =as.Date(df_sim$date), y = df_sim[["gpp"]],color="ED Simulation"),size=1) +
  geom_line(data = df_landsat, aes(x =as.Date(df_landsat$date), y = df_landsat[["gpp"]],color="LANDSAT"),size=1) +
  geom_line(data = df_modis, aes(x =as.Date(df_modis$date), y = df_modis[["gpp"]],color="MODIS"),size=1)+
  scale_color_manual(values = c(
    'Observation' = 'brown',
    'ED Simulation' = 'green',
    'LANDSAT' = 'red',
    'MODIS' = 'black')) +
  labs(color = '') + theme_bw() +labs(x="Time", y=paste(ylabel_p,unit))+ 
theme(legend.text = element_text(size=12),
      axis.text=element_text(size=12),
      axis.title=element_text(size=14,face="bold"))

