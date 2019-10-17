  ## This script is to check wrf with field data (Precipitation and Temprature)
  
###############################################################################

#                         Precipitation  

###############################################################################

# ----------------------- WS -------------------------------------------------

  rm(list = ls())
  setwd("N:/Data02/bcal/Personal/hamid/ED_opt/wrf_data/wrf_d02_hourly_30years_WS")
  library(rhdf5)
  library(ggplot2)
  #-----------------------loading WS--------------------------
  
  start <- as.Date("2000-01-01")
  end <- as.Date("2017-09-30")
  
  forced_start <- as.Date(paste0(format(start, "%Y-%m"), "-01"))
  forced_end <- as.Date(paste0(format(end, "%Y-%m"), "-01"))
  
  seq_dates <- seq.Date(forced_start, forced_end, by = "month")
  dates <- toupper(format(seq_dates, "%Y%b"))

dates2 <- seq.Date(start, end, by = "month")

n <- length(dates)
var_ws <- rep(0, n)

for (i in 1:n ){
print(i)
fName <- paste0("WS_",dates[i],".h5")
var_tmp1 <-drop(h5read(fName,"/prate"))
#var[i] <- mean(var_tmp1)*length(var_tmp1)*60*60   
var_ws[i] <- sum(var_tmp1)*(3600)
}


data_ws <- data.frame(dates2,var_ws)

yearly_ws <- aggregate(data_ws$var_ws, list(format(dates2, "%Y")), mean)
# -------------------- LS -----------------------------------------------


setwd("N:/Data02/bcal/Personal/hamid/ED_opt/wrf_data/wrf_d02_hourly_30years_LS")
library(rhdf5)
library(ggplot2)
#-----------------------Ploting temprature--------------------------

start <- as.Date("2000-01-01")
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


data_ls <- data.frame(dates2,var_ls)

yearly_ls <- aggregate(data_ls$var_ls, list(format(dates2, "%Y")), mean)

# ------------------------ MBS -----------------------------------------


setwd("N:/Data02/bcal/Personal/hamid/ED_opt/wrf_data/wrf_d02_hourly_30years_MBS")
library(rhdf5)
library(ggplot2)
#-----------------------Ploting temprature--------------------------

start <- as.Date("2000-01-01")
end <- as.Date("2017-09-30")

forced_start <- as.Date(paste0(format(start, "%Y-%m"), "-01"))
forced_end <- as.Date(paste0(format(end, "%Y-%m"), "-01"))

seq_dates <- seq.Date(forced_start, forced_end, by = "month")
dates <- toupper(format(seq_dates, "%Y%b"))

dates2 <- seq.Date(start, end, by = "month")

n <- length(dates)
var_mbs <- rep(0, n)

for (i in 1:n ){
  print(i)
  fName <- paste0("MBS_",dates[i],".h5")
  var_tmp1 <-drop(h5read(fName,"/prate"))
  #var[i] <- mean(var_tmp1)*length(var_tmp1)*60*60   
  var_mbs[i] <- sum(var_tmp1)*(3600)
}


data_mbs <- data.frame(dates2,var_mbs)

yearly_mbs <- aggregate(data_mbs$var_mbs, list(format(dates2, "%Y")), mean)




data <- data.frame(dates2,var_ws,var_ls,var_mbs)
colnames(data) <- c("Date","WS","LS","MBS")

#######


df = melt(data, id.vars=c("Date"))

graphics.off()
#tiff("N:/Data02/bcal/Personal/hamid/ED_opt/working/Manuscript/Figures/precept.tiff", units="in", width=10, height=5.5, res=300)
ggplot(df, aes(Date, value, fill=variable)) +
  geom_bar(stat='Identity',position=position_dodge())+
  theme_bw() +labs(x="Time", y="Precipitation [mm]") +
  theme(legend.title = element_blank(),legend.text = element_text(size=12),
        axis.text=element_text(size=18),
        axis.title=element_text(size=18,face="bold"),
        legend.box.margin=margin(-10,-10,-10,-10))


#dev.off()

boxplot(yearly_mbs$x)

############################################################################

#                             Temprature

############################################################################


# ----------------------- WS -------------------------------------------------

rm(list = ls())
setwd("N:/Data02/bcal/Personal/hamid/ED_opt/wrf_data/wrf_d02_hourly_30years_WS")
library(rhdf5)
library(ggplot2)
#-----------------------Ploting temprature--------------------------

start <- as.Date("2000-01-01")
end <- as.Date("2017-09-30")

forced_start <- as.Date(paste0(format(start, "%Y-%m"), "-01"))
forced_end <- as.Date(paste0(format(end, "%Y-%m"), "-01"))

seq_dates <- seq.Date(forced_start, forced_end, by = "month")
dates <- toupper(format(seq_dates, "%Y%b"))

dates2 <- seq.Date(start, end, by = "month")

n <- length(dates)
var_ws <- rep(0, n)

for (i in 1:n ){
  print(i)
  fName <- paste0("WS_",dates[i],".h5")
  var_tmp1 <-drop(h5read(fName,"/tmp"))
  var_ws[i] <- mean(var_tmp1)-273.15   
  #var_ws[i] <- sum(var_tmp1)*(3600)
}


data_ws <- data.frame(dates2,var_ws)

yearly_ws <- aggregate(data_ws$var_ws, list(format(dates2, "%Y")), mean)

# -------------------- LS -----------------------------------------------


setwd("N:/Data02/bcal/Personal/hamid/ED_opt/wrf_data/wrf_d02_hourly_30years_LS")
library(rhdf5)
library(ggplot2)
#-----------------------Ploting temprature--------------------------

start <- as.Date("2000-01-01")
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
  var_tmp1 <-drop(h5read(fName,"/tmp"))
  var_ls[i] <- mean(var_tmp1)-273.15    
  #var_ls[i] <- sum(var_tmp1)*(3600)
}


data_ls <- data.frame(dates2,var_ls)

yearly_ls <- aggregate(data_ls$var_ls, list(format(dates2, "%Y")), mean)

# ------------------------ MBS -----------------------------------------


setwd("N:/Data02/bcal/Personal/hamid/ED_opt/wrf_data/wrf_d02_hourly_30years_MBS")
library(rhdf5)
library(ggplot2)
#-----------------------Ploting temprature--------------------------

start <- as.Date("2000-01-01")
end <- as.Date("2017-09-30")

forced_start <- as.Date(paste0(format(start, "%Y-%m"), "-01"))
forced_end <- as.Date(paste0(format(end, "%Y-%m"), "-01"))

seq_dates <- seq.Date(forced_start, forced_end, by = "month")
dates <- toupper(format(seq_dates, "%Y%b"))

dates2 <- seq.Date(start, end, by = "month")

n <- length(dates)
var_mbs <- rep(0, n)

for (i in 1:n ){
  print(i)
  fName <- paste0("MBS_",dates[i],".h5")
  var_tmp1 <-drop(h5read(fName,"/tmp"))
  var_mbs[i] <- mean(var_tmp1)-273.15    
  #var_mbs[i] <- sum(var_tmp1)*(3600)
}


data_mbs <- data.frame(dates2,var_mbs)

yearly_mbs <- aggregate(data_mbs$var_mbs, list(format(dates2, "%Y")), mean)




data <- data.frame(dates2,var_ws,var_ls,var_mbs)
colnames(data) <- c("Date","WS","LS","MBS")

#######


df = melt(data, id.vars=c("Date"))

graphics.off()
tiff("N:/Data02/bcal/Personal/hamid/ED_opt/working/Manuscript/Figures/temp.tiff", units="in", width=10, height=5.5, res=300)
ggplot(df, aes(Date, value, color=variable)) +
  geom_line()+
  theme_bw() +labs(x="Time", y="Temprature [C]") +
  theme(legend.title = element_blank(),legend.text = element_text(size=12),
        axis.text=element_text(size=18),
        axis.title=element_text(size=18,face="bold"),
        legend.box.margin=margin(-10,-10,-10,-10))


dev.off()

boxplot(yearly_mbs$x)

