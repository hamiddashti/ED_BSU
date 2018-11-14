## This script is to check wrf with field data (temp and precept)

rm(list = ls())
setwd("N:/Data02/bcal/Personal/hamid/ED_opt/wrf_data/wrf_d02_hourly_30years_LS")
library(rhdf5)
library(ggplot2)
#-----------------------Ploting temprature--------------------------

start <- as.Date("1988-01-01")
end <- as.Date("2016-12-31")

forced_start <- as.Date(paste0(format(start, "%Y-%m"), "-01"))
forced_end <- as.Date(paste0(format(end, "%Y-%m"), "-01"))

seq_dates <- seq.Date(forced_start, forced_end, by = "month")
dates <- toupper(format(seq_dates, "%Y%b"))

dates2 <- seq.Date(start, end, by = "month")

n <- length(dates)
var <- rep(0, n)

for (i in 1:n ){
print(i)
fName <- paste0("LS_",dates[i],".h5")
var_tmp1 <-drop(h5read(fName,"/prate"))
var[i] <- mean(var_tmp1)*length(var_tmp1)*60*60   #[kgH2O/m2/month]

}


data <- data.frame(dates2,var)

ggplot(data, aes(x=dates2, y=var)) + 
  geom_bar(stat="identity") + xlab("\nTime") + ylab("Precipitation [kgH2o/m2/Month]")+
  theme_bw() +  guides(fill=guide_legend(title="PFTs"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)
        ,axis.text=element_text(size=12),
        axis.title=element_text(size=12,face="bold"),
        legend.text=element_text(size=10,face="bold")) 



