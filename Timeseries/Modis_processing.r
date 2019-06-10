rm(list = ls())
setwd("N:/Data02/bcal/Personal/hamid/ED_opt/working/Timeseries")

# --------Load csv files downloaded from GEE ---------------
df1 <- read.csv("MODIS_Terra_SR1d_WS_2000_2004.csv", header = TRUE, as.is = TRUE)
dates_df1 <- seq(as.Date("2000/02/24"), as.Date("2004/12/31"), "days")
dates_df1_tmp <- format(as.Date(dates_df1),format = "%Y.%m.%d")
name_df1 <- colnames(df1)
df1_m <- matrix(data=NA,nrow=length(dates_df1),ncol=9)
colnames(df1_m) <- c("Date","mask","Red","NIR","Blue","Green","SWIR1","SWIR2","SWIR3")
for (i in 1:length(dates_df1_tmp)){
I <-which(grepl(dates_df1_tmp[i], name_df1))
if(length(I)==0){ 
  df1_m[i,1] <- format(as.Date(dates_df1[i]),format = "%Y/%m/%d")
  next
} else
df1_m[i,1] <- format(as.Date(dates_df1[i]),format = "%Y/%m/%d")
df1_m[i,2] <- df1[[I[1]]] 
df1_m[i,3] <- df1[[I[2]]]*0.0001 #Band 1 Red  (620-670 nm)
df1_m[i,4] <- df1[[I[3]]]*0.0001 #Band 2 NIR  (841-876 nm)
df1_m[i,5] <- df1[[I[4]]]*0.0001 #Band 3 Blue (459-479 nm)
df1_m[i,6] <- df1[[I[5]]]*0.0001 #Band 4 Green(545-565 nm)
df1_m[i,7] <- df1[[I[6]]]*0.0001 #Band 5 SWIR (1230-1250 nm)
df1_m[i,8] <- df1[[I[7]]]*0.0001 #Band 6 SWIR(1628-1652 nm)
df1_m[i,9] <- df1[[I[8]]]*0.0001 #Band 7 SWIR (2105-2155 nm)
}

df2 <- read.csv("MODIS_Terra_SR1d_WS_2005_2010.csv", header = TRUE, as.is = TRUE)
dates_df2 <- seq(as.Date("2005/01/01"), as.Date("2010/12/31"), "days")
dates_df2_tmp <- format(as.Date(dates_df2),format = "%Y.%m.%d")
name_df2 <- colnames(df2)
df2_m <- matrix(data=NA,nrow=length(dates_df2),ncol=9)
colnames(df2_m) <- c("Date","mask","Red","NIR","Blue","Green","SWIR1","SWIR2","SWIR3")
for (i in 1:length(dates_df2)){
  I <-which(grepl(dates_df2_tmp[i], name_df2))
  if(length(I)==0){ 
    df2_m[i,1] <- format(as.Date(dates_df2[i]),format = "%Y/%m/%d")
    next
  } else
  df2_m[i,1] <- format(as.Date(dates_df2[i]),format = "%Y/%m/%d")
  df2_m[i,2] <- df2[[I[1]]] 
  df2_m[i,3] <- df2[[I[2]]]*0.0001
  df2_m[i,4] <- df2[[I[3]]]*0.0001
  df2_m[i,5] <- df2[[I[4]]]*0.0001
  df2_m[i,6] <- df2[[I[5]]]*0.0001
  df2_m[i,7] <- df2[[I[6]]]*0.0001
  df2_m[i,8] <- df2[[I[7]]]*0.0001
  df2_m[i,9] <- df2[[I[8]]]*0.0001
}

df3 <- read.csv("MODIS_Terra_SR1d_WS_2010_2019.csv", header = TRUE, as.is = TRUE)
dates_df3 <- seq(as.Date("2011/01/01"), as.Date("2018/12/31"), "days")
dates_df3_tmp <- format(as.Date(dates_df3),format = "%Y.%m.%d")
name_df3 <- colnames(df3)
df3_m <- matrix(data=NA,nrow=length(dates_df3),ncol=9)
colnames(df3_m) <- c("Date","mask","Red","NIR","Blue","Green","SWIR1","SWIR2","SWIR3")
for (i in 1:length(dates_df3)){
  I <-which(grepl(dates_df3_tmp[i], name_df3))
  if(length(I)==0){ 
    df3_m[i,1] <- format(as.Date(dates_df3[i]),format = "%Y/%m/%d")
    next
  } else
  df3_m[i,1] <- format(as.Date(dates_df3[i]),format = "%Y/%m/%d")
  df3_m[i,2] <- df3[[I[1]]] 
  df3_m[i,3] <- df3[[I[2]]]*0.0001   #(620-670 nm) Red
  df3_m[i,4] <- df3[[I[3]]]*0.0001   #(841-876 nm) NIR
  df3_m[i,5] <- df3[[I[4]]]*0.0001   #(459-479 nm) Blue
  df3_m[i,6] <- df3[[I[5]]]*0.0001   #(545-565 nm) Green
  df3_m[i,7] <- df3[[I[6]]]*0.0001   #(1230-1250 nm) SWIR
  df3_m[i,8] <- df3[[I[7]]]*0.0001   #(1628-1652 nm) SWIR
  df3_m[i,9] <- df3[[I[8]]]*0.0001   #(2105-2155 nm) SWIR
}


Aqua <- rbind(df1_m,df2_m,df3_m)
I <- which(Aqua[,2] !="0")
Aqua_flaged <- Aqua[I,]
Aqua <- data.frame(Aqua)


NDVI <- (as.numeric(Aqua$NIR) -as.numeric(Aqua$Red))/(as.numeric(Aqua$NIR) + as.numeric(Aqua$Red))
NDVIv <- NDVI * as.numeric(Aqua$NIR)*0.0001
I <- which(NDVI>0)
plot(NDVI[I],ty="l")
I <- which(NDVI>0)
plot(NDVIv[I],ty="l")
EVIv <- EVI * NIR

NIR <- as.numeric(Aqua$NIR) * 0.0001
BLUE <- as.numeric(Aqua$Blue) * 0.0001
RED <- as.numeric(Aqua$Red) * 0.0001

EVI <- 2.5* (NIR-RED)/(NIR+6*RED-7.5*BLUE+1)

plot(NDVI[I],ty="l")
lines(NDVIv[I],ty="l",col="red")
lines(EVI[I],ty="l",col="blue")

plot(EVI[I],ty="l")
