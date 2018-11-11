
library(rhdf5)
library(ncdf4)
setwd("~/Desktop/wrf_d2")
nc <- nc_open('AmeriFlux.nc')

T2 <-as.vector(ncvar_get(nc, 'T2'))
RAINNC <- as.vector(ncvar_get(nc, 'RAINNC'))
Q2 <-as.vector(ncvar_get(nc, 'Q2'))
PSFC <- as.vector(ncvar_get(nc, 'PSFC'))
U10 <- as.vector(ncvar_get(nc, 'U10'))
V10 <- as.vector(ncvar_get(nc, 'V10'))
HGT <- as.vector(ncvar_get(nc, 'HGT'))
SWDOWN <- as.vector(ncvar_get(nc, 'SWDOWN'))
GLW <- as.vector(ncvar_get(nc, 'GLW'))
n<-length(T2)
nddsf <- numeric(n)
vbdsf <- numeric(n)
vddsf <- numeric(n)
time <-ncvar_get(nc, 'Times')

ym <- as.yearmon(time)
#m <- format(ym,"%b")
#m <- toupper(m)
#y <- format(ym,"%Y")
sp_T2 <- split(T2,ym)
sp_RAINNC <- split(RAINNC,ym)
sp_PSFC <- split(PSFC,ym)
sp_HGT <- split(HGT,ym)
sp_U10 <- split(U10,ym)
sp_V10 <- split(V10,ym)
sp_Q2 <- split(Q2,ym)
sp_GLW <- split(GLW,ym)
sp_SWDOWN <- split(SWDOWN,ym)
sp_nddsf <- split(nddsf,ym)
sp_vbdsf <- split(vbdsf,ym)
sp_vddsf <- split(vddsf,ym)


for (i in 1:361){
  
  a<-names(sp_T2[i])
  m<-toupper(substr(a,1,3))
  y<-substr(a,5,8)
  
  filename <- paste0("LS_",y,"_",m,".h5")  
  
  
  T2_var <- as.vector(sp_T2[[i]])
  RAINNC_var <- as.vector(sp_RAINNC[[i]])
  prate_mont_vec_diff<-diff(RAINNC_var)
  prate_mont_vec_diff <- replace(prate_mont_vec_diff, prate_mont_vec_diff<0, 0)
  prate_mont_vec_diff_kg<-(1/3600)*prate_mont_vec_diff
  prate_mont_vec_diff_kg<-append(prate_mont_vec_diff_kg,prate_mont_vec_diff_kg[length(prate_mont_vec_diff_kg)])
  PSFC_var <- as.vector(sp_PSFC[[i]])
  HGT_var <- as.vector(sp_HGT[[i]])
  U10_var <- as.vector(sp_U10[[i]])
  V10_var <- as.vector(sp_V10[[i]])
  Q2_var <- as.vector(sp_Q2[[i]])
  GLW_var <- as.vector(sp_GLW[[i]])
  SWDOWN_var <- as.vector(sp_SWDOWN[[i]])
  nddsf_var <- as.vector(sp_nddsf[[i]])
  vbdsf_var <- as.vector(sp_vbdsf[[i]])
  vddsf_var <- as.vector(sp_vddsf[[i]])
  
  n2 <- length(T2_var)
  
  h5createFile(filename)
  h5createDataset(filename, "tmp", c(n2,1,1))
  h5createDataset(filename, "prate", c(n2,1,1))
  h5createDataset(filename, "pres", c(n2,1,1))
  h5createDataset(filename, "hgt", c(n2,1,1))
  h5createDataset(filename, "ugrd", c(n2,1,1))
  h5createDataset(filename, "vgrd", c(n2,1,1))
  h5createDataset(filename, "sh", c(n2,1,1))
  h5createDataset(filename, "dlwrf", c(n2,1,1))
  h5createDataset(filename, "nbdsf", c(n2,1,1))
  h5createDataset(filename, "nddsf", c(n2,1,1))
  h5createDataset(filename, "vbdsf", c(n2,1,1))
  h5createDataset(filename, "vddsf", c(n2,1,1))
  
  h5write(T2_var, file = filename, name = "/tmp")
  h5write(prate_mont_vec_diff_kg, file = filename, name = "/prate")
  h5write(PSFC_var, file = filename, name = "/pres")
  h5write(HGT_var, file = filename, name = "/hgt")
  h5write(U10_var, file = filename, name = "/ugrd")
  h5write(V10_var, file = filename, name = "/vgrd")
  h5write(Q2_var, file = filename, name = "/sh")
  h5write(GLW_var, file = filename, name = "/dlwrf")
  h5write(SWDOWN_var, file = filename, name = "/nbdsf")
  h5write(nddsf_var, file = filename, name = "/nddsf")
  h5write(vbdsf_var, file = filename, name = "/vbdsf")
  h5write(vddsf_var, file = filename, name = "/vddsf")    
}


############################################################3

#Compare with old wrf

rm(list = ls())
setwd('/home/hamiddashti/Desktop/wrf')
source("extract_wrf.R")
JAN1 <- extract_wrf(2009,01,"prate","LS","FALSE")
t <-seq(1,length(JAN1))
setwd('/home/hamiddashti/Desktop/wrf_d2')
source("extract_wrf.R")
JAN2 <- extract_wrf(2009,01,"prate","LS","FALSE")
dim(JAN2)<-c(3,248)
b <- colMeans(JAN2)

plot(t,JAN1)
lines(t,b)
plot(JAN1,b)
