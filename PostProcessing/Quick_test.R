rm(list=ls())

setwd("N:/Data02/bcal/Personal/hamid/ED_opt/tmp_analysis")

library(rhdf5)

y1=2016;m1=01;d1=01;y2=2016;m2=11;d2=29
List1 <- list.files(pattern = "hhh-D")
pfx="hhh-D"
# The following step is just to make sure that dates are sorted 
date1<-paste(y1,"-",m1,"-",d1,sep="")
date2<-paste(y2,"-",m2,"-",d2,sep="")
dd<-seq(as.Date(date1), as.Date(date2), by="days")
n=length(dd)
pfx2 <- paste(pfx,"-",dd,sep = "")
#pfx3 <- gsub(pfx2[2],"",List1[2])   # Get the rest of the file name after date of simulation
pfx3 <- "-000000-g01.h5"
fName <- paste(pfx2,pfx3,sep="")    # A list of all ED file names in the current folder



GPP_PY <- as.vector(n)
pft<-matrix(0L, nrow = n, ncol =18)
NPP_PY <- as.vector(n)
for (i in 1:n){
  GPP_PY[i] <- h5read(fName[i],"/DMEAN_GPP_PY") 
  NPP_PY[i] <- h5read(fName[i],"/DMEAN_NPP_PY") 
  a<-h5read(fName[n],"/PFT") 
  pft[i,1] <- length(a[a==1])
  pft[i,2] <- length(a[a==2])
  pft[i,3] <- length(a[a==3])
  pft[i,4] <- length(a[a==4])
  pft[i,5] <- length(a[a==5])
  pft[i,6] <- length(a[a==6])
  pft[i,7] <- length(a[a==7])
  pft[i,8] <- length(a[a==8])
  pft[i,9] <- length(a[a==9])
  pft[i,10] <- length(a[a==10])
  pft[i,11] <- length(a[a==11])
  pft[i,12] <- length(a[a==12])
  pft[i,13] <- length(a[a==13])
  pft[i,14] <- length(a[a==14])
  pft[i,15] <- length(a[a==15])
  pft[i,16] <- length(a[a==16])
  pft[i,17] <- length(a[a==17])
  pft[i,18] <- length(a[a==18])
  print(fName[i])

  
  }
plot(GPP_PY,ty="l")
plot(NPP_PY,ty="l")
colSums(pft)


############################################################
# testing one file at a time 
rm(list=ls())
fName <- "hhh-E-2014-11-00-000000-g01.h5"
var1 <- h5read(fName,"/LAI_PY")
var2 <- h5read(fName,"/LAI_CO")
var3 <- h5read(fName,"/PFT")











