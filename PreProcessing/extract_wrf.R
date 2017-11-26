extract_wrf<-function(y,m,var,pfx,leap){
  # The out put of this function is to extract varibales from wrf h5 files. 
  # The h5 files are created by wrf_to_hdf function....
  # y:year; m:month; d1:first day,d2: last day, var:variable name; pfx: name perfix, leap:"TRUE" or "FALSE"
  library(rhdf5)
  library(ncdf4)
if (m == 01) {k2 = 31;M="JAN"}
if (m == 02) {
  if (leap=="TRUE"){k2=29;M="FEB"
  } else {k2=28;M="FEB"}}
if (m == 03) {k2 = 31;M="MAR" }
if (m == 04) {k2 = 30;M="APR"}
if (m == 05) {k2 = 31;M="MAY"}
if (m == 06) {k2 = 30;M="JUN"}
if (m == 07) {k2 = 31;M="JUL"}
if (m == 08) {k2 = 31;M="AUG"}
if (m == 09) {k2 = 30;M="SEP"}
if (m == 10) {k2 = 31;M="OCT"}
if (m == 11) {k2 = 30;M="NOV"}
if (m == 12) {k2 = 31;M="DEC"}

fName<-paste(pfx,'_',y,"_",M,".h5",sep = "")

vName<-paste("/",var,sep = "")
v1 <- h5read(fName,vName)
v2 <- v1[,,]
cName<-rep(M,length(v2))
names(v2) <- cName
  
  
return(v2)
}










