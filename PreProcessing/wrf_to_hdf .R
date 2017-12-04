wrf_to_hdf <- function(y1,y2,leap){
  
  library(openxlsx)
  library(rhdf5)
  library(ncdf4)
  
  #y1=2010;y2=2015;leap=2012
  
  filename_tmp <- paste0("T2_",y1,"_",y2,".xlsx")
  filename_pres <- paste0("PSFC_",y1,"_",y2,".xlsx")
  filename_prate <- paste0("RAINNC_",y1,"_",y2,".xlsx")
  filename_hgt <- paste0("HGT_",y1,"_",y2,".xlsx")
  filename_ugrd <- paste0("U10_",y1,"_",y2,".xlsx")
  filename_vgrd <- paste0("V10_",y1,"_",y2,".xlsx")
  filename_sh <- paste0("Q2_",y1,"_",y2,".xlsx")
  filename_dlwrf <- paste0("GLW_",y1,"_",y2,".xlsx")
  filename_nbdsf <- paste0("SWDOWN_",y1,"_",y2,".xlsx")
  
  tmp <- as.matrix(read.xlsx(filename_tmp, sheet = 1,colNames = T))
  pres <-as.matrix(read.xlsx(filename_pres, sheet = 1,colNames = T))
  prate <- as.matrix(read.xlsx(filename_prate, sheet = 1,colNames = T))
  hgt <- as.matrix(read.xlsx(filename_hgt, sheet = 1,colNames = T))
  ugrd <- as.matrix(read.xlsx(filename_ugrd, sheet = 1,colNames = T))
  vgrd <- as.matrix(read.xlsx(filename_vgrd, sheet = 1,colNames = T))
  sh <- as.matrix(read.xlsx(filename_sh, sheet = 1,colNames = T))  
  dlwrf <- as.matrix(read.xlsx(filename_dlwrf, sheet = 1,colNames = T))
  nbds <- as.matrix(read.xlsx(filename_nbdsf, sheet = 1,colNames = T))
  
  
  for (i in y1:y2){
    
    if (i==leap){
      n_day = 366;leap=(leap+4)
    } else {n_day=365}
    
    if (ncol(tmp)<n_day){
      tmp_year <- tmp[,1:ncol(tmp)]
      prate_year <- prate[,1:ncol(prate)]
      pres_year <- pres[,1:ncol(pres)]
      hgt_year <- hgt[,1:ncol(hgt)]
      ugrd_year <- ugrd[,1:ncol(ugrd)]
      vgrd_year <- vgrd[,1:ncol(vgrd)]
      sh_year <- sh[,1:ncol(sh)]
      dlwrf_year <- dlwrf[,1:ncol(dlwrf)]
      nbds_year <- nbds[,1:ncol(nbds)]
      
    } else {
      tmp_year <- tmp[,1:n_day] 
      prate_year <- prate[,1:n_day] 
      pres_year <- pres[,1:n_day]
      hgt_year <- hgt[,1:n_day]
      ugrd_year <- ugrd[,1:n_day]
      vgrd_year <- vgrd[,1:n_day]
      sh_year <- sh[,1:n_day]
      dlwrf_year <-  dlwrf[,1:n_day]
      nbds_year <- nbds[,1:n_day]
    } 
    
    
    if (i==y2){ 
      tmp <- tmp
      prate <- prate
      pres <- pres
      hgt <- hgt
      ugrd <- ugrd
      vgrd <- vgrd
      sh <- sh
      dlwrf <- dlwrf
      nbds <-nbds
    } else {
      tmp <- tmp[,(n_day+1):ncol(tmp)]
      prate <- prate[,(n_day+1):ncol(prate)]
      pres <- pres[,(n_day+1):ncol(pres)]
      hgt <- hgt[,(n_day+1):ncol(hgt)]
      ugrd <- ugrd[,(n_day+1):ncol(ugrd)]
      vgrd <- vgrd[,(n_day+1):ncol(vgrd)]
      sh <- sh[,(n_day+1):ncol(sh)]
      dlwrf <- dlwrf[,(n_day+1):ncol(dlwrf)]
      nbds <- nbds[,(n_day+1):ncol(nbds)]
    }
    
    for (j in 1:12){
      
      if (j == 1) {k2 = 31;M="JAN"}
      if (j == 2) {
        if (i==leap){k2=29;M="FEB"
        } else {k2=28;M="FEB"}}
      if (j == 3) {k2 = 31;M="MAR" }
      if (j == 4) {k2 = 30;M="APR"}
      if (j == 5) {k2 = 31;M="MAY"}
      if (j == 6) {k2 = 30;M="JUN"}
      if (j == 7) {k2 = 31;M="JUL"}
      if (j == 8) {k2 = 31;M="AUG"}
      if (j == 9) {k2 = 30;M="SEP"}
      if (j == 10) {k2 = 31;M="OCT"}
      if (j == 11) {k2 = 30;M="NOV"}
      if (j == 12) {k2 = 31;M="DEC"}
      
      n = k2*8
      if (ncol(tmp_year)<k2){
        print(paste("Nomber of days fo the",M, "of",i, "is not complete"))
        print("Process is terminated!!")
        break
        
      }
      tmp_month <- tmp_year[,1:k2]
      prate_month <- prate_year[,1:k2]
      pres_month <- pres_year[,1:k2]
      hgt_month <- hgt_year[,1:k2]
      ugrd_month <- ugrd_year[,1:k2]
      vgrd_month <- vgrd_year[,1:k2]
      sh_month <- sh_year[,1:k2]
      dlwrf_month <- dlwrf_year[,1:k2]
      nbds_month <- nbds_year[,1:k2] 
      if(j==12){
        tmp_year=tmp_year
        prate_year=prate_year
        pres_year=pres_year
        hgt_year=hgt_year
        ugrd_year=ugrd_year
        vgrd_year=vgrd_year
        sh_year=sh_year
        dlwrf_year=dlwrf_year
        nbds_year=nbds_year
      } else {
        tmp_year = tmp_year[,(k2+1):ncol(tmp_year)]
        prate_year = prate_year[,(k2+1):ncol(prate_year)]
        pres_year = pres_year[,(k2+1):ncol(pres_year)]
        hgt_year = hgt_year[,(k2+1):ncol(hgt_year)]
        ugrd_year = ugrd_year[,(k2+1):ncol(ugrd_year)]
        vgrd_year = vgrd_year[,(k2+1):ncol(vgrd_year)]
        sh_year = sh_year[,(k2+1):ncol(sh_year)]
        dlwrf_year = dlwrf_year[,(k2+1):ncol(dlwrf_year)]
        nbds_year = nbds_year[,(k2+1):ncol(nbds_year)]
      }
      tmp_mont_vec <- as.vector(tmp_month)
      prate_mont_vec <- as.vector(prate_month)
      prate_mont_vec_diff<-diff(prate_mont_vec)
      prate_mont_vec_diff <- replace(prate_mont_vec_diff, prate_mont_vec_diff<0, 0)
      prate_mont_vec_diff_kg<-(1/10800)*prate_mont_vec_diff
      prate_mont_vec_diff_kg<-append(prate_mont_vec_diff_kg,prate_mont_vec_diff_kg[length(prate_mont_vec_diff_kg)])
      
      pres_mont_vec <- as.vector(pres_month)
      hgt_mont_vec <- as.vector(hgt_month)
      ugrd_mont_vec <- as.vector(ugrd_month)
      vgrd_mont_vec <- as.vector(vgrd_month)
      sh_mont_vec <- as.vector(sh_month)
      dlwrf_mont_vec <- as.vector(dlwrf_month)
      nbds_mont_vec <- as.vector(nbds_month)
      
      filename <- paste0("HOL_",i,"_",M,".h5")
      # Create new hdf5 file
      h5createFile(filename)
      h5createDataset(filename, "tmp", c(n,1,1))
      h5createDataset(filename, "prate", c(n,1,1))
      h5createDataset(filename, "pres", c(n,1,1))
      h5createDataset(filename, "hgt", c(n,1,1))
      h5createDataset(filename, "ugrd", c(n,1,1))
      h5createDataset(filename, "vgrd", c(n,1,1))
      h5createDataset(filename, "sh", c(n,1,1))
      h5createDataset(filename, "dlwrf", c(n,1,1))
      h5createDataset(filename, "nbds", c(n,1,1))
      h5write(tmp_mont_vec, file = filename, name = "/tmp")
      h5write(prate_mont_vec_diff_kg, file = filename, name = "/prate")
      h5write(pres_mont_vec, file = filename, name = "/pres")
      h5write(hgt_mont_vec, file = filename, name = "/hgt")
      h5write(ugrd_mont_vec, file = filename, name = "/ugrd")
      h5write(vgrd_mont_vec, file = filename, name = "/vgrd")
      h5write(sh_mont_vec, file = filename, name = "/sh")
      h5write(dlwrf_mont_vec, file = filename, name = "/dlwrf")
      h5write(nbds_mont_vec, file = filename, name = "/nbds")
      
      
      
    }
    
  }
  H5close()
  
}