wrf_to_hdf <- function(y1,y2,leap){
  rm(list = ls())
  library(openxlsx)
  library(rhdf5)
  library(ncdf4)
  
  

  filename_tmp <- paste0("T2_",y1,"_",y2,".xlsx")
  #filename_pres <- paste0("PSFC_",y1,"_",y2,".xlsx")
  # filename_prate <- paste0("RAINNC_",y1,"_",y2,".xlsx")
  # filename_hgt <- paste0("HGT_",y1,"_",y2,".xlsx")
  # filename_ugrd <- paste0("U10_",y1,"_",y2,".xlsx")
  # #filename_vgrd <- paste0("V10_",y1,"_",y2,".xlsx")
  # filename_sh <- paste0("Q2_",y1,"_",y2,".xlsx")
  # #filename_dlwrf <- paste0("GLW_",y1,"_",y2,".xlsx")s
  # filename_nbdsf <- paste0("SWDOWN_",y1,"_",y2,".xlsx")
  
  tmp <- as.matrix(read.xlsx(filename_tmp, sheet = 1,colNames = T))
  #pres <-as.matrix(read.xlsx(filename_pres, sheet = 1,colNames = T))
  # prate <- read.xlsx(filename_prate, sheet = 1,colNames = T)
  # hgt <- read.xlsx(filename_hgt, sheet = 1,colNames = T)
  # #ugrd <- read.xlsx(filename_ugrd, sheet = 1,colNames = T)
  # #vgrd <- read.xlsx(filename_vgrd, sheet = 1,colNames = T)
  # sh <- read.xlsx(filename_sh, sheet = 1,colNames = T)
  # #dlwrf <- read.xlsx(filename_dlwrf, sheet = 1,colNames = T)
  # nbds <- read.xlsx(filename_nbdsf, sheet = 1,colNames = T)
 
  
  for (i in y1:y2){
    
    if (i==leap){
      n_day = 366;leap=(leap+4)
    } else {n_day=365}
    
    if (ncol(tmp)<n_day){
      tmp_year <- tmp[,1:ncol(tmp)]
    } else {tmp_year <- tmp[,1:n_day] } 
    
    
    
    if (i==y2){ tmp <- tmp
    } else {tmp <- tmp[,(n_day+1):ncol(tmp)]}
    # 
    # pres_year <- pres[,1:n_day]
    # if (i==y2){ pres <- pres
    # } else {pres <- pres[,(n_day+1):ncol(pres)]}
    
      
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
      if(j==12){tmp_year=tmp_year 
      } else {tmp_year = tmp_year[,(k2+1):ncol(tmp_year)]}
      tmp_mont_vec <- as.vector(tmp_month)
      
      
      
      # pres_month <- pres_year[,1:k2]
      # if(j==12){pres_year=pres_year 
      # } else {pres_year = pres_year[,(k2+1):ncol(pres_year)]}
      # pres_mont_vec <- as.vector(pres_month)
      
      
      
      filename <- paste0("HOL_",i,"_",M,".h5")
      # Create new hdf5 file
      h5createFile(filename)
      h5createDataset(filename, "tmp", c(n,1,1))
      h5write(tmp_mont_vec, file = filename, name = "/tmp")
      
      # h5createDataset(filename, "pres", c(n,1,1))
      # h5write(pres_mont_vec, file = filename, name = "/pres")
    }
    
  }
  H5close()
  
}
  
