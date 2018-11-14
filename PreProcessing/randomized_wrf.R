# This is for generating random climate data for many years from wrf

 rm(list = ls())
 
 library(rhdf5)
 setwd("N:/Data02/bcal/Personal/hamid/ED_opt/wrf_data/wrf_d02_hourly_30years_LS")
 tmp_year <- sample(1988:2016,1000, replace = TRUE)
 hist(tmp_year)
 sim_year<-as.character(seq(1017,2016,by=1))
 start <- as.Date("1017-01-01")
 end <- as.Date("2016-12-31")
 forced_start <- as.Date(paste0(format(start, "%Y-%m"), "-01"))
 forced_end <- as.Date(paste0(format(end, "%Y-%m"), "-01"))
 seq_dates <- seq.Date(forced_start, forced_end, by = "month")
 dates <- toupper(format(seq_dates, "%Y%b"))
 pfx11 <- "N:/Data02/bcal/Personal/hamid/ED_opt/wrf_data/test/wrf_d02_hourly_30years_LS/"
 pfx1 <- "N:/Data02/bcal/Personal/hamid/ED_opt/wrf_data/wrf_d02_hourly_30years_LS/test2/"

#rm <- list.files(pattern = "_OCT")
#file.remove(rm)
 
 
 for (i in 1:1000){
        print(i)
        files <- list.files(pattern = as.character(tmp_year[i]))
        pfx2 <- "LS_"
        pfx3 <- grep(sim_year[i], dates, value=TRUE)
        pfx4 <- sort(pfx3)
        old_name <- paste0(pfx11,files)
        new_name <- paste0(pfx1,pfx2,pfx4,".h5")
       
        file.copy(files, new_name)
}
# test
 save.image("Data_generated.RData")
 sum(drop(h5read("N:/Data02/bcal/Personal/hamid/ED_opt/wrf_data/wrf_d02_hourly_30years_LS/test2/LS_1516JUN.h5","/prate")))
 sum(drop(h5read("N:/Data02/bcal/Personal/hamid/ED_opt/wrf_data/wrf_d02_hourly_30years_LS/LS_2003JUN.h5","/prate")))
 