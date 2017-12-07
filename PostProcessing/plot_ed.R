rm(list = ls())
library(rhdf5)
library(ncdf4)

pfx = "hhh-D"
y1=2014;m1=01;d1=01


setwd("~/bcal/Data02/projects-active/NASA_TE/ED/Outputs/shrub_output")

List <- list.files(pattern = pfx)
n=length(List)
# The following step is just to make sure that dates are sorted 
dd<-seq(as.Date("2014-01-01"), as.Date("2019-01-04"), by="days")
pfx2 <- paste(pfx,"-",dd,sep = "")
pfx3 <- gsub(pfx2[1],"",List[1])
fName <- paste(pfx2,pfx3,sep="")

fcontent <- h5ls(fName[1],recursive = TRUE,all = TRUE, datasetinfo = TRUE,
        index_type = h5default("H5_INDEX"),
        order = h5default("H5_ITER"))
varName <- fcontent$name


for (i in 1:n){
  
  h5read(fName[1],"/PACO_ID")
  h5read(fName[1],"/PACO_N")
  patch_n <- h5read(fName[1],"/PATCH_COUNT")
  agb_co <- h5read(fName[1],'/AGB_CO');
  ba_co <- h5read(fName[1],'/BA_CO');
  balive_co <- h5read(fName[1],'/BALIVE');
  bdead_co <- h5read(fName[1],'/BDEAD');
  btotal <- balive_co+bdead_co
  bseeds_co <- h5read(fName[1],'/BSEEDS_CO');
  daylight <- h5read(fName[1],'/DAYLIGHT');
  dbh_co <- h5read(fName[1],'/DBH');
  dmean_a_net_co <- h5read(fName[1],'/DMEAN_A_NET_CO');
  dmean_albedo <- h5read(fName[1],'/DMEAN_ALBEDO_PA');
  deman_atm_temp_py <- h5read(fName[1],'/DMEAN_ATM_TEMP_PY');
  dmean_available_water_pa<-h5read(fName[1],'/DMEAN_AVAILABLE_WATER_PA');
  dmean_fs_open_co<-h5read(fName[1],'/DMEAN_FS_OPEN_CO');
  dmean_fsn_co<-h5read(fName[1],'/DMEAN_FSN_CO');
  dmean_fsw_co<-h5read(fName[1],'/DMEAN_FSW_CO');
  dmean_gpp_co<-h5read(fName[1],'/DMEAN_GPP_CO');
  dmean_leaf_gsw_py<-h5read(fName[1],'/DMEAN_LEAF_GSW_PY');
  dmean_npp_co<-h5read(fName[1],'/DMEAN_NPP_CO');  
  dmean_nppdaily_co<-h5read(fName[1],'/DMEAN_NPPDAILY_CO');  
  dmean_par_l_co<-h5read(fName[1],'/DMEAN_PAR_L_CO');   
  dmean_pcpg_py<-h5read(fName[1],'/DMEAN_PCPG_PY'); 
  dmean_plresp_co<-h5read(fName[1],'/DMEAN_PLRESP_CO'); 
  dmean_sfcw_depth_pa<-h5read(fName[1],'/DMEAN_SFCW_DEPTH_PA'); 
  dmean_wshed_lg_co<-h5read(fName[1],'/DMEAN_WSHED_LG_CO'); 
  fast_soil_c_py<-h5read(fName[1],'/FAST_SOIL_C_PY'); 
  slow_soil_c_py<-h5read(fName[1],'/SLOW_SOIL_C_PY'); 
  struct_soil_c_py<-h5read(fName[1],'/STRUCT_SOIL_C_PY'); 
  fast_soil_n_py<-h5read(fName[1],'/FAST_SOIL_N_PY');
  hite<-h5read(fName[1],'/HITE');
  lai_co<-h5read(fName[1],'/LAI_CO');
  mort_rate_co<-h5read(fName[1],'/MORT_RATE_CO');
  nplant<-h5read(fName[1],'/NPLANT');
  pft<-h5read(fName[1],'/PFT');
  
}