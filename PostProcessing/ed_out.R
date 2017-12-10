ed_out <- function(y1,m1,d1,y2,m2,d2,pfx){
  #rm(list = ls())
  library(rhdf5)
  library(ncdf4)
  y1=y1;m1=m1;d1=d1;y2=y2;m2=m2;d2=d2
  #pfx = "hhh-D" 
  #setwd("~/bcal/Data02/projects-active/NASA_TE/ED/Outputs/shrub_output")
  
  
  
  List1 <- list.files(pattern = pfx)
  
  # The following step is just to make sure that dates are sorted 
  date1<-paste(y1,"-",m1,"-",d1,sep="")
  date2<-paste(y2,"-",m2,"-",d2,sep="")
  dd<-seq(as.Date(date1), as.Date(date2), by="days")
  n=length(dd)
  pfx2 <- paste(pfx,"-",dd,sep = "")
  pfx3 <- "-000000-g01.h5"
  #gsub(pfx2[2],"",List1[2])
  fName <- paste(pfx2,pfx3,sep="")
  
  paco_id <- as.vector(n)
  paco_n <- as.vector(n)
  patch_n <- as.vector(n)
  ba_co<-as.vector(n)
  nplant <- as.vector(n)
  agb_co <- as.vector(n)
  balive_co <-as.vector(n)
  bdead_co <- as.vector(n)
  btotal <-as.vector(n)
  bseeds_co <-as.vector(n)
  daylight <-as.vector(n)
  dbh_co <-as.vector(n)
  dmean_fsn_co<-as.vector(n)
  dmean_atm_par_py <-as.vector(n)
  deman_atm_temp_py <- as.vector(n)
  deman_atm_rlong_py<- as.vector(n)
  deman_atm_rshort_py <- as.vector(n)
  dmean_available_water_pa <- as.vector(n)
  dmean_fs_open_co <- as.vector(n)
  dmean_fsw_co <- as.vector(n)
  dmean_gpp_co <-as.vector(n)
  dmean_leaf_gsw_py <- as.vector(n)
  dmean_npp_co <- as.vector(n)
  dmean_leaf_temp_co<-as.vector(n)
  dmean_nppcroot_py <-as.vector(n)
  dmean_nppfroot_py <-as.vector(n)
  dmean_par_l_co <-as.vector(n)
  dmean_pcpg_py <- as.vector(n)
  dmean_plresp_co <-as.vector(n)
  dmean_vapor_lc_co<-as.vector(n)
  dmean_sfcw_depth_pa<-as.vector(n)
  dmean_soil_water_pa<-list()
  dmean_transp_co <-as.vector(n)
  dmean_water_supply_co<- as.vector(n)
  dmean_wshed_lg_co<-as.vector(n)
  fast_soil_c_py <- as.vector(n)
  slow_soil_c_py <-as.vector(n)
  struct_soil_c_py<-as.vector(n)
  fast_soil_n_py<-as.vector(n)
  hite<-as.vector(n)
  lai_co<-as.vector(n)
  mort_rate_co<-list(n)
  pft<-list()
  
  for (i in 1:n){
    
    paco_id[i] <- h5read(fName[i],"/PACO_ID")
    paco_n[i] <- h5read(fName[i],"/PACO_N")
    patch_n[i] <- h5read(fName[i],"/PATCH_COUNT")
    nplant[i]<-sum(h5read(fName[i],'/NPLANT'))
    agb_co[i] <- mean(h5read(fName[i],'/AGB_CO'))*nplant[i];     #[kgC/plant].....sum
    ba_co[i] <- mean(h5read(fName[i],'/BA_CO'))*nplant[i];
    balive_co[i] <-mean(h5read(fName[i],'/BALIVE'))*nplant[i]
    bdead_co[i] <- mean(h5read(fName[i],'/BDEAD'))*nplant[i]
    btotal[i]<- balive_co[i]+bdead_co[i]
    bseeds_co[i] <- sum(h5read(fName[i],'/BSEEDS_CO'))
    daylight[i] <- h5read(fName[i],'/DAYLIGHT')/3600;      #hours
    dbh_co <- mean(h5read(fName[i],'/DBH'))            #?????
    # dmean_a_net_co <- h5read(fName[1],'/DMEAN_A_NET_CO')                    # Daily mean - Actual assimilation rate    
    dmean_atm_par_py[i] <- h5read(fName[i],'/DMEAN_ATM_PAR_PY');                       # Daily mean - Albedo - direct radiation  
    deman_atm_temp_py[i] <- h5read(fName[i],'/DMEAN_ATM_TEMP_PY');
    deman_atm_rlong_py[i] <- h5read(fName[i],'/DMEAN_ATM_RLONG_PY');
    deman_atm_rshort_py[i] <- h5read(fName[i],'/DMEAN_ATM_RSHORT_PY');
    dmean_available_water_pa[i] <-h5read(fName[i],'/DMEAN_AVAILABLE_WATER_PA');          # [kg/m2]
    dmean_fs_open_co[i] <-mean(h5read(fName[i],'/DMEAN_FS_OPEN_CO'))
    dmean_fsn_co[i] <-mean(h5read(fName[i],'/DMEAN_FSN_CO'))
    dmean_fsw_co[i]<-mean(h5read(fName[i],'/DMEAN_FSW_CO'))
    dmean_gpp_co[i]<-mean(h5read(fName[i],'/DMEAN_GPP_CO'))*nplant[i]
    dmean_leaf_gsw_py[i] <- sum(h5read(fName[i],'/DMEAN_LEAF_GSW_CO'))*nplant[i]              #Daily mean - Stomatal conductance 
    dmean_npp_co[i]<-mean(h5read(fName[i],'/DMEAN_NPP_CO'))*nplant[i]
    dmean_leaf_temp_co[i]<-mean(h5read(fName[i],'/DMEAN_LEAF_TEMP_CO'))
    dmean_nppcroot_py[i]<-h5read(fName[i],'/DMEAN_NPPCROOT_PY')
    dmean_nppfroot_py[i]<-mean(h5read(fName[i],'/DMEAN_NPPFROOT_CO'))*nplant[i]
    # dmean_nppdaily_co<-h5read(fName[1],'/DMEAN_NPPDAILY_CO') 
    dmean_par_l_co[i]<-sum(h5read(fName[i],'/DMEAN_PAR_L_CO'))                                    # Daily mean - PAR absorbed by leaves    
    dmean_pcpg_py[i] <-h5read(fName[i],'/DMEAN_PCPG_PY');                                       #Daily mean - Precipitation rate
    dmean_plresp_co[i]<-mean(h5read(fName[i],'/DMEAN_PLRESP_CO'))*nplant[i]                                       #Daily mean - Plant respiration [kgC/m2/yr]
    dmean_vapor_lc_co[i]<-sum(h5read(fName[i],'/DMEAN_VAPOR_LC_CO'))                                     #Daily mean - Leaf evaporation
    dmean_sfcw_depth_pa[i]<-h5read(fName[i],'/DMEAN_SFCW_DEPTH_PA') 
    dmean_sfcw_depth_pa[i] <-h5read(fName[i],'/DMEAN_SFCW_MASS_PA')
    dmean_soil_water_pa[i] <- list(h5read(fName[i],'/DMEAN_SOIL_WATER_PA'))
    dmean_transp_co[i] <-sum(h5read(fName[i],'/DMEAN_TRANSP_CO'))
    dmean_water_supply_co[i] <-sum(h5read(fName[i],'/DMEAN_WATER_SUPPLY_CO'))                     # Daily mean - Water supply  [kg/m2/s]  
    dmean_wshed_lg_co[i]<-h5read(fName[i],'/DMEAN_WSHED_LG_PY')
    fast_soil_c_py[i]<-h5read(fName[i],'/FAST_SOIL_C_PY') 
    slow_soil_c_py[i]<-h5read(fName[i],'/SLOW_SOIL_C_PY')
    struct_soil_c_py[i]<-h5read(fName[i],'/STRUCT_SOIL_C_PY') 
    fast_soil_n_py[i]<-h5read(fName[i],'/FAST_SOIL_N_PY');
    hite[i]<-mean(h5read(fName[i],'/HITE'))
    lai_co<-sum(h5read(fName[i],'/LAI_CO'))
    mort_rate_co[i]<-list(h5read(fName[i],'/MORT_RATE_CO'))
    pft[i]<-list(h5read(fName[i],'/PFT'))
    #names(which.max(table(pft)))               #the dominant pft
    print(fName[i])
  }
  dmean_soil_water_pa_mat <- t(matrix(unlist(dmean_soil_water_pa), ncol = 9, byrow = TRUE))
  df<-list(paco_id,paco_n,patch_n,nplant,agb_co,ba_co,balive_co,bdead_co,btotal,bseeds_co,
           daylight,dbh_co,dmean_atm_par_py,deman_atm_temp_py,deman_atm_rlong_py,deman_atm_rshort_py,
           dmean_available_water_pa,dmean_fs_open_co,dmean_fsn_co,dmean_fsw_co,dmean_gpp_co,dmean_leaf_gsw_py,
           dmean_npp_co,dmean_leaf_temp_co,dmean_nppcroot_py,dmean_nppfroot_py,dmean_par_l_co,dmean_pcpg_py,dmean_plresp_co,
           dmean_vapor_lc_co,dmean_sfcw_depth_pa,dmean_soil_water_pa_mat,dmean_transp_co,dmean_water_supply_co,dmean_wshed_lg_co,
           fast_soil_c_py,hite,lai_co,mort_rate_co,pft,y1,m1,d1,y2,m2,d2)
  
  my_names<- c('paco_id','paco_n','patch_n','nplant','agb_co','ba_co','balive_co','bdead_co','btotal','bseeds_co',
               'daylight','dbh_co','dmean_atm_par_py','deman_atm_temp_py','deman_atm_rlong_py','deman_atm_rshort_py',
               'dmean_available_water_pa','dmean_fs_open_co','dmean_fsn_co','dmean_fsw_co','dmean_gpp_co','dmean_leaf_gsw_py',
               'dmean_npp_co','dmean_leaf_temp_co','dmean_nppcroot_py','dmean_nppfroot_py','dmean_par_l_co','dmean_pcpg_py','dmean_plresp_co',
               'dmean_vapor_lc_co','dmean_sfcw_depth_pa','dmean_soil_water_pa_mat','dmean_transp_co','dmean_water_supply_co','dmean_wshed_lg_co',
               'fast_soil_c_py','hite','lai_co','mort_rate_co','pft','y1','m1','d1','y2','m2','d2')
  #names(df) <- my_names
  setNames(df,my_names)
  
}


