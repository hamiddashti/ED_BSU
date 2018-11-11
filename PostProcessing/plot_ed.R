rm(list = ls())
setwd("~/bcal/Data02/projects-active/NASA_TE/ED/Outputs/C3grass_phenology_2_h2olim_2")
source('ed_out.R')
source('my_plot.R')
#df<-ed_out(y1=2014,m1=01,d1=01,y2=2018,m2=12,d2=31,pfx = "hhh-D")
#save(df,file='df.rda')


load('df.rda')


my_plot(df =df,var = "fast_soil_c_py",steps = '1 year',y1 = 2014,m1 = 01,d1 = 01,
        y2 = 2018,m2 = 12,d2 = 31,caption = "C3grass_phenology_2_h2olim_2")

# variable names and numbers in the df
# [1] "paco_id"                  "paco_n"                   "patch_n"                  "nplant"                   "agb_co"                  
# [6] "ba_co"                    "balive_co"                "bdead_co"                 "btotal"                   "bseeds_co"               
# [11] "daylight"                 "dbh_co"                   "dmean_atm_par_py"         "deman_atm_temp_py"        "deman_atm_rlong_py"      
# [16] "deman_atm_rshort_py"      "dmean_available_water_pa" "dmean_fs_open_co"         "dmean_fsn_co"             "dmean_fsw_co"            
# [21] "dmean_gpp_co"             "dmean_leaf_gsw_py"        "dmean_npp_co"             "dmean_leaf_temp_co"       "dmean_nppcroot_py"       
# [26] "dmean_nppfroot_py"        "dmean_par_l_co"           "dmean_pcpg_py"            "dmean_plresp_co"          "dmean_vapor_lc_co"       
# [31] "dmean_sfcw_depth_pa"      "dmean_soil_water_pa_mat"  "dmean_transp_co"          "dmean_water_supply_co"    "dmean_wshed_lg_co"       
# [36] "fast_soil_c_py"           "hite"                     "lai_co"                   "mort_rate_co"             "pft (this is not working now!"                     
# [41] "y1"                       "m1"                       "d1"                       "y2"                       "m2"                      
# [46] "d2" 





