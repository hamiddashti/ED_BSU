my_plot<-function(df,var,steps,y1,m1,d1,y2,m2,d2,caption){
  # This script is to plot ED out puts at diiferent time scales
  # Inputs : 
  #          df: dataframe which is produced by the ed_out.R function
  #          var: The name of variable as it appear in ED (e.g. patch_n)
  #          steps: a combination of interval and time scale (some examples: "1 year"; "5 days"; "2 month"
  #                                                           note the space between the time interval and scale)
  #          y1: first year of simulation
  #          m1: first month of simulation
  #          d1: first day of simulation
  #          y2: second year of simulation
  #          m2: second month of simulation
  #          d2: second day of simulation
  #          caption: the caption to be inserted under the photo( can be left empty: "")
  # Example: my_plot(df =df,var = "fast_soil_c_py",steps = '1 year',y1 = 2014,m1 = 01,d1 = 01,
  #                  y2 = 2018,m2 = 12,d2 = 31,caption = "C3grass_phenology_2_h2olim_2")
  # ----------------------------------------------------------------------------------------------------------------------
  library('ggplot2')
  library('astsa')
  
  # some of the variables such as "dmean_soil_water_pa_mat" are matrix...
  # For now script just plot the
  if (var=="dmean_soil_water_pa_mat"){
    v1=df[var]
    v2<-v1$dmean_soil_water_pa_mat
    #slayer<-c('-2.307', '-1.789', '-1.340', '-0.961','-0.648', '-0.400', '-0.215', '-0.089', '-0.020')
    #row.names(v2)<-slayer 
    vList <-v2[9,] # the first layer of soil surface
  } else if (var=="mort_rate_co"){
    v1=df[var]
    v2=v1$mort_rate_co
    vList=as.vector(length(v1$mort_rate_co))
    for (i in 1:length(v1$mort_rate_co)){
      vList[i]<-mean(v2[[i]])
    }
    
  } else  {vList <- unlist(df[var])}
  
  
  
  
  y11=unlist(df[41]);m11=unlist(df[42]);d11=unlist(df[43])
  y22=unlist(df[44]);m22=unlist(df[45]);d22=unlist(df[46])
  date11<-paste(y11,"-",m11,"-",d11,sep="")
  date22<-paste(y22,"-",m22,"-",d22,sep="")
  sim_dur<-seq(as.Date(date11), as.Date(date22), by="days")
  df_var<-data.frame(vList)
  row.names(df_var)<-sim_dur;colnames(df_var)<-var
  
  date1<-paste(y1,"-",m1,"-",d1,sep="")
  date2<-paste(y2,"-",m2,"-",d2,sep="")
  tmp_time<-seq(as.Date(date1), as.Date(date2), by="days")
  t<-as.character(tmp_time)
  
  Data<-df_var[t,1]
  data=data.frame(tmp_time, Data)
  
  if(var=="paco_n"){ylabel_p="Number of patch";unit=""}
  if(var=="patch_n"){ylabel_p="Patch count for each site";unit=""}
  if(var=="nplant"){ylabel_p="Plant density";unit="[plant/m2]"}
  if(var=="agb_co"){ylabel_p="Above ground biomass";unit="[kgC/plant]"}
  if(var=="ba_co"){ylabel_p="Basal area";unit="[cm2]"}
  if(var=="balive_co"){ylabel_p="Biomass alive";unit="[kgC/m2]?? "}
  if(var=="bdead_co"){ylabel_p="Biomass dead";unit="[kgC/m2]?? "}
  if(var=="btotal"){ylabel_p="Biomass total";unit="[kgC/m2]?? "}
  if(var=="bseeds_co"){ylabel_p="Biomass seeds";unit="[kgC/m2]?? "}
  if(var=="daylight"){ylabel_p="Daylight duration";unit="[h] "}
  if(var=="dbh_co"){ylabel_p="Diameter at breast height";unit="[cm] "}
  if(var=="dmean_atm_par_py"){ylabel_p="Daily mean atmosphere PAR";unit="[W/m2] "}
  if(var=="deman_atm_temp_py"){ylabel_p="Daily mean atmosphere temprature";unit="[k] "}
  if(var=="deman_atm_rlong_py"){ylabel_p="Daily mean atmosphere longwave radiation";unit="[W/m2]"}
  if(var=="deman_atm_rshort_py"){ylabel_p="Daily mean atmosphere shortwave radiation";unit="[W/m2]"}  
  if(var=="dmean_available_water_pa"){ylabel_p="Daily mean available water ";unit="[kg/m2]"}    
  if(var=="dmean_fs_open_co"){ylabel_p="Daily mean net stress factor ";unit=""}    
  if(var=="dmean_fsn_co"){ylabel_p="Daily mean nitrogen stress factor ";unit=""}    
  if(var=="dmean_fsw_co"){ylabel_p="Daily mean moisture stress factor ";unit=""}  
  if(var=="dmean_gpp_co"){ylabel_p="Daily mean gross primary productivity ";unit="[kgC/m2/yr]"}  
  if(var=="dmean_leaf_gsw_py"){ylabel_p="Daily mean stomatal conductance ";unit="[kg/m2leaf/s]"}  
  if(var=="dmean_npp_co"){ylabel_p="Daily mean net primary productivity ";unit="[kgC/m2/yr]"}    
  if(var=="dmean_leaf_temp_co"){ylabel_p="Daily mean leaf temperature ";unit="[k]"}  
  if(var=="dmean_nppcroot_py"){ylabel_p="Daily mean net primary productivity (Coarse root) ";unit="[kgC/m2/yr]"}  
  if(var=="dmean_nppfroot_py"){ylabel_p="Daily mean net primary productivity (Fine root) ";unit="[kgC/m2/yr]"} 
  if(var=="dmean_par_l_co"){ylabel_p="Daily mean PAR absorbed by leaves  ";unit="[w/m2]"} 
  if(var=="dmean_pcpg_py"){ylabel_p="Daily mean precipitation rate  ";unit="[kg/m2/s]"} 
  if(var=="dmean_plresp_co"){ylabel_p="Daily mean plant respiration  ";unit="[kgC/m2/yr]"}  
  if(var=="dmean_vapor_lc_co"){ylabel_p="Daily mean leaf evaporation  ";unit="[kg/m2/s]"}  
  if(var=="dmean_sfcw_depth_pa"){ylabel_p="Daily mean temporary water layer depth  ";unit="[m]"}  
  if(var=="dmean_soil_water_pa_mat"){ylabel_p="Daily mean soil water content (depth=20cm)";unit="[m3/m3]"}  
  if(var=="dmean_wshed_lg_co"){ylabel_p="Daily mean leaf shedding";unit="[kg/m2/s]"} 
  if(var=="dmean_transp_co"){ylabel_p="Daily mean leaf transpiration";unit="[kg/m2/s]"} 
  if(var=="dmean_water_supply_co"){ylabel_p="Daily mean Water supply";unit="[kg/m2/s]"} 
  
  if(var=="fast_soil_c_py"){ylabel_p="Soil Carbon (Fast pool)";unit="[kgC/m2]"}   
  if(var=="hite"){ylabel_p="Mean height";unit="[m]"}   
  if(var=="lai_co"){ylabel_p="Leaf area index";unit=""}   
  if(var=="mort_rate_co"){ylabel_p="mortality rate";unit="[1/yr]"}  
  #if(var=="pft"){ylabel_p="Plant functional type";unit=""}  
  

  
  # The commented codes are for ploting all the soil layers
  # if (var=="dmean_soil_water_pa_mat"){
  #   p=ggplot(mdf, aes(x=tmp_time)) +geom_line(aes(y=value),size=0.5) + 
  #     labs(x="Time",caption=caption, y=paste(ylabel_p,unit))+ theme_bw()+theme(text = element_text(size=18),
  #     plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
  #   p
  # } 
  graphics.off()
  p=ggplot(data, aes(x=tmp_time)) +geom_line(aes(y=Data),col='black',size=1.5) + 
    labs(x="Time",caption=caption, y=paste(ylabel_p,unit))+ theme_bw()+theme(text = element_text(size=18),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
  
  if (grepl('day',steps)){
    p+scale_x_date(date_breaks = steps,date_labels ="%b-%d" )  
    } else if (grepl('month',steps)){
      p+scale_x_date(date_breaks = steps,date_labels ="%b-%d" )
    } else if (grepl('year',steps)){
      p+scale_x_date(date_breaks = steps,date_labels ="%b-%Y" ) 
    }

}

