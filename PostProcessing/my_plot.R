my_plot<-function(df,date1, date2, var){
  # This script is to plot ED out puts at diiferent time scales
  # Inputs : 
  #          df: dataframe which is produced by the ed_out.py function
  #          var: The name of variable as it appear in ED (e.g. GPP)
  # Example: my_plot(df =df,var = "fast_soil_c_py",steps = '1 year',y1 = 2014,m1 = 01,d1 = 01,
  #                  y2 = 2018,m2 = 12,d2 = 31,caption = "C3grass_phenology_2_h2olim_2")
  # ----------------------------------------------------------------------------------------------------------------------
  library('ggplot2')
  df=df
  if (date1 < as.Date(df$dates[1]) |date2 > as.Date(df$dates[dim(df)[1]])) {
   print("The input dates are out of range") 
  } else {
  
  data <- df[which(as.Date(df$dates)>=as.Date(date1) & as.Date(df$dates)<=as.Date(date2)),]
  if(var=="GPP"){ylabel_p="Daily mean gross primary productivity ";unit="[kgC/m2/yr]"}  
  
  graphics.off()
  
  ggplot(data = data, aes(x =as.Date(data$dates), y = data[[var]]))+
  geom_line(color = "#00AFBB", size = 2) +
    labs(x="Time", y=paste(ylabel_p,unit))+
    theme_bw()+theme(text = element_text(size=14),plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
  
  }
}

  