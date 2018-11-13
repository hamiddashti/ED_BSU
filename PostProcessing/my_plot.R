my_plot<-function(df,date1, date2, var){
  # This script is to plot ED out puts at diiferent time scales
  # Inputs : 
  #          df: dataframe which is produced by the ed_out.py function
  #          var: The name of variable as it appear in ED (e.g. GPP)
  # Example: 
  # ----------------------------------------------------------------------------------------------------------------------
  library('ggplot2')
  df=df
  if (date1 < as.Date(df$dates[1]) |date2 > as.Date(df$dates[dim(df)[1]])) {
   print("The input dates are out of range") 
  } else {
  
  data <- df[which(as.Date(df$dates)>=as.Date(date1) & as.Date(df$dates)<=as.Date(date2)),]
  if(var=="GPP"){ylabel_p="Daily mean gross primary productivity ";unit="[kgC/m2/yr]"}  
  if(var=="NPP"){ylabel_p="Daily mean net primary productivity ";unit="[kgC/m2/yr]"}  
  if(var=="FSC"){ylabel_p="Fast soil carbon ";unit="[kgC/m2]"}  
  if(var=="SSC"){ylabel_p="Slow soil carbon ";unit="[kgC/m2]"}  
  if(var=="STC"){ylabel_p="Structural soil carbon ";unit="[kgC/m2]"}  
  if(var=="ATC"){ylabel_p="Atmosphere carbon concentration ";unit="[ppm]"}  
  
  graphics.off()
  
  ggplot(data = data, aes(x =as.Date(data$dates), y = data[[var]]))+
  geom_line(color = "#00AFBB", size = 1) +
    labs(x="Time", y=paste(ylabel_p,unit))+
    theme_bw()+theme(text = element_text(size=12),plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
     #ggsave("output.pdf",width = 6.5, height = 4.5)
  }
  
}

  
