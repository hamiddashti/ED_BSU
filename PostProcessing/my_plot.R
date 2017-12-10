my_plot<-function(df,var,y1,m1,d1,y2,m2,d2){
  # This is daily plot
  library('ggplot2')
  
  library('astsa')
  
  y1=2014;m1=01;d1=01;y2=2018;m2=12;d2=31
  vList <- unlist(df[var])
  
  
  y11=unlist(df[41]);m11=unlist(df[42]);d11=unlist(df[43])
  y22=unlist(df[44]);m22=unlist(df[45]);d22=unlist(df[46])
  date11<-paste(y11,"-",m11,"-",d11,sep="")
  date22<-paste(y22,"-",m22,"-",d22,sep="")
  dd<-seq(as.Date(date11), as.Date(date22), by="days")
  df_var<-data.frame(vList)
  row.names(df_var)<-dd
  
  date1<-paste(y1,"-",m1,"-",d1,sep="")
  date2<-paste(y2,"-",m2,"-",d2,sep="")
  tmp<-seq(as.Date(date1), as.Date(date2), by="days")
  t<-as.character(tmp)
  Data<-df_var[t,1]
  
  data=data.frame(tmp, Data)
  
 if(var=="paco_n"){leg_p="Number of patches";ylable_p="Value"}
  
  p=ggplot(data, aes(x=tmp)) + geom_line(aes(y=Data),col='blue') + 
    labs(title="Time Series Chart", subtitle="Returns Percentage from 'Economics' Dataset", 
         caption="Source: Economics",y="Returns %")+theme(panel.background = element_blank())
  

}




