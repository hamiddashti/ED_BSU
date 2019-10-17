##########################################################
# This function plots montly data the data extracted from the csv file which is 
# poroduced with ed_out_monthly.py
# Example plot_monthly('2014-01','2016-02',"output.csv","var")
plot_monthly <- function(date1,date2,file,var){
  #rm(list=ls())
  library(ggplot2)
  library(zoo)
  library(reshape2)
  #setwd("C:/ED")  When using mylaptop!
  
  pfx1 <- "N:/Data02/bcal/Personal/hamid/ED_opt/tmp_analysis/"
  pfx2 <- file
  File <- paste0(pfx1,pfx2)
  
  df <- read.csv(File, header = TRUE)
  I1 <- which(df$dates==date1)
  I2 <- which(df$dates==date2)
  #tmp <- I2-I1
  
  #if (tmp < 5){print("Provide a wider range of dates: you might get error")}
  
  df <- df[I1:I2,]
  
  output <- matrix(0L,ncol=dim(df)[1], nrow=18)
  rownames(output) <- c('C4 Grass','Early tropical','Mid tropical','late tropical ',
                        'Temperate C3 Grass','northern pine','southern pines','late conifers',
                        'early temperate deciduous','mid temperate deciduous',
                        'late temperate deciduous','agricultural PFTs','agricultural PFTs',
                        'agricultural PFTs','agricultural PFTs','Subtropical C3 grass ','Araucaria"','Shrub')
  dates <- as.character(df$dates)
  colnames(output) <- dates
  a=df[[var]]
  for (i in 1:dim(df)[1]){
    
    pft_tmp <- as.character(df$PFT[i])
    pft_tmp <- gsub("[()]", "", pft_tmp)
    pft_tmp <- gsub(",$", "", pft_tmp)
    PFT <- as.numeric(strsplit(pft_tmp,split=", ",fixed=TRUE)[[1]])
    
    
    var_tmp <- as.character(a[i])
    var_tmp <- gsub("[()]", "", var_tmp)
    var_tmp <- gsub(",$", "", var_tmp)
    var_tmp <- as.numeric(strsplit(var_tmp,split=", ",fixed=TRUE)[[1]])
    
    nplant_tmp <- as.character(df$NPLANT[i])
    nplant_tmp<-gsub("[()]", "", nplant_tmp)
    nplant_tmp <- gsub(",$", "", nplant_tmp)
    NPLANT=as.numeric(strsplit(nplant_tmp,split=", ",fixed=TRUE)[[1]])
    
    VAR = NPLANT*var_tmp
    
      groups <- split(VAR, PFT)
    pft_name <- names(groups) 
    
    for (j in 1:length(groups)){ 
      
      pft_sum <- sum(groups[[pft_name[j]]])
      
      if (as.numeric(pft_name[j])==1){
        output[1,i] <- pft_sum 
      } else if (as.numeric(pft_name[j])==2){
        output[2,i] <- pft_sum 
      } else if (as.numeric(pft_name[j])==3){
        output[3,i] <- pft_sum 
      } else if (as.numeric(pft_name[j])==4){
        output[4,i] <- pft_sum 
      } else if (as.numeric(pft_name[j])==5){
        output[5,i] <- pft_sum 
      } else if (as.numeric(pft_name[j])==6){
        output[6,i] <- pft_sum 
      } else if (as.numeric(pft_name[j])==7){
        output[7,i] <- pft_sum 
      } else if (as.numeric(pft_name[j])==8){
        output[8,i] <- pft_sum 
      } else if (as.numeric(pft_name[j])==9){
        output[9,i] <- pft_sum 
      } else if (as.numeric(pft_name[j])==10){
        output[10,i] <- pft_sum 
      } else if (as.numeric(pft_name[j])==11){
        output[11,i] <- pft_sum 
      } else if (as.numeric(pft_name[j])==12){
        output[12,i] <- pft_sum 
      } else if (as.numeric(pft_name[j])==13){
        output[13,i] <- pft_sum 
      } else if (as.numeric(pft_name[j])==14){
        output[14,i] <- pft_sum 
      } else if (as.numeric(pft_name[j])==15){
        output[15,i] <- pft_sum 
      } else if (as.numeric(pft_name[j])==16){
        output[16,i] <- pft_sum 
      } else if (as.numeric(pft_name[j])==17){
        output[17,i] <- pft_sum 
      } else if (as.numeric(pft_name[j])==18){
        output[18,i] <- pft_sum 
      }
      
    }
  }
  
  data<-output[rowSums(output != 0) > 0,]
    
  if (is.vector(data)){
    data<-as.data.frame(data)
    data <- t(data)
    data<-as.data.frame(data)
    pft_id <- which(rowSums(output != 0) > 0)
    pft_name <- rownames(output)[pft_id]
    rownames(data)<-pft_name
    data$row <- rownames(data)
    data2<-melt(data,id.vars = "row")
    a<-data2$variable
    b=as.character(a)
    c=as.yearmon(b)
    data2$variable <- c
    
  } else if (is.matrix(data)){
    
    data<-as.data.frame(data)
    data$row <- rownames(data)
    data2<-melt(data,id.vars = "row")
    a<-data2$variable
    b=as.character(a)
    c=as.yearmon(b)
    data2$variable <- c  
  }
  
  
  if (var=="NPLANT"){
    ytitle = "Density [plant/m2]\n"
  } else if (var=="AGB"){
    ytitle = "Biomass [kg/m2]\n"
  } else if (var=="LAI"){
    ytitle = "LAI [m2/m2]\n"
  } else if (var=="GPP"){
    ytitle = "GPP [KGC/year]\n" 
  }else {
    print("data or date ir wrong!! check the CSV file.")
  }
  
  ggplot(data2, aes(x=variable, y=value, fill=row)) + 
    geom_bar(stat="identity") + xlab("\nTime") + ylab(ytitle)+
    theme_bw() +  guides(fill=guide_legend(title="PFTs"))+
    theme(axis.text.x = element_text(angle = 90, hjust = 1)
          ,axis.text=element_text(size=16),
          axis.title=element_text(size=18),
          legend.text=element_text(size=16),
          legend.title=element_text(size=18)) +
    scale_x_yearmon(format="%Y", n=5)+
    scale_fill_manual("legend", values = c("Shrub"="red","Temperate C3 Grass" = "green"))
  
}

