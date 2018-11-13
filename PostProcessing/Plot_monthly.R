####
rm(list=ls())
library(ggplot2)
library(zoo)
library(reshape2)
#setwd("C:/ED")  When using mylaptop!
setwd("N:/Data02/bcal/Personal/hamid/ED_BSU")
df = read.csv("PFT.csv", header = TRUE)
output <- matrix(0L,ncol=dim(df)[1], nrow=18)
rownames(output) <- c('C4 grass','Early tropical','Mid tropical','late tropical ',
                    'temperate C3 grass ','northern pine','southern pines','late conifers',
                    'early temperate deciduous','mid temperate deciduous',
                    'late temperate deciduous','agricultural PFTs','agricultural PFTs',
                    'agricultural PFTs','agricultural PFTs','Subtropical C3 grass ','Araucaria"','Shrub')
dates <- as.character(df$dates)
colnames(output) <- dates

for (i in 1:dim(df)[1]){
  
  pft_tmp <- as.character(df$PFT[i])
  pft_tmp<-gsub("[()]", "", pft_tmp)
  PFT=as.numeric(strsplit(pft_tmp,split=", ",fixed=TRUE)[[1]])
  
  nplant_tmp <- as.character(df$NPLANT[i])
  nplant_tmp<-gsub("[()]", "", nplant_tmp)
  NPLANT=as.numeric(strsplit(nplant_tmp,split=", ",fixed=TRUE)[[1]])
  
  groups <- split(NPLANT, PFT)
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
data<-as.data.frame(data)
data$row <- rownames(data)
data2<-melt(data,id.vars = "row")
a<-data2$variable
b=as.character(a)
c=as.yearmon(b)
data2$variable <- c

  ggplot(data2, aes(x=variable, y=value, fill=row)) + 
  geom_bar(stat="identity") + xlab("\nDate") + ylab("Density [plant/m2]\n")+
     theme_bw() +  guides(fill=guide_legend(title="PFTs"))+
    theme(axis.text.x = element_text(angle = 90, hjust = 1)
                       ,axis.text=element_text(size=12),
                       axis.title=element_text(size=14,face="bold"),
                       legend.text=element_text(size=10,face="bold")) +scale_x_yearmon(format="%b-%Y", n=5)
  



