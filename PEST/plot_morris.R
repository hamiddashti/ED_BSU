# This script is for plotting Morris and Sobol sensitivity analysis

rm(list = ls())
setwd("N:/Data02/bcal/Personal/hamid/ED_opt/working/Sensitivity_analysis")
library(ggplot2)
library(zoo)
library(reshape2)
library(wesanderson)
library(RColorBrewer)
library(ggrepel)


###########################################################
# Plot total Morris
###########################################################
pfx1 = "Morris_ws_final"
pfx2 = ".csv"
filename= paste(pfx1,pfx2,sep="")
title = "WBS"
data <- read.csv(filename,header = T)

ggplot(data,aes(x=sen_mean_abs,y=sen_std_dev))+ geom_point(size=3, shape=4)+
  geom_text_repel(aes(label=data$parameter_name))+
  theme_bw()+ ggtitle(title)+
  theme(panel.grid.major.x = element_blank(),panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank())+
  labs(x=expression(paste(mu, "*")),y=expression(sigma)) + 
  theme(axis.text=element_text(size=12),
  axis.title=element_text(size=20,face="bold"),
  axis.text.x = element_text(size=12,colour = "black"),
  axis.text.y = element_text(size=12,colour = "black"),
  plot.title = element_text( size=14,hjust = 0.5,face="bold"))
  
outname = paste(pfx1,".png",sep="")
ggsave(outname, width = 5, height = 5, units = "in",dpi=300)

#################################################################
# Plotting the temporal Mean for all outputs
#################################################################
pfx1 = "Morris_all_ls"
pfx2 = ".csv"
filename= paste(pfx1,pfx2,sep="")
title = "MBS"
num_ob = 1096

data2 <- read.csv(filename,header = F)
tmp=as.matrix(data2)

SLA_u = numeric(num_ob)
VM0_u = numeric(num_ob)
STO_S_u = numeric(num_ob)
Q_RATIO_u = numeric(num_ob)
FTR_u = numeric(num_ob)
LTR_u = numeric(num_ob)
GRESP_u = numeric(num_ob)
CUT_C_u = numeric(num_ob)
WAT_C_u = numeric(num_ob)
S_MOR_u = numeric(num_ob)
L_WID_u = numeric(num_ob)
STR_u = numeric(num_ob)
var_id = 4
for (i in 1:num_ob){
name_obs = paste("Method of Morris for observation: O",i,sep="")
I= which(data2$V1==name_obs)
SLA_u[i] = as.numeric(tmp[I+2,var_id])
VM0_u[i] = as.numeric(tmp[I+3,var_id])
STO_S_u[i] = as.numeric(tmp[I+4,var_id])
Q_RATIO_u[i] = as.numeric(tmp[I+5,var_id])
FTR_u[i] = as.numeric(tmp[I+6,var_id])
LTR_u[i] = as.numeric(tmp[I+7,var_id])
GRESP_u[i] = as.numeric(tmp[I+8,var_id])
CUT_C_u[i] = as.numeric(tmp[I+9,var_id])
WAT_C_u[i] = as.numeric(tmp[I+10,var_id])
S_MOR_u[i] = as.numeric(tmp[I+11,var_id])
L_WID_u[i] = as.numeric(tmp[I+12,var_id])
STR_u[i] = as.numeric(tmp[I+13,var_id])

}


#st <- as.Date("2015-10-01")
st <- as.Date("2014-10-01")
en <- as.Date("2017-09-30")
date_range <- seq(st, en, by = "day")

df=cbind.data.frame(date_range,SLA_u,VM0_u,STO_S_u,Q_RATIO_u,FTR_u,
                   LTR_u,GRESP_u,CUT_C_u,WAT_C_u,S_MOR_u,L_WID_u,STR_u)
b<-melt(df,id.vars = "date_range")
mycolors = c(brewer.pal(name="Dark2", n = 8), brewer.pal(name="Paired", n = 6))

ggplot(b, aes(x=date_range, y=value, fill=variable)) + 
  geom_bar(stat="identity") + xlab("")+ ylab(expression(paste(mu, "*")))+
  ggtitle(title)+ 
  theme_bw() +  guides(fill=guide_legend(title="Parameter"))+
  theme(panel.grid.major.x = element_blank(),panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),axis.text.x = element_text(angle = 90, hjust = 1)
        ,axis.text=element_text(size=12),
        plot.title = element_text( size=14,hjust = 0.5),
        axis.title=element_text(size=16,face="bold"),
        legend.text=element_text(size=6),
        legend.title = element_text(size=7))+
  scale_fill_manual(values = mycolors)

outname=paste(pfx1,"_Mean.png",sep="")
ggsave(outname, width = 5, height = 5, units = "in",dpi=300)


#################################################################
# Plotting the temporal Sigma for all outputs
#################################################################

data2 <- read.csv(filename,header = F)
tmp=as.matrix(data2)
SLA_u = numeric(num_ob)
VM0_u = numeric(num_ob)
STO_S_u = numeric(num_ob)
Q_RATIO_u = numeric(num_ob)
FTR_u = numeric(num_ob)
LTR_u = numeric(num_ob)
GRESP_u = numeric(num_ob)
CUT_C_u = numeric(num_ob)
WAT_C_u = numeric(num_ob)
S_MOR_u = numeric(num_ob)
L_WID_u = numeric(num_ob)
STR_u = numeric(num_ob)
var_id = 5
for (i in 1:num_ob){
  name_obs = paste("Method of Morris for observation: O",i,sep="")
  I= which(data2$V1==name_obs)
  SLA_u[i] = as.numeric(tmp[I+2,var_id])
  VM0_u[i] = as.numeric(tmp[I+3,var_id])
  STO_S_u[i] = as.numeric(tmp[I+4,var_id])
  Q_RATIO_u[i] = as.numeric(tmp[I+5,var_id])
  FTR_u[i] = as.numeric(tmp[I+6,var_id])
  LTR_u[i] = as.numeric(tmp[I+7,var_id])
  GRESP_u[i] = as.numeric(tmp[I+8,var_id])
  CUT_C_u[i] = as.numeric(tmp[I+9,var_id])
  WAT_C_u[i] = as.numeric(tmp[I+10,var_id])
  S_MOR_u[i] = as.numeric(tmp[I+11,var_id])
  L_WID_u[i] = as.numeric(tmp[I+12,var_id])
  STR_u[i] = as.numeric(tmp[I+13,var_id])
  
}


#st <- as.Date("2015-10-01")
st <- as.Date("2014-10-01")
en <- as.Date("2017-09-30")
date_range <- seq(st, en, by = "day")

df=cbind.data.frame(date_range,SLA_u,VM0_u,STO_S_u,Q_RATIO_u,FTR_u,
                    LTR_u,GRESP_u,CUT_C_u,WAT_C_u,S_MOR_u,L_WID_u,STR_u)
b<-melt(df,id.vars = "date_range")

mycolors = c(brewer.pal(name="Dark2", n = 8), brewer.pal(name="Paired", n = 6))

ggplot(b, aes(x=date_range, y=value, fill=variable)) + 
  geom_bar(stat="identity") + xlab("")+ ylab(expression(sigma))+
  ggtitle(title)+ 
  theme_bw() +  guides(fill=guide_legend(title="Parameter"))+
  theme(panel.grid.major.x = element_blank(),panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),axis.text.x = element_text(angle = 90, hjust = 1)
        ,axis.text=element_text(size=12),
        plot.title = element_text( size=14,hjust = 0.5),
        axis.title=element_text(size=16,face="bold"),
        legend.text=element_text(size=6),
        legend.title = element_text(size=7)) +
  scale_fill_manual(values = mycolors)

outname=paste(pfx1,"_Sigma.png",sep="")
ggsave(outname, width = 5, height = 5, units = "in",dpi=300)
