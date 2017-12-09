rm(list = ls())
source('ed_out.R')
setwd("~/bcal/Data02/projects-active/NASA_TE/ED/Outputs/C3grass_phenology_2_h2olim_2")
df<-ed_out(y1=2014,m1=01,d1=01,y2=2018,m2=12,d2=31,pfx = "hhh-D")
save(df,'df.rda')

