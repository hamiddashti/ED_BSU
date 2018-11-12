rm(list=ls())

setwd("N:/Data02/bcal/Personal/hamid/ED_opt/tmp_analysis")
df = read.csv("PFT.csv", header = TRUE)

b <- as.character(df$PFT[10])
b<-gsub("[()]", "", b)
d=as.numeric(strsplit(b,split=", ",fixed=TRUE)[[1]])
