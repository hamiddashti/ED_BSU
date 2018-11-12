rm(list=ls())

setwd("C:/ED")
df = read.csv("PFT.csv", header = TRUE)

b <- as.character(df$PFT[10])
b<-gsub("[()]", "", b)
d=as.numeric(strsplit(b,split=", ",fixed=TRUE)[[1]])
