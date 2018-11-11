# This script is to plot ED outputs that are in csv format

rm(list = ls())
setwd("N:/Data02/bcal/Personal/hamid/ED_opt/tmp_analysis")
source('N:/Data02/bcal/Personal/hamid/ED_BSU-master/PostProcessing/my_plot.R')

df = read.csv("100years.csv", header = TRUE)

my_plot(df,"2016-01-01","2016-12-28","GPP")
