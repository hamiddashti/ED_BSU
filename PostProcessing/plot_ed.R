# This script is to plot ED outputs that are in csv format

rm(list = ls())
setwd("~/bcal/Data02/bcal/Personal/hamid/ED_opt/tmp_analysis")
source('~/bcal/Data02/bcal/Personal/hamid/ED_BSU/PostProcessing/Plot_daily.R')
source('~/bcal/Data02/bcal/Personal/hamid/ED_BSU/PostProcessing/Plot_monthly.R')

############################################################
# List of daily variables : GPP, NPP, FSC, SSC, STC
# List of monthly variables: NPLANT, LAI, AGB

df = read.csv("daily_test.csv", header = TRUE)
Plot_daily(df,"1818-01-01","1917-12-29","GPP")  # This is for daily plots
plot_monthly('1818-01','1918-11',"monthly_test.csv","NPLANT")  # this is for monthly plot



df2= read.csv("Ploting.xlsx", header = TRUE)

df = read.csv("200_opt0_daily.csv", header = TRUE)
Plot_daily(df,"1918-01-01","2016-11-29","NPP")  # This is for daily plots
ggsave("GPP_200y.png",width = 6.5, height = 4.5)

Plot_daily(df,"1817-01-01","2016-11-29","FSC")  # This is for daily plots
ggsave("FSC_200y.png",width = 6.5, height = 4.5)

Plot_daily(df,"1817-01-01","2016-11-29","SSC")  # This is for daily plots
ggsave("SSC_200y.png",width = 6.5, height = 4.5)

Plot_daily(df,"1817-01-01","2016-11-29","STC")  # This is for daily plots
ggsave("STC_200y.png",width = 6.5, height = 4.5)


plot_monthly('1818-01','2016-11',"200_ws_monthly.csv","LAI")  # this is for monthly plot
ggsave("LAI_200y.png",width = 6.5, height = 4.5)

plot_monthly('1817-01','2016-11',"200_ws_monthly.csv","NPLANT")  # this is for monthly plot
ggsave("NPLANT_200y.png",width = 6.5, height = 4.5)

plot_monthly('1817-01','2016-11',"200_ws_monthly.csv","AGB")  # this is for monthly plot
ggsave("AGB_200y.png",width = 6.5, height = 4.5)





