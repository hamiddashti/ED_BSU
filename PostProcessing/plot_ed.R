# This script is to plot ED outputs that are in csv format

rm(list = ls())
setwd("N:/Data02/bcal/Personal/hamid/ED_opt/tmp_analysis")
source('N:/Data02/bcal/Personal/hamid/ED_BSU/PostProcessing/Plot_daily.R')
source('N:/Data02/bcal/Personal/hamid/ED_BSU/PostProcessing/Plot_monthly.R')

############################################################
# List of daily variables : GPP, NPP, FSC, SSC, STC
# List of monthly variables: NPLANT, LAI, AGB

df_daily = read.csv("ws_karun.csv", header = TRUE)
Plot_daily(df_daily,"2000-10-01","2017-09-27","GPP")  # This is for daily plots

df_monthly = read.csv("ws_30_monthly.csv",header = TRUE)

plot_monthly('1988-10','2017-08',"ws_30_monthly.csv","AGB")  # this is for monthly plot
plot(df_monthly$GPP,ty="l")


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


