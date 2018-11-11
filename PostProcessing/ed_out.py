# This script extract the Ed outputs for the simulation range and save it in a out.dat file
# out.dat file will be used in the instruction file of the PEST

#import sys
#sys.modules[__name__].__dict__.clear()
import h5py as h5
import numpy as np
#import matplotlib.pyplot as plt
from datetime import date, datetime, timedelta
import pandas as pd
import csv

#f = h5.File("NEON-DS-Imaging-Spectrometer-Data.h5", "r")
#datasetNames = [n for n in f.keys()]
#NPLANT = sum(f['NPLANT'])
#NPP=np.mean(f['DMEAN_NPP_CO'])*NPLANT

# following steps is just to make sure the h5 file reading is in the correct order of time
def perdelta(start, end, delta):
    curr = start
    while curr < end:
        yield curr
        curr += delta
pfx1="/home/hdashti/scratch/ED_BSU/old_ed2/26jan17/ED/150_years_v2/"
pfx2="hhh-D-"
pfx3="-000000-g01.h5"

##############################################################################################
# This line needs to be modified (input correct date for the ED simulation range (year,month,day)
# The end date should be a day after the actual end date of ED simulation 
                                                                                             
dates=[result for result in perdelta(date(2000, 1, 1), date(2016,11 , 29), timedelta(days=1))]

##############################################################################################
names=[]
for x in dates:
    tmp1=pfx1+pfx2+str(x)+pfx3
    names.append(tmp1)
#print(test)

gpp_tmp=[]
GPP=[]
npp_tmp=[]
NPP=[]
fsc_tmp=[]       #Fast soil carbon[kg/m2]
FSC=[]
ssc_tmp=[]
SSC=[]           # Slow soil carbon [kg/m2]
stc_tmp=[]
STC=[]          # Structural soil carbon [kg/m2]
atc_tmp=[]
ATC=[]          # Atmospheric CO2 [ppm]
leaf_drop_tmp=[]
LEAF_DROP=[]


for f in names:
    name_tmp = h5.File(f,"r")
    gpp_tmp = name_tmp['DMEAN_GPP_PY'][0]
    GPP.append(gpp_tmp)
    npp_tmp = name_tmp['DMEAN_NPP_PY'][0]
    NPP.append(npp_tmp)
    fsc_tmp = name_tmp['FAST_SOIL_C_PY'][0]
    FSC.append(fsc_tmp)
    ssc_tmp = name_tmp['SLOW_SOIL_C_PY'][0]
    SSC.append(ssc_tmp)
    stc_tmp = name_tmp['STRUCT_SOIL_C_PY'][0]
    STC.append(stc_tmp)
    atc_tmp=names_tmp['DMEAN_ATM_CO2_PY'][0]
    ATC.append(atc_tmp)
    



df = pd.DataFrame({"dates":dates,"GPP":GPP,"NPP":NPP,"FSC":FSC,"SSC":SSC,"STC":STC,"ATC[ppm]":ATC})
df = df[['dates','GPP','NPP','FSC','SSC','STC','ATC[ppm]']]
df.to_csv("/home/hdashti/BCAL/Data02/bcal/Personal/hamid/ED_opt/tmp_analysis/150_years_v2_2000.csv", index=False)






