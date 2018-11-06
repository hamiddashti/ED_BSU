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
pfx1="/home/hdashti/scratch/ED_BSU/old_ed2/26jan17/ED/output2/"
pfx2="hhh-D-"
pfx3="-000000-g01.h5"

##############################################################################################
# This line needs to be modified (input correct date for the ED simulation range (year,month,day)
# The end date should be a day after the actual end date of ED simulation 
                                                                                             
dates=[result for result in perdelta(date(1989, 1, 1), date(2015,12 , 30), timedelta(days=1))]

##############################################################################################
names=[]
for x in dates:
    tmp1=pfx1+pfx2+str(x)+pfx3
    names.append(tmp1)
#print(test)

NPLANT = []
name_tmp = []
nplant_tmp=[]
npp_tmp=[]
NPP=[]
gpp_tmp=[]
GPP=[]
gpp_py_tmp=[]
GPP_PY=[]
rh_tmp=[]
RH = []
root_rh_tmp=[]
Root_Resp=[]
fast_soil_tmp=[]
Fast_Soil=[]
slow_soil_tmp=[]
Slow_Soil=[]

for f in names:
    name_tmp = h5.File(f,"r")
    nplant_tmp = sum(name_tmp['NPLANT'])
    npp_tmp=np.mean(name_tmp['DMEAN_NPP_CO'])*nplant_tmp
    NPP.append(npp_tmp)
    gpp_tmp = np.mean(name_tmp['DMEAN_GPP_CO']*nplant_tmp)
    GPP.append(gpp_tmp)
    NPLANT.append(nplant_tmp)
    gpp_py_tmp = np.mean(name_tmp['DMEAN_GPP_PY'])
    GPP_PY.append(gpp_py_tmp)
    rh_tmp=np.mean(name_tmp['DMEAN_RH_PY'])
    RH.append(rh_tmp)
    root_rh_tmp=np.mean(name_tmp['DMEAN_ROOT_RESP_PY'])
    Root_Resp.append(root_rh_tmp)
    fast_soil_tmp=np.mean(name_tmp['FAST_SOIL_C_PY'])
    Fast_Soil.append(fast_soil_tmp)
    slow_soil_tmp=np.mean(name_tmp['SLOW_SOIL_C_PY'])
    Slow_Soil.append(slow_soil_tmp)




df = pd.DataFrame({"dates":dates, "NPP":NPP,"NPLANT":NPLANT,"GPP":GPP,"GPP_PY":GPP_PY,"RH":RH,"Root_Resp":Root_Resp,"Fast_Soil":Fast_Soil,"Slow_Soil":Slow_Soil})
df = df[['dates','GPP','NPP','NPLANT','GPP_PY','RH','Root_Resp','Fast_Soil','Slow_Soil']]
df.to_csv("/home/hdashti/BCAL/Data02/bcal/Personal/hamid/ED_opt/tmp_analysis/karun_opt6.csv", index=False)






