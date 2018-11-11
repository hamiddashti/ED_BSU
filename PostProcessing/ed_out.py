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
pfx1="/home/hdashti/scratch/ED_BSU/old_ed2/26jan17/ED/test/"
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

gpp_tmp=[]
GPP=[]


for f in names:
    name_tmp = h5.File(f,"r")
    gpp_tmp = name_tmp['DMEAN_GPP_PY'][0]
    GPP.append(gpp_tmp)
    




df = pd.DataFrame({"dates":dates,"GPP":GPP})
df = df[['dates','GPP']]
df.to_csv("/home/hdashti/BCAL/Data02/bcal/Personal/hamid/ED_opt/tmp_analysis/karun.csv", index=False)






