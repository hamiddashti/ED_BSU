# This script extract the Ed outputs for the simulation range and save it in a out.dat file
# out.dat file will be used in the instruction file of the PEST

#import sys
#sys.modules[__name__].__dict__.clear()
import h5py as h5
import numpy as np
#import matplotlib.pyplot as plt
from datetime import date, datetime, timedelta

import matplotlib as mpl
import pandas as pd
import os
from shutil import copyfile
#f = h5.File("NEON-DS-Imaging-Spectrometer-Data.h5", "r")
#datasetNames = [n for n in f.keys()]
#NPLANT = sum(f['NPLANT'])
#NPP=np.mean(f['DMEAN_NPP_CO'])*NPLANT
dirpath = os.getcwd()
foldername = os.path.basename(dirpath)
# following steps is just to make sure the h5 file reading is in the correct order of time
pfx1="/home/hdashti/scratch/ED_BSU/old_ed2/26jan17/ED/morris_out_ws/200_opt0_"+foldername+"/"
pfx2=os.path.basename(dirpath)
pfx2="hhh-D-"
pfx3="-000000-g01.h5"

##############################################################################################
# This line needs to be modified (input correct date for the ED simulation range (year,month,day)
# The end date should be a day after the actual end date of ED simulation 
                                                                                             
start = datetime(2014, 10, 1)
end = datetime(2015, 9, 30)

dates_tmp=pd.date_range(start, end)
dates = dates_tmp.date
##############################################################################################
names=[]
for x in dates:
    tmp1=pfx1+pfx2+str(x)+pfx3
    names.append(tmp1)


npp_tmp=[]
GPP=[]

for f in names:
    name_tmp = h5.File(f,"r")
    npp_tmp = name_tmp['DMEAN_GPP_PY'][0]
    GPP.append(npp_tmp)

outfolder = "/home/hdashti/scratch/ED_BSU/old_ed2/26jan17/ED/working_morris_ws/"+foldername+"/out.dat"
np.savetxt(outfolder,GPP,fmt=("%1.9f"))

## Make a copy of in.dat and out.dat files
##import datetime
#currentTime = str(datetime.datetime.now())
#currentTime = currentTime.replace(':', '_')


#infile ="/home/hdashti/scratch/ED_BSU/old_ed2/26jan17/ED/working/master/in.dat"
#newin = "/home/hdashti/scratch/ED_BSU/old_ed2/26jan17/ED/working/master/inmaster"+currentTime

#outfile = "/home/hdashti/scratch/ED_BSU/old_ed2/26jan17/ED/working/master/out.dat"
#newout = "/home/hdashti/scratch/ED_BSU/old_ed2/26jan17/ED/working/master/outmaster"+currentTime


#copyfile(infile, newin)
#copyfile(outfile,newout)










































