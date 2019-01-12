

##############################################################################

# This script extract the Ed daily outputs for the simulation range and save it in a csv filei

    # arg1: "Year-Month-Day"
    # arg2: "Year-Month-Day"
    # arg3: "Output file name" 

    # Example: python ed_out_daily.py "2000-01-01" "2016-12-30" "output"

##############################################################################

import sys
def ed_out(date1,date2,fname):
    import h5py as h5
    import numpy as np
    import os
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
    indir=os.getcwd()
    pfx2="/hhh-D-"
    pfx3="-000000-g01.h5"
    y1=int(date1[0:4])
    m1=int(date1[5:7])
    d1=int(date1[8:10])
    y2=int(date2[0:4])
    m2=int(date2[5:7])
    d2=int(date2[8:10])
    ##############################################################################################
    # This line needs to be modified (input correct date for the ED simulation range (year,month,day)
    # The end date should be a day after the actual end date of ED simulation 
                                                                                                 
    dates=[result for result in perdelta(date(y1, m1, d1), date(y2, m2, d2), timedelta(days=1))]
    
    ##############################################################################################
    names=[]
    for x in dates:
        tmp1=indir+pfx2+str(x)+pfx3
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
    #leaf_drop_tmp=[]
    #LEAF_DROP=[]
    
    
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
           
    
    df = pd.DataFrame({"dates":dates,"GPP":GPP,"NPP":NPP,"FSC":FSC,"SSC":SSC,"STC":STC})
    df = df[['dates','GPP','NPP','FSC','SSC','STC']]
    
    outdir="/home/hdashti/BCAL/Data02/bcal/Personal/hamid/ED_opt/tmp_analysis/"
    fout = outdir+fname+".csv"
    df.to_csv(fout, index=False)
    
    print("WARNING! WARNING! WARNING!\n")
    print("CURRENT DIRECTORY:{}\n".format(indir))
    print("OUTPUT DIRECTORY:{}\n".format(outdir))
    print("If in or out dir is wrong change indir or outdir in the code\n")    
if __name__ == '__main__':
    # Map command line arguments to function arguments.
    ed_out(*sys.argv[1:])

