## test

##############################################################################

# This script extract the Ed monthly outputs for the simulation range and save it in a csv filei

    # arg1: "Year-Month-Day"
    # arg2: "Year-Month-Day"
    # arg3: "Output file name" 

    #Example: python ed_out_monthly.py "2000-01-01" "2016-12-30" "output"

##############################################################################

import sys
def ed_out_monthly(date1,date2,fname):
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
    indir=os.getcwd()
    pfx2="/hhh-E-"
    pfx3="-00-000000-g01.h5"
    ##############################################################################################
    # This line needs to be modified (input correct date for the ED simulation range (year,month,day)
    # The end date should be a day after the actual end date of ED simulation 
    dates_tmp = pd.date_range(start=date1, end=date2, freq='MS')
    dates=dates_tmp.strftime('%Y-%m')
   
    
    ##############################################################################################
    names=[]
    for x in dates:
        tmp1=indir+pfx2+str(x)+pfx3
        names.append(tmp1)
    
    
   # gpp_tmp=[]
   # GPP=[]
   # npp_tmp=[]
   # NPP=[]
   # fsc_tmp=[]       #Fast soil carbon[kg/m2]
   # FSC=[]
   # ssc_tmp=[]
   # SSC=[]           # Slow soil carbon [kg/m2]
   # stc_tmp=[]
   # STC=[]          # Structural soil carbon [kg/m2]
   # atc_tmp=[]
   # ATC=[]          # Atmospheric CO2 [ppm]
   # leaf_drop_tmp=[]
   # LEAF_DROP=[]
    pft_tmp=[]
    PFT=[]
    nplant_tmp=[]
    NPLANT=[]
    agb_tmp=[]
    AGB=[]
    lai_tmp=[]
    LAI=[]
    for f in names:
        name_tmp = h5.File(f,"r")
        pft_tmp = name_tmp['PFT']
        pft_tmp =tuple(pft_tmp.value)
        PFT.append(pft_tmp)
        nplant_tmp = name_tmp['NPLANT']
        nplant_tmp = tuple(nplant_tmp.value)
        NPLANT.append(nplant_tmp)
        lai_tmp = name_tmp['LAI_CO']
        lai_tmp = tuple(lai_tmp.value)
        LAI.append(lai_tmp)
        agb_tmp = name_tmp['AGB_CO']
        agb_tmp = tuple(agb_tmp.value)
        AGB.append(agb_tmp)


    df = pd.DataFrame({"dates":dates,"PFT":PFT,"NPLANT":NPLANT,"LAI":LAI,"AGB":AGB})
    df = df[['dates','PFT','NPLANT','LAI','AGB']]
    outdir="/home/hdashti/BCAL/Data02/bcal/Personal/hamid/ED_opt/tmp_analysis/"
    fout = outdir+fname+".csv"
    df.to_csv(fout, index=False)

   #     gpp_tmp = name_tmp['DMEAN_GPP_PY'][0]
   #     GPP.append(gpp_tmp)
   #     npp_tmp = name_tmp['DMEAN_NPP_PY'][0]
   #     NPP.append(npp_tmp)
   #     fsc_tmp = name_tmp['FAST_SOIL_C_PY'][0]
   #     FSC.append(fsc_tmp)
   #     ssc_tmp = name_tmp['SLOW_SOIL_C_PY'][0]
   #     SSC.append(ssc_tmp)
   #     stc_tmp = name_tmp['STRUCT_SOIL_C_PY'][0]
   #     STC.append(stc_tmp)
   #     atc_tmp=name_tmp['DMEAN_ATM_CO2_PY'][0]
   #     ATC.append(atc_tmp)
        
    
    
    
   # df = pd.DataFrame({"dates":dates,"GPP":GPP,"NPP":NPP,"FSC":FSC,"SSC":SSC,"STC":STC,"ATC[ppm]":ATC})
   # df = df[['dates','GPP','NPP','FSC','SSC','STC','ATC[ppm]']]
    
   # outdir="/home/hdashti/BCAL/Data02/bcal/Personal/hamid/ED_opt/tmp_analysis/"
   # fout = outdir+fname+".csv"
   # df.to    (fout, index=False)
    
   # print("WARNING! WARNING! WARNING!\n")
   # print("CURRENT DIRECTORY:{}\n".format(indir))
   # print("OUTPUT DIRECTORY:{}\n".format(outdir))
   # print("If in or out dir is wrong change indir or outdir in the code\n")    
if __name__ == '__main__':
    # Map command line arguments to function arguments.
    ed_out_monthly(*sys.argv[1:])

