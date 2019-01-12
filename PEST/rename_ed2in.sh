#!/bin/bash
FOLDERS=(`seq 1 112`)

for i in `seq 0 111`; do
    sed -i -e "s/master/wrk${FOLDERS[$i]}/g" /home/hdashti/scratch/ED_BSU/old_ed2/26jan17/ED/working_morris_ws/remove/wrk${FOLDERS[$i]}/ed2in_200_hist  
done
