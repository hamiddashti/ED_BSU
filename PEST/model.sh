#!/bin/bash

# First read the in.dat and write it to the config.xml



# Read the in.dat file and change the config_pest.xml
python ./prerun.py

# run the ed model using new config
let start_time=$(date "+%-s")
./ed_2.1-opt-master-0331853 -f ed2in_200_hist

# Retrieve the ed output and save them in the out.dat file
python ./postrun.py

# get the time just after job runs
let end_time=$(date "+%-s")
# get run time for the job (arithmetic evaluation)
run_time=$(( end_time - start_time ))
# create output string
timing_output="Elapsed time in seconds:  $run_time"
# create timing file, append runtime string
echo $timing_output >> ./runtime.log

