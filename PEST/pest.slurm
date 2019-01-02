#!/bin/bash

#SBATCH -J pestpp-gsa      # job name
#SBATCH -o log_gsa.o%j   # output and error file name (%j expands to jobID)
#SBATCH -n 1               # total number of tasks requested
#SBATCH --cpus-per-task=18 # cpus to allocate per task
#SBATCH -p defq            # queue (partition) -- defq, eduq, gpuq.
#SBATCH -t 12:00:00        # run time (hh:mm:ss) - 12.0 hours in this example.

module purge
module load  gcc/6.4.0 openblas pest++/gcc/serial/20181024

cd /home/hdashti/scratch/working/master
pestpp-gsa twofit /h :4004 &
MASTER_PID=$!

cd /home/hdashti/scratch/working
parallel -i bash -c "cd {} ; pestpp-gsa twofit /h 127.0.0.1:4004" -- wrk1 wrk2 wrk3 wrk4 wrk5 wrk6 wrk7 wrk8 wrk9 wrk10 wrk11 wrk12 wrk13 wrk14 wrk15 wrk16 wrk17

kill ${MASTER_PID}
