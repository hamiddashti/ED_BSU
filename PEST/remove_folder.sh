#!/bin/bash

FOLDERS=(`seq 1 112`)
for i in `seq 0 111`; do
    rm -r wrk${FOLDERS[$i]}
done
