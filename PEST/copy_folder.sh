#!/bin/bash

FOLDERS=(`seq 1 112`)
for i in `seq 0 111`; do
      cp -r master wrk${FOLDERS[$i]}
done

