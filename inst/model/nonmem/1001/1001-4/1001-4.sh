#!/bin/bash

#$ -wd /data/bbr-nonmem-poppk-bayes/model/pk/1001/1001-4

/opt/NONMEM/nm75/run/nmfe75 1001-4.ctl  1001-4.lst  -parafile=1001-4.pnm -maxlim=2
