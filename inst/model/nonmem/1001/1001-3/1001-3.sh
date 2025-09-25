#!/bin/bash

#$ -wd /data/bbr-nonmem-poppk-bayes/model/pk/1001/1001-3

/opt/NONMEM/nm75/run/nmfe75 1001-3.ctl  1001-3.lst  -parafile=1001-3.pnm -maxlim=2
