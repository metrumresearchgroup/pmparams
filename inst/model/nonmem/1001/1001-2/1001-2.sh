#!/bin/bash

#$ -wd /data/bbr-nonmem-poppk-bayes/model/pk/1001/1001-2

/opt/NONMEM/nm75/run/nmfe75 1001-2.ctl  1001-2.lst  -parafile=1001-2.pnm -maxlim=2
