#!/bin/bash

#$ -wd /data/bbr-nonmem-poppk-bayes/model/pk/1001/init

/opt/NONMEM/nm75/run/nmfe75 init.ctl  init.lst  -parafile=init.pnm -maxlim=2
