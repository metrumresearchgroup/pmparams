#!/bin/bash

#$ -wd /data/Projects/package_dev/pmparams/inst/model/nonmem/102

/opt/NONMEM/nm75/run/nmfe75 102.ctl  102.lst  -maxlim=2
