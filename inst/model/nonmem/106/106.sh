#!/bin/bash

#$ -wd /data/Projects/package_dev/pmparams/inst/model/nonmem/106

/opt/NONMEM/nm75/run/nmfe75 106.ctl  106.lst  -maxlim=2
