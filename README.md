
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Overview

mrgparamtab helps define and format parameter tables.

An example parameter table script highlighting `mrgparamtab` functions
to create a parameter table can be found
[here](https://github.com/metrumresearchgroup/expo1-nonmem-foce/blob/main/script/pk-final-model-table.R).

An example parameter table script highlighting `mrgparamtab` functions
to create a bootstrap parameter table can be found
[here](https://github.com/metrumresearchgroup/expo1-nonmem-foce/blob/main/script/pk-final-model-table-boot.R).

## Documentation

Public documentation of all functions is hosted at
<https://metrumresearchgroup.github.io/mrgparamtab/>

## Development

`mrgparamtab` uses [pkgr](https://github.com/metrumresearchgroup/pkgr)
to manage development dependencies and
[renv](https://rstudio.github.io/renv/) to provide isolation. To
replicate this environment,

1.  clone the repo

2.  install pkgr

3.  open package in an R session and run `renv::init(bare = TRUE)`

    -   install `renv` \> 0.8.3-4 into default `.libPaths()` if not
        already installed

4.  run `pkgr install` in terminal within package directory

5.  restart session

Then, launch R with the repo as the working directory (open the project
in RStudio). renv will activate and find the project library.

## Getting help

If you encounter a clear bug, please file an issue with a minimal
reproducible example on
[mrgparamtab](https://github.com/mrgparamtab/issues).
