
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pmparams <a href='https:/metrumresearchgroup.github.io/pmparams'><img src='man/figures/logo.png' align="right" width="135px"/></a>

<!-- badges: start -->

[![Build
Status](https://github-drone.metrumrg.com/api/badges/metrumresearchgroup/pmparams/status.svg)](https://github-drone.metrumrg.com/metrumresearchgroup/pmparams)
<!-- badges: end -->

## Overview

`pmparams` helps define and format parameter tables. Example code
demonstrating the package as part of an analysis workflow will soon be
available in our [MeRGE Expo 1
repository](https://github.com/metrumresearchgroup/expo1-nonmem-foce/).

## Documentation

Public documentation of all functions is hosted at
<https://metrumresearchgroup.github.io/pmparams/>

## Development

`pmparams` uses [pkgr](https://github.com/metrumresearchgroup/pkgr) to
manage development dependencies and
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
reproducible example on [pmparams](https://github.com/pmparams/issues).
