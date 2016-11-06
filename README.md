# Solving the problem of incomplete data in medical diagnosis via interval modeling

## Introduction

This project describes files which compose the research implementation on supporting medical diagnosis under incomplete data. The approach includes interval modeling of incomplete data, uncertaintification of classical models and aggregation of incomplete results. The evaluation of the approach uses medical data for ovarian tumor diagnosis, where the problem of missing data is commonly encountered.

### Citation

If you use the code or results of this project in your research, please cite the following article:

```
 Wójtowicz, A., Żywica, P., Stachowiak, A., & Dyczkowski, K. (2016).
 Solving the problem of incomplete data in medical diagnosis via interval modeling.
 Applied Soft Computing, 47, 424-437.
```
[![DOI](https://img.shields.io/badge/DOI-10.1016%2Fj.asoc.2016.05.029-blue.svg)](http://dx.doi.org/10.1016/j.asoc.2016.05.029)

### Technical details

All scripts are written in [R 3.1.2](http://cran.r-project.org/). The [RStudio](http://www.rstudio.com/) project is supervised by [packrat](https://rstudio.github.io/packrat) software to maintain compatibility of R packages. Documents are generated with use of [knitr](http://yihui.name/knitr).

### Experiment at a glance

The research consists of 3 steps:

 1. [making analytic datasets](https://ovaexpert.github.io/ovarian-tumor-aggregation/make-datasets.html),
 1. [training and evaluation](https://ovaexpert.github.io/ovarian-tumor-aggregation/training-and-evaluation.html),
 1. [visualizing results](https://ovaexpert.github.io/ovarian-tumor-aggregation/results-overview.html).

```
--< datasets/db-2015-04-30.csv
|
|              STEP 1                                  STEP 2                                STEP 3
|
|  ##############################     #########################################     #########################
|  #  make-datasets.Rmd         #     #  training-and-evaluation.Rmd          #     #  results-overview.Rmd #
|  ##############################     #########################################     #########################
|  #                            #     #                                       #     #                       #
----> make-datasets.R           #  ----> training-and-evaluation.R            #  ----> results-overview.R   #
   #   |                        #  |  #   |                                   #  |  #                       #
   #   -> datasets/training.csv >---  #   -> datasets/evaluation-output.RData >---  #                       #
   #   -> datasets/test.csv     >---  #                                       #     #                       #
   #                            #     #                                       #     #                       #
   ##############################     #########################################     #########################
     |                                  |                                             |
     -> make-datasets.html              -> training-and-evaluation.html               -> results-overview.html
```
## Downloading the results

To view outputs of the experiment, run `download-data.R` script. It will download CSV datasets and binary RData output:

 * `datasets/training.csv`,
 * `datasets/test.csv`,
 * `datasets/evaluation-output.Rdata`.

## Reproducing the research

To prepare the software environment to the experiment, open `ovarian-tumor-aggregation.Rproj` file in RStudio in order to launch packrat and download necessary libraries. The installation process may take from a few to several minutes.

Due to legal restrictions, the initial database `datasets/db-2015-04-30.csv` can not be published. Therefore, **the first step is not reproducible**. The remaining steps can be reproduced in two ways (A or B, see sections below). To reflect whole experiment, non-reproducible steps also will be mentioned.

### A. Creating datasets and final results

To create only datasets and results, which can be further investigated, execute the following scripts:

 1. `make-datasets.R` (not reproducible),
 1. `training-and-evaluation.R`.

Created CSV and RData files are mentioned in the section [Downloading the results](#downloading-the-results).

**Caution**: running `training-and-evaluation.R` is very time-consuming and extensively absorbs computational resources; it is recommended to run it in environment with 32 x 2.0 GHz cores and at least 200 GB RAM in such setting; the calculation process should take approximately 18 hours.

### B. Creating datasets, final results and documents

To create the datasets, the results and additionaly generate the documentation (which explain the implementation of the experiment and the results) launch in `knitr` following `.Rmd` files:

 1. `make-datasets.Rmd` (not reproducible),
 1. `training-and-evaluation.Rmd`,
 1. `results-overview.Rmd`.

Created CSV and RData files are mentioned in the section [Downloading the results](#downloading-the-results). Created HTML files are linked in the section [Experiment at a glance](#experiment-at-a-glance). 
