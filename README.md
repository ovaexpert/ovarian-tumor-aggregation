# ReadMe

This document describes files which compose the research implementation 
on supporting medical diagnosis under incomplete data.

All scripts are written in [R 3.1.2](http://cran.r-project.org). 
The [RStudio](http://www.rstudio.com) project is supervised by
[packrat](https://rstudio.github.io/packrat) software to maintain compatibility 
of R packages. PDF documents are generated with use 
of [knitr](http://yihui.name/knitr)

The research consists of 3 steps:

 0. making analytic datasets,
 0. evaluation process,
 0. visualizing results.

## Downloading the results

To view outputs of the experiment, run `download-data.R` script. It will 
download CSV datasets and binary Rdata evaluation output:
  * `datasets/training.csv`,
  * `datasets/test.csv`,
  * `datasets/evaluation-output.Rdata`.

The process of the research and the results are described in generated documents,
which are available on 
[Github the project site](http://ovaexpert.github.io/ovarian-tumor-aggregation).

## Reproducing the research

Due to legal restrictions, the initial database cannot be published. 
Therefore, **the first step *making analytic datasets* is not reproducible**.
 
To prepare the software environment to the experiment, open 
`ovarian-tumor-aggregation.Rproj` file in RStudio in order to launch packrat and 
download necessary libraries.

### Creating datasets and final results

To create only datasets and results, which can be further investigated,
execute the following scripts:

 * `make-datasets.R` - it generates `datasets/training.csv` and
 `datasets/test.csv` files (not reproducible),
 * `evaluation.R` - it generates `datasets/evaluation-output.RData` file.

**Caution**: running `evaluation.R` is very time-consuming and extensively 
absorbs computational resources; it is recommended to run it in environment with
32 x 2.0 GHz cores and at least 200 GB RAM in such setting; the calculation process should take
approximately 18 hours.
 
### Creating datasets, final results and documents

To additionaly generate documents, which explain the implementation 
of the experiment and the results, instead of running above-mentioned two R script, 
launch in `knitr`:

   * `make-datasets.Rmd` (not reproducible),
   * `evaluation.Rmd`,
   * `results-overview.Rmd`.
