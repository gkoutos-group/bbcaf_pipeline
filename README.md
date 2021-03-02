# BBCAF ML Pipeline

[![DOI](https://zenodo.org/badge/187649498.svg)](https://zenodo.org/badge/latestdoi/187649498)

Author: Victor Roth Cardoso - V.RothCardoso@bham.ac.uk

E-mail me suggestions, comments and issues.

## Introduction

This is the repository for the ML scripts used in *Winnie Chua, Yanish Purmah, Victor R Cardoso, Georgios V Gkoutos, Samantha P Tull, Georgiana Neculau, Mark R Thomas, Dipak Kotecha, Gregory Y H Lip, Paulus Kirchhof, Larissa Fabritz, Data-driven discovery and validation of circulating blood-based biomarkers associated with prevalent atrial fibrillation, European Heart Journal, Volume 40, Issue 16, 21 April 2019, Pages 1268–1276, https://doi.org/10.1093/eurheartj/ehy815.*

There are many packages that were installed with our setup. You won't need all of them and they may be installed as required. The recommended packages are in  "install_packages.R". The version of R used is 3.4.1.

## Instructions

The script works in 3 main steps:

1. Import the settings and the pipe script
2. Change settings as required
3. Run the pipe

You'll require a function that loads your dataset. This functions must return a list with `data`. This dataset output (dependent) variable should be in a column named "ResultVariable"!

Example:

```R
dataset <- function() {
    return(list(data=my_df))
}
```

Follow the example in test/sample.R or test/minimal_sample.R

## Output and generated files

Some output files might be generated depending on the settings. These files will be located in:
- output/env: saved environment files
- output/models: created models
- output/run_datasets: the datasets which the model was run
- output/img: AUC or other images

## Code organization

The remaining of the tool is organized in the following manner:
- util: scripts needed by `pipe.R`
- datasets: scripts to load datasets (you may save your own here to your branch)
- output: output files (check above)
- test: sample scripts to check functionality
- tools: other auxiliary scripts
 
## Issues

If there is a column called `Bio_Panel` it will expect to split the dataset into train ("CVD1" and test "CVD2")

