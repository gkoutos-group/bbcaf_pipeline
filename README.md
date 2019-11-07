# bbcaf_pipeline
## Needed and recommended packages

Use the installed script "install_packages.R". The version of R used is 3.4.1.

## How to use

- Import the settings and the pipe script
- Set up
- Run the pipe

The function that loads a set must return a list with an element data. Example:

"""
dataset <- function() {
    return(list(data=my_df))
}
"""

The output variable should be "ResultVariable"

Follow the example in test/sample.R or test/minimal_sample.R

## Output and generated files

Some files are generated depending (or not) on the settings. They will be generated to the folder output/env, output/models, output/run_datasets and output/img

## Folders
 
### util

Needed scripts by the main pipe.R

### datasets

Scripts to load datasets

### output

Output generated files

### test

Sample scripts

### tools

Auxiliary scripts
 
## Issues

If there is a variable called Bio_Panel it will expect to split the dataset into train ("CVD1" and test "CVD2")