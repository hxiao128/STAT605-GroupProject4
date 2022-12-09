#!/bin/bash
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/scratch/jxia53/lib
# untar your R installation
tar -xzf R405.tar.gz
tar -xzf packages.tar.gz

# make sure the script will use your R installation, 
# and the working directory as its home location
export PATH=$PWD/R/bin:$PATH
export RHOME=$PWD/R
export R_LIBS=$PWD/packages

# run your script
Rscript project.R $1 
