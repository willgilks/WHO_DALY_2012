#!/bin/bash
#$ -N log_whoMortGraphs


## DESCRIPTION

## Starts an R script for plotting WHO 2015 mortality data


    echo "`date +%y/%m/%d_%H:%M:%S`"
    uname -sa
    set -ex
    ls -lah

            R --vanilla < who_mortality.R
            
            rm Rplots.pdf

## http://www.sussex.ac.uk/lifesci/morrowlab/
## wpgilks@gmail.com


    echo "`date +%y/%m/%d_%H:%M:%S`"
    exit

##
##
##