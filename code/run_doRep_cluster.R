### Title:   Run replication on Lisa
### Author:  Edoardo Costantini
### Created: 2021-06-10
### Notes:   Assumes ./code/cluster as wd (because that's how it's run on lisa)

## Make sure we have a clean environment:
rm(list = ls(all = TRUE))

## Initialize the environment:
source("./init.R")

## Extract command line arguments
# args <- commandArgs(trailingOnly = TRUE)
# rp   <- as.numeric(args[1]) # replication rp = 1 to desired
# settings$outDir <- args[2]   # overwrite output directory w/ user supplied

## Run one replication of the simulation:
doRep(rp = rp, conds = conds, parms = parms)
