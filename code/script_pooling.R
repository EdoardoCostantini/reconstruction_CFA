### Title:    Pooling results
### Project:  Reconstruction CFA
### Author:   Edoardo Costantini
### Created:  2021-06-29
### Modified: 2021-06-29

  ## Make sure we have a clean environment:
  rm(list = ls())

  ## Support Functions
  source("./helper/functions.R")

# Load Results ----------------------------------------------------------

  inDir <- "../output/"
  files <- list.files(inDir)
  target_tar <- files[length(files)]
  output <- read.tar.gz(target_tar)

# Restructure Results -----------------------------------------------------
# list of conditions containing results for every repetition

  # Give unique name to all objects
  names(output$out) <- output$file_names

  out <- do.call(rbind, output$out)
  gg_shape <- reshape2::melt(out, id.var = colnames(out)[1:6])

  # Save
  saveRDS(gg_shape,
          file = paste0("../output/",
                        output$name_run,
                        "_res",
                        ".rds")
  )