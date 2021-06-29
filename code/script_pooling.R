### Title:    Pooling results
### Project:  Ordinality
### Author:   Edoardo Costantini
### Created:  2021-06-10
### Modified: 2021-06-22

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

  # Define the Unique repetitions
  reps <- output$sInfo$parms$dt_rep # you need to input this manually right now
  cond_tag <- output$sInfo$conds$tag

  # Create an index based on the repetition membership
  index <- lapply(cond_tag,
                  function(x) {
                    grep(x, names(output$out), value = TRUE)
                  }
  )
  names(index) <- cond_tag

  # Re-aggregate Results
  out_list <- lapply(index, function(x) {
    temp_out <- output$out[x]
    names(temp_out) <- gsub("_cond.*",
                            "",
                             names(temp_out))
    return(temp_out)
    }
  )

  # Append the parms object for this run
  out <- list()
  out$results <- out_list
  out$parms <- output$sInfo$parms
  out$conds <- output$sInfo$conds
  out$session_info <- output$sInfo$session_info

  # Shape results for ggplot
  store <- as.data.frame(vector("list", 12))
  for (i in 1:nrow(out$conds)){
    cond_length <- length(out$results[[i]])
    print(out$conds[i, ])
    print(paste0("Condition legnth: ", cond_length))
    # i <- 1
    for(r in 1:cond_length){
      # r <- 1
      content <- data.frame(condTag = out$conds$tag[i],
                            K = out$conds$K[i],
                            D = out$conds$D[i],
                            interval = out$conds$interval[i],
                            mses = as.data.frame(
                              t(
                                sqrt(
                                  out$results[[i]][[r]]$mses
                                )
                              )
                            ),
                            r2 = as.data.frame(
                              t(
                                out$results[[i]][[r]]$r2
                              )
                            ),
                            cors = as.data.frame(
                              t(
                                out$results[[i]][[r]]$cors
                              )
                            )
      )
      store <- rbind(store, content)
    }
  }

  # Melt for ggplot
  gg_shape <- reshape2::melt(store, id.var = c("condTag", "K", "D", "interval"))
  out$gg_shape <- gg_shape

  # Save
  saveRDS(out,
          file = paste0("../output/",
                        output$name_run,
                        "_res",
                        ".rds")
  )