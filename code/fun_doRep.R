### Title:    Subroutine doRep
### Project:  Ordinality
### Author:   Edoardo Costantini
### Created:  2021-06-10
### Modified: 2021-06-15
### Note:     doRep performs a single run of fun_runCell for every condition

## Run one replication of the simulation:
doRep <- function(rp, conds, parms, settings) {
  ## For internals
  # rp = 1

  ## Set seed
  .lec.SetPackageSeed(rep(parms$seed, 6))
  if(!rp %in% .lec.GetStreams()) # if the streams do not exist yet
    .lec.CreateStream(c(1 : parms$nStreams)) # then
  .lec.CurrentStream(rp) # this is equivalent to setting the seed Rle
 
  ## Progress report - Start
  # parms$rep_counter <- parms$rep_counter + 1 # increase progres report counter
  cat(paste0(Sys.time(), " - Starts Repetition: ", rp, 
             "\n"),
      file = paste0(settings$outDir, settings$fileName_progress, ".txt"),
      append = TRUE)

# Cycle through conditions ------------------------------------------------

  for(i in 1 : nrow(conds)) {
    tryCatch(
    {
      # Try running simulation for condition i, repetition rp
      runCell(cond = conds[i, ],
              parms = parms,
              settings = settings,
              rp = rp)
    },
      error = function(report) {
        err <- paste0("Original Error: ", report)
        return(err)
      }
    )
  }
}
