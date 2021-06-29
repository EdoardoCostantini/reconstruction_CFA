### Title:    Subroutine runCell
### Project:  Reconstruction CFA
### Author:   Edoardo Costantini
### Created:  2021-06-29
### Modified: 2021-06-29
### Note:     A "cell" is a cycle through the set of conditions.
###           The function in this script generates 1 data set, performs 
###           imputations for every condition in the set.

runCell <- function(cond,
                    parms,
                    settings,
                    rp) {

# Example Internals -------------------------------------------------------
  
  # set.seed(1234)
  # cond    = conds[3, ]
  # rp = 1

# Data Generation ---------------------------------------------------------
  


# Analysis ----------------------------------------------------------------



# Store Output ------------------------------------------------------------

  ## Define storing object
  output <- list(cond  = cond,
                 coefs = coefs,
                 cors = cors,
                 r2 = r2,
                 mses = mses)

  ## Return it
  saveRDS(output,
          file = paste0(settings$outDir,
                        "rep", rp,
                        "_cond", cond$tag,
                        ".rds")
  )
}