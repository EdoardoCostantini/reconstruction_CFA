### Title:    Defining Fixed Parameters
### Project:  Ordinality
### Author:   Edoardo Costantini
### Created:  2021-06-10
### Modified: 2021-06-24

# Packages ----------------------------------------------------------------

  pack_list <- c("parallel",
                 "MASS",
                 "lavaan",
                 "rlecuyer",
                 "parallel",
                 "MLmetrics",
                 "PCAmixdata",
                 "psych",
                 "stringr",
                 "FactoMineR")

  lapply(pack_list, library, character.only = TRUE, verbose = FALSE)

# Load Functions ----------------------------------------------------------

  # Simulation
  source("./fun_runCell.R")
  source("./fun_doRep.R")

  # Support Functions
  source("./helper/functions.R")

# Fixed Parameters --------------------------------------------------------

  # Empty List
  parms    <- list()

  # Generic
  parms$dt_rep <- 1e3

  # Seed related
  parms$seed <- 2021
  parms$nStreams <- 1000

  # Data generation
  parms$N <- 1e3 # sample size
  parms$P <- 50 # number of variables
  parms$item_mean <- 0 # true item mean
  parms$item_var  <- 1 # true item variance
  parms$item_cor  <- .8 # true item variance

  # Analysis
  parms$npcs <- 1

# Experimental Conditions -------------------------------------------------

  # Parallel Experiments: for the continuous and attenuated relationship
  # Alternative experimental factor
  n_cate <- c(10, 7, 5, 3, 2)
  p_cate <- round(seq(.25, 1, length.out = 4), 2)
  interval <- c(TRUE, FALSE)

  # Make Conditionsa
  conds <- expand.grid(N  = parms$N, # sample size
                       P  = parms$P, # number of total variables
                       K = n_cate, # number of categories
                       D = p_cate, # ordinality degree
                       interval = interval,
                       stringsAsFactors = FALSE)

  # Print
  round(conds, 2)

  # Append Condition Tag
  conds$tag <- sapply(1:nrow(conds), function(i) {
    conds$D <- conds$D*100 # to improve tag name
    paste0(colnames(conds), conds[i, ], collapse = "_")
  }
  )

