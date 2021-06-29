### Title:    Defining Fixed Parameters
### Project:  Reconstruction CFA
### Author:   Edoardo Costantini
### Created:  2021-06-29
### Modified: 2021-06-29

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
  parms$dt_rep <- 10

  # Seed related
  parms$seed <- 2021
  parms$nStreams <- 1000

  # Data generation
  parms$N <- 1e3 # sample size
  parms$L <- 8 # number of latent variables
  parms$J <- 3 # number of measured items for latent variable
  parms$P <- parms$L*parms$J # number of observed items
  parms$fl <- .8 # factor loadings level
  parms$lv_mean   <- 0 # true latent mean
  parms$lv_var    <- 1 # true latent variance
  parms$lv_cov_ta <- .8 # true latent cov for target variables
  parms$lv_cov_mp <- .8 # for mar predictors
  parms$lv_cov_ax <- .8 # for auxiliary set
  parms$item_mean <- 0 # true item mean
  parms$item_var  <- 1 # true item variance
  parms$item_cor  <- .8 # true item variance
  parms$varMap <- list(ta = 1:2,  # TArget of analysis
                       mp = 3:5, # Mar Predictors
                       ax = 6:parms$L # Auxiliary variables
  )
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

