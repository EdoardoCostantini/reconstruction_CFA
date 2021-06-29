### Title:    helper functions
### Project:  Reconstruction CFA
### Author:   Edoardo Costantini
### Created:  2021-06-29
### Modified: 2021-06-29

# Discretize --------------------------------------------------------------

genData <- function(parms, cond, fl_ta){

# Example Input -----------------------------------------------------------

  # cond    <-  conds[3, ]
  # fl_ta <- parms$fl

# Latent Variables Covariance matrix --------------------------------------

  Phi <- diag(parms$L)

  # Target Variables
  Phi[parms$varMap$ta, ] <- parms$lv_cov_ta

  # MAR Predictors
  Phi[parms$varMap$mp, ] <- parms$lv_cov_mp

  # Other Predictors
  Phi[parms$varMap$ax, ] <- parms$lv_cov_ax

  # Fix diagonal
  diag(Phi) <- 1

  # Make symmetric
  Phi[upper.tri(Phi)] <- t(Phi)[upper.tri(Phi)]

  # Make it covariance instead of correlation matrix (if lv_var != 1)
  Phi <- Phi * sqrt(parms$lv_var) * sqrt(parms$lv_var)

# Factor loadings (random factor) -----------------------------------------

  lambda <- fl_ta + runif(parms$P, min = -.02, max = .02)

# Observed Items Error Covariance matrix ----------------------------------
# Note: you are creating uncorrelated errors for the observed items

  Theta <- diag(parms$P)
  for (i in 1:length(lambda)) {
    Theta[i, i] <- parms$item_var - lambda[i]^2 * Phi[1, 1]
  }

# Items Factor Complexity = 1 (simple measurement structure) --------------
# Reference: Bollen1989 p234

  Lambda <- matrix(nrow = parms$P, ncol = parms$L)
  start <- 1
  for (j in 1:parms$L) {
    end <- (start + parms$J) - 1
    vec <- rep(0, parms$P)
    vec[start:end] <- lambda[start:end]
    Lambda[, j] <- vec
    start <- end + 1
  }

# Sample Scores -----------------------------------------------------------

  scs_lv    <- mvrnorm(parms$N, rep(parms$lv_mean, parms$L), Phi)
  scs_delta <- mvrnorm(parms$N, rep(parms$item_mean, parms$P), Theta)

# Compute Observed Scores -------------------------------------------------

  x <- matrix(nrow = parms$N, ncol = parms$P)
  for(i in 1:parms$N){
    x[i, ] <- t(parms$item_mean + Lambda %*% scs_lv[i, ] + scs_delta[i, ])
  }

# Give meaningful names ---------------------------------------------------

  colnames(x) <- paste0("z", 1:ncol(x))
  colnames(scs_lv) <- paste0("lv", 1:ncol(scs_lv))

# Return Output -----------------------------------------------------------

  return(
    list(dat_ob = x,
         dat_lv = scs_lv,
         Phi    = Phi,
         Theta  = Theta,
         Lambda = Lambda)
  )
}


# Discretize --------------------------------------------------------------

dis_data <- function(x, K, interval = TRUE){
  # Given a continuous varible x, and a number of categories K,
  # this function return a discretized version (either ordinal or
  # interval scale)

  if (interval == TRUE){
    x_dis <- as.numeric(cut(x, K))
  } else {
    prob_reduction <- .6 # every subsequent bin contains .6 of the remaining obs
    prob_in <- .2 # first bin probability
    probs <- rep(NA, K)

    for(k in 1:K){
      if(k < K){
        probs[k] <- prob_in
        whats_left <- (1-sum(probs, na.rm = TRUE))
        prob_in <- whats_left*prob_reduction
      } else {
        probs[k] <- whats_left
      }
    }
    x_sort <- sort(x)
    x_mem <- sort(sample(1:K, length(x), probs, replace = TRUE))
    map <- data.frame(value = as.character(x_sort),
                      bin = x_mem)
    target <- as.character(x)
    x_dis <- map[match(target, map$value), "bin"]
  }
  return(x_dis)
}

# Write tar.gz ------------------------------------------------------------

write.tar.gz <- function(folder_name){
  # Description:
  # Given the name of a folder name in the "output" project folder
  # it zips it and deletes the original folder

  # Move to Output folder
  setwd("../output/")

  # Zip folder
  system(command = paste0("tar cvzf ", folder_name, ".tar.gz",
                          " ./", folder_name, "/"))

  # Delete Folder
  system(command = paste0("rm -rf ", folder_name))

  # Revert WD to code folder
  setwd("../code/")
}

# Read tar.gz ------------------------------------------------------------

read.tar.gz <- function(tar_name){
  # Description:
  # Given the name of a tar.gz folder in the "output" project folder
  # it unzips it, reads the content, and deletes the unziepped folder

  # Move to Output folder
  setwd("../output/")

  # Unzip folder
  untar_command <- paste0("tar -xvf ", tar_name)
  system(untar_command)

  # Unzipped folder name
  name_run <- str_replace(tar_name, ".tar.gz", "")

  # Session info
  sInfo <- readRDS(paste0(name_run, "/sInfo.rds"))

  # Read .rds
  rds_names <- grep(".rds",
                  list.files(name_run),
                  value = TRUE)
  rds_keep <- rds_names[!grepl("sInfo", rds_names)]
  out <- lapply(paste0(name_run, "/", rds_keep), readRDS)

  # Delete Folder
  system(command = paste0("rm -rf ", name_run))

  # Revert WD to code folder
  setwd("../code/")

  # Return outputs
  return(list(out = out,
              name_run = name_run,
              file_names = rds_keep,
              sInfo = sInfo))
}