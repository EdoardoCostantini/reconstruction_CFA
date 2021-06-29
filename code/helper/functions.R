### Title:    helper functions
### Project:  Imputing High Dimensional Data
### Author:   Edoardo Costantini
### Created:  2020-05-19
### Modified: 2021-06-26

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

# Average correlation -----------------------------------------------------

extract_avg_cor <- function(dt = matrix()){
  # Given a dataset in matrix, it returns the average correlation
  # dt = MASS::mvrnorm(1e2, rep(0, 3), diag(3))
  # Body
  cmat <- cor(dt)
  imat <- lower.tri(cmat) # index matrix
  mean_cor <- mean(cmat[imat])

  return(mean_cor)
}

# LM coefficients and CIs -------------------------------------------------

extract_lm <- function(dt = matrix()){
  # Given a data set in matrix form, this function fits a lm and extracts
  # regression coefficient estiamtes
  # Example input
  # dt = MASS::mvrnorm(1e2, rep(0, 3), diag(3))

  # Fit lm model
  lm_fit <- lm(dt[, 1] ~ dt[, 2:3])

  # Extract coefficients
  lm_coefs <- coef(lm_fit)

  # Output
  return(lm_coefs)
}

# Extract PCS -------------------------------------------------------------

extract_pcs <- function(dt = matrix(), npcs = 1){
  # Given a data set A in matrix for, it extracts the first npcs principal
  # components from A (excluding the first column), and retunrs a dataset
  # with the first column of A cobined with the extracted components.
  # It also retunrs the info regarding the proportion of explained variance
  # by the defined number of components
  # Example input
  # dt = MASS::mvrnorm(1e2, rep(0, 3), diag(3))
  # npcs = 1

  prcomp_out <- prcomp(dt[, -1], scale = TRUE, retx = TRUE)
  prcomp_dat <- prcomp_out$x[, 1:npcs, drop = FALSE]
  pcs_dat <- cbind(z1 = dt[, 1], prcomp_dat)
  r2 <- (cumsum(prcomp_out$sdev^2) / sum(prcomp_out$sdev^2))[npcs]

  return(list(dat = pcs_dat,
              r2 = r2))
}

# Extract MSE -------------------------------------------------------------

extract_mse <- function(dt = matrix(),
                        train = vector("integer"),
                        test = vector("integer")){
  # Given a dependent variable and a list of PCs, this regresses y on
  # the PCs and extracts then it extracts the test MSE
  # Example input
  # dt    = MASS::mvrnorm(1e2, rep(0, 3), diag(3))
  # ind   = sample(1 : nrow(dt))
  # train = ind[1 : (.9*nrow(dt))]
  # test  = ind[(.9*nrow(dt)+1) : nrow(dt)]

  ## Transform into data frame
  dt_df <- as.data.frame(dt)

  ## Check column names have no spaces
  colnames(dt_df) <- sapply(colnames(dt_df), str_replace, " ", "")

  ## Estimate model
  vars <- colnames(dt_df)
  lm_out <- lm(formula = paste0(vars[1],
                                " ~ ",
                                paste0(vars[-1], collapse = " + ")),
               data = dt_df[train, ])

  ## Generate test-set predictions (i.e., y-hats):
  preds <- predict(lm_out, newdata = dt_df[test, ])

  ## Generate test-set MSEs:
  mse <- MSE(y_pred = preds, y_true = dt_df[test, 1])

  # Output
  return(mse)
}

# Average results from sim ------------------------------------------------

average_result <- function(cond,
                           d_place = 3,
                           result = vector("character", 1)){
  collection <- lapply(cond, function(i){
    i[[result]]
  })
  out <- Reduce('+', collection)/length(collection)
  return(round(out, d_place))
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