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

# Data Generation ---------------------------------------------------------

  # Generate Continuous Data
  dat_list <- genData(parms = parms, cond = cond,
                      fl_ta = parms$fl)
  dat_cont <- dat_list$dat_ob

  # Discretise
  n_var_cate <- ceiling((parms$P-1) * cond$D) # number of categorical variables
  keep_continuous <- 1:(ncol(dat_cont)-n_var_cate)
  dat_disc <- apply(dat_cont[, -keep_continuous],
                    2,
                    dis_data,
                    K = cond$K,
                    interval = cond$interval)
  dat_disc <- cbind(dat_cont[, keep_continuous, drop = FALSE], dat_disc)


  # Generate Continuous Data w/ attenuated relationships
  # Define CFA model text object
  ids_items <- split(x = colnames(dat_list$dat_ob),
                     f = rep(1:(ncol(dat_list$dat_ob)/parms$J),
                             each = parms$J))
  ids_lv <- colnames(dat_list$dat_lv)
  names(ids_items) <- ids_lv

  lv_models <- sapply(1:length(ids_items), function(it){
    paste0(ids_lv[it],
           " =~ ",
           paste0(ids_items[[it]], collapse = " + ")
    )

  })
  CFA_model <- paste(lv_models, collapse = "\n")

  # Fit CFA model on Categorical (scaled) data
  CFA_fit_d <- cfa(CFA_model,
                   data = scale(dat_disc),
                   std.lv = TRUE)
  CFA_par_d <- parameterEstimates(CFA_fit_d,
                                  se = FALSE, zstat = FALSE,
                                  pvalue = FALSE, ci = FALSE,
                                  standardized = TRUE)
  fl_atte_ta <- mean(CFA_par_d[1:ncol(dat_disc), "est"])

  # Generate counterfactual continous data
  dat_list_atte <- genData(parms = parms, cond = cond,
                           fl_ta = fl_atte_ta)
  dat_atte <- dat_list_atte$dat_ob

  # Cast to data frames
  dat_cont <- as.data.frame(dat_cont)
  dat_disc <- as.data.frame(dat_disc)
  dat_atte <- as.data.frame(dat_atte)

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