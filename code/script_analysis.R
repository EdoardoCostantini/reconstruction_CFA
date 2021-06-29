### Title:    Analysing results
### Project:  Ordinality
### Author:   Edoardo Costantini
### Created:  2021-06-10
### Modified: 2021-06-22

  ## Make sure we have a clean environment:
  rm(list = ls())

# Packages ----------------------------------------------------------------

  pack_list <- c("ggplot2",
                 "dplyr",
                 "forcats",
                 "stringr")

  lapply(pack_list, library, character.only = TRUE, verbose = FALSE)

# Load Results ----------------------------------------------------------

  inDir <- "../output/"
  runName <- "20212606_100646"

  # Read output
  sim_out <- readRDS(paste0(inDir, runName, "_res.rds"))
  gg_shape <- sim_out$gg_shape

  # Support Functions
  source("./helper/functions.R")

# Analyse -----------------------------------------------------------------

  # Possible Results
  names(sim_out$results[[1]]$rep1)

  # Mean result by name
  lapply(sim_out$results, average_result, result = "coefs")
  t(sapply(sim_out$results, average_result, result = "cors"))
  t(sapply(sim_out$results, average_result, result = "r2"))
  t(sapply(sim_out$results, average_result, result = "mses"))

# Plots -------------------------------------------------------------------

  ## Obtain plots
  result <- c("cors.", "mses.", "r2.")[2]

  K_conditions <- unique(gg_shape$K)
  D_conditions <- unique(gg_shape$D)
  int_conditions <- unique(gg_shape$interval)[2]

  methods <- paste(
    c("cont", "disc", "disj", "atte", "dumm", "PCAmix"),
    collapse = "|"
  )
  plot1 <- gg_shape %>%
    # Subset
    filter(grepl(result, variable)) %>%
    filter(grepl(methods, variable)) %>%
    filter(D %in% D_conditions) %>%
    filter(K %in% K_conditions) %>%
    filter(interval %in% int_conditions) %>%
    filter(value < 1) %>% # to get rid of some outliers
    # Change labels of X axis
    mutate(variable = fct_relabel(variable, str_replace, result, "")
    ) %>%
    # Main Plot
    ggplot(aes(x = variable, y = value)) +
    geom_boxplot() +
    # Grid
    facet_grid(rows = vars(factor(D,
                                  labels = paste0("D = ", D_conditions))),
               cols = vars(factor(K,
                                  levels = K_conditions,
                                  labels = paste0("K = ", K_conditions))),
               scales = "free") +
    # Format
    theme(text = element_text(size = 15),
          plot.title = element_text(hjust = 0.5),
          axis.text = element_text(size = 15),
          axis.text.x = element_text(angle = 45, hjust = 0.95),
          axis.title = element_text(size = 15)) +
    labs(title = paste0("Interval Scale = ", int_conditions,
                        " (", result, ")"),
         x     = NULL,
         y     = NULL)

  plot1

# Save plots --------------------------------------------------------------

  file_format <- ".png"
  plot_name <- paste0("interval_sacle_", int_conditions)
  out_dir <- "~/Desktop/"
  file_name <- paste0(out_dir, plot_name, file_format)
  if(file_format == ".pdf"){
    pdf(file_name, width = 15, height = 15)
  }
  if(file_format == ".png"){
    png(file_name, width = 15, height = 15, units = "in", res = 384)
  }
  plot1
  dev.off()