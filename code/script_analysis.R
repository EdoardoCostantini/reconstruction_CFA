### Title:    Analysing results
### Project:  Reconstruction CFA
### Author:   Edoardo Costantini
### Created:  2021-06-29
### Modified: 2021-06-29

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
  runName <- "20210629_170139"

  # Read output
  gg_shape <- readRDS(paste0(inDir, runName, "_res.rds"))

  # Support Functions
  source("./helper/functions.R")

# Plots -------------------------------------------------------------------

  ## Obtain plots
  result <- levels(gg_shape$variable)
  K_conditions <- rev(sort(unique(gg_shape$K)))
  D_conditions <- sort(unique(gg_shape$D))
  int_conditions <- unique(gg_shape$interval)[2]

  methods <- paste(levels(gg_shape$variable)[(1:3)], collapse = "|")
  plot1 <- gg_shape %>%
    # Subset
    filter(grepl(methods, variable)) %>%
    filter(D %in% D_conditions) %>%
    filter(K %in% K_conditions) %>%
    filter(interval %in% int_conditions) %>%
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
  out_dir <- paste0(inDir, "graphs/")
  file_name <- paste0(out_dir, plot_name, file_format)
  if(file_format == ".pdf"){
    pdf(file_name, width = 15, height = 15)
  }
  if(file_format == ".png"){
    png(file_name, width = 15, height = 15, units = "in", res = 384)
  }
  plot1
  dev.off()