### Title:    Subroutine doRep (windows parallelization framework)
### Project:  Ordinality
### Author:   Edoardo Costantini
### Created:  2021-06-10
### Modified: 2021-06-22

## Make sure we have a clean environment:
rm(list = ls())

## Initialize the environment:
source("./settings.R")
source("./init.R")

## Create a cluster object:
clus <- makeCluster(10)

## Progress report file
file.create(paste0(settings$outDir, settings$fileName_progress, ".txt"))

cat(paste0("SIMULATION PROGRESS REPORT",
           "\n",
           "Starts at: ", Sys.time(),
           "\n", "------", "\n" ),
    file = paste0(settings$outDir, settings$fileName_progress, ".txt"),
    sep = "\n",
    append = TRUE)

## Export to worker nodes
clusterExport(cl = clus, varlist = "settings", envir = .GlobalEnv) # export global env
clusterEvalQ(cl = clus, expr = source("./init.R")) # execute script

# mcApply parallel --------------------------------------------------------

sim_start <- Sys.time()

## Run the computations in parallel on the 'clus' object:
out <- parLapply(cl    = clus,
                 X     = 1 : parms$dt_rep,
                 fun   = doRep,
                 conds = conds,
                 parms = parms,
                 settings = settings)

## Kill the cluster:
stopCluster(clus)

sim_ends <- Sys.time()

cat(paste0("\n", "------", "\n",
           "Ends at: ", Sys.time(), "\n",
           "Run time: ",
           round(difftime(sim_ends, sim_start, units = "hours"), 3), " h",
           "\n", "------", "\n"),
    file = paste0(settings$outDir, settings$fileName_progress, ".txt"),
    sep = "\n",
    append = TRUE)

# Attach Extract Info Objects
out_support <- list()
out_support$parms <- parms
out_support$conds <- conds
out_support$session_info <- devtools::session_info()

# Save output -------------------------------------------------------------

saveRDS(out_support,
        paste0(settings$outDir, "sInfo.rds"))

# Zip output folder -------------------------------------------------------

write.tar.gz(settings$fileName)