##### Meta #####

# Author: Andreas Hoehn
# Version: 1.0
# Date:  2023-03-07
# About: The Main Control File

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Preparation ###

# clean workspace
rm(list = ls())  # remove all objects from work space 
gc(full = TRUE)  # deep clean garbage
dir()

# display format
options(scipen = 1000)

# benchmark time 
benchmark_time <- list() 
benchmark_time$start <- Sys.time()

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Libraries ###

# List of Required Packages 
RequiredPackages <- c("here",                               # Folder Structure
                      "data.table",                         # Dialect 
                      "ggplot2", "ggtext")                  # Data Visualization

# ensure all packages are installed and loaded 
.EnsurePackages <- function(packages_vector) {
  new_package <- packages_vector[!(packages_vector %in% 
                                     installed.packages()[, "Package"])]
  if (length(new_package) != 0) {
    install.packages(new_package) }
  sapply(packages_vector, suppressPackageStartupMessages(require),
         character.only = TRUE)
}
.EnsurePackages(RequiredPackages)
here <- here::here # in order to ensure correct file paths 

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Definitions ###

# initialize ...
definitions <- list()

# set seed 
definitions$seed <- 1
set.seed(definitions$seed) 

# US Wave & Geo Subset 
definitions$US_wave    <- "l"
definitions$geo_subset <- c("E","W")

# Plots and Maps
definitions$fig_width  <- 25  # = width of output figures in cm
definitions$fig_height <- 20  # = height of output figures in cm

# overall: report
definitions$round_results <- 2

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Source Code Files ###

source("RCode/02_linking_analysis.R")

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Benchmark Time ###

benchmark_time$end <- Sys.time()
print("Duration of Program:")
print(round(benchmark_time$end - benchmark_time$start), 
      definitions$rounding_results)
