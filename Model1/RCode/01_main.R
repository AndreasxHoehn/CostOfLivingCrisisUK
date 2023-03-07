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
                      "ggplot2","RColorBrewer",             # Data Visualization
                      "maps","rgeos","rgdal","maptools",    # Mapping
                      "rmarkdown","tinytex","knitr")        # Report

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
definitions$geo_subset <- c("W")       # run for Wales only to save time
# definitions$geo_subset <- c("E","W")

# model parameters: initialization
definitions$n.t      <- 3                       # time horizon, 3 cycles
definitions$v.n      <- c("H","S1","S2")        # the model states: Healthy (H), Sick (S1), Sicker (S2)
definitions$n.s      <- length(definitions$v.n) # the number of health states

# Transition probabilities (per cycle)
definitions$p.HS1  <- 0.20  # probability to become sick when healthy
definitions$p.S1H  <- 0.10  # probability to become healthy when sick
definitions$p.S1S2 <- 0.20  # probability to become sicker when sick
definitions$p.S2S1 <- 0.10  # probability to become sick when sicker
definitions$p.S2H  <- 0.05  # probability to become healthy when sicker

# Plots and Maps
definitions$fig_width  <- 25  # = width of output figures in cm
definitions$fig_height <- 20  # = height of output figures in cm
definitions$map_cols   <- colorRampPalette(c("#F7FBFF","#9ECAE1","#08306B"))(3)

# overall: report
definitions$round_results <- 2

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Source Code Files ###

source("RCode/02_linking_analysis.R")
source("RCode/03_visuals.R")

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Benchmark Time ###

benchmark_time$end <- Sys.time()
print("Duration of Program:")
print(round(benchmark_time$end - benchmark_time$start), 
      definitions$rounding_results)
