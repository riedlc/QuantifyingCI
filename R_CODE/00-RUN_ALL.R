##############################################
##    Replication code and data for
##
## Riedl, C., Kim, Y.J., Gupta, P., Malone, T.W., Woolley, A.W. (2021). 
## Quantifying Collective Intelligence in Human Groups
## Proceedings of the National Academy of Sciences, in press.
##
## This is the main replication file. This file runs
## - 01-Fit-meta-analysis.R which will fit the meta-analysis models and store them in DATA_FITTED_MODELS
## - 02-Tables-and-Figures.R which will generate all tables (as .tex) and most figures
## - 03-Variable-Importance.R which will generate two more figures
##############################################

# Change working directory
# Change this to wherever you put this folder
setwd("~/Downloads/QuantifyingCI")

# Start with clean slate
rm(list=ls())

# Check for and install missing packages (if any)
list.of.packages <- c("ggplot2", "lavaan", "metaSEM", "texreg", "lmtest", "semPlot", "xtable", "jtools", "broom.mixed", "corrplot", "sandwich", "car", "mediation", "data.table", "randomForest")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Load libraries
require(ggplot2)
require(lavaan)
require(metaSEM)
require(texreg)
require(lmtest)
require(semPlot)
require(xtable)
require(jtools)
require(corrplot)
require(sandwich)
require(car)
require(mediation)
require(data.table)
require(randomForest)


# Load helper functions
source("R_CODE/helper_getStars.R")
source("R_CODE/helper_extract.wls.R")
source("R_CODE/helper_simulation.R")
source("R_CODE/helper_getVariableImportance.R")

## CONFIGURATION
# Holds name of main data file
mainDataFile <- "DATA/full.csv"

runRerun <- FALSE    # Run rerun() command for two-stage meta analysis (very slow!) set to FALSE for testing


# Run all scripts
## Fit all meta-analysis models
source("R_CODE/01-Fit-meta-analysis.R")

## Produce main figures and tables
source("R_CODE/02-Tables-and-Figures.R")

## Produce additoinal figuers for variable importance plots
source("R_CODE/03-Variable-Importance.R")
