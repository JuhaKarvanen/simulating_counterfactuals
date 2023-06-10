# Filename: linear_Gaussian_run.R
# Author: Juha Karvanen
# Date: 2023-06-10
# Usage: This code is meant to be run via linear_Gaussian_run.bash. Modifications
# may be needed depending on the system. The current settings are for csc.fi.
# Description: Code to reproduce the simulation experiment in Section 4 of paper
# J. Karvanen, S. Tikka, M. Vihola (2023) Simulating counterfactuals. ArXiv


sink(file="linear_Gaussian_run.log")

# Next two lines are user specific and not needed in general
.libPaths(c(.libPaths(),"/projappl/jkarvane/r_packages_singularity"))
.libPaths(c("/projappl/jkarvane", .libPaths()))

library(data.table)
library(MASS)
library(psych)
library(R6causal) 
stopifnot(packageVersion("R6causal")>="0.8.0")

source("linear_Gaussian_setup.R")

args <- commandArgs(trailingOnly = TRUE)
seed <- 101062023 + as.numeric(args[1])

nsim <- 1
simid <- 1:nsim
simsettings <- list(
  ssize = c(1000,10000,100000,1000000),
  nv = c(5,10,50),
  ncond = c(1,2,4,9),
  avgneighbors = c(3,5,7),
  avgu2 = c(0,1)
)
ssize <- simsettings$ssize
nv <- simsettings$nv
ncond <- simsettings$ncond
avgneighbors <- simsettings$avgneighbors
avgu2 <- simsettings$avgu2

results <- expand.grid(ssize = ssize,
                       nv = nv, ncond = ncond, 
                       avgneighbors = avgneighbors,
                       avgu2 = avgu2,
                       simid = simid)
results <- subset(results, (ssize %in% c(1000,10000,100000,1000000) & 
                              ((nv == 5 & ncond == 1 &  avgneighbors == 3 & avgu2 == 0) |
                                 (nv == 10 & ncond == 4 &  avgneighbors == 5 & avgu2 == 1) |
                                 (nv == 10 & ncond == 9 &  avgneighbors == 5 & avgu2 == 1) | 
                                 (nv == 50 & ncond == 2 &  avgneighbors == 5 & avgu2 == 1) |
                                 (nv == 50 & ncond == 9 &  avgneighbors == 7 & avgu2 == 1) ))
)

sfile <- paste0("~/simulating_counterfactuals/data/linear_Gaussian_result_", as.character(args[1]) ,".Rdata")

reps <- 10
result <- simulate(reps = reps, seed = seed, simsettings = simsettings, 
                   results = results[ rep(1:nrow(results), reps), ],
                   savefile = sfile)


save(result, file = sfile)

sink()







