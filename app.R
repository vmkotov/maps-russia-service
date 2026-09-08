#!/usr/bin/env Rscript
library(plumber)
pr <- plumb("scripts/api.R")
pr$run(host = "0.0.0.0", port = 8080)
