library(here)
library(BayesianTools)
library(tidyverse)
source(here("utils.R"))

####### adapt to your needs
path_figures <- here::here("../figures")
path_data <- here::here("../data")

load(paste0(path_data, "/out_beta_10_rep_real_ref_10000_iter_DEzs_harpalus.RData"))
out_har = out

# here are some plots and diagnostics
gelmanDiagnostics(out, plot = T) # should be below 1.05 for all parameters to demonstrate convergence 
marginalPlot(out)
correlationPlot(out, scaleCorText = F,start = 400)
correlationPlot(out, scaleCorText = T,start = 400)
summary(out)
tracePlot(out, start = 100)
MAP(out)

# in order to save an image automatically, run the following three lines
png(file=paste0(path_figures, "/", "real_mcmc_harpalus_trace", ".png"),
    width = 30, height = 30, units = "cm", res = 240)
tracePlot(out_har,start = 400)  
dev.off()
