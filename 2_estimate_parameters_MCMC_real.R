library(nlrx)
library(BayesianTools)
library(tidyverse)
library(extraDistr)
library(here)

# if this gives an error, make sure to have an R Project in the folder of the code, so that the here package works properly
source(here("utils.R"))

# you might have to uncomment this and adapt to your needs
# if(Sys.getenv("JAVA_HOME")==""){
# Sys.setenv(JAVA_HOME = "C:/Program Files/Java/jdk-11.0.1")}

## experiment setup
####### adapt paths to your needs
path_netlogo <- "F:/_Software/NetLogo6.2.0"
path_headless <- paste0(path_netlogo, "/netlogo-headless.bat")
# Linux version:
# path_netlogo <- "/home/lena/Schreibtisch/NetLogo6.1.1"
# path_headless <- paste0(path_netlogo, "/netlogo-headless.sh")
path_model <- here::here("../kaeferModel.nlogo")
path_data <- here::here("/data")
path_output <- here::here("/output")


# set constants for experiment
constants = list(max_ticks = 300 * 21, 
                 worldsize = 750, 
                 "include_traps?" = "true",
                 trap_scenario = "\"original\"",
                 init_beetles = "\"normal\"",
                 init_sd = 3,
                 n_beetles = 1000)

# set of parameters (default value, lower boundary, upper boundary, name of parameters)
####### adapt to your needs
params <- data.frame(best = c(0.4, 0.1, 23, 0),
                     lower = c(0.1, 0.01, 0, -1),
                     upper = c(0.8, 0.3, 90, 1),
                     name = c("step_size_mean", "step_size_variance", "turning_angle_sd", "bias"))

# #choose which parameters to calibrate
parSel = c(1,2,3,4)


####### adapt to your needs
# get the real data
load(here("dataReal.RData"))

####### adapt to your needs
# look at one genus
referenceData = real.rel %>%
  ungroup()%>%
  filter(genus == "Harpalus")%>%
  filter(count_abs >= 20)%>%
  dplyr::select(`circle_1stRow_rel`:`out_2ndRow_rel`)

# likelihood of observed data based on beta distribution
likelihood <- function(par){
  # # set parameters that are not calibrated on default values 
  x = params$best
  len = length(par)
  x[parSel] = par
  
  # run simulation
  predictedData = get_trapping_pattern(x, constants, repetitions = 10)
  
  # make trapping pattern relative
  predictedData.rel = predictedData %>%
    mutate(count_abs = circle_1stRow + circle_2ndRow +in_1stRow + in_2ndRow + out_1stRow +  out_2ndRow) %>%
    mutate(circle_1stRow_rel = ifelse(is.na(circle_1stRow/count_abs), 0, circle_1stRow/count_abs))%>%
    mutate(circle_2ndRow_rel = ifelse(is.na(circle_2ndRow/count_abs), 0, circle_2ndRow/count_abs))%>%
    mutate(in_1stRow_rel = ifelse(is.na(in_1stRow/count_abs), 0, in_1stRow/count_abs))%>%
    mutate(in_2ndRow_rel = ifelse(is.na(in_2ndRow/count_abs), 0, in_2ndRow/count_abs))%>%
    mutate(out_1stRow_rel = ifelse(is.na(out_1stRow/count_abs), 0, out_1stRow/count_abs))%>%
    mutate(out_2ndRow_rel = ifelse(is.na(out_2ndRow/count_abs), 0, out_2ndRow/count_abs))%>%
    mutate(count_rel = circle_1stRow_rel + circle_2ndRow_rel +in_1stRow_rel + in_2ndRow_rel + out_1stRow_rel + out_2ndRow_rel)
  
  relData = predictedData.rel %>% 
    dplyr::select(`circle_1stRow_rel`:`out_2ndRow_rel`)

  # calculate parameters for beta distribution
  mu = colMeans(relData)
  var = colMeans((relData - mu)^2)
  alpha = mu^2 * (1-mu)/var - mu
  beta = alpha* (1-mu)/mu
  
  # calculate log likelihoodvalue
  llValue = vector(mode = "numeric", length = 6)
  llValue[1] = sum(dbeta(deframe(referenceData[1]), shape1 = as.numeric(alpha[1]), shape2 = as.numeric(beta[1]), log = TRUE))
  llValue[2] = sum(dbeta(deframe(referenceData[2]), shape1 = as.numeric(alpha[2]), shape2 = as.numeric(beta[2]), log = TRUE))
  llValue[3] = sum(dbeta(deframe(referenceData[3]), shape1 = as.numeric(alpha[3]), shape2 = as.numeric(beta[3]), log = TRUE))
  llValue[4] = sum(dbeta(deframe(referenceData[4]), shape1 = as.numeric(alpha[4]), shape2 = as.numeric(beta[4]), log = TRUE))
  llValue[5] = sum(dbeta(deframe(referenceData[5]), shape1 = as.numeric(alpha[5]), shape2 = as.numeric(beta[5]), log = TRUE))
  llValue[6] = sum(dbeta(deframe(referenceData[6]), shape1 = as.numeric(alpha[6]), shape2 = as.numeric(beta[6]), log = TRUE))
  
  llValue = sum(llValue, na.rm=TRUE)
  return(llValue)
}

# optional, you can also directly provide lower, upper in the createBayesianSetup, see help
####### adapt to your needs if you don't want a uniform prior
prior <- createUniformPrior(lower = params$lower[parSel], 
                            upper = params$upper[parSel], best = params$best[parSel])

bayesianSetup <- createBayesianSetup(likelihood, prior, names = params$name[parSel])

# settings for the sampler, iterations should be increased for real applications
settings <- list(iterations = 4)

####### adapt to your needs
# in case you want to elongate an MCMC run, use this
#load(here("out_beta_10_rep_real_ref_10000_iter_DEzs_harpalus.RData"))
#bayesianSetup = out

# run MCMC, this can take some time
out <- runMCMC(bayesianSetup = bayesianSetup, sampler = "DEzs", settings = settings)

####### adapt to your needs
save(out, file = here("out_beta_10_rep_real_ref_10000_iter_DEzs_harpalus.RData"))
