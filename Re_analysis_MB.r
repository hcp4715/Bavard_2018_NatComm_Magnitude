####################################################################################################
###        This script is reproduce model-based analysis of Bavard et al., 2018. nat. comm.      ###
###           original data and matlab code:https://github.com/sophiebavard/Magnitude            ###
###            by Chuan-Peng Hu, PhD., twitter: @hcp4715, email: hcp4715@gmail.com               ###
####################################################################################################

####################################################################################################
###  Input data: Data_expe1.mat; Data_expe2.mat                                                  ###
###  Variables: subjectlist --  subject id no.                                                   ###
###             cond_s      --  conditions diff between sessions, from 1 ~ 8                     ###
###                             condition 1 (5): 75% vs. 25%  prob. of gain 1 Euro               ###
###                             condition 2 (6): 75% vs. 25%  prob. of gain 0.1 Euro             ###
###                             condition 3 (7): 25% vs. 75%  prob. of loss 0.1 Euro             ###
###                             condition 4 (8): 25% vs. 75%  prob. of loss 1 Euro               ###
###             cond_whole  --  conditions same between sessions, from 1 ~ 4 (as cond_s)         ###
###             choice      --  participants' choice during learning (1 - wrong; 2- correct)     ###
###             out_abs     --  outcome (of reward) under absolute model                         ###
###             out_rel     --  outcome under relative model                                     ###
###             out_c_abs   --  counterfactual outcome under absolute model (only for exp.2)     ###
###             out_c_rel   --  counterfactual outcome under relative model (only for exp.2)     ###
###                                                                                              ###
###             stim_test   --  stimuli presented during tranfer test                            ###
###                             including two stimulus each trias: left and right;               ###
###                             8 stimuli in total:                                              ###
###                                stim 1 (A): 25% of  1 Euro    0.25   fav                      ###
###                                stim 2 (B): 25% of  1 Euro    0.25   no-fav
###                                stim 3 (C): 75% of  0.1 Euro  0.075  fav
###                                stim 4 (D): 25% of  0.1 Euro  0.025  no-fav
###                                stim 5 (E): 25% of -0.1 Euro -0.025  fav
###                                stim 6 (F): 75% of -0.1 Euro -0.075  no-fav
###                                stim 7 (G): 25% of -1 Euro   -0.25   fav
###                                stim 8 (H): 75% of -0.1 Euro -0.75   no-fav
###
###             choice_test --  choise during tranfer test 
###                             incuding two options: 1- left-side; 2-right-side                 ###
####################################################################################################

# ---------------------------------------------------------------------------------------------------------------
# ------ preparation --------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------

### get the directory of the current script
curDir <- dirname(rstudioapi::getSourceEditorContext()$path) #Get the directory ofcurrent script
setwd(curDir)

### clear the working memory
rm(list = ls())

### load the library
library(R.matlab)
library(hBayesDM)
library(tidyverse)

### load data
rawdata <- read.csv('exp1_data.csv',sep = ',',header = T)
rawdata$outcome <- rawdata$absOut

subjList <- unique(rawdata[, "subjID"])
numSubjs <- length(subjList)
numPars <- 3
POI <- c("mu_C_eta", "mu_U_eta", "mu_beta", "sigma", "eta_C","eta_U", 
          "beta", "log_lik") # no '"alpha",' 

modelName <- "prl_fictitious"

niter = 3000
nwarmup = 1000
nchain = 2 
ncore = 2
nthin = 2
inits = "random"
indPars = "mean"
saveDir = NULL
modelRegressor = FALSE
vb = FALSE
inc_postpred = FALSE
adapt_delta = 0.95
stepsize = 1
max_treedepth = 10

Tsubj <- as.vector(rep(0, numSubjs))
for (i in 1:numSubjs) {
      curSubj <- subjList[i]
      Tsubj[i] <- sum(rawdata$subjID == curSubj)  # total trials of each subject
}
maxTrials <- max(Tsubj)

choice <- array(-1, c(numSubjs, maxTrials))
outcome <- array(0, c(numSubjs, maxTrials))

for (i in 1:numSubjs) {
      curSubj <- subjList[i]
      useTrials <- Tsubj[i]
      tmp <- subset(rawdata, rawdata$subjID == curSubj)
      choice[i, 1:useTrials] <- tmp$choice  # combine the data so that each row represent each subject's choice
      outcome[i, 1:useTrials] <- sign(tmp$outcome) # make the outcome -1 0 or 1
}

dataList <- list(N = numSubjs, T = maxTrials, Tsubj = Tsubj, 
                 choice = choice, outcome = outcome, numPars = numPars)

genInitList <- inits
rstan::rstan_options(auto_write = TRUE)

numCores <- parallel::detectCores()
options(mc.cores = ncore)

m = rstan::stan_model('prl_fictitious_woa_Absolute.stan') # load the prl_fictitious.stan file

fit = rstan::sampling(m, data = dataList, pars = POI, 
                      warmup = nwarmup, init = genInitList, iter = niter,
                      chains = nchain, thin = nthin, 
                      control = list(adapt_delta = adapt_delta,
                                     max_treedepth = max_treedepth, stepsize = stepsize))

parVals <- rstan::extract(fit, permuted = T)

eta_C <- parVals$eta_C
eta_U <- parVals$eta_U
#alpha <- parVals$alpha
beta <- parVals$beta
allIndPars <- array(NA, c(numSubjs, numPars))
allIndPars <- as.data.frame(allIndPars)

for (i in 1:numSubjs) {
      if (indPars == "mean") {
            allIndPars[i, ] <- c(mean(eta_C[, i]), mean(eta_U[,i]), mean(beta[, i]))
      }
      else if (indPars == "median") {
            allIndPars[i, ] <- c(median(eta_C[, i]), median(eta_U[, i]), median(beta[, i]))
      }
      else if (indPars == "mode") {
            allIndPars[i, ] <- c(estimate_mode(eta_C[, i]), estimate_mode(eta_U[, i]), estimate_mode(beta[, i]))
      }
}
allIndPars <- cbind(allIndPars, subjList)
colnames(allIndPars) <- c("eta_C", "eta_U", "beta", "subjID")
modelData <- list(modelName, allIndPars, parVals, fit, rawdata)
names(modelData) <- c("model", "allIndPars", "parVals", "fit", 
                      "rawdata")

