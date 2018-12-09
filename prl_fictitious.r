function (data = "choice", niter = 3000, nwarmup = 1000, nchain = 1, 
          ncore = 1, nthin = 1, inits = "random", indPars = "mean", 
          saveDir = NULL, modelRegressor = FALSE, vb = FALSE, inc_postpred = FALSE, 
          adapt_delta = 0.95, stepsize = 1, max_treedepth = 10) 
{
      if (modelRegressor) {
            cat("************************************\n")
            cat("** Extract model-based regressors **\n")
            cat("************************************\n")
      }
      modelPath <- system.file("stan", "prl_fictitious.stan", package = "hBayesDM")
      startTime <- Sys.time()
      if (data == "example") {
            data <- system.file("extdata", "prl_exampleData.txt", 
                                package = "hBayesDM")
      }
      else if (data == "choose") {
            data <- file.choose()
      }
      if (file.exists(data)) {
            rawdata <- read.table(data, header = T, sep = "\t")
      }
      else {
            stop("** The data file does not exist. Please check it again. **\n  e.g., data = '/MyFolder/SubFolder/dataFile.txt', ... **\n")
      }
      NA_rows_all = which(is.na(rawdata), arr.ind = T)
      NA_rows = unique(NA_rows_all[, "row"])
      if (length(NA_rows) > 0) {
            rawdata = rawdata[-NA_rows, ]
            cat("The number of rows with NAs=", length(NA_rows), 
                ". They are removed prior to modeling the data. \n", 
                sep = "")
      }
      subjList <- unique(rawdata[, "subjID"])
      numSubjs <- length(subjList)
      numPars <- 3
      POI <- c("mu_eta", "mu_alpha", "mu_beta", "sigma", "eta", 
               "alpha", "beta", "log_lik")
      if (inc_postpred) {
            POI <- c(POI, "y_pred")
      }
      modelName <- "prl_fictitious"
      cat("\nModel name = ", modelName, "\n")
      cat("Data file  = ", data, "\n")
      cat("\nDetails:\n")
      if (vb) {
            cat(" # Using variational inference # \n")
      }
      else {
            cat(" # of chains                   = ", nchain, "\n")
            cat(" # of cores used               = ", ncore, "\n")
            cat(" # of MCMC samples (per chain) = ", niter, "\n")
            cat(" # of burn-in samples          = ", nwarmup, "\n")
      }
      cat(" # of subjects                 = ", numSubjs, "\n")
      Tsubj <- as.vector(rep(0, numSubjs))
      for (i in 1:numSubjs) {
            curSubj <- subjList[i]
            Tsubj[i] <- sum(rawdata$subjID == curSubj)  # total trials of each subject
      }
      maxTrials <- max(Tsubj)
      cat(" # of (max) trials per subject = ", maxTrials, "\n\n")
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
      if (inits[1] != "random") {
            if (inits[1] == "fixed") {
                  inits_fixed <- c(0.5, 0.1, 1)
            }
            else {
                  if (length(inits) == numPars) {
                        inits_fixed <- inits
                  }
                  else {
                        stop("Check your inital values!")
                  }
            }
            genInitList <- function() {
                  list(mu_p = c(qnorm(inits_fixed[1]), qnorm(inits_fixed[2]), 
                                qnorm(inits_fixed[3]/5)), sigma = c(1, 1, 1), 
                       eta_pr = rep(qnorm(inits_fixed[1]), numSubjs), 
                       alpha_pr = rep(qnorm(inits_fixed[2]), numSubjs), 
                       beta_pr = rep(qnorm(inits_fixed[3]/5), numSubjs))
            }
      }
      else {
            genInitList <- "random"
      }
      rstan::rstan_options(auto_write = TRUE)
      if (ncore > 1) {
            numCores <- parallel::detectCores()
            if (numCores < ncore) {
                  options(mc.cores = numCores)
                  warning("Number of cores specified for parallel computing greater than number of locally available cores. Using all locally available cores.")
            }
            else {
                  options(mc.cores = ncore)
            }
      }
      else {
            options(mc.cores = 1)
      }
      cat("************************************\n")
      cat("** Building a model. Please wait. **\n")
      cat("************************************\n")
      m = rstan::stan_model(modelPath) # load the prl_fictitious.stan file
      if (vb) {
            fit = rstan::vb(m, data = dataList, pars = POI, init = genInitList)
      }
      else {
            fit = rstan::sampling(m, data = dataList, pars = POI, 
                                  warmup = nwarmup, init = genInitList, iter = niter, 
                                  chains = nchain, thin = nthin, control = list(adapt_delta = adapt_delta, 
                                                                                max_treedepth = max_treedepth, stepsize = stepsize))
      }
      parVals <- rstan::extract(fit, permuted = T)
      if (inc_postpred) {
            parVals$y_pred[parVals$y_pred == -1] <- NA
      }
      eta <- parVals$eta
      alpha <- parVals$alpha
      beta <- parVals$beta
      allIndPars <- array(NA, c(numSubjs, numPars))
      allIndPars <- as.data.frame(allIndPars)
      for (i in 1:numSubjs) {
            if (indPars == "mean") {
                  allIndPars[i, ] <- c(mean(eta[, i]), mean(alpha[, 
                                                                  i]), mean(beta[, i]))
            }
            else if (indPars == "median") {
                  allIndPars[i, ] <- c(median(eta[, i]), median(alpha[, 
                                                                      i]), median(beta[, i]))
            }
            else if (indPars == "mode") {
                  allIndPars[i, ] <- c(estimate_mode(eta[, i]), estimate_mode(alpha[, 
                                                                                    i]), estimate_mode(beta[, i]))
            }
      }
      allIndPars <- cbind(allIndPars, subjList)
      colnames(allIndPars) <- c("eta", "alpha", "beta", "subjID")
      modelData <- list(modelName, allIndPars, parVals, fit, rawdata)
      names(modelData) <- c("model", "allIndPars", "parVals", "fit", 
                            "rawdata")
      class(modelData) <- "hBayesDM"
      endTime <- Sys.time()
      timeTook <- endTime - startTime
      if (!is.null(saveDir)) {
            currTime <- Sys.time()
            currDate <- Sys.Date()
            currHr <- substr(currTime, 12, 13)
            currMin <- substr(currTime, 15, 16)
            timeStamp <- paste0(currDate, "_", currHr, "_", currMin)
            dataFileName = sub(pattern = "(.*)\\..*$", replacement = "\\1", 
                               basename(data))
            save(modelData, file = file.path(saveDir, paste0(modelName, 
                                                             "_", dataFileName, "_", timeStamp, ".RData")))
      }
      cat("\n************************************\n")
      cat("**** Model fitting is complete! ****\n")
      cat("************************************\n")
      return(modelData)
}