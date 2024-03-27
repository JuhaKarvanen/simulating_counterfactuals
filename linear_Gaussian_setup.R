# Filename: linear_Gaussian_setup.R
# Author: Juha Karvanen
# Date: 2024-03-14
# Usage: sourced by linear_Gaussian_run.R
# Description: Functions needed to reproduce the simulation experiment in Section 4 of paper
# J. Karvanen, S. Tikka, M. Vihola (2023) Simulating counterfactuals. 
# # arXiv:2306.15328, https://arxiv.org/pdf/2306.15328

stopifnot(packageVersion("R6causal")>="0.8.3")

standardize <- function(x, mean = 0, sd = 1) {
  return((x - mean) / sd) 
}

simulate <- function(reps, seed, simsettings, results = NULL, lglist = NULL, 
                     onecaselist = NULL, savefile = NULL, sigmaoklimit = 0.1) {
  set.seed(seed)
  nsim <- reps
  simid <- 1:nsim
  ssize <- simsettings$ssize
  nv <- simsettings$nv
  ncond <- simsettings$ncond
  avgneighbors <- simsettings$avgneighbors
  avgu2 <- simsettings$avgu2
  if(is.null(results)) {
    results <- expand.grid(ssize = ssize,
                         nv = nv, ncond = ncond, 
                         avgneighbors = avgneighbors,
                         avgu2 = avgu2,
                         simid = simid
                         )
  }
  results <- subset(results, nv >  ncond & nv > avgneighbors)
  results$runtime <- NA
  results$meanz <- NA
  results$sdz <- NA
  results$meansqz <- NA
  results$meankurtosis <- NA
  results$meanabsskewness <- NA
  results$meanskewness <- NA
  results$cor <- NA
  results$ksstat <- NA
  results$ksp <- NA
  results$nofreev <- FALSE
  results$nunique <- NA
  results$truemean <- NA
  results$truesd <- NA
  results$truecor <- NA
  nlg <- length(lglist)
  nonecase <- length(onecaselist)
  
  for (j in 1:nrow(results)) {
    nv <- results$nv[j]
    ncond <- results$ncond[j]
    n <- results$ssize[j]
    avgneighbors <- results$avgneighbors[j]
    avgu2 <- results$avgu2[j]
    repeat {
      if(is.null(lglist)) {
      lg <- LinearGaussianSCM$new("Linear Gaussian",
                    random_linear_gaussian = list(nv = nv, 
                                                  avgneighbors = avgneighbors, 
                                                  avgu2 = avgu2,
                                                  vcoefdistr=function(n) {rnorm(n)},
                                                  ccoefdistr=function(n) {rnorm(n)},
                                                  ucoefdistr=function(n) {rep(1,n)},
                                                  u2coefdistr=function(n) {rnorm(n)}))
      } else {
        jj <- (j %% nlg)
        if(jj==0) jj <- nlg
        lg <- lglist[[jj]]
      }
      freevnames <- lg$vnames[(ncond + 1):nv]
      if(is.null(onecaselist)) {
        onecase <- lg$simulate(n = 1, return_simdata = TRUE, store_simdata = FALSE)
      } else {
        jj <- (j %% nonecase)
        if(jj==0) jj <- nonecase
        onecase <- onecaselist[[jj]]
      }
      cond <- subset(onecase, select = lg$vnames[1:ncond])
      situation <- list(condition = cond)
      trueparam <- analytic_linear_gaussian_conditining(lg, situation)
      if( all(is.finite(trueparam$mu)) & all(is.finite(trueparam$sigma)) ) break
    }
    sigmaok <- diag(trueparam$sigma)[freevnames] >= sigmaoklimit
    tested_freevnames <- freevnames[sigmaok]
    if(length(tested_freevnames) == 0) {
      results$nofreev[j] <- TRUE 
      next
    }
    situation$condition_type <- rep("cont", ncond)
    names(situation$condition_type) <- lg$vnames[1:ncond]
    tested_freevnames1 <- tested_freevnames[1]
    results$truemean[j] <- trueparam$mu[tested_freevnames1,,drop=FALSE]
    results$truesd[j] <- sqrt(diag(trueparam$sigma)[tested_freevnames1,drop=FALSE])
    if( length(tested_freevnames) > 1) {
      results$truecor[j] <- trueparam$sigma[tested_freevnames1,tested_freevnames[2]] /
        ( results$truesd[j] * sqrt(diag(trueparam$sigma)[tested_freevnames[2]]))
    }
    starttime <- Sys.time()
    cfsim <-  try(counterfactual(lg,
                                 situation,
                                 n = n,
                                 method="u_find",
                                 control = list(
                                   # method="u_find",
                                   # condition_type = condition_type,
                                   max_iterations = 50,
                                   sampling_replace = TRUE,
                                   batchsize = n
                                 )))
    endtime <- Sys.time()
    results$runtime[j] <- difftime(endtime,starttime, units="secs")
    if(!inherits(cfsim, "try-error")) {
      freevsim1 <- subset(cfsim, select = tested_freevnames1)
      mudiff1 <- as.matrix(freevsim1 - tcrossprod(rep(1,n),results$truemean[j]))
      stdv1 <-  mudiff1 / tcrossprod(rep(1,n), results$truesd[j])
      unstdv <- unlist(stdv1)
      results$nunique[j] <- nrow(unique(freevsim1 ))
      results$meanz[j] <- mean(unstdv)  
      results$sdz[j] <- sd(unstdv)  
      results$meansqz[j] <- mean(unstdv^2)  
      results$meankurtosis[j] <- mean(apply(freevsim1,2,kurtosi))
      results$meanabsskewness[j] <- mean(abs(apply(freevsim1,2,skew)))
      results$meanskewness[j] <- mean(apply(freevsim1,2,skew))
      if( length(tested_freevnames) > 1) {
        results$cor[j] <- cor(freevsim1, subset(cfsim, select = tested_freevnames[2]))
      }
      kstest <- ks.test(unstdv + rnorm(nrow(freevsim1),0,0.001), "pnorm") #small noise to prevent ties
      results$ksstat[j] <- kstest$statistic
      results$ksp[j] <- kstest$p.value
    }
  if(!is.null(savefile)) {
    save(results, file = sfile)
  }
  }
  return(results)
}
