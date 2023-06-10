# Filename: fairness_example.R
# Author: Juha Karvanen
# Date: 2023-06-10
# Description: Reproduces the example in Section 5 of paper
# J. Karvanen, S. Tikka, M. Vihola (2023) Simulating counterfactuals. ArXiv


library(R6causal)
stopifnot(packageVersion("R6causal")>="0.8.0")
library(data.table)
#devtools::install_github("https://github.com/dmlc/xgboost/", subdir="R-package")
library(xgboost)

set.seed(102052023)

cumsum0 <- function(x) {
  cs <- cumsum(x)
  return( c(0,cs[1:(length(cs)-1)]))
}

logsumexp <- function (x) {
  y = max(x)
  y + log(sum(exp(x - y)))
}

softmax <- function (x) {
  exp(x - logsumexp(x))
}

usample <- function(probs, categories, u) {
  lower <- t(apply(probs,1,cumsum0))
  upper <- t(apply(probs,1,cumsum))
  u_ind <- ( (u >= lower) & (u < upper))
  catmat <- matrix(categories, nrow(probs), ncol(probs), byrow = TRUE)
  as.vector(t(catmat))[as.vector(t(u_ind))]
}
#usample(matrix(c(0.2,0.3,0.5),10,3, byrow = TRUE), 1:3, runif(10))

rcateg <- function(x, u, softmax = FALSE, classnames = NULL, asfactor = TRUE, ordered = FALSE) {
  x <- rbind(x)
  if(softmax) {
    probs <- t(apply(x,1,softmax))
  } else {
    probs <- x
  }
  if(nrow(probs) == 1) probs <- cbind(rep(1,length(u))) %*% probs
  if(is.null(classnames)) classnames <- 1:ncol(x)
  simvar <- usample(probs, classnames, u)
  if(ordered) {
    return(as.ordered(simvar))
  }
  if(asfactor) {
    return(as.factor(simvar))
  } else {
   return(simvar) 
  }  
}
#rcateg(matrix(rnorm(30),10,3), runif(10))
#rcateg(matrix(rnorm(30),10,3), runif(10), ordered = TRUE)

credit <- SCM$new("credit scoring",
                  uflist = list(
                    u_age = "n : rnorm(n)",
                    u_marital = "n : rnorm(n)",
                    u_gender = "n : rnorm(n)",
                    u_education = "n : rnorm(n)",
                    u_children = "n : rnorm(n)",
                    u_job = "n : rnorm(n)",
                    u_income = "n : rnorm(n)",
                    u_housing = "n : rnorm(n)",
                    u_address = "n : rnorm(n)",
                    u_savings = "n : rnorm(n)",
                    u_credit_history = "n : rnorm(n)",
                    u_length_of_employment = "n : rnorm(n)",
                    u_ethnicity = "n : rnorm(n)",
                    u_credit_amount = "n : rnorm(n)",
                    u_default = "n : rnorm(n)",
                    u1 = "n : rnorm(n)", 
                    u2 = "n : rnorm(n)", 
                    u3 = "n : rnorm(n)", 
                    u4 = "n : rnorm(n)",
                    u5 = "n : rnorm(n)"
                  ),
                  vflist = list(
                    age = "u_age : round(18 + 60*pnorm(u_age)) ",
                    ethnicity = "u_ethnicity : rcateg(c(0.75,0.15,0.10), pnorm(u_ethnicity), softmax = FALSE)",
                    marital = "u_marital, u2, age: 
                                rcateg(cbind(1, 
                                         0.5 + 0.1*(age + 3*u2 - 22), 
                                         0 + 0.2*(age + 2*u2 - 29)), 
                                      pnorm(u_marital), softmax = TRUE)",
                    gender = "u_gender: as.numeric(u_gender > 0)",
                    education = "u_education, u4, u2, gender, age, ethnicity : 
                                  rcateg(cbind(-gender, 
                                  u4 + u2 - gender + 0.1*age + as.numeric(ethnicity == 1),
                                  2*u4 + 2*u2 - 2*gender + 0.2*(age - 22) + 2*as.numeric(ethnicity == 1),
                                  3*u4 + 3*u2 - 3*gender + 0.3*(age - 28) + 3*as.numeric(ethnicity == 1)),
                                   pnorm(u_education), softmax = TRUE, asfactor = FALSE)",
                    children = "u_children, age, ethnicity, education, marital: 
                                 qpois( p = pnorm(u_children),
                                 lambda = pmax(0.2, -0.2 * (ethnicity == 1) + 0.5*(marital == 3) +
                                 as.numeric(age < 45)*(age - 18)/13 + 2*as.numeric(age >= 45)))",
                    job = "u_job, u4, u2, gender, ethnicity, education, children, age:
                            rcateg(cbind( 0.2*(ethnicity != 1) + (education <= 1),
                            u2 + u4 + (education >= 2),
                            u2 + u4 + (education == 3) + 0.01*(age - 28)), 
                            pnorm(u_job), softmax = TRUE)",
                    income = "u_income, u4, job, education, length_of_employment, age:
                              pmax(0, -10000 + 25000*as.numeric(job) + 5000*u4 + 10000*u_income + 2000*education + 
                              200*length_of_employment + 100*(age - 35))",
                    housing = "u_housing, u3, u5, marital, children, age, education, income:
                                rcateg(cbind(7, 0.1*( u3 + u5 + as.numeric(marital) + children + age + education + income/10000)), 
                                pnorm(u_housing), softmax = TRUE)", # 1 rent, 2 own
                    address = "u_address, u5, marital, children, ethnicity, age, income:
                                round( pmin(10,pmax(1, -4 + u_address + u5 + as.numeric(marital) + 
                                      (ethnicity == 1) + age/30 + income/10000 )))",
                    savings = "u_savings, u1, u3, marital, gender, ethnicity, children, education, income, age:
                                pmin(900000 + 10000*exp(u_savings), pmax(0, -5000 + exp(u_savings + u1 + u3 + income/10000 + age/10)))",
                    length_of_employment = "u_length_of_employment, age, education:
                                    pmax(0, pmin(age - 18 - 3*education + u_length_of_employment, 
                                                0.3*(age - 18)  + 3*u_length_of_employment))",
                    credit_amount = "u_credit_amount, u1, income, age, housing, job, savings, marital, children:
                     pmax(5000, 110000 - 4000*abs(age - 40) + 2*(income - 30000) - 20000*as.numeric(housing) + 10000*as.numeric(job) +
                     - 0.2*savings + 20000*as.numeric(marital) + 10000*children + 40000*u_credit_amount + 10000*u1)",
                    default = "u_default, u1, length_of_employment, housing, income, age, savings, education, job, 
                    credit_amount, ethnicity:
                               as.numeric(qlogis(p = pnorm(u_default), 2.2*income + 10000*education + 
                               10000*as.numeric(job) + 3000*length_of_employment + 10000*u1 - 0.7*credit_amount +
                                5000*as.numeric(ethnicity), scale = 5000) < 0)" # 1 defaults
                  )
)


trainingdata <- credit$simulate(10000, return_simdata = TRUE)
print(summary(trainingdata))
testdata <- credit$simulate(n=2000, return_simdata = TRUE)

returnlevels <- function(datatab) {
  factornames <- sapply(datatab,is.factor)
  factornames <- names(factornames[factornames == TRUE])
  levellist <- sapply(datatab[,..factornames],levels)
  return(levellist)
}

xgbmatrix <- function(datatab, labelname, xlev = NULL, select = NULL, debug = FALSE) {
  label <- datatab[, get(labelname)]
  if(!is.null(datatab)) datatab <- datatab[, ..select]
  if(debug) browser()
  hotone <- model.matrix(~.+0, data = datatab, xlev = xlev)
  xgbmat <- xgb.DMatrix(data = hotone, label = label)
  return(xgbmat)
}

credit_xgb <- function(trainingdata, testdata, select = NULL, params) {
  xgbtrain <- xgbmatrix(trainingdata,"default", select = select)
  xgbcv <- xgb.cv( params = params, data = xgbtrain, nrounds = 100, nfold = 5, 
                   showsd = T, stratified = T, print_every_n = 10, early_stop_round = 20, 
                   maximize = F)
  bestrounds <- which.min(xgbcv$evaluation_log$test_logloss_mean)
  xgbtest <- xgbmatrix(testdata,"default", select = select)
  xgb <- xgb.train(params = params, data = xgbtrain, nrounds = bestrounds, 
                   watchlist = list(val = xgbtest, train = xgbtrain), print_every_n = 10, 
                   maximize = F , eval_metric = "error")
  return(xgb)
}

params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.3, 
               gamma=0, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1)

xgba <- credit_xgb(trainingdata = trainingdata, 
                   testdata = testdata, 
                   select = setdiff(credit$vnames,"default"),
                   params = params
)
xgbb <- credit_xgb(trainingdata = trainingdata, 
                   testdata = testdata, 
                   select = setdiff(credit$vnames,c("default","gender","ethnicity")),
                   params = params
)
xgbc <- credit_xgb(trainingdata = trainingdata, 
                   testdata = testdata, 
                   select = setdiff(credit$pa("default"),
                                    union(credit$unames,
                                          c("default","gender","ethnicity"))),
                   params = params
)

xgb_predict <- function(xgbmodel, select, xlev = NULL) {
  func <- function(newdata) {
    xgbmat <- xgbmatrix(newdata, "default", xlev = xlev, select = select)
    return( predict(xgbmodel, xgbmat) )
  }
  return(func)
}

traininglevels <- returnlevels(trainingdata)
xgba_predict <- xgb_predict(xgba, select = setdiff(credit$vnames,"default"),
                            xlev = traininglevels)
xgbb_predict <- xgb_predict(xgbb, select = setdiff(credit$vnames, 
                                                   c("default","gender","ethnicity")),
                            xlev = within(traininglevels, rm(ethnicity)))
xgbc_predict <- xgb_predict(xgbc, setdiff(credit$pa("default"),
                                          union(credit$unames,
                                                c("default","gender","ethnicity"))),
                            xlev = within(traininglevels, rm(ethnicity,marital)))

newdata <- credit$simulate(n=1000, return_simdata = TRUE)

vnames <- credit$vnames
modellist <- list(xgba_predict,xgbb_predict,xgbc_predict) 
results <- matrix(NA, nrow = nrow(newdata), ncol = length(modellist))
for(i in 1:nrow(newdata)) {
  cat(i,date(),"\n")
  fairlist <- try(fairness( modellist = modellist,
           scm = credit, 
           sensitive = c("gender","ethnicity"),
           condition = newdata[i,..vnames], 
           parents = setdiff(credit$pa("default",includeself = FALSE),
                             union( c("gender","ethnicity"), credit$unames)),
           n = 1000,
           modeltype = "function", 
           control = list(method = "u_find",
                          condition_type = list(age = "cont", 
                                                ethnicity = "disc", 
                                                marital = "disc", 
                                                gender = "disc", 
                                                education = "disc",
                                                children = "disc",
                                                job = "disc",
                                                income = "cont",
                                                housing = "disc",
                                                address = "disc",
                                                savings = "cont",
                                                credit_history = "disc",
                                                length_of_employment = "cont",
                                                credit_amount = "cont",
                                                default = "disc"
                          ))
  ))
  if(!inherits(fairlist, "try-error")) {
    for(j in 1:length(modellist)) {
      results[i,j] <- max(apply(fairlist[[j]],1,max) - apply(fairlist[[j]],1,min))
    }
  }
}

#save(results, xgba, xgbb, xgbc, file = "credit_scoring_results.Rdata")

latextable<-function(td) {
  tddim<-dim(td);
  lt<-"\n";
  for (i in 1:tddim[1]) {
    for (j in 1:tddim[2]) {
      if (j==1) lt <- paste(lt, td[i,j])
      else lt<-paste(lt," & ",td[i,j])
    }
    if (i==tddim[1]) lt <- paste(lt, "\n")
    else lt<-paste(lt, "\\\\ \n")
  }
  cat(lt)
} 

restable <- rbind(
  apply(results,2,function(x) mean(x==0)),
  apply(results,2,function(x) mean(x<0.01)),
  apply(results,2,median),
  apply(results,2,max)
  )

restablet <- rbind(
  c("Fairness %",formatC(100*apply(results,2,function(x) mean(x==0)), digits=0, format = "f")),
  c("Difference <0.01 %",formatC(100*apply(results,2,function(x) mean(x<0.01)), digits=0, format = "f")),
  c("Median difference",formatC(apply(results,2,median), digits=5, format = "f")),
  c("Maximum difference",formatC(apply(results,2,max), digits=2, format = "f"))
)
latextable(restablet)


