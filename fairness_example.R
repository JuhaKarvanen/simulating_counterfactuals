# Filename: fairness_example.R
# Author: Juha Karvanen
# Date: 2024-03-14
# Description: Reproduces the example in Section 5 of paper
# J. Karvanen, S. Tikka, M. Vihola (2023) Simulating counterfactuals, 
# arXiv:2306.15328, https://arxiv.org/pdf/2306.15328


library(R6causal)
stopifnot(packageVersion("R6causal")>="0.8.3")
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
                    gender = "u_gender: as.numeric(u_gender > 0)",
                    marital = "u_marital, u2, age: 
                                rcateg(cbind(-1 + 0.25*(42 - pmin(age,42)), 
                                         pmax(0.2,1 + 3*u2), 
                                         0 + 0.03*(age + 12*(age/78)^2+ u2 - 32)), 
                                      pnorm(u_marital), softmax = TRUE)", #1 single, 2 married or cohabiting, 3 divorced or widowed
                    education = "u_education, u4, u2, gender, age, ethnicity : 
                                  rcateg(cbind(-1 - u2 - u4 + as.numeric(ethnicity == 2) + as.numeric(ethnicity == 2)*gender, 
                                  4 + u4 + u2 - gender + 0.1*(pmin(10,abs(30-age))) + as.numeric(ethnicity == 1), 
                                  2*u4 + 2*u2 - 2*gender + 0.6*(pmin(age,30) - 22) + 2*as.numeric(ethnicity == 1), 
                                  0.6*(3*u4 + 3*u2 + 30/(6 + sqrt(abs(40-age)) + gender) + 3*pmin(0,age - 25) + pmin(0,age - 30) + 3*as.numeric(ethnicity == 1))), 
                                   pnorm(u_education), softmax = TRUE, asfactor = FALSE)", # 1 primary, 2 secondary, 3 tertiary, 4 doctorate
                    children = "u_children, age, ethnicity, education, marital: 
                                 qpois( p = pnorm(u_children),
                                 lambda = pmax(0.2, -0.3 * (ethnicity == 1) + 0.5*(marital == 2) + 0.5*(marital == 3) +
                                 0.1*education - 0.4*as.numeric(education == 1) +
                                 as.numeric(age < 45)*(age - 18 - education)/13 + 2*as.numeric(age >= 45)))",
                    job = "u_job, u4, u2, gender, ethnicity, education, age:
                            rcateg(cbind( -3 + 0.2*(ethnicity != 1) + (education == 1) + 1.4*pmax(0,22-age) + 1*pmax(0,pmin(4,28-age)), 
                            u2 + u4 + (education >= 2), 
                            -u2 - u4 +  pmin(8,pmax(-8,age - 63) - education + gender)),  
                            pnorm(u_job), softmax = TRUE)", #1 not working and not retired, 2 working, 3 retired
                    length_of_employment = "u_length_of_employment, job, age, education:
                                    (job==1)*0 + (job==3)*0 +
                                    (job==2) * pmax(0, pmin(age - 18 - 3*education + 2*u_length_of_employment, 
                                                0.3*(age - 18)  + 6*u_length_of_employment))",
                    income = "u_income, u4, job, education, length_of_employment, age:
                              (job == 1) * pmax(0,10000 + 2000*u4 + 5000*u_income) +
                              (job == 2) * pmax(10000 + 1000*u_income, 25000 + 5000*u4 + 10000*qt(pnorm(u_income),df=5) + 2000*education + 
                              200*length_of_employment + 200000/(10 + sqrt(abs(58-age))))  +
                              (job == 3) * pmax(0,20000 + 5000*u4 + 6000*u_income + 2000*education)",
                    address = "u_address, u5, marital, children, ethnicity, age, income:
                                round( pmin(10,pmax(1, -4 + u_address + u5 + as.numeric(marital) + 
                                      (ethnicity == 1) + age/30 + income/10000 )))", # 1,2,...,10
                    housing = "u_housing, u3, u5, marital, children, age, education, income:
                                rcateg(cbind(7, 0.18*( 10 + u3 + u5 + as.numeric(marital) + children + age/3 + education + income/10000)), 
                                pnorm(u_housing), softmax = TRUE)", # 1 rent, 2 own
                    savings = "u_savings, u1, u3, marital, gender, ethnicity, children, education, income, age:
                                0.2*pmax(0, -5000 + (qgamma(pnorm(u_savings+ u1 + u3), shape = 3, scale = 1)) * 
                                (5000 + 0.05*income*(age-17) - 20000/(1+sqrt(abs(age-27)))  + 2000*(education-2) + 2000*(ethnicity==3) + 2000*(marital==2) - 2000*children))",
                   credit_amount = "u_credit_amount, u1, income, age, housing, job, savings, marital, children:
                     pmax(5000, 110000 - 4000*abs(age - 40) + 2*(income - 30000) - 20000*as.numeric(housing) + 10000*as.numeric(job==2) +
                     - 0.2*savings + 20000*as.numeric(marital) + 10000*children + 40000*u_credit_amount + 10000*u1)",
                    default = "u_default, u1, length_of_employment, housing, income, age, savings, education, job, 
                    credit_amount, ethnicity:
                               as.numeric(qlogis(p = pnorm(u_default), -4000 + 2.2*income + 10000*education + 
                               10000*as.numeric(job==2) + 3000*length_of_employment + 10000*u1 - 0.7*credit_amount +
                                5000*as.numeric(ethnicity) + 500*age + 10000*(as.numeric(housing) - 1), scale = 5000) < 0)" # 0 no default,  1 default
                  )
)


trainingdata <- credit$simulate(10000, return_simdata = TRUE)
print(summary(trainingdata))
testdata <- credit$simulate(n=2000, return_simdata = TRUE)

# Descriptive statistics for trainingdata
with(trainingdata,table(age))
with(trainingdata,table(ethnicity))
with(trainingdata,table(gender))
with(trainingdata,prop.table(table(age,marital), margin = 1))
with(trainingdata,prop.table(table(age,education), margin = 1))
with(trainingdata,prop.table(table(age,children), margin = 1))
with(trainingdata,prop.table(table(marital,children), margin = 1))
with(trainingdata,prop.table(table(education,children), margin = 1))
with(trainingdata,prop.table(table(age,job), margin = 1))
with(trainingdata,prop.table(table(education,job), margin = 1))
ddply(subset(trainingdata,job==2),.(age),summarize, meanlength = mean(length_of_employment), maxlength = max(length_of_employment))
ddply(trainingdata,.(job,age),summarize, meanincome = mean(income), minincome = min(income), maxincome = max(income))
with(trainingdata,table(address))
with(trainingdata,prop.table(table(marital,address), margin = 1))
with(trainingdata,prop.table(table(children,address), margin = 1))
with(trainingdata,prop.table(table(ethnicity,address), margin = 1))
ddply(trainingdata,.(address),summarize, meanincome = mean(income), minincome = min(income), maxincome = max(income))
with(trainingdata,table(housing))
with(trainingdata,prop.table(table(address,housing), margin = 1))
with(trainingdata,prop.table(table(marital,housing), margin = 1))
with(trainingdata,prop.table(table(age,housing), margin = 1))
ddply(trainingdata,.(age),summarize, meansavings = mean(savings), mediansavings = median(savings), minsavings = min(savings), maxsavings = max(savings))
ddply(trainingdata,.(ethnicity),summarize, meansavings = mean(savings), mediansavings = median(savings), minsavings = min(savings), maxsavings = max(savings))
ddply(trainingdata,.(marital),summarize, meansavings = mean(savings), mediansavings = median(savings), minsavings = min(savings), maxsavings = max(savings))
ddply(trainingdata,.(education),summarize, meansavings = mean(savings), mediansavings = median(savings), minsavings = min(savings), maxsavings = max(savings))
ddply(trainingdata,.(gender),summarize, meansavings = mean(savings), mediansavings = median(savings), minsavings = min(savings), maxsavings = max(savings))
#with(trainingdata,plot(income,savings))
with(trainingdata,cor(income,savings))
ddply(trainingdata,.(age),summarize, meancredit = mean(credit_amount), mediancredit = median(credit_amount), mincredit = min(credit_amount), maxcredit = max(credit_amount))
ddply(trainingdata,.(children),summarize, meancredit = mean(credit_amount), mediancredit = median(credit_amount), mincredit = min(credit_amount), maxcredit = max(credit_amount))
ddply(trainingdata,.(housing),summarize, meancredit = mean(credit_amount), mediancredit = median(credit_amount), mincredit = min(credit_amount), maxcredit = max(credit_amount))
ddply(trainingdata,.(job),summarize, meancredit = mean(credit_amount), mediancredit = median(credit_amount), mincredit = min(credit_amount), maxcredit = max(credit_amount))
# with(trainingdata,plot(income,credit_amount))
# with(trainingdata,plot(savings,credit_amount))
# with(trainingdata,plot(savings,credit_amount))
ddply(trainingdata,.(default),summarize, meanincome = mean(income), minincome = min(income), maxincome = max(income))
ddply(trainingdata,.(default),summarize, meansavings = mean(savings), mediansavings = median(savings), minsavings = min(savings), maxsavings = max(savings))
ddply(trainingdata,.(default),summarize, meancredit = mean(credit_amount), mediancredit = median(credit_amount), mincredit = min(credit_amount), maxcredit = max(credit_amount))
ddply(subset(trainingdata,job==2),.(default),summarize, meanlength = mean(length_of_employment), maxlength = max(length_of_employment))
with(trainingdata,prop.table(table(education,default), margin = 1))
with(trainingdata,prop.table(table(ethnicity,default), margin = 1))
with(trainingdata,prop.table(table(age,default), margin = 1))



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
fairresults <- matrix(NA, nrow = nrow(newdata), ncol = length(modellist))
for(i in 1:nrow(newdata)) {
  cat(i,date(),"\n")
  fairlist <- try(fairness( modellist = modellist,
           scm = credit, 
           sensitive = c("gender","ethnicity"),
           condition = newdata[i,..vnames], 
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
           ),
           parents = setdiff(credit$pa("default",includeself = FALSE),
                             union( c("gender","ethnicity"), credit$unames)),
           n = 1000,
           modeltype = "function", 
           method = "u_find",
           control = list(batchsize = 1000,
                          maxbatchs = 1)
  ))
  if(!inherits(fairlist, "try-error")) {
    for(j in 1:length(modellist)) {
      fairresults[i,j] <- max(apply(fairlist[[j]],1,max) - apply(fairlist[[j]],1,min))
    }
  }
}

save(fairresults, xgba, xgbb, xgbc, file = "credit_scoring_results_20240325.Rdata")

print(summary(fairresults))

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
  apply(fairresults,2,function(x) mean(x==0, na.rm = TRUE)),
  apply(fairresults,2,function(x) mean(x<0.01, na.rm = TRUE)),
  apply(fairresults,2,median, na.rm = T),
  apply(fairresults,2,max, na.rm = T)
)

restablet <- rbind(
  c("Fairness %",formatC(100*apply(fairresults,2,function(x) mean(x==0, na.rm = TRUE)), digits=0, format = "f")),
  c("Difference <0.01 %",formatC(100*apply(fairresults,2,function(x) mean(x<0.01, na.rm = TRUE)), digits=0, format = "f")),
  c("Median difference",formatC(apply(fairresults,2,median, na.rm = TRUE), digits=5, format = "f")),
  c("Maximum difference",formatC(apply(fairresults,2,max, na.rm = TRUE), digits=2, format = "f"))
)
latextable(restablet)

