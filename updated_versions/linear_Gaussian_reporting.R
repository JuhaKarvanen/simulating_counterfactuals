# Filename: linear_Gaussian_reporting.R
# Author: Juha Karvanen
# Date: 2024-03-14
# Usage: This code is meant to be run after running linear_Gaussian_run.bash. 
# Description: Code to report the simulation experiment in Section 4 of paper
# J. Karvanen, S. Tikka, M. Vihola (2023) Simulating counterfactuals. 
# # arXiv:2306.15328, https://arxiv.org/pdf/2306.15328



library(data.table)
library(plyr)

datainpath <- "../data/" # Location of files created by linear_Gaussian_run.bash

resl <- vector(mode='list', length=100)
for(j in 1:100) {
  filename <- paste0(datainpath,"linear_Gaussian_result_",j,".Rdata")
  if(file.exists(filename)) {
    load(filename)
    resl[[j]] <- result
  }
}
res <- rbind.fill(resl)

mformatC <- function(x, digits, ...) {
  y <- formatC(x, digits, ...)
  y <- paste0("$",y,"$")
  y[!is.finite(x)] <- "---"
  return(y)
}

res$propunique <- res$nunique/res$ssize
res$cordiff <- res$cor - res$truecor

resa4 <- ddply(subset(res, !is.na(nunique) & !nofreev),
                .(nv,ncond,avgneighbors,avgu2,ssize),summarize, 
                simn = length(ssize),
                mean_propunique = mean(propunique, na.rm = TRUE),
                mean_meanz = mean(meanz, na.rm = TRUE),
                min_meanz = min(meanz, na.rm = TRUE),
                max_meanz = max(meanz, na.rm = TRUE),
                mean_sdz = mean(sdz, na.rm = TRUE),
                min_sdz = min(sdz, na.rm = TRUE),
                max_sdz = max(sdz, na.rm = TRUE),
                mean_cordiff = mean(cordiff, na.rm = TRUE),
                mean_ks = mean(ksstat, na.rm = TRUE),
                min_ks = min(ksstat, na.rm = TRUE),
                max_ks = max(ksstat, na.rm = TRUE),
                mean_runtime = mean(runtime, na.rm = TRUE),
                min_runtime = min(runtime, na.rm = TRUE),
                max_runtime = max(runtime, na.rm = TRUE)
)

repa4 <-  data.frame( nv = resa4$nv, 
                       ncond = resa4$ncond,
                       avgneig = resa4$avgneighbors,
                       avgu2 = resa4$avgu2,
                       ssize = formatC(resa4$ssize, digits = 0, format = "f"),
                       mean_propunique = formatC(100*resa4$mean_propunique, digits = 0, format = "f"),
                       mean_meanz = mformatC(resa4$mean_meanz, digits = 2, format = "f"),
                       minmax_meanz = paste0("(",mformatC(resa4$min_meanz, digits = 2, format = "f"),
                                             ",\\,",
                                             mformatC(resa4$max_meanz, digits = 2, format = "f"),")"),
                       mean_sdz = formatC(resa4$mean_sdz, digits = 2, format = "f"),
                       minmax_sdz = paste0("(",formatC(resa4$min_sdz, digits = 2, format = "f"),
                                           ",\\,",
                                           formatC(resa4$max_sdz, digits = 2, format = "f"),")"),
                       mean_cordiff = mformatC(resa4$mean_cordiff, digits = 2, format = "f"),
                       mean_ks = formatC(resa4$mean_ks, digits = 2, format = "f"),
                       minmax_ks = paste0("(",formatC(resa4$min_ks, digits = 2, format = "f"),
                                          ",\\.",
                                          formatC(resa4$max_ks, digits = 2, format = "f"),")"),
                       mean_runtime = formatC(resa4$mean_runtime, digits = 1, format = "f"),
                       minmax_runtime = paste0("(",formatC(resa4$min_runtime, digits = 2, format = "f"),
                                               ",\\,",
                                               formatC(resa4$max_runtime, digits = 2, format = "f"),")")
)


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

latextable(cbind(LETTERS[1:5], unique(repa4[,(1:4)])))

latextable(cbind(rep(LETTERS[1:5],each = 4), 
                 subset(repa4, select = c(ssize, mean_propunique, mean_meanz,
                                            minmax_meanz, mean_sdz,  
                                            minmax_sdz, mean_ks, 
                                            mean_cordiff)) ))

