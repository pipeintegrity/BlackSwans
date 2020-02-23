library(tidyverse)
library(extRemes)

all_combine <- read.csv("D:/Documents/Analysis/PPIM2019/all_combine.csv")
all_combine <- all_combine <- all_combine %>% mutate(CoF=INJURE*10e6*0.331+FATAL*10e6+TOTAL_COST_IN84*2.5)
all_combine <- all_combine %>% filter(CoF>1e5)
fit <- fevd(all_combine$CoF,threshold = 1e5)


rm(r)
r=1
#r=mean(revd(15,fit$results[1]$par[[1]],fit$results[1]$par[[2]],fit$results[1]$par[[3]]))
#generate some averages
for (i in 1:10) {
  r[i] =mean(revd(20,fit$results[1]$par[[1]],fit$results[1]$par[[2]],fit$results[1]$par[[3]]))
}

r <- tibble(r)
r$i <- 1:i
r

barplot(height = r$r, names.arg = r$i)
