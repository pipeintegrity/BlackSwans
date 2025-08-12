library(dplyr)
library(ggplot2)
#library(readr)
library(MASS)
library(ggpubr)
library(data.table)

setwd("~/PPIM2019")
incidents <- fread("all_combine.csv")
incidents <- incidents[is.na(Year)==FALSE]
#incidents <- incidents[1:2716,] #Drop 40 NAs at the end
#incidents <- incidents %>% filter(is.na(Year)==FALSE)
incidents[,inflation_adjstd :=incidents$TOTAL_COST_IN84*2.49] #Per CPI calculator adjusted 1984 to 2019
incidents[, CoF:=inflation_adjstd+INJURE*0.33*10e6+FATAL*10e6]

#incidents <- tibble::tibble(incidents)

#incidents$CoF <- incidents$inflation_adjstd+incidents$INJURE*0.33*10e6+incidents$FATAL*10e6
#incidents %>% mutate(CoF=inflation_adjstd+INJURE*0.33*10e6+FATAL*10e6)

setkey(incidents,SIGNIFICANT)
incidents <- incidents["YES"]

incidents <- incidents[is.na(CoF)==FALSE]

#serious <- filter(incidents,SERIOUS=="YES")


#Exporatory data analysis ####
fathist <- ggplot(incidents, aes(FATAL))+
  geom_histogram(fill='firebrick2', col='black')+
  theme_bw(16,"serif")+
  theme(plot.margin = margin(0.5,0.5,0.5,0.5,"cm"))+
  labs(x= "Fatalities", title="Fatalities per Incident Histogram",
       subtitle="PHMSA Incident Data (1986 -2018)", y="Incident Count")+
  ylim(0,50)+
  annotate("text", x=6,y=30,label="San Bruno = 8 Fatalities\nCarlsbad = 12 Fatalities",size=6)

injhist <- ggplot(incidents, aes(INJURE))+
  geom_bar(fill='firebrick2', col='black',stat = "count")+
  theme_bw(16,"serif")+
  theme(plot.margin = margin(0.5,0.5,0.5,0.5,"cm"))+
  labs(x= "Injuries", title="Injuries per Incident Histogram",
       subtitle="PHMSA Incident Data (1986-2018)",y="Incident Count")+
  scale_x_continuous(breaks = c(seq(0,14,1)),limits = c(0,14))+
  ylim(0,150)+
  annotate("text",x=8,y=100,label="San Bruno = 51 Injuries\nnot shown",size=6)

cofhist <- ggplot(incidents, aes(CoF))+
  geom_histogram(fill='firebrick2', col='black')+scale_x_log10()+
  theme_bw(16,"serif")+
  theme(plot.margin = margin(0.5,0.5,0.5,0.5,"cm"))+
  labs(x= "Consequence of Failure ($) - Log Scale", title="Incident Consequences",
       subtitle="PHMSA Incident Data (1986-2018)",y="Incident Count")+
  geom_vline(xintercept =10e6, lty=2)

pdhist <- ggplot(incidents, aes(inflation_adjstd))+
  geom_histogram(fill='firebrick2', col='black')+
  scale_x_log10()+
  theme_bw(16,"serif")+
  theme(plot.margin = margin(0.5,0.5,0.5,0.5,"cm"))+
  labs(x= "Property Damage ($) - Log Scale", title="Incident Property Damage",
       subtitle="PHMSA Incident Data (1986-2018)",y="Incident Count")

ln_cof <- fitdistr(incidents$CoF,"lognormal")

ggarrange(pdhist, injhist,  fathist, cofhist, nrow = 4)


#fitting an extreme value distribution to the CoF data####
library(extRemes)
library(evd)

threshold <- 9e6
fevd_cof <- fevd(filter(incidents,CoF>threshold)$CoF) #fit evd to CoF data

#histogram with evd distribution curve over it####
ggplot(filter(incidents,CoF>threshold), aes(CoF))+
  geom_histogram(fill='firebrick2', col='black', aes(y=..density..),bins=50)+
  theme_bw(16,"serif")+
  theme(plot.margin = margin(0.5,0.5,0.5,0.5,"cm"), axis.text.y = element_blank(), axis.ticks.y = element_blank())+
  stat_function(fun=dgev, args = list(fevd_cof$result$par[[1]],fevd_cof$result$par[[2]],fevd_cof$result$par[[3]]),col='blue',lwd=1.2,n = 401)+
  labs(x= "Consequence of Failure ($) - Log Scale", title="Incident Consequences for Catastrophic Incidents",subtitle="PHMSA Incident Data (1986-2018)", y=NULL, caption = "Filtered for incidents > $10MM")+
  xlim(threshold,5e7)

qevd(c(0.025, 0.975), fevd_cof$result$par[[1]],fevd_cof$result$par[[2]],fevd_cof$result$par[[3]])/1e6
