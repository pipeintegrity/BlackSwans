library(tidyverse)
library(rriskDistributions)
library(evd)
library(tidymodels)

pop <- tibble(p = rnorm(1e5))


samps <- pop %>% rep_sample_n(size = 5,replace = TRUE,reps = 5e4) %>% 
  summarise(mp = min(p))

samps %>% 
  ungroup() %>% 
  mutate(y = ifelse(mp <= qnorm(0.2),1,0)) %>% 
  summarise(q = sum(y/5e4))

samps10 <- pop %>% rep_sample_n(size = 10,replace = TRUE,reps = 5e4) %>% 
  summarise(mp = sort(p)[2])

samps10 %>% 
  ungroup() %>% 
  mutate(y = ifelse(mp <= qnorm(0.2),1,0)) %>% 
  summarise(q = sum(y/5e4))


samps15 <- pop %>% 
  rep_sample_n(size = ,replace = TRUE,reps = 5e4) 

samps15 %>% 
  ungroup() %>% 
  mutate(y = ifelse(mp <= qnorm(0.1),1,0)) %>% 
  summarise(q = sum(y/5e4))
