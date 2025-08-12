#This is a MC simulation for CoF for a pipeline incident###
library(MASS)
library(tidyverse)
library(evd)
theme_set(theme_bw(16,"serif"))


all_combine <- read_csv("gt_incidents.csv") %>% clean_names() %>% rename(cof=co_f)


sigser <-
  all_combine %>%
  filter(significant == "YES" | serious == "YES") %>%
  mutate(cost_reported = ifelse(cost_reported == 0,
                                5e4,
                                cost_reported))


nsim=1e6

pd_inc <- fitdistr(sigser$cost_reported,"lognormal")

pd <- rlnorm(nsim,pd_inc[[1]][[1]],pd_inc[[1]][[2]])

names(pd) <- "pd"

minj <- mean(sigser$injuries)
inj <- rpois(nsim, minj)
inj <- as_tibble(inj)
names(inj) <- "inj"


mfat <- mean(sigser$fatalities)
fat <- rpois(nsim, mfat)
fat <- as_tibble(fat)
names(fat) <- "fat"

cof <- bind_cols(fat, inj, pd) %>%
  mutate(fatal = ifelse(fat > 0 |inj>0, "Yes", "No"),
         cof = fat * 10e6 + inj * 10e6/3 + pd)

n=0:6
cof_tib <- tibble(n=0:6, inj=dpois(n, minj), fat=dpois(n, mfat))

ggplot(cof, aes(cof))+
geom_histogram(aes(y=..density..),fill='dodgerblue3',col='black')+
  theme_bw(16,"serif")+
  scale_x_log10()+
  facet_wrap(~fatal, scales ="free")+
  labs(title="Consequence of Failure",
       x="CoF ($) - log scale",
       y=NULL)+
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

wrap_fat <- cof %>%
  group_by(fat) %>%
  summarise(prop = n() / nsim) %>%
  mutate(name = c(0:2, "3+"))

wrap_inj <- cof %>%
  group_by(inj) %>%
  summarise(prop=n()/nsim) %>%
  mutate(name=c(0:3,"4+",5))

wrap_fat %>%
  filter(fat > 0, fat<4) %>%
  ggplot(aes(reorder(name,-fat), prop)) +
  geom_col(fill = 'coral3') +
  geom_label(aes(
    label = paste(round(prop * 100, 3), "%"),
    vjust = 0,
    hjust = -0.1
  )) +
  scale_x_discrete() +
  coord_flip() +
  ylim(0, 0.05) +
  labs(title = "Estimated Proportion of Incidents With Fatalities",
       x = "Number of Fatalities",
       y = "Proportion of Incidents")



wrap_inj %>%
  filter(inj > 0, inj < 5) %>%
  ggplot(aes(x = reorder(name, -inj), y = prop)) +
  geom_col(fill = 'aquamarine3') +
  geom_label(aes(
    label = paste(round(prop * 100, 3), "%"),
    vjust = 0,
    hjust = -0.1
  )) +
  scale_x_discrete() +
  coord_flip() +
  ylim(0, 0.2) +
  labs(title = "Estimated Proportion of Incidents With Injuries",
       x = "Number of Injuries",
       y = "Proportion of Incidents")
