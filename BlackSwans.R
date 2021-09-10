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


# example plot of male height ---------------------------------------------

mu <- 70
sdev <- 4

height <- rnorm(1e3, mu, sdev)

hist(height)

pnorm(91, mu, sdev)

hw <- read_csv("01_heights_weights_genders.csv")

stats <- hw %>% filter(Gender=="Male") %>% summarise(mu = mean(Height), std = sd(Height))

hw %>%
  filter(Gender == "Male") %>%
  ggplot(aes(Height)) +
  geom_histogram(
    aes(y = ..density..),
    fill = 'royalblue2',
    col = 'black',
    alpha = 0.5,
    bins = 40
  ) +
  stat_function(
    fun = dnorm,
    args = list(stats$mu, stats$std),
    col = 'red',
    lwd = 1.1,
    n=201
  ) +
  labs(title = "Survey of 5,000 Male Heights",
          x = "Height (in.)") +
  theme_bw(18)

# EVD fit to log of data --------------------------------------------------

incidents <- read_csv("gt_all.csv")

maxi <- incidents %>%
  group_by(Year) %>%
  summarise(maxi = max(CoF))

fit_inc <- fevd(incidents$CoF,threshold = 1e7)

threshold <- 10e6
fevd_cof <- fevd(filter(incidents, CoF > threshold)$CoF) # fit evd to CoF data
incfil <- filter(incidents, incidents$SIGNIFICANT == "YES")
sigcof <- fevd(incidents[incidents$SIGNIFICANT == "YES"]$CoF) # fit evd to CoF data
qevd(
  c(0.025, 0.975),
  fevd_cof$results[[1]][[1]],
  fevd_cof$results[[1]][[2]],
  fevd_cof$results[[1]][[3]]
) / 1e6
qevd(
  c(0.025, 0.975),
  sigcof$results[[1]][[1]],
  sigcof$results[[1]][[2]],
  sigcof$results[[1]][[3]]
) / 1e6

#histogram with evd distribution curve over it####
incidents %>%
  filter(Significant=="YES",
         CoF>1e7) %>%
ggplot(aes(CoF)) +
  geom_histogram(
    fill = "firebrick2",
    col = "black",
    aes(y = ..density..),
    bins = 40
  ) +
  theme_bw(16, "serif") +
  scale_x_log10() +
  theme(
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  stat_function(
    fun = evd::dgev,
    args = list(
      fevd_cof$result$par[[1]],
      fevd_cof$result$par[[2]],
      fevd_cof$result$par[[3]]
    ),
    col = "blue", lwd = 1.2, n = 401
  ) +
  labs(
    x = "Consequence of Failure ($) - Log Scale",
    title = "Extreme Value Distribution",
    subtitle = "PHMSA Incident Data (1986-2018)",
    y = NULL
  )



