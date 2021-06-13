
# prob of exccedances -----------------------------------------------------
library(tidyverse)

N <- 1:100
n <- 30
m <- 1

a <- 1-factorial(n)*factorial(N+n-m)/(factorial(N+n)*factorial(n-m))
a


# For exceedance twice of m ---------------------------------------------

a1 <-
  1 - (factorial(n) * factorial(N + n - m) /
         (factorial(N + n) * factorial(n - m))) *
  ((N + n - m + N * m) / (N + n - m))


# combine data ------------------------------------------------------------

data <- bind_cols(N=N,a=a, a1=a1)

data %>%
  filter(a<=0.999, a1<=0.999) %>%
  rename(Once = a, Twice = a1) %>%
  pivot_longer(cols = Once:Twice) %>%
  ggplot(aes(N,value))+
  geom_line(aes(col=name),lwd=0.9)+
  # scale_x_log10()+
  # scale_y_log10()+
  theme_minimal(14, "serif")+
  labs(title= "Cummulative Probability of Exceedance of Largest Event in N Future Years",
       subtitle = "Based on 30 Observed Years",
       x = "Future Years",
       y = "Probability of Exceedance",
       color = "Exceedance")+
  scale_color_brewer(palette = "Set1")+
  scale_x_continuous(breaks = scales::pretty_breaks())
  # geom_hline(yintercept = 0.5,  col='red', lty=2,lwd=0.9)


