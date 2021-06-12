
# prob of exccedances -----------------------------------------------------
library(tidyverse)

N <- 2:200
n <- 25
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
  pivot_longer(cols = a:a1) %>%
  ggplot(aes(N,value))+
  geom_line(aes(col=name))+
  scale_x_log10()+
  scale_y_log10()+
  theme_minimal()+
  geom_hline(yintercept = 0.5,  col='red', lty=2,lwd=0.9)


