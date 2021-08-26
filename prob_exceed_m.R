#How many trials N have to be made in order that there is a given probability a
#for the m^th largest among n values to be exceeded at least once

library(tidyverse)
n <- 20
hi <- n*10
N <- 1:hi
m <- 1


a <- 1-((factorial(n)*factorial(N+n-m))/(factorial(N+n)*factorial(n-m)))
a

plot(a ~ N, type="l")

dataf <- bind_cols(a1 = a, N1 = N)

dataf %>%
  filter(a<=0.999) %>%
  ggplot(aes(N1,a1))+
  geom_line(col='red')+
  theme_minimal()+
  scale_y_continuous(breaks = scales::pretty_breaks())+
  scale_x_continuous(breaks = seq(0, hi, by=20))


# Distribution of Exceedances ---------------------------------------------
#probability of exceedance given n,m,N
# for x number of exceedances

n <- 30 # yrs. observation
N <- 10 # future yrs.
x <- 1:4 # number exceedances
m <- 1 # rank, 1 = largest

w <- choose(n,m)*m*choose(N,x)/((N+n)*choose(N+n-1, x+m-1))
sum(w)
