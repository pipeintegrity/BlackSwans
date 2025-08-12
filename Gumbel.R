##fitting a gumbel distribution to pressure data
library(extRemes)
library(evd)
library(scales)
library(patchwork)
library(ggplot2)
library(dplyr)

dat <- rgumbel(1000,1.5,3.0 ) #generate some fake Gumbel data

#fit gumbel distribution to data - pretending we don't know the loc & scale used
#Real world you don't know the parameters ahead of time
fit <- fevd(dat,type = "Gumbel")

par <- fit$results$par #save out the fitted parameters to use

hist(dat,probability = T, col='steelblue2', ylim=c(0,0.13)) #create a histogram

curve(dgumbel(x, par[1],par[2]), add=T, col='red',lwd=2,n=301 ) #plot the curve over the histo


n = 60
N = rep(1:15,3)
x=1
m <- rep(1:3,each=15)
#probability of exceeding the m_th highest observations, x times in N future trials based on n observations in past
w = function(N, m)
  ncol(combn(n, m)) * m * ncol(combn(N, x)) / ((N + n) * ncol(combn(N + n -
                                                                      1, m + x - 1)))

pw <- tibble(p = map2_dbl(N, m, .f = w)) %>%
  bind_cols(N = N, m = m)


one <- pw %>% ggplot(aes(N, p))+
  geom_line(aes(col=factor(m)),lwd=1)+
  theme_bw(14,"serif")+
  labs(title = "Probability of One Exceedance in N Future Years",
       subtitle = "Based on 30 Years Observed Data",
       y="Probability",
       color="Event Ranking")


match(max(pw), pw)
a1 = 0.75
n=60
m <- 1


f <- function(N)
  factorial(n) * factorial(N + n - m) / (factorial(N + n) * factorial(n - m)) -
  1 + a1


uniroot(f,interval = c(1,74))


PS=52000
#L=5
D=30
t=0.365
d = 0.4*t


f2 <-  function(L)
  - PS * d * (0.85 * d / t - 1) * (
    -0.56897778125 * L ^ 2 * d * (0.0107569721115538 * L ^ 2 * (D * t) ^ (-2.0) - (D * t) ^ (-1.0)) ^ 2 * (-0.003375 * L ^ 4 * (D * t) ^ (-2.0) + 0.6275 * L ^ 2 * (D * t) ^ (-1.0) + 1) ^ (-3.0) / (t * (
      0.85 * d * (-0.003375 * L ^ 4 * (D * t) ^ (-2.0) + 0.6275 * L ^ 2 * (D * t) ^ (-1.0) + 1) ^ (-0.5) / t - 1
    )) + 0.85 * L ^ 2 * (0.00675 * L ^ 2 * (D * t) ^ (-2.0) - 0.6275 * (D * t) ^ (-1.0)) * (0.02025 * L ^ 2 * (D * t) ^ (-2.0) - 1.8825 * (D * t) ^ (-1.0)) * (-0.003375 * L ^ 4 * (D * t) ^ (-2.0) + 0.6275 * L ^ 2 * (D * t) ^ (-1.0) + 1) ^ (-2.5) + 0.85 * (0.02025 * L ^ 2 * (D * t) ^ (-2.0) - 0.6275 * (D * t) ^ (-1.0)) * (-0.003375 * L ^ 4 * (D * t) ^ (-2.0) + 0.6275 * L ^ 2 * (D * t) ^ (-1.0) + 1) ^ (-1.5)
  ) / (t * (0.85 * d * (
    -0.003375 * L ^ 4 * (D * t) ^ (-2.0) + 0.6275 * L ^ 2 * (D * t) ^ (-1.0) + 1
  ) ^ (-0.5) / t - 1) ^ 2)

uniroot(f2,c(1,3))

## looking at just m=1
n = 10
N = 5
x = 0:N

#probability of exceeding the m_th highest observations, x times in N future trials based on n observations in past
w <-  function(x)  ncol(combn(n, m)) * m * ncol(combn(N, x)) / ((N + n) * ncol(combn(N + n - 1, m + x - 1)))
# df <- data.frame(N)
# df$w = ncol(combn(n,m))*m*ncol(combn(N,x))/((N + n)*ncol(combn(N+n-1,m+x-1)))
m <- 1
pw1 <- sapply(x, w)
pw1 <- unlist(pw1)
pw <- data.frame(pw1, N,x)

pw %>%
  ggplot(aes(x, pw1)) +
  geom_col(aes(col = factor(m)), lwd = 1) +
  theme_bw(14, "serif") +
  labs(
    title = "Probability of One Exceedance in N Future Years",
    subtitle = paste("Based on ", n, "Years Observed Data"),
    y = "Probability",
    x = "Number of Exceedances"
  ) +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = pretty_breaks())
sum(pw$pw1[1:10]) / sum(pw$pw1[1:15])

pw$pw1[15]

