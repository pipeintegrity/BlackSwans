library(ggplot2)
library(dplyr)
library(readr)
library(scales)

n = 45
N = 1:n
x=1

#probability of exceeding the m_th highest observations, x times in N future trials based on n observations in past
w =function(N) ncol(combn(n,m))*m*ncol(combn(N,x))/((N + n)*ncol(combn(N+n-1,m+x-1)))
# df <- data.frame(N)
# df$w = ncol(combn(n,m))*m*ncol(combn(N,x))/((N + n)*ncol(combn(N+n-1,m+x-1)))
m <- 1
pw1 <- sapply(N, w)
pw1 <- data.frame(unlist(pw1))
# m <- 2
# pw2 <- sapply(N, w)
# pw2 <- unlist(pw2)
# m <- 3
# pw3 <- sapply(N, w)
# pw3 <- unlist(pw3)

pw <- data.frame(cbind(pw1,N))
names(pw) <- c("pw","N")
# library(reshape2)
# pw <- melt(pw)
#
# pw$m <- c(rep(1,length(N)), rep(2,length(N)), rep(3,length(N)))
# pw$N <- rep(N,3)
#plot(pw ~ N, type="l")

pw %>%
  ggplot(aes(N, pw))+
  geom_line(aes(col=factor(m)),lwd=1)+
  theme_bw(14,"serif")+
  labs(title = "Probability of One Exceedance in N Future Years",
       subtitle = "Based on 30 Years Observed Data",
       y="Probability",
       color="Event Ranking")
