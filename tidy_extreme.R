library(tidyverse)
library(ggpubr)
it <- 1:1000
n <- rep(0,max(it))

df2 <- bind_cols(n=n, it =it)

df <- map(n, ~ rnorm(n = 1000, mean = .x, 1)) %>%
  tibble() %>%
  bind_cols(df2) %>%
  unnest(cols = 1) %>%
  rename(value = ".")

max <- df %>%
  group_by(it) %>%
  summarise(max=max(value),
            ave=mean(value),.groups = "drop")


norm_dist <- df %>%
  ggplot(aes(value)) +
  geom_density(col = 'grey',
               aes(group = it),
               alpha = 0.005) +
  # geom_histogram(
  #   aes(x = value, y = ..density..),
  #   col = 'black',
  #   fill = "white",
  #   alpha = 0.1
  # )+
  geom_point(data = max,
             aes(x = max, y = 0),
             col = 'red',
             shape = 2) +
  geom_point(data = max,
             aes(x = ave, y = 0),
             col = 'blue',
             shape = 2, alpha=0.1) +
  theme_minimal(17)+
  labs(x="Normal Distribution", y=NULL)


norm_dist

gumbel <- max %>%
  # filter(max>2.5) %>%
  ggplot(aes(max))+
  stat_density(geom="line",col='red',lwd=1, alpha=0.5)+
  theme_minimal()+
  coord_cartesian(xlim=c(-5,5))
  # geom_hline(col="grey50", yintercept = 0, lwd=1)
  # scale_x_continuous(limits = c(-5,5),
  # expand_limits(x=c(-5,5))



ggarrange(norm_dist,gumbel,nrow = 2)
