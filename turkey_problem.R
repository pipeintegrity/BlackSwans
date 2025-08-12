## Turkey Problem
library(tidyverse)

days <- c(seq(0, 1000, 15), 1000)
noise <- runif(length(days)) * 10
satisfy <- seq(100, 225, length.out = length(days)) + noise
plot(satisfy ~ days, type = "l")

thanksgiving <- bind_cols(days = c(1000, 1000),
                          satisfaction = c(last(satisfy), 0))
turkey <- bind_cols(days = days,
                    satisfaction = satisfy)

turkey %>%
  ggplot(aes(days, satisfaction)) +
  geom_line(col = 'aquamarine3',
            lwd = 1.2) +
  geom_line(data = thanksgiving,
            aes(days, satisfaction),
            col = 'red',
            lwd = 1.2) +
  theme_bw(14, "serif") +
  labs(title = "The Turkey Problem",
       x = "Days",
       y = "Satisfaction") +
  geom_segment(
    x = 800,
    xend = 995,
    y = 75,
    yend = 75,
    arrow = arrow(length = unit(0.1, "inches"),
                  type = "closed")
  ) +
  annotate(
    geom = "text",
    label = "Surprise!",
    x = 730,
    y = 75,
    size = 5
  )
