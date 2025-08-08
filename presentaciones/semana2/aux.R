pacman::p_load(tidyverse, lubridate, readxl, janitor, glue, here)

set.seed(121)
data <- tibble(x = 200 + round(rnorm(10),2)*100)

escala <- tibble(x = seq(100, 400, 50))
data %>%
  ggplot(aes(x, y = 0)) +
  geom_text(data = escala, aes(label = x), vjust = 1.2, size = 4) +
  geom_hline(yintercept = 0, color = "black") +
  geom_jitter(height = .1, pch = 21, fill = "darkorange", alpha  = .5, size = 3, color = "black") +
  coord_cartesian(clip = "off") +
  theme_void()

ggsave("presentaciones/semana2/fig1.png", width = 8, height = 1)

data %>%
  ggplot(aes(x, y = 0)) +
  geom_text(data = escala, aes(label = x), vjust = 1.2, size = 4) +
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = mean(data$x), color = "black", linetype  = "dashed") +
  geom_jitter(height = .1, pch = 21, fill = "darkorange", alpha  = .5, size = 3, color = "black") +
  coord_cartesian(clip = "off") +
  theme_void()

ggsave("presentaciones/semana2/fig2.png", width = 8, height = 1)

x <- c(97, 170, 173, 180, 213, 215, 236, 267, 301, 407)
mean(x)
