pacman::p_load(tidyverse, lubridate, readxl, janitor, glue, here)
score = 120
m = 178
std = 30

funcShaded_bottom <- function(x, lower_bound) {
  y = dnorm(x, mean = m, sd = std)
  y[x > lower_bound] <- NA
  return(y)
}

funcShaded_top <- function(x, lower_bound) {
  y = dnorm(x, mean = m, sd = std)
  y[x < lower_bound] <- NA
  return(y)
}

ggplot(data.frame(x = c(m-3*std, m+3*std)), aes(x = x)) + 
  stat_function(fun = dnorm, args = list(mean = m, sd = std)) + 
  stat_function(fun = funcShaded_top, args = list(lower_bound = 120), 
                geom = "area", fill = "#84CA72", alpha = .2) +
  stat_function(fun = funcShaded_bottom, args = list(lower_bound = 160), 
                geom = "area", fill = "steelblue", alpha = .2) +
  scale_x_continuous(name = "Score", breaks = seq(m-3*std, m+3*std, std)) +
  theme_minimal(base_size = 16) +
  labs(x = "Altura", y = NULL) +
  theme(
        panel.background= element_rect(fill = "#EEEEEE", color = "#EEEEEE"),
        plot.background= element_rect(fill = "#EEEEEE", color = "#EEEEEE"))
