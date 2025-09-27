# Simulemos un experimento de Bernoulli ####
## Cómo evoluciona la media con n ####
library(tidyverse)
library(latex2exp)

ggplot(data = data.frame(x = c(1.125, 1.875)), aes(x)) +
  stat_function(fun = dnorm, args = list(mean = 1.5, sd = .5/sqrt(30)), color = "steelblue", size = 1) +
  labs(x = "T30",
       y = "Densidad") +
  theme_minimal() +
  theme(legend.position = "top", legend.title = element_blank())

ggplot(data = data.frame(x = c(1.125, 1.875)), aes(x)) +
  stat_function(fun = dnorm, args = list(mean = 1.5, sd = .5/sqrt(30)), color = "steelblue", size = 1) +
  geom_vline(xintercept = 1.5, color = "darkorange", linetype = "dashed") +
  geom_vline(xintercept = 1.59, color = "darkred", linetype = "dashed") +
  labs(x = "T30",
       y = "Densidad") +
  theme_minimal()

  ggplot(data = data.frame(x = c(-3, 3)), aes(x))+
  stat_function(mapping = aes(fill = "Area 02"), geom = "area", fun = function(x) ifelse(abs(x) > 1.6, dnorm(x), NA)) +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), color = "steelblue", size = 1) +
  geom_vline(xintercept = c(-1.6, 1.6), color = "darkorange", linetype = "dashed") +
  labs(x = TeX(r"($\bar{X}$)"),
       y = "Densidad") +
  theme_linedraw() +
  theme(legend.position = "none", panel.background = element_rect(fill = "#EEEEEE"), 
        plot.background = element_rect(fill = "#EEEEEE"))

ggplot(data = data.frame(x = c(1.125, 1.875)), aes(x)) +
 stat_function(fun = dnorm, args = list(mean = 1.5, sd = .5/sqrt(30)), color = "steelblue", size = 1) +
  geom_vline(xintercept = c(1.5-.09, 1.59), color = "darkorange", linetype = "dashed") +
  labs(x = TeX(r"($\bar{X}$)"),
       y = "Densidad") +
  theme_linedraw() +
  theme(legend.position = "none")

2*pnorm(1.41, mean = 1.5, sd = .5/sqrt(30))

ggplot(data = data.frame(x = c(1.125, 1.875)), aes(x)) +
  stat_function(fun = dnorm, args = list(mean = 1.5, sd = .5/sqrt(200)), color = "steelblue", size = 1) +
  geom_vline(xintercept = c(1.5-.09, 1.59), color = "darkorange", linetype = "dashed") +
  labs(x = TeX(r"($\bar{X}$)"),
       y = "Densidad") +
  theme_linedraw() +
  theme(legend.position = "none")

2*pnorm(1.41, mean = 1.5, sd = .5/sqrt(200))

set.seed(123)
x <- rnorm(30, mean = 1.75, sd = .5)
mean(x)
sd(x)/sqrt(30)
t_obs <- (mean(x) - 1.5)/(sd(x)/sqrt(30))
pt(mean(x), df = 29, lower.tail = FALSE)*2
t.test(x, mu = 1.5, alternative = "two.sided")
pt(t_obs, df = 29, lower.tail = FALSE)*2

ggplot(data = data.frame(x = c(-4,4)), aes(x)) +
  stat_function(mapping = aes(fill = "Area 02"), geom = "area", fun = function(x) ifelse(abs(x) > t_obs, dnorm(x), NA)) +
  stat_function(fun = dt, args = list(df = 29), color = "steelblue", size = 1) +
  geom_vline(xintercept = c(-t_obs, t_obs), color = "darkorange", linetype = "dashed") +
  labs(x = TeX(r"($\frac{\bar{T}_{30} - \mu_0}{\sqrt{S^2/30}} $)"),
       y = "Densidad") +
  theme_linedraw() +
  theme(legend.position = "none")

