pacman::p_load(tidyverse, lubridate, readxl, janitor, glue, here)

set.seed(121)
data <- tibble(x = 200 + round(rnorm(10),2)*100)

escala <- tibble(x = seq(100, 400, 50))
data %>%
  ggplot(aes(x, y = 0)) +
  geom_text(data = escala, aes(label = x), vjust = 1.2, size = 4) +
  geom_hline(yintercept = 0, color = "black") +
  geom_jitter(height = .1, width = 0, pch = 21, fill = "darkorange", alpha  = .5, size = 3, color = "black") +
  coord_cartesian(clip = "off") +
  theme_void()

ggsave("presentaciones/semana2/fig1.png", width = 8, height = 1)

data %>%
  ggplot(aes(x, y = 0)) +
  geom_text(data = escala, aes(label = x), vjust = 1.2, size = 4) +
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = mean(data$x), color = "black", linetype  = "dashed") +
  geom_jitter(height = .1, width = 0, pch = 21, fill = "darkorange", alpha  = .5, size = 3, color = "black") +
  coord_cartesian(clip = "off") +
  theme_void()

ggsave("presentaciones/semana2/fig2.png", width = 8, height = 1)

x <- c(174, 211, 213, 192, 129, 362, 230, 194,  90, 291)
mean(x)
median(x)

# Para calcular la mediana
data <- data %>%
  bind_rows(tibble(x = 3267))

escala <- tibble(x = seq(0, 4000, 500))
data %>%
  ggplot(aes(x, y = 0)) +
  geom_text(data = escala, aes(label = x), vjust = 1.2, size = 4) +
  geom_hline(yintercept = 0, color = "black") +
  geom_jitter(height = .1, width = 0, pch = 21, fill = "darkorange", alpha  = .5, size = 3, color = "black") +
  coord_cartesian(clip = "off") +
  theme_void()

ggsave("presentaciones/semana2/fig3.png", width = 8, height = 1)

data %>%
  ggplot(aes(x, y = 0)) +
  geom_text(data = escala, aes(label = x), vjust = 1.2, size = 4) +
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = mean(data$x), color = "black", linetype  = "dashed") +
  geom_jitter(height = .1, width = 0, pch = 21, fill = "darkorange", alpha  = .5, size = 3, color = "black") +
  coord_cartesian(clip = "off") +
  theme_void()

ggsave("presentaciones/semana2/fig4.png", width = 8, height = 1)

x <- c(90, 129, 174, 192, 194, 211, 213, 230, 291, 362, 3267)
mean(x)

data %>%
  ggplot(aes(x, y = 0)) +
  geom_text(data = escala, aes(label = x), vjust = 1.2, size = 4) +
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = median(data$x), color = "black", linetype  = "dashed") +
  geom_jitter(height = .1, width = 0, pch = 21, fill = "darkorange", alpha  = .5, size = 3, color = "black") +
  coord_cartesian(clip = "off") +
  theme_void()

ggsave("presentaciones/semana2/fig5.png", width = 8, height = 1)

median(x)

# Para calcular la mediana
data <- data %>%
  bind_rows(tibble(x = 3244))

data %>%
  ggplot(aes(x, y = 0)) +
  geom_text(data = escala, aes(label = x), vjust = 1.2, size = 4) +
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = median(data$x), color = "black", linetype  = "dashed") +
  geom_jitter(height = .1, width = 0, pch = 21, fill = "darkorange", alpha  = .5, size = 3, color = "black") +
  coord_cartesian(clip = "off") +
  theme_void()

ggsave("presentaciones/semana2/fig6.png", width = 8, height = 1)
median(data$x)

set.seed(121)
data <- data %>%
  bind_rows(tibble(x = round(rnorm(8, 3300, 100))))

data %>%
  ggplot(aes(x, y = 0)) +
  geom_text(data = escala, aes(label = x), vjust = 1.2, size = 4) +
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = median(data$x), color = "black", linetype  = "dashed") +
  geom_jitter(height = .1, width = 0, pch = 21, fill = "darkorange", alpha  = .5, size = 3, color = "black") +
  coord_cartesian(clip = "off") +
  theme_void()
ggsave("presentaciones/semana2/fig7.png", width = 8, height = 1)

median(data$x)
sort(data$x)

data <- data %>%
  bind_rows(tibble(x = 3500))

data %>%
  ggplot(aes(x, y = 0)) +
  geom_text(data = escala, aes(label = x), vjust = 1.2, size = 4) +
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = median(data$x), color = "black", linetype  = "dashed") +
  geom_jitter(height = .1, width = 0, pch = 21, fill = "darkorange", alpha  = .5, size = 3, color = "black") +
  coord_cartesian(clip = "off") +
  theme_void()
ggsave("presentaciones/semana2/fig8.png", width = 8, height = 1)

median(data$x)
sort(data$x)

# Datos de altura
library(modelr)

mean_height <- mean(heights %>% pull(height))*2.54
median_height <- median(heights %>% pull(height))*2.54

heights %>%
  ggplot(aes(x = height*2.54, y = 1)) +
  geom_jitter(height = .1, width = 0,  pch = 21, fill = "darkorange", alpha  = .5, size = 3, color = "black") +
  geom_vline(xintercept = mean_height, color = "black", linetype  = "dashed", size = 1) +
  geom_vline(xintercept = median_height, color = "red", linetype  = "dashed", size = 1) +
  coord_cartesian(clip = "off") +
  scale_y_continuous(breaks=c()) +
  labs(x = "Altura (cm)", y = "") +
  theme_minimal()
ggsave("presentaciones/semana2/fig9.png", width = 12, height = 3)

mean_income <- mean(heights %>% filter(income < 340000) %>% pull(income))
median_income <- median(heights %>% filter(income < 340000) %>% pull(income))

heights %>%
  filter(income < 340000) %>%
  ggplot(aes(x = income, y = 1)) +
  geom_jitter(height = .1, width = 0, pch = 21, fill = "darkorange", alpha  = .5, size = 3, color = "black") +
  geom_vline(xintercept = mean_income, color = "black", linetype  = "dashed", size = 1) +
  geom_vline(xintercept = median_income, color = "red", linetype  = "dashed", size = 1) +
  coord_cartesian(clip = "off") +
  scale_y_continuous(breaks=c()) +
  labs(x = "Ingreso (USD por año)", y = "") +
  theme_minimal()
ggsave("presentaciones/semana2/fig10.png", width = 12, height = 3)


# Varianza
set.seed(121)
data_chica <- tibble(x = round(rnorm(10,300,40)))
data_grande <- tibble(x =round(rnorm(10,300,100)))

data_chica %>%
  ggplot(aes(x, y = 1)) +
  geom_jitter(height = .4, width = 0, pch = 21, fill = "darkorange", alpha  = .5, size = 3, color = "black") +
  geom_jitter(data = data_grande, aes(y=0), height = .4, width = 0, pch = 21, fill = "darkred", alpha  = .5, size = 3, color = "black") +
  coord_cartesian(clip = "off") +
  coord_cartesian(clip = "off") +
  scale_y_continuous(breaks=c()) +
  labs(x = "Tiempo de permanencia en ms.", y = "") +
  theme_minimal()
ggsave("presentaciones/semana2/fig11.png", width = 12, height = 4)

data_chica %>%
  ggplot(aes(x, y = 1)) +
  geom_jitter(height = .4, width = 0, pch = 21, fill = "darkorange", alpha  = .5, size = 3, color = "black") +
  geom_vline(xintercept = mean(data_chica$x), color = "black", linetype  = "dashed", size = 1) +
  coord_cartesian(clip = "off") +
  coord_cartesian(clip = "off") +
  scale_y_continuous(breaks=c()) +
  labs(x = "Tiempo de permanencia en ms.", y = "") +
  theme_minimal()
ggsave("presentaciones/semana2/fig12.png", width = 12, height = 3)

# Covarianza
set.seed(121)
data <- tibble(tr = 200 + round(rnorm(10),2)*100,
               edad = round(10 + tr/10 + rnorm(10,0,10)))

data %>%
  ggplot(aes(x = edad, y = tr)) +
  geom_point( pch = 21, fill = "darkorange", alpha  = .5, size = 5, color = "black") +
  coord_cartesian(clip = "off") +
  labs(x = "Edad en años", y = "Tiempo de permanencia en ms.") +
  theme_bw()

ggsave("presentaciones/semana2/fig13.png", width = 6, height = 6)

data %>%
  ggplot(aes(x = edad, y = tr)) +
  geom_point( pch = 21, fill = "darkorange", alpha  = .5, size = 5, color = "black") +
  geom_vline(xintercept = mean(data$edad), color = "black", linetype  = "dashed", size = 1) +
  geom_hline(yintercept = mean(data$tr), color = "black", linetype  = "dashed", size = 1) +
  coord_cartesian(clip = "off") +
  labs(x = "Edad en años", y = "Tiempo de permanencia en ms.") +
  theme_bw()

ggsave("presentaciones/semana2/fig14.png", width = 6, height = 6)

mean(data$edad)
mean(data$tr)

cov(data$edad, data$tr)
cor(data$edad, data$tr)
