library(tidyverse)

data <- read_csv("Semana9/data/dat-musicians.csv")
head(data)

data <- data %>%
  select(c("sex", "age", "country", "years_of_education", "group", "melody_span", "spatial_span"))

# Años de educación musical y memoria musical ####
data %>% ggplot(aes(x = melody_span)) +
  geom_histogram(color = "white", fill = "steelblue") +
  labs(x = "Memoria musical (número de notas)", y = "Cuenta") +
  theme_minimal()

data %>% ggplot(aes(x = years_of_education, y = melody_span)) +
  geom_point(color = "steelblue") + 
  geom_smooth(color = "darkorange", method = "lm", se = FALSE) +
  labs(x = "Años de educación musical", y = "Memoria musical (número de notas)") +
  theme_minimal()

lm1 <- lm(melody_span ~ years_of_education, data = data)
summary(lm1)

# Sexo y memoria espacial ####
str(data)
table(data$sex)

data_filtrada <- data %>% filter(sex != "nonbinary")
lm2 <- lm(spatial_span ~ sex, data = data_filtrada)
summary(lm2)

data_filtrada %>% 
  ggplot(aes(x = sex, y = spatial_span)) +
  geom_jitter(width = .2, color = "steelblue") + 
  stat_summary(fun = mean, geom = "point", color = "darkorange", size = 4) +
  geom_smooth(color = "darkorange", method = "lm", se = FALSE) +
  labs(x = "Sexo", y = "Memoria espacial (número de cajas)") +
  theme_minimal()

data_mas_filtrada <- data_filtrada %>% filter(spatial_span <20)

data_mas_filtrada %>% 
  ggplot(aes(x = sex, y = spatial_span)) +
  geom_jitter(width = .2, color = "steelblue") + 
  stat_summary(fun = mean, geom = "point", color = "darkorange", size = 4) +
  geom_smooth(color = "darkorange", method = "lm", se = FALSE) +
  labs(x = "Sexo", y = "Memoria espacial (número de cajas)") +  
  theme_minimal()

lm3 <- lm(spatial_span ~ sex, data = data_mas_filtrada)
summary(lm3)

# Pais y memoria espacial ####
# Comparemos Finlandia e italia

data_paises <- data %>% filter(country %in% c("Finland", "Germany", "Italy"))
data_paises %>% 
  ggplot(aes(x = country, y = spatial_span)) +
  geom_jitter(width = .2, color = "steelblue") + 
  stat_summary(fun = mean, geom = "point", color = "darkorange", size = 4) +
  geom_smooth(color = "darkorange", method = "lm", se = FALSE) +
  labs(x = "País", y = "Memoria espacial (número de cajas)") +  
  theme_minimal()

lm4 <- lm(spatial_span ~ country, data = data_paises)
summary(lm4)

# Efectos conjuntos
anova(lm4)

# Comparaciones múltiples
TukeyHSD(aov(lm4))

# Para todos los países ####
lm5 <- lm(spatial_span ~ country, data = data)
summary(lm5)

# Efectos conjuntos
anova(lm5)

# Interaccion ####
data %>% 
  ggplot(aes(x = years_of_education, y = melody_span, color = group)) +
  geom_jitter(width = .1, height = .1, alpha = .3) + 
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values = c("darkorange", "steelblue")) +
  labs(x = "Años de educación", color = NULL, y = "Memoria espacial (número de cajas)") +
  theme_minimal() +
  theme(legend.position = "top")

lm6 <- lm(melody_span ~ years_of_education * group,  data = data)
summary(lm6)

