# Bienvenidos y bienvenidas a la semana 3 de MCC322
# Sigamos mirando los datos de consciencia con R y tidyverse

# 1- Carguemos el paquete tidyverse (que por supuesto todos y todas tienen ya instalado) ####
library(tidyverse)

# Fijensé que instalar el paquete se hace sólo una vez en cada computadora mientras que cargarlo se hace cada vez que se inicia una nueva sesión de R.

# 2- Carguemos de nuevo los los datos de "Folk psychological attributions of consciousness to large language models" ####
# Si quieren cargar los datos desde un archivo del que desconocen su ubicación pueden usar la función file.choose() para abrir un explorador de archivos.
df <- read_csv(file.choose())

# Por las dudas les digo que este está acá: df <- read_csv("Semana3/data/Data.csv")

head(df) # Muestra las primeras 6 filas del data frame

# 3- Vamos a preguntarles cosas a los datos ####
# Seamos muy cancheros y cancheras y usemos las funciones del tidyverse para explorar los datos.

glimpse(df) # Muestra un resumen de las columnas y sus tipos

# Vemoas el ránking promedio de consciencia en función de la frecuencia de uso
df %>%
  group_by(use_often) %>% # Agrupamos por frecuencia
  summarise(mean_pheno = mean(pheno, na.rm = TRUE))

# Por qué aparece un grupo NA?
df %>%
  filter(is.na(use_often)) # Filtramos por el grupo NA

# ¿Cómo nos las sacamos de encima?
df %>%
  drop_na(use_often) %>% # Eliminamos las filas con NA en use_often
  group_by(use_often) %>% # Agrupamos por frecuencia
  summarise(mean_pheno = mean(pheno, na.rm = TRUE))
# Ojo que esto no elimina las filas con NA de la columna pheno, sólo de use_often
# Pero esas nos las sacamos de encima con el na.rm = TRUE

# Guardemos esto en una variable que se llame promedios_pheno_por_uso
promedios_pheno_por_uso <- df %>%
  drop_na(use_often) %>% # Eliminamos las filas con NA en use_often
  group_by(use_often) %>% # Agrupamos por frecuencia
  summarise(mean_pheno = mean(pheno, na.rm = TRUE))

# Lindo repaso, no?

# 4- Ahora vamos a graficar unos datos con ggplot2 ####
# Miremos la figura 1-d del paper y tratemos de copiarla

ggplot(df, # Los datos
       aes(x = pheno, y = pheno_pred)) + # El mapeo de la estética
  geom_point() 

# ¿Vamos bien? Vamos bien
# Cambiemos el color de los puntos a azul
ggplot(df, # Los datos
       aes(x = pheno, y = pheno_pred)) + # El mapeo de la estética
  geom_point(color = "steelblue") # Cambiamos el color de los puntos a azul

# A ver qué pasa si cambiamos el alpha 
ggplot(df, # Los datos
       aes(x = pheno, y = pheno_pred)) + # El mapeo de la estética
  geom_point(color = "steelblue", alpha = .5) # Cambiamos el color de los puntos a azul

# Está bueno qué pasa cuando tenemos puntos superpuestos

# Agreguemos una línea de tendencia
ggplot(df, # Los datos
       aes(x = pheno, y = pheno_pred)) + # El mapeo de la estética
  geom_point(color = "steelblue", alpha = .5) + # Cambiamos el color de los puntos a azul
  geom_smooth() # Agregamos una línea de tendencia

# Y si quiero que sea recta
ggplot(df, # Los datos
       aes(x = pheno, y = pheno_pred)) + # El mapeo de la estética
  geom_point(color = "steelblue", alpha = .5) + # Cambiamos el color de los puntos a azul
  geom_smooth(method = lm) # Agregamos una línea de tendencia

# Le cambio el color y la hago más ancha
ggplot(df, # Los datos
       aes(x = pheno, y = pheno_pred)) + # El mapeo de la estética
  geom_point(color = "steelblue", alpha = .5) + # Cambiamos el color de los puntos a azul
  geom_smooth(method = lm, color = "steelblue", linewidth = 2) # Agregamos una línea de tendencia

# Lo último sería agregar la línea punteada, no?
ggplot(df, # Los datos
       aes(x = pheno, y = pheno_pred)) + # El mapeo de la estética
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", linewidth = 1) + # ¿Entendemos todo lo que dice adentro?
  geom_point(color = "steelblue", alpha = .5) + # Cambiamos el color de los puntos a azul
  geom_smooth(method = lm, color = "steelblue", linewidth = 2) # Agregamos una línea de tendencia

# Vamos con los cambiso estéticos. 
# El "tema primero". Veamos acá (https://ggplot2.tidyverse.org/reference/ggtheme.html). El que más se parece es Classic, no?
ggplot(df, # Los datos
       aes(x = pheno, y = pheno_pred)) + # El mapeo de la estética
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", linewidth = 1) + # Agregamos la línea punteada
  geom_point(color = "steelblue", alpha = .5) + # Cambiamos el color de los puntos a azul
  geom_smooth(method = lm, color = "steelblue", linewidth = 2) + # Agregamos una línea de tendencia
  theme_classic() # Cambiamos el tema a Classic

# Ahora vamos a agregarle un título y etiquetas a los ejes
ggplot(df, # Los datos
       aes(x = pheno, y = pheno_pred)) + # El mapeo de la estéticaç
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", linewidth = 1) + # Agregamos la línea punteada
  geom_point(color = "steelblue", alpha = .5) + # Cambiamos el color de los puntos a azul
  geom_smooth(method = lm, color = "steelblue", linewidth = 2) + # Agregamos una línea de tendencia
  theme_classic() + # Cambiamos el tema a Classic
  labs(title = "Overestimation of public opinion", # Título
       x = "Participant's consciusness ratings", # Etiqueta del eje x
       y = "Predicions of others' ratings") # Etiqueta del eje y 

# Y la guardamos 
ggsave("Semana3/figura1D.png", width = 6, height = 4.5)

# 5 - Repliquemos la figura 1-a ao vivo ####

# 6 - Ahora la 1-b ####
  