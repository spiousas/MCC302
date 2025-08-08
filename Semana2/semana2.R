# Bienvenidos y bienvenidas a la semana 2 de MCC322
# En esta semana vamo s empezar a utilizar las funciones del tidyverse para hacer análisis exploratoiros de datos.
# Como ya vimos en la clase, el tidyverse es un conjunto de paquetes que nos permiten manipular y visualizar datos de una manera más sencilla y eficiente.
# Manos a la obra ...

# 1- Instalar y cargar el paquete ####
# En caso que aún no lo tengan instalado, pueden instalar el paquete tidyverse corriendo la la siguiente línea de código en la consola:
# install.packages("tidyverse")
# Una vez instalado, pueden cargar el paquete con la siguiente línea de código:
library(tidyverse)

# Fijensé que instalar el paquete se hace sólo una vez en cada computadora mientras que cargarlo se hace cada vez que se inicia una nueva sesión de R.

# 2- Cargar los datos ####
df <- read_csv("Semana2/data/Data.csv")

# Estos datos provienen del paper "Folk psychological attributions of consciousness to large language models" donde exploran la posibilidad
# de que los modelos de lenguaje tengan conciencia. El enfoque experimental consiste en entrevistar a personas (US) y hacerles preguntas relacionadas con 
# la "phenomenal consciousness".

# 3- Explorar los datos ####
# Usemos las funciones para explorar el dataset
# Empecemos por summary()
summary(df)

# Y ahora por str()
str(df)

# También podemos usar glimpse() para ver una vista rápida de los datos
glimpse(df)

# Podemos ver las primeras filas del dataset con head()
head(df)

# O las últimas filas con tail()
tail(df)

# Otra función muy útil es View(), que nos permite ver el dataset en una ventana separada.
# Esto también se puede hacer desde el menú de RStudio, haciendo clic en el nombre del dataset en el panel de Environment.
View(df)

# Veamos la variable age.
df$age

# Y ahora la variable use_often
df$use_often

# ¿Qué creen que mide esta variable?

# ¿Cuántas observaciones tiene el dataset?
# ¿Y cuántas variables?
# ¿Podemos identificar los tipos de variables?
# ¿Hay datos faltantes? 

# Para los análisis que vienen nos vamos a quedar sólo con algunas variables:
# pheno: un score de 1 a 100 que indica cuánta conciencia fenomenal creen que tiene el modelo de lenguaje.
# pheno_pred: un score de 1 a 100 que indica cuánta conciencia fenomenal creen que el resto de la gente va a responder.
# age: edad.
# gender: género.
# use_often: una variable discreta que indica con qué frecuencia la persona usa modelos de lenguaje.
# `act morally`: un score de 1 a 100 que indica cuán moralmente consideran que actúa el modelo de lenguaje.

# Para esto vamos a usar nuestra primera función del tidyverse: select()
df_filtrado <- select(df, c("pheno", "pheno_pred", "age", "gender", "act morally", "use_often"))

# Es una buena práctica guardar el dataset filtrado en una nueva variable para no perder el original.
# Ahora podemos ver el dataset filtrado
summary(df_filtrado)
str(df_filtrado)

# Acá vamos a presentar al operador pipe %>% que nos permite encadenar funciones de una manera más legible.
# Por ejemplo, podemos filtrar el dataset y luego ver un resumen de las variables numéricas de la siguiente manera:
df %>%
  select(c("pheno", "pheno_pred", "age", "gender", "act morally", "use_often")) %>%
  summary()

# En este caso no lo asignamos a ninguna variable.
  
# Tip avanzado: Fijense que pasa cuando tenemos una variable con un espacio en el nombre, como "act morally".
# Exploren la función clean_names() del paquete janitor para limpiar los nombres de las variables (recuerden que hay que instalar el paquete).  

# 4- Medias y medianas ####

# Así calculamos la media y la mediana de la variable pheno
mean(df_filtrado$pheno)
median(df_filtrado$pheno)

# Calculen lo mismo para todas las variables numéricas del dataset filtrado.

## Medias por género ####
# Para calcular las medias de la variable pheno por género tenemos que separar los datos de acuedo a esa columna
pheno_male <- df_filtrado$pheno[df_filtrado$gender == "Male"]
mean(pheno_male)
pheno_female <- df_filtrado$pheno[df_filtrado$gender == "Female"]
mean(pheno_female)

# Pero también podemos ser cancheros y cancheras y usar el tidyverse
df_filtrado %>%
  group_by(gender) %>%
  summarise(mean_pheno = mean(pheno))
# ¿Ven algo distinto?

## Medias por uso. ####
# Ahora calculen las medias de la variable pheno por uso frecuente de modelos de lenguaje

# ¿Qué pueden decir al respecto?

# 4- Desviaciones estándar ####

# Así calculamos la varianza y la desviación estándar de la variable pheno
var(df_filtrado$pheno)
sd(df_filtrado$pheno)

# Calculen lo mismo para todas las variables numéricas del dataset filtrado.

# Hagan lo mismo por género y por uso frecuente de modelos de lenguaje usando tidyverse.

# 5- Correlación (que, como sabemos, NO IMPLICA CAUSALIDAD) ####
# Así calculamos la correlación entre pheno y la edad
cor(df_filtrado$pheno, df_filtrado$age)
# ¿Por qué me tira error?
# Porque tengo datos faltantes en la variable age.

# Construyamos un dataset sin datos faltantes en age
df_filtrado_sin_na <- df_filtrado %>%
  filter(!is.na(age))

# Y ahora:
cor(df_filtrado_sin_na$pheno, df_filtrado_sin_na$age)

# Una correlación que puede estar interesante es entre pheno y pheno_pred. ¿Por qué creen?
# Calculenla e interpreten el resultado.

# Ejercicio avanzado: ¿Pueden calcular e interpretar la correlación entre pheno y pheno_pred dependiendo de la frecuencia de uso?

# Tip avanzado: Usando el paquete corrplot (lo pueden instalar con install.packages("corrplot")) pueden hacer un gráfico de 
# correlación entre todas las variables numéricas del dataset filtrado.
library(corrplot)
data <- df_filtrado %>% 
  select_if(is.numeric) %>% # Me quedo sólo con las variables numéricas
  drop_na() # Me saco del encima los na

M <- cor(data)
corrplot(M, method="color")
