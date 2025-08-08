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
for (var in colnames(df_filtrado)){
  var_values = select(df_filtrado,var)
  mean_var = mean_age = mean(var_values,na.rm=TRUE)
  sd_var = sd(var_values,na.rm=TRUE)
  median_var = median(var_values,na.rm=TRUE)
  
  print(paste(var, "Promedio: ;", mean_var, " Desvío estándar: ;", sd_var, " Mediana: ", median_var))
}

# Medias generales, medianas generales
# Medias por género
  
# medias por uso. Qué pueden decir al respecto?

# 4- Desviaciones estándar e IQR ####

# 5- Correlación (que, como sabemos, NO IMPLICA CAUSALIDAD) ####
 