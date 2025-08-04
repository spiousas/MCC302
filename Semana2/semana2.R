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

# ¿Cuántas observaciones tiene el dataset?
# ¿Y cuántas variables?
# ¿Podemos identificar los tipos de variables?
# ¿Hay datos faltantes? 

# 4- Medias y medianas ####

# 4- Desviaciones estándar e IQR ####

# 5- Correlación (que, como sabemos, NO IMPLICA CAUSALIDAD) ####
 