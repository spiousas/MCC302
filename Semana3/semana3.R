# Bienvenidos y bienvenidas a la semana 3 de MCC322
# Sigamos mirando los datos de consciencia con R y tidyverse

# 1- Carguemos el paquete tidyverse (que por supuesto todos y todas tienen ya instalado) ####
library(tidyverse)

# Fijensé que instalar el paquete se hace sólo una vez en cada computadora mientras que cargarlo se hace cada vez que se inicia una nueva sesión de R.

# 2- Carguemos de nuevo los los datos de "Folk psychological attributions of consciousness to large language models" ####
# Si quieren cargar los datos desde un archivo del que desconocen su ubicación pueden usar la función file.choose() para abrir un explorador de archivos.
df <- read_csv(file.choose())

# 3- Ahora repliquemos la figura 1.c ####
# Recordemos que el histograma sólo tiene la coordenada x

# 4- Ahora ploteen algo que les interesa mirando el dataframe df ####
# Exploren las posibles geometrías para buscar inspiración
# Piensen en una pregunta que se pueda responder gráficamente y escríbanla.
# Tengan en cuenta esta pregunta al momento de tomar decisiones estéticas sobre mapeoas, geometrías, colores, tamaños, etc.
# Hagan énfasis en las dimensiones que son importantes para transmitir la idea.