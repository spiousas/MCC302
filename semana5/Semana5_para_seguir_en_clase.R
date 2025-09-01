# Semanas 4 y 5: Variables aleatorias ####

## Para seguir en la clase ####

### Calculo de probabilidades con R ####
# Supongamos que sabemos que la altura de los hombres en centímetros en Argentina se distribuye de la siguiente manera: altura ~ N(178, 30^2)

# ¿Cuál es la probabilidad de que un hombre mida menos de 178 cm?
pnorm(mean = 178, sd = 30, q = 178)

# ¿Cuál es la probabilidad de que un hombre mida menos de 120 cm?
pnorm(mean = 178, sd = 30, q = 120)

# ¿Cuál es la probabilidad de que un hombre mida menos de 120 cm?
1 - pnorm(mean = 178, sd = 30, q = 120)

# ¿Cuál es la probabilidad de que un hombre mida MÁS de 120 cm y MENOS de 160?
pnorm(mean = 178, sd = 30, q = 160) - pnorm(mean = 178, sd = 30, q = 120)

### Simulemos un experimento de Bernoulli ####

rbinom(n = 1, size = 1, prob = 0.5) # 1 experimento de Bernoulli con p=0.5

set.seed(123) #Reproducibilidad (para que siempre me genere los mismos números)
X1 <- rbinom(n = 100, size = 1, prob = 0.5) # 1 experimento de Bernoulli con p=0.3
X1
mean(X1) #= proporción de éxitos
var(X1) #= p*(1-p)

### Simulemos un experimento de Bernoulli con una moneda no justa ####

rbinom(n = 1, size = 1, prob = 0.3) # 1 experimento de Bernoulli con p=0.3

set.seed(123) #Reproducibilidad (para que siempre me genere los mismos números)
X2 <- rbinom(n = 100, size = 1, prob = 0.3) # 1 experimento de Bernoulli con p=0.3
X2
mean(X2) #= proporción de éxitos
var(X2) #= p*(1-p)

### Cómo evoluciona la media con n ####

# DISCLAIMER: No tienen que entender este código, lo dejo por si hay valientes que quieren replicarlo
# Con X1 y X2
n <- 500
medias1 <- rep(NA, n)
medias2 <- rep(NA, n)
X1 <- c()
X2 <- c()
set.seed(123)
for (i in 1:n) {
  X1 <- c(X1, rbinom(n = 1, size = 1, prob = 0.5))
  X2 <- c(X2, rbinom(n = 1, size = 1, prob = 0.3))
  medias1[i] <- mean(X1)
  medias2[i] <- mean(X2)
}

tibble(medias1, medias2) %>%
  gather(key = "experimento", value = "medias") %>%
  mutate(n = rep(1:n,2)) %>%
  ggplot(aes(x = n, y = medias, color = experimento)) +
  geom_hline(yintercept = 0.3, color = "black", linetype = "dashed") +
  geom_hline(yintercept = 0.5, color = "black", linetype = "dashed") +
  geom_line(linewidth = 1) +
  labs(color = NULL,
       x = "Tamaño de la muestra",
       y = "Media muestral") +
  scale_color_manual(values = c("steelblue", "darkorange"), labels = c("p=0.5", "p=0.3")) +
  theme_minimal() +
  theme(legend.position = "top")

### Simulemos un experimento de Poisson ####
set.seed(123)
X3 <- rpois(n = 30, lambda = 4) # experimento de Poisson con lambda = 4 y n=30
mean(X3)
var(X3)

# Densidad teórica de poisson con lambda = 4 y histograma de x3
data.frame(X3) %>%
  ggplot(aes(x = X3)) +
  geom_bar(data= transform(data.frame(x=c(0:10)), y=dpois(x, 4)), aes(x, y), stat="identity") +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "lightblue", color = "black", alpha = .5) +
  scale_x_continuous(breaks = 0:10) +
  labs(title = "Histograma muestral vs. poblacional (gris) para n=30",
       x = "X3",
       y = "densidad") +
  theme_minimal()

set.seed(123)
X3 <- rpois(n = 100, lambda = 4) # experimento de Poisson con lambda = 4 y n=100
mean(X3)
var(X3)

# Densidad teórica de poisson con lambda = 4 y histograma de x3data.frame(X3) %>%
data.frame(X3) %>%
  ggplot(aes(x = X3)) +
  geom_bar(data= transform(data.frame(x=c(0:10)), y=dpois(x, 4)), aes(x, y), stat="identity") +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "lightblue", color = "black", alpha = .5) +
  scale_x_continuous(breaks = 0:10) +
  labs(title = "Histograma muestral vs. poblacional (gris) para n=100",
       x = "X3",
       y = "Densidad") +
  theme_minimal()

### Simulemos un experimento con la distribución normal ####
set.seed(123)
X4 <- rnorm(n = 30, mean = 10, sd = 2) # experimento normal con mu=10, sigma=2 y n=30
mean(X4)
var(X4)

# EL histograma de la muestra y la distribución normal teóricadata.frame(X4) %>%
data.frame(X4) %>%
  ggplot(aes(x = X4)) +
  stat_function(fun = dnorm, args = list(mean = 10, sd = 2), color = "red", size = 1) +
  geom_histogram(aes(y = ..density..), fill = "lightblue", color = "black", alpha = .5) +
    labs(title = "Histograma muestral vs. distribución teórica para n=30",
       x = "X4",
       y = "Densidad") +
  theme_minimal()

set.seed(123)
X4 <- rnorm(n = 500, mean = 10, sd = 2) # experimento normal con mu=10, sigma=2 y n=100
mean(X4)
var(X4)

# El histograma de la muestra y la distribución normal teórica
data.frame(X4) %>%
  data.frame(X4) %>%
  ggplot(aes(x = X4)) +
  stat_function(fun = dnorm, args = list(mean = 10, sd = 2), color = "red", size = 1) +
  geom_histogram(aes(y = ..density..), fill = "lightblue", color = "black", alpha = .5) +
  labs(title = "Histograma muestral vs. distribución teórica para n=500",
       x = "X4",
       y = "Densidad") +
  theme_minimal()

# ¿Como evolucionan la media y la varianza con n?
n <- 1000
medias <- rep(NA, n)
vars <- rep(NA, n)
X4 <- c()
set.seed(123)
for (i in 1:n) {
  X4 <- c(X4, rnorm(n = 1, mean = 10, sd = 2) )
  medias[i] <- mean(X4)
  vars[i] <- var(X4)
}

# Evolución de la media muestral con el n
tibble(medias) %>%
  mutate(n = 1:n) %>%
  ggplot(aes(x = n, y = medias)) +
  geom_hline(yintercept = 10, color = "black", linetype = "dashed") +
  geom_line(linewidth = 1, color = "steelblue") +
  labs(color = NULL,
       x = "Tamaño de la muestra",
       y = "Media muestral") +
  theme_minimal() +
  theme(legend.position = "top")

# Evolución de la varianza muestral con el n
tibble(vars) %>%
  mutate(n = 1:n) %>%
  ggplot(aes(x = n, y = vars)) +
  geom_hline(yintercept = 4, color = "black", linetype = "dashed") +
  geom_line(linewidth = 1, color = "steelblue") +
  labs(color = NULL,
       x = "Tamaño de la muestra",
       y = "Varianza muestral") +
  theme_minimal() +
  theme(legend.position = "top")