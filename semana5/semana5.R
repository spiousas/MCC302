# Semana 5: Variables aleatorias
# En esta clase vamos a trabajar con simulaciones en R para entender 
# distribuciones de probabilidad discretas y continuas.

# -------------------------------------------------------------
# Convención en R para funciones de distribuciones, R tiene muchas funciones que simulan o calculan distribuciones, 
# estas funciones suelen ser "norm" (para la normal) o "binom" para la binomial y tener prefijos 
# para decirnos que queremos de esas dsitribuciones :
# - r* : genera valores aleatorios (random)
# - d* : devuelve densidad/probabilidad puntual (density)
# - p* : devuelve probabilidad acumulada (distribution function)
# - q* : devuelve cuantiles (quantile)
# Ejemplo: rnorm()-genera una distribucion normal de valores aleatorios-, dnorm() -devueleve la densidad de la normal-, pnorm()-y ya se imaginaran-, qnorm()

# Si tienen dudas de cada funcion pueden usar heklp, googlear o charlar con algun LLM -esto haganlo solo despues de googlear un poco-

## otra cosa que tenemos que saber son las "semillas", las computadoras no son capaces de crear un evento realmente aletorio, 
## estos son "pseudoaleatorios" es decir lo busca dentro de una lista creada aleatoriamente, 
## si nosotros queremos que ese evento aleatorio ocurra siempre de la misma manera (que cuando tiremos la moneda siempre caiga cara por ejemplo)
## podemos indicarle que posicion de la lista (aunque nosotros nunca sabremos si en esa posicion hay una cara o una seca), a eso lo llamamos semilla
## es importante que fijemos una semilla porque si un calculo tiene un componente aleatorio y nosotros no lo dejamos fijo cada vez que corramos el codigo
###el resultado srá distinto

# -------------------------------------------------------------

## 1. Variables aleatorias discretas
# -------------------------------------------------------------
# 1.1 Distribución Bernoulli: X ~ Be(p)
# Una Bernoulli es una v.a. que solo toma dos valores: 0 (fracaso) o 1 (éxito),
# con probabilidad P(X=1)=p y P(X=0)=1-p.

set.seed(42)  # Fijamos semilla para reproducibilidad
p <- 0.3      # Probabilidad de éxito
n <- 1000     # Tamaño de la muestra (cantidad de observaciones simuladas)

# Simulamos variables aleatorias Bernoulli.
# Recordar: Bernoulli es un caso particular de la Binomial con size = 1.
X <- rbinom(n, size = 1, prob = p)

# La esperanza teórica de una Bernoulli es p.
mean(X)  # ≈ p, porque la media muestral aproxima a la media teórica

# La varianza teórica es p*(1-p).
var(X)   # ≈ p*(1-p)

# Pregunta para reflexionar:
# ¿Qué pasa si aumentamos el tamaño de la muestra n?
# (La media y varianza muestral se acercan más a los valores teóricos por la Ley de los Grandes Números).

### hagan la prueba con  10, 100, 1 000 y 1 000 000





# -------------------------------------------------------------
# Función de probabilidad puntual (PMF) y acumulada (CDF)
# -------------------------------------------------------------

# Definimos un conjunto de valores de x (incluyendo algunos imposibles, como -1 y 2)
x_vals <- c(-2,-1,0,1,2)

# Probabilidad puntual: P(X=x) para cada valor de x
# dbinom() devuelve la probabilidad de un valor puntual de la binomial.
f_x <- dbinom(x_vals, size = 1, prob = p)

# Graficamos la función de probabilidad (PMF)
data_frame(x = x_vals, f_x = f_x) %>%
  ggplot(aes(x = x, y = f_x)) +
  geom_col() +
  labs(title = "Bernoulli: f(x) (función de probabilidad puntual)") +
  theme_bw()

# Nota: para valores que no son 0 o 1, la probabilidad es cero.

# Ahora graficamos la función de distribución acumulada F(x)=P(X≤x).
F_x <- pbinom(x_vals, size = 1, prob = p)

data_frame(x = x_vals, F_x =F_x) %>%
  ggplot(aes(x =  x)) +
  geom_point(aes(y = F_x)) +
  geom_line(aes(y = F_x)) +
  labs(title = "Bernoulli: F(x) (función de distribución acumulada)") +
  theme_bw()

# Superponemos ambas: f(x) (PMF) y F(x) (CDF)
data_frame(x = x_vals, f_x = f_x, F_x =F_x) %>%
  ggplot(aes(x =  x)) +
  geom_col(aes(y = f_x), fill = "steelblue", alpha = .2) +
  geom_point(aes(y = F_x)) +
  geom_line(aes(y = F_x)) +
  labs(title = "Bernoulli: f(x) y F(x)") +
  theme_bw()

# -------------------------------------------------------------
# 1.2 Distribución Binomial: X ~ Bin(n,p)
# La binomial cuenta cuántos éxitos hay en n ensayos de Bernoulli con probabilidad p.
# -------------------------------------------------------------

set.seed(123)
n_sims <- 10000  # Número de simulaciones
n_tr <- 10       # Número de ensayos (trials)
p <- 0.4         # Probabilidad de éxito en cada ensayo

# Simulamos la binomial
X <- rbinom(n_sims, size = n_tr, prob = p)

# Esperanza teórica: E[X] = n * p
mean(X)

# Varianza teórica: Var(X) = n * p * (1-p)
var(X)

# -------------------------------------------------------------
# Comparación entre frecuencias relativas y probabilidad teórica
# -------------------------------------------------------------

x <- 0:n_tr  # Posibles valores que puede tomar X
f_teo <- dbinom(x, size = n_tr, prob = p)  # Probabilidades teóricas

# Histograma de la simulación + puntos de la probabilidad teórica
hist(X, breaks = -0.5:(n_tr + 0.5), freq = FALSE,
     main = "Binomial: histograma vs f(x) teórica", xlab = "x", ylab="f(x)")

points(x, f_teo, pch = 19)  # Los puntos negros representan la probabilidad exacta

# Pregunta: ¿Qué pasa si aumentamos n_sims?
# (Las frecuencias relativas se acercan aún más a la distribución teórica).

# Función de distribución acumulada
plot(x, pbinom(x, size = n_tr, prob = p), type = "s",
     main = "Binomial: F(x)", xlab = "x", ylab = "F(x)")

# -------------------------------------------------------------
# 1.3 Distribución de Poisson: X ~ Poi(λ)
# La Poisson se usa para contar la cantidad de eventos en un intervalo
# dado un promedio λ de ocurrencias.
# -------------------------------------------------------------

set.seed(99)
lambda <- 3
X <- rpois(5000, lambda)

mean(X)  # ≈ λ
var(X)   # ≈ λ  (propiedad de la Poisson: media = varianza)

x <- 0:max(X)

# Función de probabilidad puntual
plot(x, dpois(x, lambda), type = "h", lwd = 2, main = "Poisson: f(x)")

# Función de distribución acumulada
plot(x, ppois(x, lambda), type = "s", main = "Poisson: F(x)")

# -------------------------------------------------------------
# 2. Variables aleatorias continuas
# -------------------------------------------------------------

# 2.1 Uniforme continua: X ~ U(a,b)
# Cada valor entre a y b es igualmente probable.
set.seed(1)
n_samples <- 2000
a <- 0; b <- 1
X <- runif(n_samples, min = a, max = b)

mean(X)  # Teórico: (a+b)/2
var(X)   # Teórico: (b-a)^2/12

# Histograma + función de densidad
hist(X, breaks = 30, freq = FALSE, main = "Uniforme: hist + f(x)")
curve(dunif(x, a, b), from = a, to = b, add = TRUE, lwd = 2)

# Función de distribución acumulada
curve(punif(x, a, b), from = a - 0.2, to = b + 0.2, lwd = 2,
      main = "Uniforme: F(x)", ylab = "F(x)")

# -------------------------------------------------------------
# 2.2 Normal: X ~ N(μ, σ^2)
# La distribución más importante en estadística: campana de Gauss.
# -------------------------------------------------------------

set.seed(21)
n_samples <- 50
mu <- 10; sigma <- 2
X <- rnorm(n_samples, mean = mu, sd = sigma)

mean(X); sd(X)  # ≈ μ, σ

# Si aumentamos el tamaño de la muestra, la aproximación mejora
set.seed(21)
n_samples <- 5000
X <- rnorm(n_samples, mean = mu, sd = sigma)
mean(X); sd(X)

# Histograma + densidad teórica
hist(X, breaks = 40, freq = FALSE, main = "Normal: hist + f(x)")
curve(dnorm(x, mu, sigma), add = TRUE, lwd = 2)

# Función de distribución acumulada
curve(pnorm(x, mu, sigma), from = mu - 4*sigma, to = mu + 4*sigma,
      lwd = 2, main = "Normal: F(x)", ylab = "F(x)")