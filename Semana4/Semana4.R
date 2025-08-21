#Semana 4: Variables aleatorias

#Convención en R: para muchas distribuciones existe una familia de funciones con prefijos:
#r* simula (random), d* densidad/probabilidad (density), p* distribución acumulada (probability), q* cuantiles (quantile).

##1. Variables aleatorias discretas
#1.1 Bernoulli X ~ Be(p)

set.seed(42) #Reproducibilidad (para que siempre me genere los mismos números)
p <- 0.3 #Probabilidad de éxito
n <- 1000 #Tamaño de la muestra
X <- rbinom(n, size = 1, prob = p)     # Bernoulli = Binomial con size=1
mean(X)                                # ≈ p
var(X)                                 # ≈ p*(1-p)

#¿Qué pasa si agrandan el tamaño de la muestra?

#Vamos a graficar la función de probabilidad puntual y la función de distribución acumulada

x_vals <- c(-2,-1,0,1,2)

#La función dbinom calcula la probabilidad puntual para una distribución binomial 
# (de la cual la Bernoulli es un caso espacial para size=1)
f_x <- dbinom(x_vals, size = 1, prob = p)

data_frame(x = x_vals, f_x = f_x) %>%
  ggplot(aes(x =  x, y = f_x)) +
  geom_col() +
  labs(title = "Bernoulli: f(x)") +
  theme_bw()

# ¿Qué pasa para los valores que no son 0 o 1?

# Agrego dos puntos fuera de los posibles valores
x_vals <- c(-2, -1,0,1,2)
F_x <- pbinom(x_vals, size = 1, prob = p)

data_frame(x = x_vals, F_x =F_x) %>%
  ggplot(aes(x =  x)) +
  geom_point(aes(y = F_x)) +
  geom_line(aes(y = F_x)) +
  labs(title = "Bernoulli: f(x) y F(x)") +
  theme_bw()

data_frame(x = x_vals, f_x = f_x, F_x =F_x) %>%
  ggplot(aes(x =  x)) +
  geom_col(aes(y = f_x), fill = "steelblue", alpha = .2) +
  geom_point(aes(y = F_x)) +
  geom_line(aes(y = F_x)) +
  labs(title = "Bernoulli: f(x) y F(x)") +
  theme_bw()

#1.2. Binomial X ~ Bi(n,p)
set.seed(123)
n_sims <- 10000 #Tamaño de la simulación
n_tr <- 10 #Cantidad de experimentos en cada simulación
p <- 0.4 #Probabilidad de éxito

X <- rbinom(n_sims, size = n_tr, prob = p)
mean(X)        # ≈ n_tr * p
var(X)         # ≈ n_tr * p * (1 - p)

# Función de probabilidad puntual teórica vs. frecuencia relativa
x <- 0:n_tr
f_teo <- dbinom(x, size = n_tr, prob = p)
 
# Lo vamos a plotear con herramientas de Rbase para no complicarnos
hist(X, breaks = -0.5:(n_tr + 0.5), freq = FALSE,
     main = "Binomial: histograma vs f(x) teórica", xlab = "x", ylab="f(x)")
# No hace falta que entiendan el código sino lo que está pasando
points(x, f_teo, pch = 19)

#¿Cómo ven ambas cosas? ¿Qué pasa si cambian el tamaño de la simulación?

#Función de distribución acumulada
plot(x, pbinom(x, size = n_tr, prob = p), type = "s",
     main = "Binomial: CDF", xlab = "x", ylab = "F(x)")

#1.3. Poisson
set.seed(99)
lambda <- 3
X <- rpois(5000, lambda)
mean(X)
var(X)     # ≈ lambda

x <- 0:max(X)
plot(x, dpois(x, lambda), type = "h", lwd = 2, main = "Poisson: f(x)")
plot(x, ppois(x, lambda), type = "s", main = "Poisson: F(x)")

##2. Variables continuas
#2.1 Uniforme X ~ U(a,b)

set.seed(1)
n_samples <- 2000
a <- 0; b <- 1
X <- runif(n_samples, min = a, max = b)
mean(X)         # ≈ (a+b)/2
var(X)          # ≈ (b-a)^2/12

hist(X, breaks = 30, freq = FALSE, main = "Uniforme: hist + f(x)")
curve(dunif(x, a, b), from = a, to = b, add = TRUE, lwd = 2)

curve(punif(x, a, b), from = a - 0.2, to = b + 0.2, lwd = 2,
      main = "Uniforme: F(x)", ylab = "F(x)")

#2.2 Normal X~N(mu,sigma^2)

set.seed(21)
# Para 50 samples
n_samples <- 50
mu <- 10; sigma <- 2
X <- rnorm(n_samples, mean = mu, sd = sigma)
mean(X); sd(X)   # ≈ mu, sigma

set.seed(21)
# Para 5000 samples
n_samples <- 5000
mu <- 10; sigma <- 2
X <- rnorm(n_samples, mean = mu, sd = sigma)
mean(X); sd(X)   # ≈ mu, sigma

hist(X, breaks = 40, freq = FALSE, main = "Normal: hist + f(x)")
curve(dnorm(x, mu, sigma), add = TRUE, lwd = 2)

curve(pnorm(x, mu, sigma), from = mu - 4*sigma, to = mu + 4*sigma,
      lwd = 2, main = "Normal: F(x)", ylab = "F(x)")

