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

#Vamos a graficar la función de probabilidad puntual y la función de distribución acumulada

x_vals <- c(-0.1,0,1,1.1)
f_x <- dbinom(x_vals, size = 1, prob = p)
barplot(f_x, names.arg = x_vals, ylim = c(0,1), main = "Bernoulli: f(x)")

F_x <- pbinom(x_vals, size = 1, prob = p)
plot(x_vals,F_x,, type = "s", xlab = "x", ylab = "F(x)", main = "Bernoulli: F(x)",ylim = c(0,1))

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

hist(X, breaks = -0.5:(n_tr + 0.5), freq = FALSE,
     main = "Binomial: histograma vs f(x) teórica", xlab = "x", ylab="f(x)")

points(x, f_teo, pch = 19)

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
n_samples <- 5000
mu <- 10; sigma <- 2
X <- rnorm(n_samples, mean = mu, sd = sigma)
mean(X); sd(X)   # ≈ mu, sigma

hist(X, breaks = 40, freq = FALSE, main = "Normal: hist + f(x)")
curve(dnorm(x, mu, sigma), add = TRUE, lwd = 2)

curve(pnorm(x, mu, sigma), from = mu - 4*sigma, to = mu + 4*sigma,
      lwd = 2, main = "Normal: F(x)", ylab = "F(x)")

#Podemos convertirla a distribución normal estándar
Z <- (X-mu)/sigma

hist(Z, breaks = 40, freq = FALSE, main = "Normal estándar: hist + f(x)")
curve(dnorm(x, 0, 1), add = TRUE, lwd = 2)

curve(pnorm(x, 0, 1), from =- 4, to = 4,
      lwd = 2, main = "Normal estándar: F(x)", ylab = "F(x)")
