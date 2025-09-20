# Simulemos un experimento de Bernoulli ####
## Cómo evoluciona la media con n ####

library(tidyverse)

# DISCLAIMER: No tienen que entender este código, lo dejo por si hay valientes que quieren replicarlo
# Con X1 y X2
n <- 200
medias <- rep(NA, n)
X1 <- c()
set.seed(123)
for (i in 1:n) {
  X1 <- c(X1, rbinom(n = 1, size = 1, prob = 0.5))
  medias[i] <- mean(X1)
}

medias_tbl <- tibble(medias) %>%
  mutate(n = 1:n)

medias_tbl %>%
  ggplot(aes(x = n, y = medias)) +
  geom_hline(yintercept = 0.5, color = "black", linetype = "dashed") +
  geom_vline(xintercept = 25, color = "darkred", linetype = "dashed") +
  geom_vline(xintercept = 100, color = "darkred", linetype = "dashed") +
  geom_line(linewidth = 1, color = "darkorange") +
  labs(color = NULL,
       x = "Tamaño de la muestra",
       y = "Media muestral") +
  theme_minimal() +
  theme(legend.position = "top")
ggsave("f2825b4f.png", width = 6, height = 4)

## Y varios experimentos al mismo tiemp ####

library(tidyverse)

# DISCLAIMER: No tienen que entender este código, lo dejo por si hay valientes que quieren replicarlo
# Con X1 y X2
n_final <- 200
M <- 20 # 100 experimentos
medias <- c()

set.seed(123)
for (k in 1:M) {
  X1 <- c()
  for (i in 1:n_final) {
    X1 <- c(X1, rbinom(n = 1, size = 1, prob = 0.5))
    medias <- c(medias, mean(X1))
  }  
}

medias_tbl <- tibble(medias) %>%
  mutate(n = rep(1:n_final, M),
         experiment = rep(1:M, each = n_final))

medias_tbl %>%
  ggplot(aes(x = n, y = medias, group = experiment)) +
  geom_hline(yintercept = 0.5, color = "black", linetype = "dashed") +
  geom_vline(xintercept = 25, color = "darkred", linetype = "dashed") +
  geom_vline(xintercept = 100, color = "darkred", linetype = "dashed") +
  geom_line(linewidth = .5, alpha = .5) +
  labs(color = NULL,
       x = "Tamaño de la muestra",
       y = "Media muestral") +
  theme_minimal() +
  theme(legend.position = "top")

## La distribución de la media muestral para n25 y n100 ####
medias_25 <- medias_tbl %>%
  filter(n == 25)

medias_25 %>%
  ggplot(aes(x = medias)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = "steelblue", color = "black", alpha = .5) +
  geom_vline(xintercept = 0.5, color = "black", linetype = "dashed") +
  labs(x = "Media muestral",
       y = "Densidad") +
  theme_minimal()

medias_25_100 <- medias_tbl %>%
  filter(n %in% c(25, 100))

medias_25_100 %>%
  ggplot(aes(x = medias, fill = factor(n))) +
  geom_histogram(aes(y = ..density..), bins = 20, color = "black", alpha = .5) +
  geom_vline(xintercept = 0.5, color = "black", linetype = "dashed") +
  scale_fill_manual(values = c("steelblue", "darkorange"), labels = c("n=25", "n=100")) +
  labs(x = "Media muestral",
       y = "Densidad") +
  theme_minimal() +
  theme(legend.position = "top", legend.title = element_blank())

## Con 200 experimentos ####
n_final <- 200
M <- 200 # 100 experimentos
medias <- c()

set.seed(123)
for (k in 1:M) {
  X1 <- c()
  for (i in 1:n_final) {
    X1 <- c(X1, rbinom(n = 1, size = 1, prob = 0.5))
    medias <- c(medias, mean(X1))
  }  
}

medias_tbl <- tibble(medias) %>%
  mutate(n = rep(1:n_final, M),
         experiment = rep(1:M, each = n_final))
medias_25_100 <- medias_tbl %>%
  filter(n %in% c(25, 100))

medias_25_100 %>%
  ggplot(aes(x = medias, fill = factor(n))) +
  geom_histogram(aes(y = ..density..), bins = 20, color = "black", alpha = .5) +
  geom_vline(xintercept = 0.5, color = "black", linetype = "dashed") +
  scale_fill_manual(values = c("steelblue", "darkorange"), labels = c("n=25", "n=100")) +
  labs(x = "Media muestral",
       y = "Densidad") +
  theme_minimal() +
  theme(legend.position = "top", legend.title = element_blank())

## Con 2000 experimentos ####
n_final <- 100
M <- 2000 # 100 experimentos
medias <- c()

set.seed(123)
for (k in 1:M) {
  X1 <- c()
  for (i in 1:n_final) {
    X1 <- c(X1, rbinom(n = 1, size = 1, prob = 0.5))
    medias <- c(medias, mean(X1))
  }  
}

medias_tbl <- tibble(medias) %>%
  mutate(n = rep(1:n_final, M),
         experiment = rep(1:M, each = n_final))
medias_25_100 <- medias_tbl %>%
  filter(n %in% c(25, 100))

medias_25_100 %>%
  ggplot(aes(x = medias, fill = factor(n))) +
  geom_histogram(aes(y = ..density..), bins = 23, color = "black", alpha = .5) +
  geom_vline(xintercept = 0.5, color = "black", linetype = "dashed") +
  scale_fill_manual(values = c("steelblue", "darkorange"), labels = c("n=25", "n=100")) +
  labs(x = "Media muestral",
       y = "Densidad") +
  theme_minimal() +
  theme(legend.position = "top", legend.title = element_blank())

## Con 2000 experimentos ####
n_final <- 2000
M <- 20000 # 100 experimentos
medias <- c()

set.seed(1234)
for (k in 1:M) {
  X1 <-  rbinom(n = n_final, size = 1, prob = 0.5)
  medias <- c(medias, mean(X1))
}

medias_tbl <- tibble(medias) %>%
  mutate(experiment = 1:M)

medias_tbl %>%
  ggplot(aes(x = medias)) +
  geom_histogram(aes(y = ..density..), fill = "darkorange", bins = 30, color = "black", alpha = .5) +
  geom_vline(xintercept = 0.5, color = "black", linetype = "dashed") +
  stat_function(fun = dnorm, args = list(mean = 0.5, sd = sqrt(0.5*0.5/n_final)), color = "blue", size = 1) +
  labs(x = "Media muestral",
       y = "Densidad") +
  theme_minimal() +
  theme(legend.position = "top", legend.title = element_blank())

## Las rachas de ceros y unos ####

# Hacemos un experimento tirando una moneda y de una serie de 120 tiradas contamos la racha más larga de unos
set.seed(123)
n_tiradas <- 200
n_experimentos <- 1000
rachas <- rep(NA, n_experimentos)
for (j in 1:n_experimentos) {
  tiradas <- rbinom(n = n_tiradas, size = 1, prob = 0.5)
  racha_actual <- 0
  racha_maxima <- 0
  for (i in 1:n_tiradas) {
    if (tiradas[i] == 1) {
      racha_actual <- racha_actual + 1
      if (racha_actual > racha_maxima) {
        racha_maxima <- racha_actual
      }
    } else {
      racha_actual <- 0
    }
  }
  rachas[j] <- racha_maxima
}

rachas_tbl <- tibble(rachas)

rachas_tbl %>%
  ggplot(aes(x = rachas)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "darkorange", color = "black", alpha = .5) +
  labs(x = "Racha máxima en 200 tiradas",
       y = "Densidad") +
  theme_minimal() +
  scale_x_continuous(limits = c(0, max(rachas)), breaks = seq(0, max(rachas), by = 2)) +
  theme(legend.position = "top", legend.title = element_blank())

# Ahora veamos que pasa con la media de una normal ####

## Con 20 experimentos ####
n_final <- 200
M <- 20 # 20 experimentos
mu <- 10
sigma <- 2
medias <- c()

set.seed(123)
for (k in 1:M) {
  X1 <- c()
  for (i in 1:n_final) {
    X1 <- c(X1, rnorm(n = 1, mean = mu, sd = sigma))    
    medias <- c(medias, mean(X1))
  }  
}

medias_tbl <- tibble(medias) %>%
  mutate(n = rep(1:n_final, M),
         experiment = rep(1:M, each = n_final))

medias_tbl %>%
  ggplot(aes(x = n, y = medias, group = experiment)) +
  geom_hline(yintercept = 10, color = "black", linetype = "dashed") +
  geom_vline(xintercept = 25, color = "darkred", linetype = "dashed") +
  geom_vline(xintercept = 100, color = "darkred", linetype = "dashed") +
  geom_line(linewidth = .5, alpha = .5) +
  labs(color = NULL,
       x = "Tamaño de la muestra",
       y = "Media muestral") +
  theme_minimal() +
  theme(legend.position = "top")

## Con 20 experimentos ####
n_final <- 200
M <- 20 # 20 experimentos
medias <- c()

set.seed(123)
for (k in 1:M) {
  X1 <- c()
  for (i in 1:n_final) {
    X1 <- c(X1, rnorm(n = 1, mean = mu, sd = sigma))
    medias <- c(medias, mean(X1))
  }  
}

medias_tbl <- tibble(medias) %>%
  mutate(n = rep(1:n_final, M),
         experiment = rep(1:M, each = n_final))
medias_25_100 <- medias_tbl %>%
  filter(n %in% c(25, 100))

medias_25_100 %>%
  ggplot(aes(x = medias, fill = factor(n))) +
  geom_histogram(aes(y = ..density..), bins = 20, color = "black", alpha = .5) +
  geom_vline(xintercept = 10, color = "black", linetype = "dashed") +
  scale_fill_manual(values = c("steelblue", "darkorange"), labels = c("n=25", "n=100")) +
  labs(x = "Media muestral",
       y = "Densidad") +
  theme_minimal() +
  theme(legend.position = "top", legend.title = element_blank())

## Con 200 experimentos ####
n_final <- 200
M <- 2000 # 2000 experimentos
medias <- c()

set.seed(123)
for (k in 1:M) {
  X1 <- c()
  for (i in 1:n_final) {
    X1 <- c(X1, rnorm(n = 1, mean = mu, sd = sigma))
    medias <- c(medias, mean(X1))
  }  
}

medias_tbl <- tibble(medias) %>%
  mutate(n = rep(1:n_final, M),
         experiment = rep(1:M, each = n_final))
medias_25_100 <- medias_tbl %>%
  filter(n %in% c(25, 100))

medias_25_100 %>%
  ggplot(aes(x = medias, fill = factor(n))) +
  geom_histogram(aes(y = ..density..), position = 'identity', bins = 40, color = "black", alpha = .4) +
  geom_vline(xintercept = 10, color = "black", linetype = "dashed") +
  scale_fill_manual(values = c("steelblue", "darkorange"), labels = c("n=25", "n=100")) +
  stat_function(fun = dnorm, args = list(mean = mu, sd = sigma/sqrt(25)), color = "steelblue", size = 1) +
  stat_function(fun = dnorm, args = list(mean = mu, sd = sigma/sqrt(100)), color = "darkorange", size = 1) +
  labs(x = "Media muestral",
       y = "Densidad") +
  theme_minimal() +
  theme(legend.position = "top", legend.title = element_blank())

## Con 1000 experimentos ####
n <- 1000
M <- 2000 # 2000 experimentos
medias_1 <- c()
medias_2 <- c()
mu1 <- 10
mu2 <- 9.5

set.seed(123)
for (k in 1:M) {
  X1 <- c()
  X2 <- c()
  X1 <- rnorm(n = n, mean = mu1, sd = sigma)
  X2 <- rnorm(n = n, mean = mu2, sd = sigma)
  medias_1 <- c(medias_1, mean(X1))
  medias_2 <- c(medias_2, mean(X2))
}

medias_tbl <- tibble(medias = c(medias_1, medias_2)) %>%
  mutate(condicion = rep(c("pre", "post"), each = M))

medias_tbl %>%
  ggplot(aes(x = medias, fill = condicion)) +
  geom_histogram(aes(y = ..density..), position = 'identity', bins = 40, color = "black", alpha = .4) +
  geom_vline(xintercept = 10, color = "black", linetype = "dashed") +
  scale_fill_manual(values = c("steelblue", "darkorange"), labels = c("post", "pre")) +
  stat_function(fun = dnorm, args = list(mean = mu2, sd = sigma/sqrt(1000)), color = "steelblue", size = 1) +
  stat_function(fun = dnorm, args = list(mean = mu1, sd = sigma/sqrt(1000)), color = "darkorange", size = 1) +
  labs(x = "Media muestral",
       y = "Densidad") +
  theme_minimal() +
  theme(legend.position = "top", legend.title = element_blank())

## hagamos una realización

st.seed(421)
mean(rnorm(n = 25, mean = mu1, sd = sigma)) 
mean(rnorm(n = 25, mean = mu2, sd = sigma))

