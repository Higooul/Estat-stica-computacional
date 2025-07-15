gerar_exponencial <- function(n, lambda) {
  u <- runif(n)
  x <- -log(u) / lambda
  hist(x, breaks = 30, col = "skyblue", main = paste("Exponencial(λ =", lambda, ")"))
  return(x)
}

# Exemplo: gerar 1000 valores com λ = 2
valores <- gerar_exponencial(1000, 2)

gerar_rayleigh <- function(n, sigma) {
  u <- runif(n)
  x <- sigma * sqrt(-2 * log(u))
  hist(x, breaks = 30, col = "orange", main = paste("Rayleigh(σ =", sigma, ")"))
  return(x)
}

# Exemplo: gerar 1000 valores com σ = 1.5
valores <- gerar_rayleigh(1000, 1.5)

gerar_custom <- function(n) {
  u <- runif(n)
  x <- u^(1/3)
  hist(x, breaks = 30, col = "green", main = "f(x) = 3x², 0 < x < 1")
  return(x)
}

# Exemplo: gerar 1000 valores
valores <- gerar_custom(1000)
