geraExp = function(lambda, n){
  u = runif(n)
  x = -log(1-u)/lambda
  return(x)
} 
set.seed(123)
valores = geraExp(lambda = 3, n = 1000)
hist(valores, breaks = 30, col = "skyblue", main = "Exponencial(λ = 3)")
hist(valores, breaks = 30, col = "skyblue", freq = FALSE,
     main = "Exponencial(λ = 3)", xlab = "x", ylab = "Densidade")
curve(dexp(x, rate = 3), col = "red", lwd = 2, add = TRUE)



gerar_rayleigh <- function(n, sigma) {
  u <- runif(n)
  x <- sigma * sqrt(-2 * log(u))
  
  return(x)
}
valores1 <- gerar_rayleigh(1000, 1.5)

hist(valores1, breaks = 30, col = "orange", main = paste("Rayleigh(σ = 1.5)"))
