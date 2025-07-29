# Função que define o valor de lambda de acordo com a hora
lambdat <- function(t){
  t <- trunc(t)
  lambda <- ifelse(t < 1, 1,
                   ifelse(t < 2, 2,
                          ifelse(t < 3, 3,
                                 ifelse(t < 4, 4,
                                        ifelse(t < 5, 5, 10)))))
  return(lambda)
}

# Função que simula o processo de Poisson
ppnh <- function(lambda_t, lambda, T = 12){ # T = 12: loja 12h aberta
  t <- 0; I <- 0; S <- vector()
  repeat{
    t <- t + rexp(1, lambda) # incrementando o tempo até nova chegada
    if(t > T)    # se o tempo t é maior que o tempo total T
      break      # sai do laço
    u <- runif(1)
    if(u <= lambda_t(t)/lambda){
      I <- I + 1  # incrementa o contador de eventos
      S[I] <- t   # registra o tempo do evento
    }
  }
  cat("Número de eventos:", I,"\n")
  return(S)
}


set.seed(1234)
tempos1 <- ppnh(lambda_t = lambdat, lambda = 10)
tempos2 <- ppnh(lambda_t = lambdat, lambda = 10)
tempos3 <- ppnh(lambda_t = lambdat, lambda = 10)

# convertendo o tempo para as horas que a loja está aberta
tempos1 <- tempos1 + 8
tempos2 <- tempos2 + 8
tempos3 <- tempos3 + 8

par(mar=c(5,0,0,0))
plot(c(tempos1, tempos2, tempos3),
     c(rep(2, length(tempos1)), rep(1, length(tempos2)),
       rep(0, length(tempos3))), axes = F, ylim = c(-1, 3),
     xlim = c(8, 20), ylab = "",
     xlab = "Tempo de ocorrência", pch = 16)
segments(x0 = 8, y0 = 2, x1 = 20, y1 = 2, col = 4)
segments(x0 = 8, y0 = 1, x1 = 20, y1 = 1, col = 4)
segments(x0 = 8, y0 = 0, x1 = 20, y1 = 0, col = 4)
axis(1, at = seq(8,20,2))
