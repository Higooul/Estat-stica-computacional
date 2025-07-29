dados = read.csv("C:/Users/202100027495/Downloads/registros.csv")


# Transforma todos os horários em vetor único
horarios_str <- unlist(dados, use.names = FALSE)
horarios_str <- horarios_str[horarios_str != ""]

# Converte para POSIXct
horarios <- as.POSIXct(horarios_str, format = "%H:%M:%S")

# Converter para número de horas desde 08:00
minutos_desde_8 <- as.numeric(difftime(horarios, as.POSIXct("08:00:00", format = "%H:%M:%S"), units = "mins"))
minutos_desde_8 <- minutos_desde_8[minutos_desde_8 >= 0 & minutos_desde_8 <= 720]  # entre 08:00 e 20:00


# Colocar em qual hora cada cliente chegou (ex: 0 = entre 08:00-08:59, 1 = 09:00-09:59, etc)
horas <- trunc(minutos_desde_8 / 60)

# Contar quantos clientes chegaram por hora
tab <- table(horas)
lambda_hora <- as.numeric(tab) / 60  # λ(t) em clientes por minuto

# Para visualização
barplot(lambda_hora, names.arg = paste0(8 + as.numeric(names(tab)), "h"),
        main = "Estimativa de λ(t) por hora", ylab = "Clientes por minuto")

lambdat <- function(t) {
  h <- trunc(t)  # hora desde 08:00 (0 = 08h, 1 = 09h, ..., 11 = 19h)
  
  if (is.na(h) || h < 0 || h >= length(lambda_hora)) {
    return(0)  # fora do intervalo: retorna 0 (sem chegada)
  } else {
    return(lambda_hora[h + 1])  # +1 pois índice em R começa em 1
  }
}


# Mantemos sua função ppnh
ppnh <- function(lambda_t, lambda_max, T = 12){
  t <- 0; I <- 0; S <- vector()
  repeat {
    t <- t + rexp(1, lambda_max)
    if (t > T) break
    
    lt <- lambda_t(t)
    u <- runif(1)
    
    # Debug opcional (remover depois)
    # cat("t =", t, "| lambda_t(t) =", lt, "\n")
    
    if (!is.na(lt) && u <= lt / lambda_max) {
      I <- I + 1
      S[I] <- t
    }
  }
  cat("Número de eventos:", I, "\n")
  return(S)
}


# Definir lambda_max como o máximo da função estimada
lambda_max <- max(lambda_hora)

# Simular 3 dias
set.seed(1234)
tempos1 <- ppnh(lambdat, lambda_max)
tempos2 <- ppnh(lambdat, lambda_max)
tempos3 <- ppnh(lambdat, lambda_max)

# Converter para hora do dia
tempos1 <- tempos1 + 8
tempos2 <- tempos2 + 8
tempos3 <- tempos3 + 8

# Visualizar
par(mar = c(5, 0, 0, 0))
plot(c(tempos1, tempos2, tempos3),
     c(rep(2, length(tempos1)), rep(1, length(tempos2)), rep(0, length(tempos3))),
     axes = FALSE, ylim = c(-1, 3), xlim = c(8, 20),
     xlab = "Hora do dia", ylab = "", pch = 16)
segments(x0 = 8, y0 = 2:0, x1 = 20, y1 = 2:0, col = "blue")
axis(1, at = seq(8, 20, 1))


# Já carregado e convertido anteriormente:
# horarios <- as.POSIXct(horarios_str, format = "%H:%M:%S")

# Extrair apenas a hora dos horários
horas <- format(horarios, format = "%H")  # "08", "09", ...
tabela_horas <- table(horas)

print(tabela_horas)

clientes_por_hora <- c(33, 39, 42, 63, 51, 41, 31, 19)
media_simples <- mean(clientes_por_hora)
cat("Média de clientes por hora:", round(media_simples, 2), "\n")




