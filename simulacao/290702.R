dados = read.csv("C:/Users/202100027495/Downloads/registros.csv")

# Transformar em vetor único com todos os horários
horarios_str <- unlist(dados, use.names = FALSE)
horarios_str <- horarios_str[horarios_str != ""]  # Remove vazios (caso existam)

# Converter para POSIXct (só horário)
horarios <- as.POSIXct(horarios_str, format = "%H:%M:%S")

# Ordenar todos os horários
horarios <- sort(horarios)

# Calcular interchegadas (em minutos)
inter <- diff(horarios)
inter_min <- as.numeric(inter, units = "mins")

media_inter <- mean(inter_min)
lambda <- 1 / media_inter
cat("Taxa estimada λ:", round(lambda, 4), "clientes por minuto\n")


set.seed(1234)
T <- 720  # minutos
chegadas_sim <- cumsum(rexp(1000, rate = lambda))
chegadas_sim <- chegadas_sim[chegadas_sim <= T]

# Gerar horários a partir de 08:00:00
inicio <- as.POSIXct("08:00:00", format = "%H:%M:%S")
horarios_simulados <- inicio + chegadas_sim * 60


hist(inter_min, breaks = 40, col = rgb(1,0,0,0.5),
     main = "Tempos entre chegadas: Real vs Simulada",
     xlab = "Minutos entre chegadas")
hist(diff(c(0, chegadas_sim)), breaks = 40, col = rgb(0,0,1,0.4), add = TRUE)
legend("topright", legend = c("Real", "Simulada"),
       fill = c(rgb(1,0,0,0.5), rgb(0,0,1,0.4)))



# Já carregado e convertido anteriormente:
# horarios <- as.POSIXct(horarios_str, format = "%H:%M:%S")

# Extrair apenas a hora dos horários
horas <- format(horarios, format = "%H")  # "08", "09", ...
tabela_horas <- table(horas)

print(tabela_horas)

