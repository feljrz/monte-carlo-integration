
f.function <- function(x){
  return(2*x)
}

integral <- function(N, a, b){
  vet <- c()
  current.value <- c()
  v.dp <- c()
  v.var <- c()
  e.sd <- c()
  error <- c(1)
  for(i in 1:N){
    u <- runif(1, a, b)
    vet <- sum(vet, f.function(u))
    current.value <- c(current.value, (b-a)*vet/i)
    v.dp <- c(v.dp, sd(current.value))
    v.var <- c(v.var, var(current.value))
    error <- c(error, abs(((tail(current.value, n = 2)[1] - tail(current.value, n = 2)[2])/tail(current.value, n = 2)[2])*100))
    if(!is.na(tail(error, n = 1)) & tail(error, n = 1) < 0.000001){
      break
    }
      
  }
  par(mfrow = c(1 ,3))
  plot(v.dp, xlab = "Número de Iterações", ylab = "Desvio padrão", pch = 19, col='blue')
  plot(error, xlab = "Número de Iterações", ylab = "Erro Absoluto Relativo", pch = 19, col = "red")
  plot(v.var, xlab = "Número de Iterações", ylab = "Variância", pch = 19, col = "pink")
  return(current.value)
}



a = 4
b = 7
N = 10000
y <- integral(N, a, b) #Caso deseje ver a progressão da aproximação
plot(y, xlab = "Número de Iterações", ylab = "Valor da Integral", pch = 19, col= "black")
y <- tail(y, n = 1) # Ultimo valor de y 

