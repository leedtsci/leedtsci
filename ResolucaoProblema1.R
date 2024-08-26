# Dados da amostra 1 - Nível de Colesterol dos Adultos Fumantes
amostra1 <- c(215, 190, 282, 186, 184, 231, 240, 230, 
              178, 219, 166, 199, 221, 176, 225, 213)
# Analisando a estatística dos dados da amostra 1
info1 <- summary(amostra1)
cat("\nEstatistica dos dados da amostra 1:\n")
print(info1)
# Desvio da amostra 1
desv_amostra1 <- sd(amostra1)
cat("\nDesvio da Amostra: ", desv_amostra1, "\n")
# Verificando o tamanho da amostra
tam_amostra1 <- length(amostra1)
cat("\nQtde. de elementos da amostra 1:", tam_amostra1, "\n\n")
# Teste t da amostra
cat("Resultado do Teste T da amostra 1:")
res_amostra1 <- t.test(amostra1, mu = 200, alternative = "less")
print(res_amostra1)
# Condição para o resultado
if (res_amostra1$p.value < 0.05) {
  cat("Resultado da Amostra 1: 
Rejeita-se a hipótese nula. O nível de colesterol é menor que 200 mg/dL.
A média dos elementos da amostra 1 é", sprintf("%.2f", mean(amostra1)))
} else {
  cat("Resultado da Amostra 1:
Não se rejeita a hipótese nula. 
Evidências insuficientes para afirmar que o nível de colesterol é menor que 200 mg/dL.
A média dos elementos da amostra 1 é ", sprintf("%.2f", mean(amostra1)))
}
# Representação gráfica da amostra 1
boxplot(amostra1, horizontal = TRUE, main = "Nível de Colesterol dos Adultos Fumantes", 
        xlab = "Nível de Colesterol (mg/dL)", col = "lightgrey", border = "black")
# Definindo o limite desejável e a média da amostra
abline(v = 200, col = "red", lty = 2)
media1 <- mean(amostra1, na.rm = TRUE)
abline(v = media1, col = "blue", lty = 2)
# Legenda do gráfico
legend("topright", legend = c("Limite Desejável", "Média dos dados da amostra"), 
       col = c("red", "blue"), lty = c(2, 2), pch = c(NA, NA), cex = 0.7)
# Intervalos e rótulos do eixo x
axis(1, at = seq(170, 300, by = 5), labels = seq(170, 300, by = 5))