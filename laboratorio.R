intervalo_confianca <- function(media, dp = NULL, n = NULL, distribuicao = NULL, z = 1.96) {
  if (!is.null(distribuicao)) {
    n <- NROW(distribuicao)
    media <- mean(distribuicao)
    dp <- sd(distribuicao)
  }
  
  margem_erro <- z * dp/sqrt(n)
  c(media - margem_erro, media + margem_erro)
}

# Exemplo de uso:
# Se você quiser usar a função com parâmetros média, dp e n
intervalo_confianca(10,2,100)

# Se você quiser usar a função com um vetor de distribuição
distribuicao <- rnorm(100, mean = 10, sd = 2)
intervalo_confianca(distribuicao = distribuicao)




arrecadaco_52<-
calcula_arrecadacao_total(52, parametros)

arrecadaco_40<-
  calcula_arrecadacao_total(40, parametros)



library(ggplot2)

# Função para plotar os intervalos de confiança
plotar_intervalo_confianca <- function(intervalos, nomes) {
  data <- data.frame(Intervalo = nomes,
                     Valor = intervalos,
                     stringsAsFactors = FALSE)
  
  ggplot(data, aes(x = Intervalo, y = Valor)) +
    geom_bar(stat = "identity", fill = "skyblue", width = 0.5) +
    geom_errorbar(aes(ymin = Valor[1], ymax = Valor[2]), width = 0.2, color = "black") +
    labs(title = "Intervalos de Confiança", y = "Valor") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Valores dos intervalos de confiança e seus nomes
intervalos <- c(50, 70)  # Exemplo de intervalo de confiança
nomes <- c("Intervalo 1", "Intervalo 2")  # Nomes dos intervalos de confiança

# Plotar os intervalos de confiança
plotar_intervalo_confianca(intervalos, nomes)

plotar_intervalo_confianca(c(arrecadaco_52$intervalo_confianca,
                             arrecadaco_40$intervalo_confianca),c("Média de 52 horas semanais","Média de 40 horas semanais"))


calcula_media_intervalo_confianca<- function (intervalo_confianca_medido){
  media<- intervalo_confianca_medido[1] + intervalo_confianca_medido[2]
  
}


mean(arrecadaco_52$intervalo_confianca)



resultados <- calcula_arrecadacao_total(media_horas_trabalhadas_semanais, parametros)


# Separando os conteúdos dos componentes da lista
resultados_separados <- lapply(resultados, function(comp) {
  if (is.numeric(comp) && !grepl("total_arrecadacao_mensal|total_arrecadacao_geral", names(resultados))) {
    list(Limite_Min = min(comp), Limite_Max = max(comp))
  } else {
    comp
  }
})

# Criando novos componentes para os limites mínimo e máximo
resultados_separados$Limite_Min <- NULL
resultados_separados$Limite_Max <- NULL

# Criando novos componentes para os limites mínimo e máximo de cada componente
resultados_separados <- c(resultados_separados, lapply(resultados_separados[-c("total_arrecadacao_mensal", "total_arrecadacao_geral")], function(comp) {
  list(Limite_Min = comp$Limite_Min, Limite_Max = comp$Limite_Max)
}))

# Removendo os componentes originais de cada componente
resultados_separados <- resultados_separados[c("total_arrecadacao_mensal", "total_arrecadacao_geral", paste0(names(resultados), "_Limite_Min"), paste0(names(resultados), "_Limite_Max"))]
