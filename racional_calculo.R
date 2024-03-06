library(tidyverse)

### funções
intervalo_confianca <- function(media, dp = NULL, n = NULL, distribuicao = NULL, z = 1.96) {
  if (!is.null(distribuicao)) {
    n <- NROW(distribuicao)
    media <- mean(distribuicao)
    dp <- sd(distribuicao)
  }
  
  margem_erro <- z * dp/sqrt(n)
  c(media - margem_erro, media + margem_erro)
}


### Variáveis

remuneracao_servico_prestado<- 8.03 #parcela da remuneração mínima indicada para a remuneração do trabalhador
ressarcimento_custos<- 24.07 #parcela da remuneração mínima indicada ressarcimento de custos do trabalhador
remuneracao_minima<- remuneração_servico_prestado + ressarcimento_custos #valor por hora que deve ser garantido pelo aplicativo. corresponde  ao período entre a aceitação da viagem pelo trabalhador e a chegada do usuário ao destino

referencia_salario_contribuicao<- 0.25 #percentual sobre o valor bruto auferido no mês
aliquota_previdenciaria_empresa<- 0.2 #alíquota de contribuição à previdência aplicada sobre o salário contribuição paga pela empresa
aliquota_previdenciaria_autonomo<- 0.075 #alíquota de contribuição à previdência aplicada sobre o salário contribuição paga pelo trabalhador

tamanho_amostra<- 400
mediana_horas_trabalhadas_semanais <- 50

media_horas_trabalhadas_semanais <- 52

#### Referência de dados de uma pesquisa de BH

parametros <- list(
  remuneracao_servico_prestado = remuneracao_servico_prestado,
  ressarcimento_custos = ressarcimento_custos,
  remuneracao_minima = remuneracao_minima,
  referencia_salario_contribuicao = referencia_salario_contribuicao,
  aliquota_previdenciaria_empresa = aliquota_previdenciaria_empresa,
  aliquota_previdenciaria_autonomo = aliquota_previdenciaria_autonomo,
  tamanho_amostra = tamanho_amostra,
  mediana_horas_trabalhadas_semanais = mediana_horas_trabalhadas_semanais
)


calcula_arrecadacao_total <- function(media_horas_trabalhadas_semanais, parametros) {
  
  mediana_horas_trabalhadas_semanais <- media_horas_trabalhadas_semanais * (1 - 0.04)
  
  # Uma distribuição aleatória baseada na amostra
  
  # Considerando a amostra da pesquisa em BH
  # Identificação de uma distribuição de valores compatível com os dados da pesquisa
  
  monte_carlo <- map_dfr(1:1000, function(my_seed) {
    set.seed(my_seed)
    dp <- sample(5:20, size = 1)
    set.seed(my_seed)
    distribuicao <- rnorm(parametros$tamanho_amostra, media_horas_trabalhadas_semanais, dp)
    media <- mean(distribuicao)
    mediana <- median(distribuicao)
    diferenca <- abs(media - media_horas_trabalhadas_semanais) + abs(mediana - parametros$mediana_horas_trabalhadas_semanais)
    tibble::tibble(my_seed, media = media, mediana = mediana, dp = dp, diferenca = diferenca)
  })
  
  # Ordena o dataframe com a simulação para capturar o valor com menor diferença em relação à média e mediana
  monte_carlo <- arrange(monte_carlo, diferenca)
  
  desvio_padrao <- monte_carlo$dp[1]
  
  # Lista para armazenar os intervalos de confiança e resultados
  resultados <- list()
  
  # Intervalo de confiança
  resultados$horas_trabalhadas_semanais <- intervalo_confianca(media_horas_trabalhadas_semanais, desvio_padrao, parametros$tamanho_amostra)
  
  # Margem de erro mensal
  set.seed(monte_carlo$my_seed[1])
  distribuicao_aleatoria <- rnorm(400, mean = media_horas_trabalhadas_semanais, desvio_padrao)
  distribuicao_aleatoria_mensal <- distribuicao_aleatoria * 4
  
  resultados$horas_trabalhadas_mensais <- intervalo_confianca(distribuicao = distribuicao_aleatoria_mensal)
  
  # Parâmetros de remuneração e custos
  ###### Remuneração mínima
  
  distribuicao_remuneracao_minima <- distribuicao_aleatoria_mensal * parametros$remuneracao_minima
  resultados$remuneracao_minima <- intervalo_confianca(distribuicao = distribuicao_remuneracao_minima)
  
  ##### Salário contribuição
  
  distribuicao_salario_contribuicao <- distribuicao_remuneracao_minima * parametros$referencia_salario_contribuicao 
  resultados$salario_contribuicao <- intervalo_confianca(distribuicao = distribuicao_salario_contribuicao)
  
  ##### Contribuição empresa
  
  contribuicao_empresa <- distribuicao_salario_contribuicao * parametros$aliquota_previdenciaria_empresa
  resultados$contribuicao_empresa <- intervalo_confianca(distribuicao = contribuicao_empresa)
  
  ######### Contribuição autônomo
  
  contribuicao_autonomo <- distribuicao_salario_contribuicao * parametros$aliquota_previdenciaria_autonomo
  resultados$contribuicao_autonomo <- intervalo_confianca(distribuicao = contribuicao_autonomo)
  
  ########## Remuneração líquida de previdência
  
  remuneracao_liquida_de_previdencia <- distribuicao_remuneracao_minima - contribuicao_autonomo
  resultados$remuneracao_liquida_previdencia <- intervalo_confianca(distribuicao = remuneracao_liquida_de_previdencia)
  
  ### Arrecadação Receita previdenciária
  
  arrecadacao_receita <- contribuicao_autonomo + contribuicao_empresa
  
  resultados$arrecadacao_receita <- intervalo_confianca(distribuicao = arrecadacao_receita)
  
  # Considerando a extrapolação para o número do IBGE que indica 778 mil motoristas de aplicativo
  
  set.seed(1972)
  
  # Distribuição hipotética da arrecadação tributária no Brasil
  
  distribuicao_aleatoria_brasil <- rnorm(778000, mean = mean(arrecadacao_receita), sd(arrecadacao_receita))
  
  resultados$total_arrecadacao_mensal <- sum(distribuicao_aleatoria_brasil)
  
  resultados$total_arrecadacao_anual <- sum(distribuicao_aleatoria_brasil) * 12
  
  return(resultados)
}



possiveis_arrecadaoes<-
  purrr::map_dfr(30:52, function(horas_semanais){
    print(horas_semanais)
    tibble(media_horas_semanais = horas_semanais, 
           valor_arrecadado_estimado = calcula_arrecadacao_total(horas_semanais, parametros))
  })
  

plot(possiveis_arrecadaoes)



