library(tidyverse)

### funções
intervalo_confianca<- function(media, dp, n, z=1.96){
  margem_erro<- z * dp/sqrt(n)
  c(media-margem_erro, media+margem_erro)
}

### Variáveis

remuneracao_minima<- 32 #valor por hora que deve ser garantido pelo aplicativo. corresponde  ao período entre a aceitação da viagem pelo trabalhador e a chegada do usuário ao destino
remuneração_servico_prestado<- 8.03 #parcela da remuneração mínima indicada para a remuneração do trabalhador
ressarcimento_custos<- 24.07 #parcela da remuneração mínima indicada ressarcimento de custos do trabalhador
salario_contribuicao<- 0.25 #percentual sobre o valor bruto auferido no mês
contribuicao_previdenciaria_trabalhador<- 0.2 #alíquota de contribuição à previdência aplicada sobre o salário contribuição paga pela empresa
contribuicao_previdenciaria_autonomo<- 0.075 #alíquota de contribuição à previdência aplicada sobre o salário contribuição paga pelo trabalhador


#### Referência de dados de uma pesquisa de BH
tamanho_amostra<- 400
media_horas_trabalhadas_semanais <- 52
mediana_horas_trabalhadas_semanais <- 50


#Uma distribuição aleatória baseada na amostra


#Considerando a amostra da pesquisa em BH
#identificação de uma distribuição de valores compatível com os dados da pesquisa

monte_carlo<-
  map_dfr(1:1000, function(my_seed){
    print(my_seed)
    set.seed(my_seed)
    dp<-sample(5:20,size = 1)
    set.seed(my_seed)
    distribuicao<- rnorm(tamanho_amostra,media_horas_trabalhadas_semanais, dp)
    media<- mean(distribuicao)
    mediana<- median(distribuicao)
    diferenca<- abs(media- media_horas_trabalhadas_semanais ) + abs(mediana - mediana_horas_trabalhadas_semanais)
    tibble::tibble(my_seed, media=media, mediana=mediana, dp=dp, diferenca=diferenca)
    
  })

#ordena o dataframe com a simulação para capturar o valor com menor diferença em relação à média e mediana
monte_carlo<- arrange(monte_carlo, diferenca)


desvio_padrao<- monte_carlo$dp[1]

#intervalo de confiança
intervalo_confianca(media_horas_trabalhadas_semanais,desvio_padrao, tamanho_amostra  )


#margem de erro mensal

set.seed(1972)
distribuicao_aleatoria<-
  rnorm(400, mean = media_horas_trabalhadas_semanais,  desvio_padrao)

distribuicao_aleatoria_mensal<-
  distribuicao_aleatoria * 4


media_mensal_distribuicao<- mean(distribuicao_aleatoria_mensal)
desvio_padrao_mensal_distribuicao <- sd(distribuicao_aleatoria_mensal)



intervalo_confianca(media_mensal_distribuicao,desvio_padrao_mensal_distribuicao, tamanho_amostra)


hist(distribuicao_aleatoria_mensal)


#Parâmetros de custos


distribuicao_remuneracao_minima<- distribuicao_aleatoria_mensal * remuneracao_minima 

media_remuneracao_minima <- mean(distribuicao_remuneracao_minima)
desvio_padrao_remuneracao_minima <- sd(distribuicao_remuneracao_minima)

intervalo_confianca(media_remuneracao_minima,desvio_padrao_remuneracao_minima, tamanho_amostra)


salario_contribuicao <- distribuicao_remuneracao_minima * 0.25 

media_salario_contribuicao<- mean(salario_contribuicao)
desvio_padrao_salario_contribuicao <- median(salario_contribuicao)

intervalo_confianca(media_salario_contribuicao,desvio_padrao_salario_contribuicao, tamanho_amostra)

#Considerando a extrapolação para o número do IBGE que indica 778 mil motoristas de aplicativo

set.seed(1972)

distribuicao_aleatoria_brasil<-
  rnorm(778000, mean = media_mensal_distribuicao,  desvio_padrao_mensal_distribuicao)

hist(distribuicao_aleatoria_brasil)
