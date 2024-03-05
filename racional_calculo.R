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


media_horas_trabalhadas_mensais <- media_horas_trabalhadas_semanais * 4
mediana_horas_trabalhadas_mensais <- mediana_horas_trabalhadas_semanais * 4

#estimativa de variância
variancia_horas_trabalhadas_mensais<- 9*(sqrt(media_horas_trabalhadas_mensais-mediana_horas_trabalhadas_mensais) )
desvio_padrao<- sqrt(variancia_horas_trabalhadas_mensais)


#margem de erro
margem_erro<- 1.96*(media_horas_trabalhadas_mensais/sqrt(tamanho_amostra))