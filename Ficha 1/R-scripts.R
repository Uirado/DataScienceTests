
setwd("~/git/R/Ficha 1")

#install.packages('bit64')
#install.packages('data.table')
#install.packages("ggplot2")
#install.packages('ggplot2', dep=TRUE, lib=NULL)

library(data.table)
library('bit64')
library("ggplot2")



# dados das despesas da prefeitura do recife, dos anos 2015 a 2018, vindos do dados.recife.pe.gov.br
dados = fread("pre-processado/recife.despesas.2015-2018.csv", sep = ";")

# dados dos credores das despesas da prefeitura do recife, do ano de 2018, vindos do transparencia.recife.pe.gov.br
dados.credores.2018 = fread("pre-processado/dados.credores.2018.csv", sep=";")

# filtro para pegar apenas dados do ano de 2018, com as colunas nome de credor e valor pago das duas fontes de dados
dados.credores.2018.trasnp = dados.credores.2018[,
                              list(`CPF/CNPJ`, `Nome Credor`, Pago)]
dados.credores.2018.dadosrecife = dados[ano_movimentacao == 2018,
                             list(credor_nome, valor_pago)]


# normalização dos nomes das colunas
credores.2018.fonte1 = dados.credores.2018.dadosrecife[,
                         list(Pago=sum(valor_pago)), list(Credor=credor_nome)][,list(Pago=sum(Pago)), list(Credor)]
credores.2018.fonte2 = dados.credores.2018.trasnp[,
                         list(Pago=sum(Pago),`CPF/CNPJ`), list(Credor=`Nome Credor`)][,list(Pago=sum(Pago)), list(Credor, `CPF/CNPJ`)]

setkey(credores.2018.fonte1, Credor)
setkey(credores.2018.fonte2, Credor)



credores = merge(credores.2018.fonte1, credores.2018.fonte2, all = TRUE)[,list(Pago.fonte1=Pago.x, Pago.fonte2=Pago.y), list(Credor, `CPF/CNPJ`)]

# Credores, CPF/CNPJ e valores comparados de duas fontes
credores = credores.2018.fonte1[credores.2018.fonte2]

# Classificação pra saber os meses com mais e menos despesas.
despesas.meses = dados[, list(despesa=sum(valor_pago)), by=mes_movimentacao][order(-despesa)]

totalGastos = dados[,sum(valor_pago)]
despesas.meses.pct = dados[, list(despesa=sum(valor_pago), pct=sum(valor_pago)*100/totalGastos), by=mes_movimentacao][order(-despesa)]

# Classificação pra saber os credores que mais receberam da prefeitura
despesas.credor = dados[, list(despesa=sum(valor_pago)), by=credor_nome][order(-despesa)]

# Classificação pra saber quais os tipos (sub elementos) com mais despesa
despesas.subelementos = dados[, list(despesa=sum(valor_pago),
                                     pct=sum(valor_pago)*100/total), 
                                    by=subelemento_nome][order(-despesa)]


# 5 maiores credores
despesas.credores.top5 = dados[, 
                               list(Valor=sum(valor_pago)), 
                               list(Credor=credor_nome, Ano=ano_movimentacao)][order(-Valor)]

# 5 maiores credores
despesas.funcao.anos = dados[, 
                               list(Valor=sum(valor_pago)/1000000000), 
                               list(Ano=ano_movimentacao, Area=funcao_nome)][order(-Valor)]

ggplot(despesas.funcao.anos, aes(Ano, Valor)) + geom_bar(stat="identity")


despesas.funcao = dados[, 
                        list(despesa=sum(valor_pago), 
                        pct=sum(valor_pago)*100/totalGastos),
                        list(funcao_nome, subfuncao_nome)][order(funcao_nome)]

ranking.programas = dados[, list(Valor=sum(valor_pago)/1000000), list(Funcao=funcao_nome)][1:5]

ggplot(ranking.programas, aes(Funcao, Valor, fill=Funcao)) + geom_bar(stat="identity") + coord_polar("y")

# Classificação dos credores, funções, subfunções, gastos e percentual de gasto em relação ao total gasto nos 4 anos.
despesas.credores = dados[, 
                          list(despesa=sum(valor_pago), 
                               pct=sum(valor_pago)*100/totalGastos),
                          list(credor_nome, funcao_nome, subfuncao_nome)][order(funcao_nome)]

# gastos com programas
despesas.programas = dados[, list(
                              q25=quantile(valor_pago, 0.25),
                              q50=quantile(valor_pago, 0.50),
                              q75=quantile(valor_pago, 0.75),
                              total=sum(valor_pago),
                              gasto_medio=mean(valor_pago),
                              desvio_padrão=sd(valor_pago)
                                ),
                           by=list(programa_nome)]


# empresa de engenharia que mais recebe da prefeitura do recife: VITAL ENGENHARIA AMBIENTAL S/A (Queiroz Galvão)

#Programa com menor investimento: EXPANSÃO E MELHORIA NA INFRAESTRUTURA DE ATENDIMENTO EM SAÚDE



# ggplot2
# gnuplot


# 30% das despesas são para credores não informados. 66 milhões
# Explorar melhor esse resultado ano a ano

# verificar a porcentagem de gastos pra saber se o dinheiro está bem distribuido

#areas, dos credores

# ranking dos programas
#total de dinehri
#quanto ele representa da media
#desvio padrao 
#quartis

# se os programas sociais estão bem distribuidos (de dinheiro)

#como gerar relatorio sobre esses dados?