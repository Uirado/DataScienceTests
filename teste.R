
setwd("~/git/R")
library(data.table)

dados = data.table(read.csv("final.csv"))

produtos = dados[,list(Produto)]
totais = dados[1,list(
  Produto, 
  Preço, 
  qtd = Quantidade, 
  Total = (Quantidade * Preço)
  )]

mean(dados$Preço, na.rm = TRUE)

