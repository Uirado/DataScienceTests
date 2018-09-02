
setwd("~/git/R/Ficha 1")

#install.packages('bit64')
#install.packages('data.table')
#install.packages("ggplot2")
#install.packages('ggplot2', dep=TRUE, lib=NULL)

library(data.table)
library('bit64')
library("ggplot2")

meses = c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")


# dados das despesas da prefeitura do recife, dos anos 2015 a 2018, vindos do dados.recife.pe.gov.br
dados = fread("pre-processado/recife.despesas.2015-2018.csv", sep = ";")



ano.mes.area.valor = dados[,
                           list(Valor=sum(valor_pago)/1000000),
                           list(
                             Ano=ano_movimentacao, 
                             mes_indice=mes_movimentacao, 
                             Mes=meses[mes_movimentacao], 
                             Area=funcao_nome,
                             SubArea=subfuncao_nome)
                           ]

ggplot(ano.mes.area.valor, aes(reorder(Mes, mes_indice), Valor)) + 
  geom_bar(stat="identity") +
  xlab("Meses") + 
  ylab("Valor gasto (em milhões)") + 
  facet_wrap(~Ano) +
  theme(axis.text.x = element_text(angle=65, 0.6))


ggplot(ano.mes.area.valor) +
  geom_histogram(aes(x=reorder(Mes, mes_indice), y=Valor, fill=Area), stat="identity") +
  ylab("Valor gasto (em milhões)") + 
  xlab("2015 a 2018")

total = sum(dados$valor_pago)
credor.valor.pct.top10 = dados[,
                        list(Valor=sum(valor_pago)/1000000, Pct=100*sum(valor_pago)/total),
                        list(Credor=credor_nome)][order(-Valor)][1:10]

ggplot(ano.mes.area.valor, aes(Ano, Valor)) + 
  geom_bar(stat="identity") +
  xlab("Anos") + 
  ylab("Valor gasto (em milhões)") + 
  facet_wrap(~Area)


# pie chart
ggplot(credor.valor.pct.top10, aes(x = "", y=Pct, fill = factor(Credor))) + 
  geom_bar(width = 1, stat = "identity") +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill="Credor", 
       x=NULL, 
       y=NULL) +
  coord_polar(theta = "y", start=0)



ggplot(credor.valor.pct.top10, aes(Credor, Valor)) +
  geom_boxplot(varwidth=T, fill="plum")


ggplot(ano.mes.area.valor, aes(x=reorder(Mes, mes_indice), Valor)) + 
  geom_point() + 
  geom_smooth(method="lm", se=FALSE) + 
  theme_bw()


# prep data
area.valor.2015 = dados[ano_movimentacao == 2015,
                        list(Valor=sum(valor_pago)/1000000),
                        by=list(Area=funcao_nome)]
area.valor.2016 = dados[ano_movimentacao == 2016,
                        list(Valor=sum(valor_pago)/1000000),
                        by=list(Area=funcao_nome)]
area.valor.2017 = dados[ano_movimentacao == 2017,
                        list(Valor=sum(valor_pago)/1000000),
                        by=list(Area=funcao_nome)]

setkey(area.valor.2015, Area)
setkey(area.valor.2016, Area)
setkey(area.valor.2017, Area)

area.valor.2015_2017 = area.valor.2015[area.valor.2016[area.valor.2017]]
colnames(area.valor.2015_2017) <- c("Area", "2015", "2016", "2017")

ggplot(area.valor.2015_2017) + geom_segment(aes(x="2015", xend="2016", y=`2015`, yend=`2016`, col=Area), size=1, show.legend=T) +
  geom_segment(aes(x="2016", xend="2017", y=`2016`, yend=`2017`, col=Area), size=1, show.legend=T) +
  xlab("Anos") + 
  ylab("Valor gasto (em milhões)")
  