library(xml2)
library(xtable)
library(ggplot2)
library(RColorBrewer)

setwd("~/etiquetadas")

#Importação dos dados das etiquetas sintáticas
datashag <- read_xml("ShaKaw EN-PT tagged G 2.0 - sem jikai.xml")
datakaig <- read_xml("Kaizoku Fansubs JP-EN tagged G 2.0.xml")
datacreg <- read_xml("Crunchyroll JP-EN tagged G 2.0 - sem jikai.xml")
datacrpg <- read_xml("Crunchyroll JP-PT tagged G 2.0 - sem jikai.xml")

#Unindo os bancos de dados em uma única matriz
datag <- list(datashag, datakaig, datacreg, datacrpg)

#Valor para busca das etiquetas
teste <- c(".//G1",".//G2a", ".//G2b", ".//G2c", ".//G2d", ".//G3", ".//G4a", ".//G4b", ".//G5a", ".//G5b", ".//G6a", ".//G6b", ".//G7x", ".//G7", ".//G8a", ".//G8b", ".//G8c", ".//G9a", ".//G9b", ".//G10a", ".//G10b", ".//G10c", ".//G10d")

#Função de busca dos valores da variável teste nos xml e aplicação para os dados
func <- function(x, y) length(xml_find_all(x, y))
datag1 <- sapply(datag, func, y= teste[1])
for (i in 2:23){
  datag1 <- rbind(datag1, sapply(datag, func, y= teste[i]))}

#Função para contagem do número de sentenças
func2 <- function(x) xml_length(xml_children(x))
datag1 <- rbind(datag1, sapply(datag, func2))

#Legenda da matriz
legenda <- c("G1", "G2a", "G2b", "G2c", "G2d", "G3", "G4a", "G4b", "G5a", "G5b", "G6a", "G6b", "G7x", "G7", "G8a", "G8b", "G8c", "G9a", "G9b", "G10a", "G10b", "G10c", "G10d", "Sentenças")


rownames(datag1) <- legenda
colnames(datag1) <- c("ShaKaw","Kaizoku","Crunchyroll-EN","Crunchyroll-PT")


#Gráfico
barplot(t(datag1), main="Estratégias Sintáticas por Grupo", ylab="Número de Estratégias", legend.text = colnames(datag1), beside=TRUE, col = brewer.pal(n=4, name="Set3"), args.legend = list(x="top"), ylim = c(0,400))

#Geração automática de gráficos
for (i in 1:23)
barplot(t(datag1[i,1:4]), main=paste("Estratégias",rownames(datag1)[i],"por Grupo"), ylab="Número de Estratégias", legend.text = colnames(datag1), beside=TRUE, col = brewer.pal(n=4, name="Set3"), args.legend = list(x="top"))
png(file = paste("Gráfico",rownames(datag1)[i],".png"))
}


#Agrupamento das estratégias subdivididas e gráfico
datag2 <- rbind(datag1[1,1:4],datag1[2,1:4]+datag1[3,1:4]+datag1[4,1:4]+datag1[5,1:4], datag1[6,1:4], datag1[7,1:4]+datag1[8,1:4],datag1[9,1:4]+datag1[10,1:4],datag1[11,1:4]+datag1[12,1:4],datag1[13,1:4]+round(datag1[14,1:4]/2),datag1[15,1:4]+datag1[16,1:4]+datag1[17,1:4],datag1[18,1:4]+datag1[19,1:4],datag1[20,1:4]+datag1[21,1:4]+datag1[22,1:4]+datag1[23,1:4],datag1[24,1:4])
rownames(datag2) <- c("G1","G2", "G3", "G4","G5","G6","G7","G8","G9","G10","Sentenças")
barplot(t(datag2), main="Estratégias Sintáticas por Grupo", ylab="Número de Estratégias", legend.text = colnames(datag2), beside=TRUE, col = brewer.pal(n=4, name="Set3"), args.legend = list(x="top", cex = 1.0), ylim = c(0,400))


#Importação dos dados das etiquetas semânticas
datashas <- read_xml("ShaKaw EN-PT tagged S.xml")
datakais <- read_xml("Kaizoku Fansubs JP-EN tagged S.xml")
datacres <- read_xml("Crunchyroll JP-EN tagged S - sem jikai.xml")
datacrps <- read_xml("Crunchyroll JP-PT tagged S - sem jikai.xml")

datas <- list(datashas, datakais, datacres, datacrps)

teste2 <- c(".//S0",".//S1", ".//S2", ".//S3", ".//S4", ".//S5", ".//S6", ".//S7", ".//S8", ".//S9", ".//S10")
datas1 <- sapply(datas, func, y= teste2[1])
for (i in 2:11){
  datas1 <- rbind(datas1, sapply(datas, func, y= teste2[i]))}
datas1 <- rbind(datas1, sapply(datas, func2))

#Legendas para a matriz de etiquetas semânticas e gráfico
legenda2 <- c("S0","S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8", "S9", "S10", "Sentenças")

rownames(datas1) <- legenda2
colnames(datas1) <- c("ShaKaw","Kaizoku","Crunchyroll-EN","Crunchyroll-PT")
barplot(t(datas1), main="Estratégias Semânticas por Grupo", ylab="Número de Estratégias", legend.text = colnames(datag1), beside=TRUE, col = brewer.pal(n=4, name="Set3"), args.legend = list(x="top"), ylim = c(0,400))

#Importação dos dados das etiquetas pragmáticas
datashap <- read_xml("ShaKaw EN-PT tagged Pr.xml")
datakaip <- read_xml("Kaizoku Fansubs JP-EN tagged Pr.xml")
datacrep <- read_xml("Crunchyroll JP-EN tagged Pr.xml")
datacrpp <- read_xml("Crunchyroll JP-PT tagged Pr.xml")

datap <- list(datashap, datakaip, datacrep, datacrpp)

teste3 <- c(".//Pr0",".//Pr1", ".//Pr2", ".//Pr3", ".//Pr4", ".//Pr5", ".//Pr6", ".//Pr7", ".//Pr8", ".//Pr9", ".//Pr10")
datap1 <- sapply(datap, func, y= teste3[1])
for (i in 2:11){
  datap1 <- rbind(datap1, sapply(datap, func, y= teste3[i]))}
datap1 <- rbind(datap1, sapply(datap, func2))

legenda3 <- c("Pr0","Pr1", "Pr2", "Pr3", "Pr4", "Pr5", "Pr6", "Pr7", "Pr8", "Pr9", "Pr10", "Sentenças")

rownames(datap1) <- legenda3
colnames(datap1) <- c("ShaKaw","Kaizoku","Crunchyroll-EN","Crunchyroll-PT")
barplot(t(datap1), main="Estratégias Pragmáticas por Grupo", ylab="Número de Estratégias", legend.text = colnames(datag1), beside=TRUE, col = brewer.pal(n=4, name="Set3"), args.legend = list(x="top"))

#Gráficos G2
barplot(t(datag1[2:5,1:4]), main= "Estratégias G2 por Grupo", ylab="Número de Estratégias", xlab="Empréstimos e Calques", legend.text = colnames(datag1), beside=TRUE, col = brewer.pal(n=4, name="Set3"), args.legend = list(x="topright", cex=1.0))

#Gráficos G4
barplot(t(datag1[7:8,1:4]), main= "Estratégias G4 por Grupo", ylab="Número de Estratégias", xlab="Deslocamento de Unidade", legend.text = colnames(datag1), beside=TRUE, col = brewer.pal(n=4, name="Set3"), args.legend = list(x="topright", cex=1.2), ylim = c(0,120))

#Gráficos G5
barplot(t(datag1[9:10,1:4]), main= "Estratégias G5 por Grupo", ylab="Número de Estratégias", xlab="Mudança na Estrutura da Frase", legend.text = colnames(datag1), beside=TRUE, col = brewer.pal(n=4, name="Set3"), args.legend = list(x="topright", cex = 0.7), ylim = c(0,40))


#Gráficos G6
barplot(t(datag1[11:12,1:4]), main= "Estratégias G6 por Grupo", ylab="Número de Estratégias", xlab="Mudança na Estrutura da Oração", legend.text = colnames(datag1), beside=TRUE, col = brewer.pal(n=4, name="Set3"), args.legend = list(x="topright"), ylim = c(0,120))


#Gráficos G7
barplot(t(datag1[13:14,1:4]), main= "Estratégias G7 por Grupo", ylab="Número de Estratégias", xlab="Mudança na Estrutura do Período", legend.text = colnames(datag1), beside=TRUE, col = brewer.pal(n=4, name="Set3"), args.legend = list(x="topright", cex = 0.9),ylim = c(0,60))


#Gráficos G8
barplot(t(datag1[15:17,1:4]), main= "Estratégias G8 por Grupo", ylab="Número de Estratégias",  xlab="Mudança Coesiva", legend.text = colnames(datag1), beside=TRUE, col = brewer.pal(n=4, name="Set3"), args.legend = list(x="topright", cex = 0.8),ylim = c(0,140))


#Gráficos G9
barplot(t(datag1[18:19,1:4]), main= "Estratégias G9 por Grupo", ylab="Número de Estratégias", xlab="Mudança de Nível",legend.text = colnames(datag1), beside=TRUE, col = brewer.pal(n=4, name="Set3"), args.legend = list(x="topright", cex = 1.2),ylim = c(0,100))

#Gráficos G10
barplot(t(datag1[20:23,1:4]), main= "Estratégias G10 por Grupo", ylab="Número de Estratégias", xlab="Mudança de Esquema", legend.text = colnames(datag1), beside=TRUE, col = brewer.pal(n=4, name="Set3"), args.legend = list(x="topleft"), ylim = c(0,16))


#Testes estatísticos
#H1a
t.test(cbind(dataall[1:45,2],dataall[1:45,1]), cbind(dataall[1:45,3],dataall[1:45,4]), paired = TRUE)
#H1b
t.test(dataall[1:45,1],dataall[1:45,2], paired = TRUE)
#H1c
t.test(dataall[1:45,3],dataall[1:45,4], paired = TRUE)

#H2a
t.test(cbind(dataall[1:45,2],dataall[1:45,3]), cbind(dataall[1:45,1],dataall[1:45,4]), paired = TRUE)
t.test(cbind(dataall[1:45,2],dataall[1:45,3]), cbind(dataall[1:45,1],dataall[1:45,4]))

#H2b
t.test(dataall[1:45,1],dataall[1:45,4], paired = TRUE)
#H2c
t.test(dataall[1:45,2],dataall[1:45,3], paired = TRUE)

#H3a
prop.test(dataall[1,1:4],datag1[24,1:4],alternative = "greater")
prop.test(dataall[1,1],datag1[24,1], p = mean(dataall[1,2:4]/datag1[24,2:4]), alternative = "greater")

#H3b
prop.test(datag2[2,1:4],datag2[11,1:4],alternative = "greater")
prop.test(mean(datag2[2,1:2]),mean(datag2[11,1:2]), p= mean(datag2[2,3:4]/datag2[11,3:4]),alternative = "greater")

prop.test(dataall[2,1:4],datag1[24,1:4],alternative = "greater")
prop.test(dataall[3,1:4],datag1[24,1:4],alternative = "greater")
prop.test(dataall[4,1:4],datag1[24,1:4],alternative = "greater")
prop.test(dataall[5,1:4],datag1[24,1:4],alternative = "greater")

prop.test(mean(dataall[3,1:2]),mean(dataall[11,1:2]), p= mean(dataall[3,3:4]/datag2[11,3:4]),alternative = "greater")
prop.test(mean(dataall[4,1:2]),mean(dataall[11,1:2]), p= mean(dataall[4,3:4]/datag2[11,3:4]),alternative = "greater")
prop.test(dataall[4,2:3],datag1[24,2:3],alternative = "greater")
prop.test(mean(dataall[5,1:2]),mean(dataall[11,1:2]), p= mean(dataall[5,3:4]/datag2[11,3:4]),alternative = "greater")

#H4a
prop.test(dataall[32,1:4],datag1[24,1:4],alternative = "less")
prop.test(dataall[32,1],datag1[24,1],p = mean(dataall[32,2:4]/datag1[24,2:4]), alternative = "less")

#H4b
prop.test(dataall[26,1:4],datag1[24,1:4],alternative = "greater")
prop.test(mean(dataall[26,2:3]),mean(datag1[24,2:3]),p = mean(cbind(dataall[26,1],dataall[26,4])/cbind(datag1[24,1],datag1[24,4])), alternative = "greater")

#H5a
prop.test(dataall[36,1:4],datag1[24,1:4],alternative = "less")
prop.test(mean(dataall[36,1:2]),mean(datag1[24,1:2]), p = mean(dataall[36,3:4]/datag2[11,3:4]), alternative = "less")

#H5b
prop.test(dataall[42,1:4],datag1[24,1:4],alternative = "greater")
prop.test(mean(dataall[42,1:2]),mean(datag1[24,1:2]),p = mean(dataall[42,3:4]/datag2[11,3:4]), alternative = "greater")
