#TITLE: ANALISE ESPACIAL DA MORTALIDADE COVID-19 EM SÃO PAULO 

#TESTE LOCAL DE ENTRADA DE DADOS
#SESSION > SET WORKING DIRECTORY

#limpeza de memoria
rm(list = ls())

##importação e instalação de pacotes
if(!require(pacman)){install.packages("pacman")}
pacman::p_load(ggplot2, psych, descr, e1071, dplyr, tidyverse, geobr, raster, ggspatial, 
               fields, sf, brazilmaps, readxl, MASS, knitr, data.table, lubridate,
               surveillance, gridExtra, grid, ggpubr, httr, rvest, readxl, gganimate)

## inserindo dados, base dos dados do SIVEP/Gripe

## 2020
## base online, atualizada
BR20 <- fread("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SRAG/2020/INFLUD20-29-08-2022.csv")

## filtro para variáveis de interesse da pesquisa
var.20 <- BR20[,c("DT_NOTIFIC", "SEM_NOT", "ID_MUNICIP", "CO_MUN_NOT", "SG_UF_NOT", "ID_REGIONA", "ID_UNIDADE",
                  "NU_IDADE_N", "CS_SEXO", "CS_RACA", "ID_MN_RESI", "CO_MUN_RES", "CS_ZONA", "HOSPITAL", "EVOLUCAO",
                  "FATOR_RISC", "UTI", "SUPORT_VEN", "CLASSI_FIN")]

## filtro para notificações do Estado do Maranhão
SP20 <- filter(var.20, SG_UF_NOT =="SP")


#### 2021      
## base excel e online, atualizada
BR21 <- fread("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SRAG/2021/INFLUD21-29-08-2022.csv")

## filtro para variáveis de interesse da pesquisa
var.21 <- BR21[,c("DT_NOTIFIC", "SEM_NOT", "ID_MUNICIP", "CO_MUN_NOT", "SG_UF_NOT", "ID_REGIONA", "ID_UNIDADE",
                  "NU_IDADE_N", "CS_SEXO", "CS_RACA", "ID_MN_RESI", "CO_MUN_RES", "CS_ZONA", "HOSPITAL", "EVOLUCAO",
                  "FATOR_RISC", "UTI", "SUPORT_VEN", "CLASSI_FIN")]

## filtro para notificações do Estado do Maranhão
SP21 <- filter(var.21, SG_UF_NOT=="SP")


#### 2022         
## base excel e online, atualizada
BR22 <- fread("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SRAG/2022/INFLUD22-29-08-2022.csv")

## filtro para variáveis de interesse da pesquisa
var.22 <- BR22[,c("DT_NOTIFIC", "SEM_NOT", "ID_MUNICIP", "CO_MUN_NOT", "SG_UF_NOT", "ID_REGIONA", "ID_UNIDADE",
                  "NU_IDADE_N", "CS_SEXO", "CS_RACA", "ID_MN_RESI", "CO_MUN_RES", "CS_ZONA", "HOSPITAL", "EVOLUCAO",
                  "FATOR_RISC", "UTI", "SUPORT_VEN", "CLASSI_FIN")]

## filtro para notificações do Estado do Maranhão
SP22 <- filter(var.22, SG_UF_NOT=="SP")

#############################
##### união das séries anuais

SP20a22 <- rbind(SP20, SP21, SP22)
View(SP20a22)

## arquivo excel com os dados da covid-19 para o SP
install.packages("writexl")
library(writexl)

write.table(SP20a22, file= "sivepSP20a22.csv", sep=";", dec=",")
write.table(SP22, file= "sivepSP22.csv", sep=";", dec=",")
write.table(SP21, file= "sivepSP21.csv", sep=";", dec=",")
write.table(SP20, file= "sivepSP20.csv", sep=";", dec=",")

#####################################
## análises descritivas, início aqui para análise
## dados gerais para sivep/gripe

sivepSP <- read.csv2("C:/Users/prisc/OneDrive/Documentos/sivepSP20a22.csv",
                     header = TRUE, sep = ";", dec = ",")

sivepSP20 <- read.csv2("C:/Users/prisc/OneDrive/Documentos/sivepSP20.csv",
                       header = TRUE, sep = ";", dec = ",")

sivepSP21 <- read.csv2("C:/Users/prisc/OneDrive/Documentos/sivepSP21.csv",
                       header = TRUE, sep = ";", dec = ",")

sivepSP22 <- read.csv2("C:/Users/prisc/OneDrive/Documentos/sivepSP22.csv",
                       header = TRUE, sep = ";", dec = ",")

### tipos de frequências:
library(descr)

#### filtro para notificações do total dos pacientes do MA
##sexo
table(sivepSP20$CS_SEXO)
table(sivepSP21$CS_SEXO)
table(sivepSP22$CS_SEXO)

##idade
mean(sivepSP20$NU_IDADE_N)
mean(sivepSP21$NU_IDADE_N)
mean(sivepSP22$NU_IDADE_N)

median(sivepSP20$NU_IDADE_N)
median(sivepSP21$NU_IDADE_N)
median(sivepSP22$NU_IDADE_N)

boxplot(sivepSP20$NU_IDADE_N)
boxplot(sivepSP21$NU_IDADE_N)
boxplot(sivepSP22$NU_IDADE_N)

##raça
table(sivepSP20$CS_RACA)
table(sivepSP21$CS_RACA)
table(sivepSP22$CS_RACA)

##zona
table(sivepSP20$CS_ZONA)
table(sivepSP21$CS_ZONA)
table(sivepSP22$CS_ZONA)

##internação hospitalar
table(sivepSP20$HOSPITAL)
table(sivepSP21$HOSPITAL)
table(sivepSP22$HOSPITAL)

##evolução
table(sivepSP20$EVOLUCAO)
table(sivepSP21$EVOLUCAO)
table(sivepSP22$EVOLUCAO)

##fator de risco
table(sivepSP20$FATOR_RISC)
table(sivepSP21$FATOR_RISC)
table(sivepSP22$FATOR_RISC)

##UTI
table(sivepSP20$UTI)
table(sivepSP21$UTI)
table(sivepSP22$UTI)

##suporte ventilatório
table(sivepSP20$SUPORT_VEN)
table(sivepSP21$SUPORT_VEN)
table(sivepSP22$SUPORT_VEN)

##classificação final
table(sivepSP20$CLASSI_FIN)
table(sivepSP21$CLASSI_FIN)
table(sivepSP22$CLASSI_FIN)

#########################
#### dados para covid-19 no MA

library(dplyr) ## aplica os filtros para qualquer variável

## filtro, notificações para COVID-19 = "5"
covidSP20 <- filter(sivepSP20, CLASSI_FIN=="5")
covidSP21 <- filter(sivepSP21, CLASSI_FIN=="5")
covidSP22 <- filter(sivepSP22, CLASSI_FIN=="5")

##sexo
table(covidSP20$CS_SEXO)
table(covidSP21$CS_SEXO)
table(covidSP22$CS_SEXO)

##idade
mean(covidSP20$NU_IDADE_N)
mean(covidSP21$NU_IDADE_N)
mean(covidSP22$NU_IDADE_N)

median(covidSP20$NU_IDADE_N)
median(covidSP21$NU_IDADE_N)
median(covidSP22$NU_IDADE_N)

boxplot(covidSP20$NU_IDADE_N)
boxplot(covidSP21$NU_IDADE_N)
boxplot(covidSP22$NU_IDADE_N)

##raça
table(covidSP20$CS_RACA)
table(covidSP21$CS_RACA)
table(covidSP22$CS_RACA)

##zona
table(covidSP20$CS_ZONA)
table(covidSP21$CS_ZONA)
table(covidSP22$CS_ZONA)

##internação hospitalar
table(covidSP20$HOSPITAL)
table(covidSP21$HOSPITAL)
table(covidSP22$HOSPITAL)

##evolução
table(covidSP20$EVOLUCAO)
table(covidSP21$EVOLUCAO)
table(covidSP22$EVOLUCAO)

##fator de risco
table(covidSP20$FATOR_RISC)
table(covidSP21$FATOR_RISC)
table(covidSP22$FATOR_RISC)

##UTI
table(covidSP20$UTI)
table(covidSP21$UTI)
table(covidSP22$UTI)

##suporte ventilatório
table(covidSP20$SUPORT_VEN)
table(covidSP21$SUPORT_VEN)
table(covidSP22$SUPORT_VEN)

### filtro para óbitos totais, evolução = 2

obitoSP20 <- filter(covidSP20, EVOLUCAO=="2")
obitoSP21 <- filter(covidSP21, EVOLUCAO=="2")
obitoSP22 <- filter(covidSP22, EVOLUCAO=="2")

#local de internação
frequency(obitoSP20$ID_MUNICIP)
frequency(obitoSP21$ID_MUNICIP)
frequency(obitoSP22$ID_MUNICIP)

#local de residência
frequency(obitoSP20$ID_MN_RESI)
frequency(obitoSP21$ID_MN_RESI)
frequency(obitoSP22$ID_MN_RESI)

##sexo
table(obitoSP20$CS_SEXO)
table(obitoSP21$CS_SEXO)
table(obitoSP22$CS_SEXO)

##idade
mean(obitoSP20$NU_IDADE_N)
mean(obitoSP21$NU_IDADE_N)
mean(obitoSP22$NU_IDADE_N)

median(obitoSP20$NU_IDADE_N)
median(obitoSP21$NU_IDADE_N)
median(obitoSP22$NU_IDADE_N)

##raça
table(obitoSP20$CS_RACA)
table(obitoSP21$CS_RACA)
table(obitoSP22$CS_RACA)

##fator de risco
table(obitoSP20$FATOR_RISC)
table(obitoSP21$FATOR_RISC)
table(obitoSP22$FATOR_RISC)

##UTI
table(obitoSP20$UTI)
table(obitoSP21$UTI)
table(obitoSP22$UTI)

##suporte ventilatório
table(obitoSP20$SUPORT_VEN)
table(obitoSP21$SUPORT_VEN)
table(obitoSP22$SUPORT_VEN)

#### filtro pra internação, hospital = "1"

hospSP20 <- filter(covidSP20, HOSPITAL=="1")
hospSP21 <- filter(covidSP21, HOSPITAL=="1")
hospSP22 <- filter(covidSP22, HOSPITAL=="1")

frequency(hospSP20$ID_MUNICIP)
frequency(hospSP21$ID_MUNICIP)
frequency(hospSP22$ID_MUNICIP)

frequency(hospSP20$ID_MN_RESI)
frequency(hospSP21$ID_MN_RESI)
frequency(hospSP22$ID_MN_RESI)

##sexo
table(hospSP20$CS_SEXO)
table(hospSP21$CS_SEXO)
table(hospSP22$CS_SEXO)

##idade
mean(hospSP20$NU_IDADE_N)
mean(hospSP21$NU_IDADE_N)
mean(hospSP22$NU_IDADE_N)

median(hospSP20$NU_IDADE_N)
median(hospSP21$NU_IDADE_N)
median(hospSP22$NU_IDADE_N)

##raça
table(hospSP20$CS_RACA)
table(hospSP21$CS_RACA)
table(hospSP22$CS_RACA)

##fator de risco
table(hospSP20$FATOR_RISC)
table(hospSP21$FATOR_RISC)
table(hospSP22$FATOR_RISC)

##UTI
table(hospSP20$UTI)
table(hospSP21$UTI)
table(hospSP22$UTI)

##suporte ventilatório
table(hospSP20$SUPORT_VEN)
table(hospSP21$SUPORT_VEN)
table(hospSP22$SUPORT_VEN)

#### filtro para óbitos em hospitalização, evolução ="2"

obitoSP20 <- filter(hospSP20, EVOLUCAO=="2")
obitoSP21 <- filter(hospSP21, EVOLUCAO=="2")
obitoSP22 <- filter(hospSP22, EVOLUCAO=="2")

#local de internação
frequency(obitoSP20$ID_MUNICIP)
frequency(obitoSP21$ID_MUNICIP)
frequency(obitoSP22$ID_MUNICIP)

#local de residência
frequency(obitoSP20$ID_MN_RESI)
frequency(obitoSP21$ID_MN_RESI)
frequency(obitoSP22$ID_MN_RESI)

##sexo
table(obitoSP20$CS_SEXO)
table(obitoSP21$CS_SEXO)
table(obitoSP22$CS_SEXO)

##idade
mean(obitoSP20$NU_IDADE_N)
mean(obitoSP21$NU_IDADE_N)
mean(obitoSP22$NU_IDADE_N)

median(obitosMA20$NU_IDADE_N)
median(obitosMA21$NU_IDADE_N)
median(obitosMA22$NU_IDADE_N)

##raça
table(obitosMA20$CS_RACA)
table(obitosMA21$CS_RACA)
table(obitosMA22$CS_RACA)

##fator de risco
table(obitosMA20$FATOR_RISC)
table(obitosMA21$FATOR_RISC)
table(obitosMA22$FATOR_RISC)

##UTI
table(obitosMA20$UTI)
table(obitosMA21$UTI)
table(obitosMA22$UTI)

##suporte ventilatório
table(obitosMA20$SUPORT_VEN)
table(obitosMA21$SUPORT_VEN)
table(obitosMA22$SUPORT_VEN)

#############################
#############################
#### agrupando dados por município

## data frame para lista dos municípios

## filtro, total de casos COVID, ano 2020
t <- as.data.frame(table(obitosMA20$CO_MUN_NOT))
t <- as.data.frame(table(obitosMA21$CO_MUN_NOT))
t <- as.data.frame(table(obitosMA22$CO_MUN_NOT))

## planilha com códigos dos municípios
codigos <- read_excel("C:/Dados - Flávio Maximino/Doutorado - Saúde Coletiva/Projeto SRAG - Análise Espacial/mapas - covid MA/códigos ibge municípios.xlsx")%>%rename(code_muni=ibge7)
codigos.ma <- filter(codigos, codigo_uf=="21") ## filtro para MA

## união das bases de acordo com as chaves
j <- merge(codigos.ma, t, by.x="ibge6", by.y="Var1", all.x=TRUE)

j$Freq[is.na(j$Freq)]<-0 #coloca zero nas frequências vazias

install.packages("clipr")
library(clipr)          ## copia e cola a tabela para o excel

write_clip(j)       ## abrir o excel e colar os dados

######## ANÁLISE ESPACIAL, PLOTAGEM DO MAPA

## mapa para SRAG por COVID-19, óbitos por local de internação e residência

### inserir planilha com as informações dos óbitos por covid-19 no MA
library(readxl)
library(geobr)
library(ggplot2)
library(brazilmaps)
library(dplyr)

mapaMA20 <- read_excel("C:/Dados - Flávio Maximino/Doutorado - Saúde Coletiva/Projeto SRAG - Análise Espacial/mapas - covid MA/covidMA - casos 2020.xlsx")
mapaMA21 <- read_excel("C:/Dados - Flávio Maximino/Doutorado - Saúde Coletiva/Projeto SRAG - Análise Espacial/mapas - covid MA/covidMA - casos 2021.xlsx")
mapaMA22 <- read_excel("C:/Dados - Flávio Maximino/Doutorado - Saúde Coletiva/Projeto SRAG - Análise Espacial/mapas - covid MA/covidMA - casos 2022.xlsx")

### incluir as taxas 

## a)	proporção de mortalidade hospitalar  por município de internação

## percentual de óbitos, mortalidade hospitalar, local de internação
mapaMA20$tx.mor.local20 <- (mapaMA20$óbitos.int /mapaMA20$hosp.intern)*100
mapaMA21$tx.mor.local21 <- (mapaMA21$óbitos.int /mapaMA21$hosp.intern)*100
mapaMA22$tx.mor.local22 <- (mapaMA22$óbitos.int /mapaMA22$hosp.intern)*100

## b)	proporção de mortalidade hospitalar  por município de residência

mapaMA20$tx.mor.resid20 <- (mapaMA20$óbitos.res /mapaMA20$hosp.resid)*100
mapaMA21$tx.mor.resid21 <- (mapaMA21$óbitos.res /mapaMA21$hosp.resid)*100
mapaMA22$tx.mor.resid22 <- (mapaMA22$óbitos.res /mapaMA22$hosp.resid)*100

##### dados do território, plotagem do mapa

## filtro para apenas um estado, modificar pelo código número code_state
## pacote 'geobr', escolher se país, UF ou município

ma.muni <- read_municipality(code_muni = "all", year=2010) %>% 
  filter(code_state == 21)   ### mapa para o MA, trocar pelo código do Estado

ggplot(ma.muni)+geom_sf(aes(fill=code_muni)) #padrão azul teste, teste

#### interpolação de dados, ano de 2020:

juntos <- full_join(ma.muni, mapaMA20, by="code_muni")  #função join, união de variáveis

#retirando os NA, para municípios sem registros
juntos[is.na(juntos)] <- 0
View(juntos)

##teste com categoria, função cut,

juntos$local20 <- cut(juntos$tx.mor.local20,breaks=c(-Inf, 1, 5, 10, 20, 30, 40, 50, 60,
                                                     70, 80, 90, 95, 99, Inf), 
                      labels=c("0,00 a 0,9", "01,0 a 04,9", "05,0 a 09,9", "10,0 a 19,9","20,0 a 29,9","30,0 a 39,9",
                               "40,0 a 49,9","50,0 a 59,9", "60,0 a 69,9", "70,0 a 79,9",
                               "80,0 a 89,9", "90,0 a 94,9 ", "95,0 a 99,9", "100,00"))

juntos$resid20 <- cut(juntos$tx.mor.resid20,breaks=c(-Inf, 1, 5, 10, 20, 30, 40, 50, 60,
                                                     70, 80, 90, 95, 99, Inf), 
                      labels=c("0,00 a 0,9", "01,0 a 04,9", "05,0 a 09,9", "10,0 a 19,9","20,0 a 29,9","30,0 a 39,9",
                               "40,0 a 49,9","50,0 a 59,9", "60,0 a 69,9", "70,0 a 79,9",
                               "80,0 a 89,9", "90,0 a 94,9 ", "95,0 a 99,9", "100,00"))

######## plotagem do mapa, estética e cores

## subtítulo, legenda no gráfico, cores padrão

## escala sem categoria, color
color <- colorRampPalette(c("white", "yellow", "orange","red", "darkred", "black"))
color <- colorRampPalette(c("white", "red", "black"))    #vermelho
color <- colorRampPalette(c("white", "gray", "black"))   #cinza
color(20)

### código padrão para mapa dos municípios

ggplot(juntos) + geom_sf(aes(fill=juntos$tx.mor.local20),           ## plotagem
                         colour = "gray", size = 0.1)+                ## linha dos municípios
  geom_sf(data = get_brmap("State", geo.filter = list(State = 21)), ## linha Estado
          fill = "transparent",
          colour = "black", size = 0.05)+
  scale_fill_gradientn(colours = color(20))+    #### escala de cor
  annotation_scale()+                           #### escala de tamanho
  annotation_north_arrow(location = 'tl', 
                         style = north_arrow_fancy_orienteering())+ ## seta direção
  labs(x=NULL, y=NULL, fill='[Letalidade\n Hospitalar - %]',        ## legenda
       title="TAXA DE MORTALIDADE HOSPITALAR POR SRAG POR COVID-19",
       subtitle = "SEGUNDO MUNICÍPIO DE INTERNAÇÃO, MARANHÃO, 2020",
       caption = "Prof. Flávio Maximino")+
  theme_classic() + theme(legend.position = c(0.9,0.2))             ## tema e posição

## salvando imagem final
ggsave(filename = 'SRAG covid MA - letalidade hospitalar - 2020 - internação.png', width = 10, height = 10 )

##################################
## Correlação Óbitos x Internações
ggplot(data = juntos, aes(x = juntos$hosp.intern, y = juntos$óbitos.int))+
  geom_point()+ geom_smooth(method = 'lm', formula = y~x, se = F)+
  theme_classic()+
  labs(x="Internações Hospitalares", y="Óbitos em Internações", 
       fill='[Óbitos / Internações]', 
       title="Correlação Entre Óbitos e Internações Hospitalares por COVID-19 nos Municípios do Maranhão",
       subtitle = "Ano de 2020")

## salvando imagem final
ggsave(filename = 'Correlação Óbitos x Internações 2020.png', width = 10, height = 10 )


##-------------------------------------------------------------------------------
#################################
## leitura dos dados do Brasil

br <- read_country(year = 2020)
View(br)
plot(br$geom)

### dados dos limites territoriais

estados <- read_state(code_state = 'all')
municipios <- read_municipality(code_muni = 'all', year = 2020)

ggplot(municipios)+geom_sf(aes(fill=code_muni))

#############################
#### agrupando dados por município

## base csv, download planilha do site, todas as notificações do país
BR20 <- read.csv2("C:/Dados - Flávio Maximino/Doutorado - Saúde Coletiva/Projeto SRAG - Análise Espacial/mapas - covid MA/INFLUD20-30-05-2022.csv",
                  header = TRUE, sep = ";", dec = ",")

## filtro para variáveis de interesse da pesquisa
var.20 <- BR20[,c("DT_NOTIFIC", "SEM_NOT", "ID_MUNICIP", "CO_MUN_NOT", "SG_UF_NOT", "ID_REGIONA", "ID_UNIDADE",
                  "NU_IDADE_N", "CS_SEXO", "CS_RACA", "ID_MN_RESI", "CO_MUN_RES", "CS_ZONA", "HOSPITAL", "EVOLUCAO",
                  "FATOR_RISC", "UTI", "SUPORT_VEN", "CLASSI_FIN")]

## filtro, notificações para COVID-19 no Brasil, classificação = "5"
covidBR20 <- filter(var.20, CLASSI_FIN=="5")

## filtro, hospitalizações para COVID-19 no Brasil, classificação = "1
hospBR20 <- filter(covidBR20, HOSPITAL=="1")

### filtro para óbitos totais por COVID-19 no Brasil, evolução = "2"

obitoBR20 <- filter(hospBR20, EVOLUCAO=="2")

## data frame para lista dos municípios

## filtro, total de casos COVID, ano 2020
br <- as.data.frame(table(obitoBR20$CO_MUN_NOT))

## planilha com códigos dos municípios
codigos <- read_excel("C:/Dados - Flávio Maximino/Doutorado - Saúde Coletiva/Projeto SRAG - Análise Espacial/mapas - covid MA/códigos ibge municípios.xlsx")%>%rename(code_muni=ibge7)

## união das bases de acordo com as chaves
dadosbrasil <- merge(codigos, br, by.x="ibge6", by.y="Var1", all.x=TRUE)

dadosbrasil$Freq[is.na(dadosbrasil$Freq)]<-0 #coloca zero nas frequências vazias

#############################################################
### plotagem do mapa para óbitos por covid-19 no Brasil, 2020

brasil <- full_join(municipios, dadosbrasil, by="code_muni")  #função join, união de variáveis

#retirando os NA, para municípios sem registros
brasil[is.na(brasil)] <- 0

## escala sem categoria, color
color <- colorRampPalette(c("white", "orange", "red", "darkred", "black"))
color <- colorRampPalette(c("white", "red", "black"))    #vermelho
color <- colorRampPalette(c("white", "black"))   #cinza
color(20)

ggplot(brasil) + geom_sf(aes(fill = brasil$Freq), 
                         colour = "NA", size = 0.1) +
  geom_sf(data = get_brmap("State"),
          fill = "transparent",
          colour = "black", size = 0.5) +
  scale_fill_gradientn(colours = color(20))+    #### escala de cor
  theme(panel.grid = element_line(colour = "transparent"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())+
  annotation_scale()+                           #### escala de tamanho
  annotation_north_arrow(location = 'tl', 
                         style = north_arrow_fancy_orienteering())+ ## seta direção
  labs(x=NULL, y=NULL, fill='[Óbitos\n COVID-19]',        ## legenda
       title="TOTAL DE ÓBITOS HOSPITALARES POR SRAG POR COVID-19 NO BRASIL",
       subtitle = "SEGUNDO MUNICÍPIO DE INTERNAÇÃO, 2020",
       caption = "Prof. Flávio Maximino")

## salvando imagem final
ggsave(filename = 'SRAG covid Brasil - Total Óbitos 2020.png', width = 10, height = 10 )


##teste com categoria, função cut,

brasil$cat <- with(brasil, cut(x = Freq, 
                               breaks =c(-Inf, 0, 5, 49, 100, 200, 500, 1000, 1500, 
                                         2000, 3000, 4000, 5000, 19000, Inf),
                               labels=c("0,00 casos","1,0 a 5,0", "5,0 a 50,0", 
                                        "50,0 a 100,0", "100,0 a 200,0", 
                                        "200,0 a 500,0", "500,0 a 1.000,0",
                                        "1.000,0 a 1.500,0",
                                        "1.500,0 a 2.000,0", "2.000,0 a 3.000,0",
                                        "3.000,0 a 4.000,0", "4.000,0 a 5.000,0", 
                                        "5.000,0 a 19.000,0",
                                        ">20.000,0"),
                               ordered_result = TRUE, right = TRUE))

## escala em categoria, paleta contínua com legenda

ggplot(brasil) + geom_sf(aes(fill = brasil$cat), 
                         colour = "NA", size = 0.1) +    ### mapa base
  geom_sf(data = get_brmap("State"),
          fill = "transparent",
          colour = "black", size = 0.5) +  ### linha dos Estados
  scale_fill_viridis_d(option = "rocket", begin = 0.01, end = 1.0,
                       direction = -1) +     #### escala de cor, formato contínuo
  theme(panel.grid = element_line(colour = "transparent"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())+          ### linha de grade e contorno
  annotation_scale()+                           #### escala de tamanho
  annotation_north_arrow(location = 'tl', 
                         style = north_arrow_fancy_orienteering())+ ## seta direção
  labs(x=NULL, y=NULL, fill='[Óbitos\n COVID-19]',        ## legenda
       title="TOTAL DE ÓBITOS HOSPITALARES POR SRAG POR COVID-19 NO BRASIL",
       subtitle = "SEGUNDO MUNICÍPIO DE INTERNAÇÃO, 2020",
       caption = "Prof. Flávio Maximino, setembro de 2022")

## salvando imagem final
ggsave(filename = 'SRAG covid Brasil - Categorias Óbitos 2020.png', width = 10, height = 10 )

