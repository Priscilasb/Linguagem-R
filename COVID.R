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

#sexo
table(obitoSP20$CS_SEXO)
table(obitoSP21$CS_SEXO)
table(obitoSP22$CS_SEXO)

##idade
mean(obitoSP20$NU_IDADE_N)
mean(obitoSP21$NU_IDADE_N)
mean(obitoSP22$NU_IDADE_N)


