#dplyr é o pacote mais útil para realizar transformação de dados, 
#aliando simplicidade e eficiência de uma forma elegante.
if(!require(dplyr)) install.packages("dplyr")
library(dplyr)

#buscar os dados
setwd("C:/Users/prisc/Downloads/dados-covid-sp-master/dados-covid-sp-master/data")

#abrir arquivo
covid_sp <- read.csv('dados_covid_sp.csv', sep=";")
View(covid_sp)

# LIMPEZA E ORGANIZAÇÃO --------------------------------------------------------

#Renomeando colunas - rename(nome do arquivo que vai ser renomeado, nome que quer mudar = nome antigo)
covid_sp_alterado <- rename(covid_sp, municipio = nome_munic)
View(covid_sp_alterado)

covid_sp_alterado <- rename(covid_sp_alterado, data = datahora, 
                            rotulo_mapa = map_leg, codigo_mapa = map_leg_s)

#Excluir uma coluna (por nome)
covid_sp_alterado$cod_ra <- NULL 

#Excluir uma coluna (por numero)
covid_sp_alterado <- select(covid_sp_alterado, -c(21))

#Excluir varias colunas (por nome)
covid_sp_alterado <- subset(covid_sp_alterado, select = -c(codigo_ibge, cod_drs))

#Excluir varias colunas (por numero)
covid_sp_alterado <- select(covid_sp_alterado, -c(14,15)) #14 e 15 
covid_sp_alterado <- select(covid_sp_alterado, -c(17:19)) #17 ao 19

#excluir uma linha (por numero)
covid_sp_alterado <- slice(covid_sp_alterado, -c(239660))

covid_sp_alterado <- slice(covid_sp_alterado, -c(239661:239666))

#Excluir várias linhas (por nome)
covid_sp_alterado <- covid_sp_alterado %>% filter(municipio!="Ignorado")
View(covid_sp_alterado)

# MISSING ----------------------------------------------------------------------

#Valores ausentes
# NA - valores ausentes
# NAN - not a number (valor indefinido)

sapply(covid_sp_alterado, function(x) sum(is.na(x)))
sapply(covid_sp_alterado, function(x) sum(is.nan(x)))

#substituir valores missing
#tidyr - é simplificar o processo de criação de dados organizados
if(!require(tidyr)) install.packages("tidyr")
library(tidyr)

covid_sp_alterado2 <- covid_sp_alterado %>% mutate_all(replace_na, 54)
View(covid_sp_alterado2)

