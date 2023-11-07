#onde estão os arquivos
setwd("C:/Users/prisc/OneDrive/Área de Trabalho/R")

#arquivo txt
df1 <- read.table("partks.txt")
df1

#arquivos csv
df2 <- read.csv("mola.csv")
df2

df3 <- read.csv("questoes.csv")

#caso de problema nas acentuações
#df3 <- read.csv("questoes.csv"), encoding = "latin-1" ou "iso-8859-1" ou "UTF-8"

#arquivo excel
install.packages("readxl") #pacote para leitura de arquivos excel
library(readxl)

df4 <- read_xlsx("registro.xlsx")

