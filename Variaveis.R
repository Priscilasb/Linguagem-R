##### OBJETOS (VARIAVEIS) #####

m <- 4 * 7 
# OU
m = 4 * 7

print(m)
m

#NÃO USAR PALAVRAS RESERVADAS:
#BREAK, ELSE, FOR, FUNCTION, IF, IN, NEXT, REPEAT, WHILE, FALSE
#NA, NaN, NULL, TRUE ...
#NÃO COLOCAR ACENTUAÇÕES

in <- 3 + 4 #NÃO RODA
p <- 15 / 3 
p

diferenca <- m - p 
diferenca

a <- 2 
b <- 4
c <- a*b
c <- a**b

###TIPO BÁSICO DO OBJETO (MODO)

#NUMERIC: NUMERICO
#INTEGER: INTEIRO
#COMPLEX: NUMERO COMPLEXO
#CHARACTER (STRING): CARACTERE
#LOGICAL (BOOLEAN): LOGICOS (TRUE E FALSE)
#FACTOR: CATEGORIAS BEM DEFINIDAS. EX:GENERO (MASCULINO E FMEININO)
#                                     ESTADO CIVIL (CASADO, SOLTEIRO, VIUVO ...)
#                                     ANO (2019,2020,2021 ... )

y = 2
mode(y)
class(y)

y <- as.integer(y)
y
class(y)
mode(y)

x = 7.5
class(x)
x <- as.integer(x)
class(x)
x

complexo <- 2i
complexo

mode(complexo)
class(complexo)

caractere <- "palavra"
class(caractere)
mode(caractere)

logica <- TRUE
class(logica)

logica <-"TRUE"
class(logica)

genero <- c("MASCULINO", "FEMININO")
genero
class(genero)

genero <- as.factor(genero)
genero
class(genero)

## TIPO BÁSICO DO OBJETO (COMPRIMENTO)

length(genero)

p<- 43
length(p)

q <- "bom dia"
length(p)

w <- c("bom dia", "boa tarde", "boa noite")
length(w)

