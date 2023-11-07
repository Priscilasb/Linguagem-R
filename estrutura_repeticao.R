#for, while e repeat

for (i in 1:10){ #i dentro do intervalo de 1 a 10
  print(i)
}

lista <- c(1,2,3,4,10)
for (numero in lista) {
  mult = numero * 2
  print(mult)
}


c <- 1
while (c <= 10) {
  print(c)
  c <- c + 2 
}

d <- 1
while (d <= 10) {
  s = d + 10
  print(s)
  d = d + 1 #contador
}


y <- 2
repeat{
print(y)
  y <- y + 1 
  if (y >= 10) break() #quando chegar no 10 ele para
}
