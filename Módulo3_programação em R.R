
# Introdução e Objetivos do Módulo ----------------------------------------
#Estruturas de controle: condicional ou seletiva
#if else, switch ou cases

#Estruturas de repetição: loops ou estruturas de iteração
#for ou foreach, while, repeat ou do-until


# Estruturas de Controle --------------------------------------------------
#IF=ELSE
faltas <- 10
nota <- 70  
if (nota>=70 & faltas<15) {
  result <- "Aprovado"
} else if (nota<70) {
  result <- "Reprovado por nota"
} else {
  result <- "Reprovado por faltas"
}
result

#EXERCÍCIO: Saudação conforme o horário em automação de e-mail
# Com base na hora do envio de uma mensagem, use
# bom dia, boa tarde ou boa noite:
#   se entre 6h00 e 11h59, então "Bom dia!";
#   se entre 12h00 e 17h59, então "Boa tarde!";
#   se entre 18h00 e 22h59, então "Boa noite!";
#   caso nenhum dos acima, não enviar a mensagem.
h <- 8
if (h>=6 & h<12) {
  saudação <- "Bom dia!"
} else if (h>=12 & h<18) {
  saudação <- "Boa tarde!"
} else if (h>=18 & h<23) {
  saudação <- "Boa noite!"
} else {
  saudação <- "Não enviar mensagem!"
  stop("Não enviar mensagem!")
}
saudação

#SWITCH
animal <- "gato"
som <- 
  switch(animal, 
         "cachorro"= {
          latir()
         },
         "gato"= {
           miar()
         },
         "vaca"= {
           mugir()
         },
         {
           silenciar()
         })
som

#EXERCÍCIO: Tipos de média
tipo <- "aritmética"
# tipo <- "harmônica"
# tipo <- "geométrica"
x <- 1:10
switch(tipo,
       "aritmética"={
         mean(x)
       },
       "harmônica" = {
         length(x)/ sum(1/x)
       },
       "geométrica"= {
         prod(x)^(1/length(x))
       },
       {
         NA_real_
       }
  
      )
#Versões vetorias
#Notas dos alunos
library(dplyr)
notas <- c("João"=70, "Ana"=89,
           "Márcia"=81, "Tiago"=65,
           "Rodrigo"=35)
#Usando IF-ELSE vetorial
ifelse(notas>=70, "Aprovado",
       ifelse(notas>=40, "Exame",
              "Reprovado"))
#Usando SWITCH vetorial
dplyr::case_when(notas>= 70 ~"Aprovado",
                 notas>=40 ~"Exame",
                 TRUE ~ "Reprovado")

# Estruturas de Repetição -------------------------------------------------
#FOR
tx_juros <- 0.01
n_meses <- 12
rend <- numeric(n_meses)
rend[1] <- 100
for (i in 2:n_meses) {
  rend[1] <- rend[i-1]*(1+tx_juros)
}
rend

y <- c(1, 2, 1, 4, 0, NA, 3, 2, 4)
s <- 0
for(i in y) {
  if(s>10) break
  if(is.na(i)) next
  s <- s+i
  print(s)
}
s

#WHILE
n_numbers <- 12
total <- 0
i <- 1L
while (i<n_numbers) {
  u <- total + runif(1)
  if (sum(u)>4) break
  total <- u
  i <- i+1L
}
total

#REPEAT
total <- 0
i <- 1L
repeat{
  u <- total+runif(1)
  if (sum(u)>4) break
  total <- u
  i <- i+1L
}
total

#EXERCÍCIO: Lançamento de dados
# Considere o seguinte jogo:
#   lançar 3 dados até que os valores das faces
#   sejam uma sequência, por exemplo
#   3, 4 e 5 ou 1, 2, 3
# 1. Use um loop WHILE ou REPEAT para contar
# quantas tentativas são necessárias até atingir o resultado
# 2. Dicas:
#   use sample() para sortear 3 valores do espaço amostral
#   em seguida, ordene os valores do menor para o maior
#   com sort()
#   calcule a diferença entre valores consecutivos com diff()
num_max <- 100
tentativas <- 1
while(tentativas<num_max) {
  l1 <- sample(1:6, 3, replace=TRUE) #joga os dados
  l1_ordenado <- sort(l1) #ordenado
  print(l1_ordenado)
  seque <- sum(ifelse(diff(l1_ordenado)==1, TRUE, FALSE)) #vendo se a diferença é 1
  if(seque==2) break
  tentativas <- tentativas +1
}
tentativas
#E se quisermos saber: em média, quantos lançamentos são necessários?
#Refaça n-vezes
#For com um while dentro do for
#Cada for determina o número de tentativas usadas no while

output <- c()
for (i in 1:1000)
  n_max <- 100
  tentativas <- 1
  while (tentativas < n_max) {
    l1 <- sample(1:6, 3, replace = TRUE)
    l1_ordenado <- sort(l1)
    print(l1_ordenado)
    seque <- sum(ifelse(diff(l1_ordenado)==1, TRUE, FALSE))
    if(seque==2) break
    tentativas <- tentativas+1
  }
  output[i] <- tentativas
mean(output)
hist(output)
#Funções -----------------------------------------------------------------
#imc= nome da função
calculo_imc <- function(peso, altura) { #lista de parâmetros
  imc <- peso/(altura^2)
  limits <- c(0, 18.5, 25, 30, Inf)
  labels <- c("Magreza", "Aquedado",
              "Pré-obeso", "Obesidade")  #corpo da função:até classif
  classif <- labels[findInterval(imc, vec = limits)]
  return(list(IMC= imc, Classificacao=classif)) #retorno da função
}
calculo_imc(peso=80, altura = 1.80)

#EXERCÍCIO: Fórmula de Bháskara
#Faça uma função que retorne as raízes
#de uma parábola com equação: ax^2+bx+c

#argumentos: a, b e c
bháskara <- function(a, b=1, c=0) { #especificou um valor default para b e c
  if (a==0){
    stop("O valor de 'a' deve ser diferente de 0")
    message("O valor de 'a' deve ser diferente de 0")
    return(c(NA_real_, NA_real_))
  }
  delta <- b^2-4*a*c
  if (delta<0) {
    return(c(NA_real_, NA_real_))
  }
  x <- (-b+ c(-1,1) * sqrt(delta))/(2*a) #o c é o + ou - da equação
}
args(bháskara)
formals(bháskara) #valores defalut
body(bháskara)

suppressMessages(bháskara(a=0, b=-3, c=3))

curve(2*x^2-3 * x -3, from = -1, to=3)
abline(h=0, col="red")

x <- bháskara(a=2, b=-3, c=-3)
x

#EXERCÍCIO: Lançamento de dados
# jogar_dados <- function(n_dados, n_max, n_simulacao) {
#   output <- c()
#   for i in 1:n_simulacao {
#     tentativas <- 1
#     while (tenativas < n_max) {
#       l1 <- 
#     }
#   }
# }


# Aspectos avançados ------------------------------------------------------
#Argumentos com valares default
calculo_imc <- function(peso=80, altura) { #lista de parâmetros
  imc <- peso/(altura^2)
  limits <- c(0, 18.5, 25, 30, Inf)
  labels <- c("Magreza", "Aquedado",
              "Pré-obeso", "Obesidade")  #corpo da função:até classif
  classif <- labels[findInterval(imc, vec = limits)]
  return(list(IMC= imc, Classificacao=classif)) #retorno da função
}
calculo_imc(altura=1.80)
#ao deixar um valor padrão, não precisa passar o peso
#para trocar: calculo_imc(altura=1.80, peso= 67)

#Tratamento de exceções
calculo_imc <- function(peso=80, altura) { #lista de parâmetros
  if(altura<0) stop("A altura deve ser maior do que zero.")
  if(peso<0) stop("O peso deve ser maior do que zero.")
  imc <- peso/(altura^2)
  limits <- c(0, 18.5, 25, 30, Inf)
  labels <- c("Magreza", "Aquedado",
              "Pré-obeso", "Obesidade")  #corpo da função:até classif
  classif <- labels[findInterval(imc, vec = limits)]
  return(list(IMC= imc, Classificacao=classif)) #retorno da função
}
calculo_imc(altura= -1)

#Função sem argumentos (não recomendado)
calculo_imc <- function() { #lista de parâmetros
  if(altura<0) stop("A altura deve ser maior do que zero.")
  if(peso<0) stop("O peso deve ser maior do que zero.")
  imc <- peso/(altura^2) 
  limits <- c(0, 18.5, 25, 30, Inf)
  labels <- c("Magreza", "Aquedado",
              "Pré-obeso", "Obesidade")  #corpo da função:até classif
  classif <- labels[findInterval(imc, vec = limits)]
  return(list(IMC= imc, Classificacao=classif)) #retorno da função
}
#calculo_imc()
peso <- 70
altura <- 1.70
calculo_imc()

#Lazy evaluation
calculo_imc <- function(altura, peso=80, altura2) { #lista de parâmetros
  if(altura<0) stop("A altura deve ser maior do que zero.")
  if(peso<0) stop("O peso deve ser maior do que zero.")
  imc <- peso/(altura^2) 
  limits <- c(0, 18.5, 25, 30, Inf)
  labels <- c("Magreza", "Aquedado",
              "Pré-obeso", "Obesidade")  #corpo da função:até classif
  classif <- labels[findInterval(imc, vec = limits)]
  return(list(IMC= imc, Classificacao=classif)) #retorno da função
}
calculo_imc(altura=1.90, peso=90, altura2 = "90")
#se colocar argumentos que não forem utilizados, o R não vai usar
#só vai utilizar o argumento quando ele for realmente chamado
