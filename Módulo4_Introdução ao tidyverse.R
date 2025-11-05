
# Introdução ao tidyverse -------------------------------------------------
# Reimplementação e extensão das funcionalidades do R
# Traz uma sintaxe nova e mais expressiva -> manipulação e visualização de dados
# 
# PRINCÍPIOS DOS DADOS ORGANIZADOS
# linha <- observações
# colunas <- variáveis
# células <- valores (tipo de unidade observacional)

# Importação de dados -----------------------------------------------------
library(tidyverse)
url <- "http://leg.ufpr.br/~wagner/scientificR/reglinear.csv"
dados <- read_tsv(url, col_names = TRUE)
head(dados)

#PLANILHA EXCEL
library(readxl)
library(httr)
url2 <- "http://leg.ufpr.br/~wagner/scientificR/meus_dados.xlsx"
GET(url2, write_disk(tf <- tempfile(fileext = ".xlsx"))) #fazer o download em disco
tb <- read_excel(tf, sheet = "mtcars")
View(tb)

# Consumindo APIs ---------------------------------------------------------
## Script 2: Exemplos de acesso a APIs públicas --------------------------------
## Prof. Wagner Hugo Bonat · LEG/UFPR ------------------------------------------

## Carregando pacotes adicionais
library(httr)
library(jsonlite)
library(dplyr)

# -----------------------------
# 1. API do IBGE - Lista de Estados do Brasil
# Documentação: https://servicodados.ibge.gov.br/api/docs/
# -----------------------------
res_ibge <- GET("https://servicodados.ibge.gov.br/api/v1/localidades/estados")

# Verificando status da resposta
stop_for_status(res_ibge)

# Conteúdo da resposta (parsed = já em lista R)
estados <- content(res_ibge, as = "parsed", encoding = "UTF-8")
estados

# Organizando para ter um data.frame
df_estados <- tibble::tibble(
  id = sapply(estados, `[[`, "id"),
  nome = sapply(estados, `[[`, "nome"),
  sigla = sapply(estados, `[[`, "sigla"),
  regiao = sapply(estados, function(x) x$regiao$nome)
)

head(df_estados)

# -----------------------------
# 2. API Open-Meteo - Previsão do tempo (sem chave)
# Documentação: https://github.com/open-meteo/open-meteo
# -----------------------------

res_meteo <- GET("https://api.open-meteo.com/v1/forecast",
                 query = list(latitude = -23.55, 
                              longitude = -46.63, 
                              hourly = "temperature_2m"))

stop_for_status(res_meteo)

dados_meteo <- content(res_meteo, as = "parsed", encoding = "UTF-8")
dados <- data.frame('tempo' = unlist(dados_meteo$hourly$time), 
                    'temperatura' = unlist(dados_meteo$hourly$temperature_2m))

head(dados)

# -----------------------------
# 3. Exemplo usando a RapidAPI (cuidado isso é um serviço pago que tem algumas requisições grátis!)
# TikTok Scraper: https://rapidapi.com/tikwm-tikwm-default/api/tiktok-scraper7
# -----------------------------

## Exemplo que precisa de chave de API.
## A minha chave está guardada no arquivo .env (não disponibilizado)
## Você pode criar a sua chave no site rapidapi.com API Tiktok Scraper.
## Grátis para até 300 requisições mês

## Carregando a chave de API
# library(dotenv)
# dotenv::load_dot_env(file = "/home/wagner/gitprojects/R_Avançado_CNJ/Modulo9/.env")
# Sys.getenv("TIKTOK_API_KEY")
# 
# 
# url <- "https://tiktok-scraper7.p.rapidapi.com/feed/list"
# 
# queryString <- list(
#   region = "br",
#   count = "20"
# )
# 
# response <- VERB("GET", url, query = queryString, 
#                  add_headers('x-rapidapi-key' = Sys.getenv("TIKTOK_API_KEY"), 
#                              'x-rapidapi-host' = 'tiktok-scraper7.p.rapidapi.com'), 
#                  content_type("application/octet-stream"))
# 
# saida <- content(response, "parsed")
# saida$code
# saida$msg
# saida$processed_time
# saida$data[[2]]

#NÃO RODAR - API PAGA

# Introdução à Manipulação de dados ---------------------------------------


# Setup pipe --------------------------------------------------------------
#Carregando pacotes neecessários
require(tidyverse)

dados <- readr::read_csv("data/Mental Health Dataset.csv")
head(dados, 2) #função para ver os dados, pode usar glimpse(dados) também

#EXEMPLO DO USO DO OPERADOR PIPE
#Suponha que queremos calcular o cos dos valores únicos de um vetor x,
#ordená-los em ordem decrescente

#Sem o uso do pipe
x <- c(-2:2)
x
sort(cos(
  unique(x)),
  decreasing = TRUE)

#Com o pipe %>%
require(magrittr)
x %>%
  unique() %>%
  cos() %>%
  sort(decreasing = TRUE)

##Pipe de atribuição
y <- 1:10
y
y %<>% log()
y


# Mutate e select ---------------------------------------------------------
#Mutate: criar novas variáveis
dados <- dados %>%
  mutate(mercosul= ifelse(
    Country %in%
      c("Argentina", "Brazil", "Paraguay", "Uruguay"),
    "Mercosul", "Não Mercosul"
  ))
glimpse(dados)
table(dados$mercosul)

#Selecionar as variáveis pelo nome
dados2 <- dados %>%
  select(Country, Timestamp, Days_Indoors, mercosul)
glimpse(dados2)
#Selecionar as variáveis pelos índices
dados3 <- dados %>%
  select(3:5)
glimpse(dados3)
#Seleção intervalar
dados4 <- dados %>%
  select(treatment:Changes_Habits)
glimpse(dados4)
#Selecionar variáveis com algum padrão
dados5 <- dados %>%
  select(starts_with("t")) #começam com a letra t
glimpse(dados5)

dados6 <- dados %>%
  select(ends_with("s")) #terminam com s

dados7 <- dados %>%
  select(contains("ing")) #palavras que contêm "ing"
glimpse(dados7)

dados8 <- dados %>%
  select(matches("[tT]")) #contém t ou T
glimpse(dados8)

#Remover variáveis usando o select
dados9 <- dados %>%
  select(-Country, -Timestamp, -Days_Indoors, -mercosul)
glimpse(dados9)

dados10 <- dados %>%
  select(-c(Country, Timestamp, Days-Indoors, mercosul))
glimpse(dados10)

#Seleção de variáveis por tipos específicos de dados (critério específico)
dados11 <- dados %>%
  select_if(is.character()) #selecionar as variáveis que são do tipo character
glimpse(dados11)

dados12 <- dados %>%
  select_if(is.numeric()) #selecionar as do tipo numeric
glimpse(dados12)

#Seleção por critérios - definidos externamente
variaveis <- c("Country", "Timestamps", "Days_Indoors", "mercosul")
dados13 <- dados %>%
  select(all_of(variaveis)) #selecionar aquelas que atendem ao critério externo
glimpse(dados13)

dados14 <- dados %>%
  select(any_of(variaveis))
glimpse(dados14)

# Filter ------------------------------------------------------------------
library(data.table)
car_crash <- fread("C:/Users/acdin/OneDrive/Documentos/UFPR- Curso de Estatística/2º Período/CE302- Elementos de Programação para Estatística/Script R/Aulas/CE302_aulas_RProg/archive/Brazil Total highway crashes 2010 - 2023.csv")
glimpse(car_crash)

#Filtrando linhas com filter ()
car_crash2 <- car_crash %>%
  filter(tipo_de_ocorrencia=="sem vítima") #ocorrência que não tenha vítimas
glimpse(car_crash2)

#Filtros combinados: múltiplas condições (operadores lógicos)
car_crash3 <- car_crash %>%
  filter(tipo_de_ocorrencia=="sem vítmia" & automovel>=3)
glimpse(car_crash3)

#Filtros envolvendo intervalos (between)
car_crash4 <- car_crash %>%
  filter(between(automovel, 3, 5)) #funciona quando se tem números
glimpse(car_crash4)

#Filtrando intervalos de strings (palavras) com o perador %in%
car_crash5 <- car_crash %>%
  filter(tipo_de_ocorrencia %in% c("sem vítima", "com vítima"))
glimpse(car_crash5)

tipos <- c("sem vítima", "com vítima")

#Filtrar o contrário (! ou not in -> %ni% <- Negate(%in%))
car_crash6 <- car_crash %>%
  filter(!tipo_de_ocorrencia %in% tipos)
glimpse(car_crash6)

#Usando o %ni%
`%ni%` <- Negate(`%in%`)
car_crash7 <-  car_crash %>%
  filter(tipo_de_ocorrencia %ni% tipos)
glimpse(car_crash7)

#Buscando padrões com filter()
car_crash8 <- car_crash %>%
  filter(tipo_de_ocorrencia %like% "vítima") #buscar as observações que têm a palavras "vítima"
glimpse(car_crash8)

#Filtrar por textos específicos
car_crash9 <- car_crash %>%
  filter(grepl("ilesa|fatal", tipo_de_ocorrencia))
glimpse(car_crash9)

# Arrange -----------------------------------------------------------------
#Ordenar por ordem descrescente
car_crash10 <- car_crash %>%
  arrange(desc(automovel))

#Ordenar mais de uma variável (uma decrescente e outra crescente)
car_crash11 <- car_crash %>%
  arrange(desc(automovel), mortos) %>%
  select(automovel, mortos)
  #na.exclude()
glimpse(car_crash11)
head(car_crash11)

#Fatiando linhas com slice()
car_crash_slice1 <- car_crash %>%
  select(1:5) %>% #selecionar as variáveis de 1 a 5
  slice(3:5) #selecionar as linhas 3 a 5
car_crash_slice1

#Fatiar as 3 primeiras linhas
car_crash_slice2 <- car_crash %>%
  select(1:5) %>% #selecionar as variáveis de 1 a 5
  slice_head(n=3) #selecionar as linhas 3 a 5
car_crash_slice2



# Exercício 1 -------------------------------------------------------------
#1. Filtre as observações cujo tipo de evento é Tropical Depression.
#Quantas observações existem?
storms %>%
  filter(status == "tropical depression") %>%
  nrow()

#2. Filtre as observações cujo tipo de evento é Tropical Depression 
#e a velocidade do vento é maior ou igual a 40. Quantas observações existem?
storms %>%
  filter(status=="tropical depression" & wind>=40) %>%
  nrow()

# 3. Selecione as variáveis numéricas e ordene as observações pela 
# variável pressure em ordem crescente
storms %>%
  select_if(is.numeric)%>%
  arrange(pressure)


# Rename ------------------------------------------------------------------
#renomear a variável automóvel
car_crash12 <- car_crash %>%
  rename(numero_automoveis = automovel)
glimpse(car_crash12)

#Realocando colunas com relocate()
#realocar a variável automoveis para a 1º posição do banco de dados
car_crash_relocate = car_crash %>%
  relocate(automovel, .before = 1)
glimpse(car_crash_relocate)

#realocar para a última
car_crash_relocate2 = car_crash %>%
  relocate(automovel, .after = last_col())
glimpse(car_crash_relocate2)

#Transformando dados com transmute
car_crash_transmute <- car_crash %>%
  transmute(automovel_10 = automovel/10)
glimpse(car_crash_transmute)

#Alterando NA com replace_na()
car_crash_replace_na <- car_crash %>%
  mutate(mortos = replace_na(mortos, 0))
glimpse(car_crash_replace_na)

#Classificando dados com cut()
car_crash_cut <- car_crash %>%
  mutate(autmovel = replace_na(automovel, 0))%>%
  mutate(automovel_cat = cut(automovel,
                             breaks = c(-Inf, 0, 3, Inf),
                             labels= c("sem automóveis",
                                       "entre 1 e 3 automóveis",
                                       "mais do que três")))
glimpse(car_crash_cut)
table(car_crash_cut$autmovel,
      car_crash_cut$automovel_cat)


# Summarize ---------------------------------------------------------------
Car_crash13 <- car_crash %>%
  summarise(total_automoveis = sum(automovel, na.rm = TRUE))
glimpse(Car_crash13)

#Sumarizar mais de uma var. (total de autmoveis e mortos)
car_crash14 <- car_crash %>%
  summarise(total_automoveis = sum(automovel, na.rm=T),
            total_mortos = sum(mortos, na.rm = TRUE),
            n = n(),
            media_mortos = mean(mortos, na.rm=T))
car_crash14

#Agrupando dados com group_by()
library(lubridate) #para trabalhar com datas
car_crash15 <- car_crash %>%
  mutate(ano = year(dmy(data))) %>% #criação da variável ano
  group_by(ano)
glimpse(car_crash15)

#Sumarizando dados com summarise()
#Sumarizar o nº total de automóveis envolvidos em acidentes e o número total de mortos por ano
car_crash16 <- car_crash %>%
  mutate(ano = year(dmy(data))) %>%
  group_by(ano) %>%
  summarise(total_automoveis = sum(automovel, na.rm = T),
            total_mortos = sum(mortos, na.rm = T))
head(car_crash16)

#Encadeando funções
#filtar as obs. cujo tipo de ocorrência é com vítima e
#sumarizar o número total de autmóveis envolvidos em acidentes e o nº total de mortos
car_crash17 <-  car_crash %>%
  filter(tipo_de_ocorrencia == "com vítima") %>%
  summarise(total_automoveis = sum(automovel, na.rm = T),
            total_mortos = sum(mortos, na.rm = T))
car_crash17


# Exercício 2 -------------------------------------------------------------
# 1. Utilizando os dados starwars faça o que se pede:
#   Qual é o número total de espécies únicas presentes? Qual a frequência de
#   indivíduos por espécie?
starwars %>%
  summarise(n_total_especies = n_distinct(species))

starwars %>%
  group_by(freq_especies = n()) %>%
  arrange(desc(freq_especies))

# 
#   Calcule a altura média de personagens masculinos e femininos.
starwars %>%
  filter(sex %in% c("female", "male")) %>%
  group_by(sex) %>%
  summarise(media_altura = mean(height, na.rm=TRUE))


#   Qual é o peso médio dos personagens de cada espécie para personagens masculinos?
starwars %>%
  filter(sex == "male") %>%
  group_by(species) %>%
  summarise(media_peso = mean(mass, na.rm =TRUE))

#   Para cada espécie prsente na base de dados, identifique o personagem mais pesado
#   e seu peso correspondente.
starwars %>%
  group_by(species) %>%
  filter(mass == max(mass, na.rm=TRUE)) %>%
  select(species, name, mass)
  


# Lubridate ---------------------------------------------------------------
#Manipulação de datas
car_crash %>%
  mutate(data = dmy(data)) %>%
  select(data) %>%
  head(n=2)

car_crash %>%
  mutate(data = dmy(data)) %>%
  mutate(ano = year(data),
         mes = month(data),
         dia = day(data)) %>%
  select(data, ano, mes, dia) %>%
  head()

#Calcular a diferente de duas datas usando difftime()
car_crash %>%
  mutate(data = dmy(data)) %>%
  mutate(dias_desde_acidente = difftime(Sys.Date(), data, units = "days")) %>%
  select(data, dias_desde_acidente) %>%
  head()

#Somar ou substituir dias de uma data usando lubridate::days()
car_crash %>%
  mutate(data = dmy(data)) %>%
  mutate(data_mais_10_dias = data + lubridate::days(10)) %>%
  select(data, data_mais_10_dias) %>%
  head()

#Manipulação de datas - horas, minutos e segundos: hour(), minute() e second()
data <- ymd_hms("2023-08-21 15:30:45")
ano <- year(data)
mes <- month(data)
dia <- day(data)
hora <- hour(data)
minuto <- minute(data)
segundo <- second(data)
print(ano)
print(mes)
print(dia)
print(hora)
print(minuto)
print(segundo)

#Conversão de fuso horário - with_tz()
#data original do fuso horário de Nova Iorque
data_ny <- ymd_hms("2025-10-21 12:00:00", tz= "America/New_York")
#Converter para fuso horário de Londres
data_london <- with_tz(data_ny,tz="Europe/London")
print(data_london)


# Exercício 3 -------------------------------------------------------------

# 1. Utilizando o banco de dados car_crash, faça o que se pede:
#   quais os meses do ano com maior número de acidentes fatais?
car_crash %>%
  mutate(data = dmy(data)) %>% #formatar para data
  mutate(ano = year(data), #preciso da informação do mês e do ano
         mes =  month(data)) %>%
  select(data, ano, mes, mortos) %>%
  filter(mortos >0)%>%
  group_by(mes) %>%
  summarise(total_mortos = sum(mortos)) %>%
  arrange(desc(total_mortos))
#   
#   quais os dias da semana com maior número de acidentes fatais?
car_crash %>%
  mutate(data = dmy(data)) %>%
  mutate(dia_semana = lubridate::wday(data, label = T, abbr = F)) %>% #abbr -> não abreviar
  select(dia_semana, mortos) %>%
  filter(mortos>0) %>%
  group_by(dia_semana) %>%
  summarise(total_mortos_dia = sum(mortos)) %>% #contar quantos mortos naquele dia da semana
  arrange(desc(total_mortos_dia))
#     dica: busque por uma função que retorne o dia da semana a partir de uma data
 

# Pivot -------------------------------------------------------------------
table1
#Transformar dados do long para wide pivot_wider()
table1 %>%
  select(-population) %>%
  pivot_wider(names_from = year,
              values_from = cases)

#pivotando com mais de uma variável
table1 %>%
  pivot_wider(names_from = year,
              values_from = c(cases, population))

#Transformar de wide para long - pivot_longer()
table1 %>%
  pivot_longer(cols = c(cases, population),
               names_to = "variable",
               values_to = "total")

#Separando observações
#em diferentes colunas - separete()
table3 %>%
  separate(rate, into =c("cases", "population"))

#Juntando observações
#de diferentes colunas em uma unite()
table1%>%
  unite(rate, cases, population, sep="/")

# Exercício 4 -------------------------------------------------------------
# 1. Utilizando os dados de flights, do pacote {nycflights13}, crie uma matriz
# que mostra o número de voos entre cada par de aeroportos
install.packages("nycflights13")
library(nycflights13)
flights %>%
  count(origin, dest)%>%
  pivot_wider(names_from = origin,
              values_from = n,
              values_fill = 0)


# String ------------------------------------------------------------------
library(stringr)
texto <- "Olá, Mundo!"
#Comprimento da string str_length()
str_length(texto)
#Converter para minúsculas str_to_lower()
str_to_lower(texto)
#Converter para maiúsculas str_to_upper()
str_to_upper(texto)
#Extrair substring str_sub()
str_sub(texto, 1, 3)
#Substituir parte da string str_replace()
str_replace(texto, "Mundo", "R")
#Verificar se a string tem padrão str_detect()
str_detect(texto, "Mundo")



# Regex Básico (expressões regulares) -------------------------------------
#Padrões utilizados para buscar e manipular strings

#corresponder qualquer caractere
str_detect("abc", "a.c")
#início da string
str_detect("abc", "^a")
#fim da string
str_detect("abc", "c$")
#zero ou mais ocorrências
str_detect("aaab", "a*b")
#Uma ou mais ocorrências
str_detect("aaab", "a+b")
#Conjunto de caracteres, corresponde a "a","b" ou "c"
str_detect("abc", "[abc]")
#Operador "ou", corresponde a 'cat' ou "dog"
str_detect("I have a cat", "cat|dog")



# Combinação de dados -----------------------------------------------------
#Concatenação

#Criação deum tribble
#por colunas
df1 <- tibble(
  mat= c(256, 487, 965, 125, 458, 874, 963),
  nome= c("João", "Vanessa", "Tiago", "Luana",
          "Gisele", "Pedro", "André"),
  curso = c("Mat", "Est", "Est", "Est", "Est", "Mat", "Est"),
  prova1= c(80, 75, 95, 70, 45, 55, 30),
  prova2= c(90, 75, 80, 85, 50, 75, NA),
  prova3= c(80, 75, 75, 50, NA, 90, 30),
  faltas= c(4,4, 0, 8, 16, 0, 20)
)


#por linhas
df_extra <- tribble(
  ~mat, ~nome, ~idade, ~bolsista,
  256, "João", 18, "S",
  
)

#Concatenação
#linhas (vertical) - pilha
bind_rows(df1[1:3, c(1, 3, 5)],
          df1[5:7, c(1, 3, 5, 4)],
          df1[4, c(1, 5, 4)])
#colunas (horizontal)
bind_cols(df1[, c(1:3)],
          df1[, c(6:7)])

#Junções
#full_join() - união
full_join(df1, df_extra,
          by=c("mat"="mat", "nome"))

#inner_join() - intersecção
inner_join(df1, 
           df_extra,
           by= c("mat"="mat",
                 "nome"))
#todos os que estão na 1ºtabela
left_join(df1, df_extra,
          by= c("mat"="mat", 
                "nome"))
#todos os que estõa na 2º tabela
right_join(df1, df_extra,
           by=c("mat"="mat",
                "nome"))
#os da 2º que não aparecem na 1º
anti_join(df1, df_extra,
          by=c("mat"="mat", 
               "nome"))

# Exportação de dados -----------------------------------------------------


