
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

#Filbros combinados: múltiplas condições (operadores lógicos)
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


# Exercício 1 -------------------------------------------------------------


# Rename ------------------------------------------------------------------


# Summarize ---------------------------------------------------------------


# Lubridate ---------------------------------------------------------------


# Pivot -------------------------------------------------------------------


# String ------------------------------------------------------------------


# Combinação de dados -----------------------------------------------------


# Exportação de dados -----------------------------------------------------


