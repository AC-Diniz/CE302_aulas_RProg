
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


# Mutate e select ---------------------------------------------------------


# Filter ------------------------------------------------------------------


# Arrange -----------------------------------------------------------------


# Exercício 1 -------------------------------------------------------------


# Rename ------------------------------------------------------------------


# Summarize ---------------------------------------------------------------


# Lubridate ---------------------------------------------------------------


# Pivot -------------------------------------------------------------------


# String ------------------------------------------------------------------


# Combinação de dados -----------------------------------------------------


# Exportação de dados -----------------------------------------------------


