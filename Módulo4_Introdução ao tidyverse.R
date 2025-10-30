
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


