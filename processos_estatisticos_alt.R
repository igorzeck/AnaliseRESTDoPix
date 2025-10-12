# Funções de estatística
# Versão ALTERNATIVA - com Tidyverse

# TODO: 1. Escrever f que dado um conj de preditoras e uma target: f precisa devolver alfa e beta (coeficientes da reta)

# 0. -- Importações --
library(tidyverse)
library(fitdistrplus)

# 1. Carrega,nto e processamento preliminar
carregar <- function(arq) {
  df <- read_csv(arq, locale = locale(decimal_mark = ",")) |> 
    janitor::clean_names() |> 
    mutate(across(where(is.character), ~tolower(.)))
  
  return(df)
}

# 2. Printar sumário por funções
printar_sumario <- function(variavel) {
  cat(
    sprintf("\t---------- Sumário ----------"),
    sprintf("\t|%-14s |%12d%s", "Mínimo:", min(variavel), "|"),
    sprintf("\t|%-14s |%12.2f%-12s", "1º Quartil:", quantile(variavel)[2], "|"),
    sprintf("\t|%-14s|%12.2f%-12s", "Mediana:", mean(variavel), "|"),
    sprintf("\t|%-14s |%12.2f%-12s", "Média:", mean(variavel), "|"),
    sprintf("\t|%-14s |%12.2f%s", "3º Quartil:", quantile(variavel)[4], "|"),
    sprintf("\t|%-14s |%12.2f%.19s", "Máximo: ", max(variavel), "|"),
    sprintf("\t-------- Por funções --------"),
    sep = "\n"
  )
}

# 3. Tabela de frequência X por Y
tabelar_freq <- function(x, y) {
  tab_original <- table(x, y)
  
  tab_cruzada <- rbind(
    cbind(tab_original,margin.table(tab_original,1)),
    c(margin.table(tab_original,2),
    sum(margin.table(tab_original,1)))
  )
  # Teria que generalizar
  # dimnames(tab_cruzada)[[1]][length(dimnames(tab_cruzada)[[1]])] <- "Total_linha"
  # dimnames(tab_cruzada)[[2]][length(dimnames(tab_cruzada)[[2]])] <- "Total_coluna"
  view(tab_cruzada)
}

# -- Funções estatísticas --
# Tamanho
tam <- function(variavel) {
  cont <- 0
  for (i in variavel) {
    cont <- cont + 1
  }
  return(cont)
}
# Soma
soma <- function(variavel) {
  total <- 0;
  for (num in variavel) {
    total <- total + num
  }
  return(total)
}

# Média
media <- function(variavel) {
  soma(variavel) / tam(variavel)
}

# -- Regressão linear --

# Coeficiente angular
achar_beta <- function(x, y) {
  n <- tam(x)
  if (n != tam(y)) {
    return(NA);
  }
  return((soma(x * y) - n*media(x)*media(y)) / (soma(x ** 2) - n * (media(x)) ** 2))
}

# Coeficiente independente
achar_alfa <- function(x, y) {
  return(media(y) - achar_beta(x, y) * media(x))
}

# Função para normalizar os dados em escala 0 a 1 (min-max)
min_max_normalizar <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

# -- Trigonometria --
# Função para converter para radianos
grau_para_rad <- function(grau) {
  grau * pi / 180
}

# Função para converter de radiano para grau
rad_para_grau <- function(rad) {
  rad * 180 / pi
}
