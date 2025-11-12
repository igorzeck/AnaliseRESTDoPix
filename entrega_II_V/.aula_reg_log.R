# Aula de Regressão Logística ----
# Tarefas:
# O que fazer:
#   - Olhar o que se quer explicar
# Aplicação da Regresão logística
#   - Pacote fastDummies (função dummy cols)
#   - Passar aos dados encoded como inteiros
#   - Garantir boa proporção dos dados
#   - Escrever uma interpretação da Regressão Logística
#   - Adaptar resultados da logística na linear

# 0. Setup ----
source("processos_estatisticos_alt.R")
# install.packages("fastDummies")

df <- carregar("estatisticas_de_transações_pix.csv")

# Proporção de pf:pj
df |> 
  group_by(pag_pfpj) |> 
  summarise(n = n()) |> 
  mutate(freq_n = n / sum(n) * 100)

# Pix médio
df <- df |> 
  mutate(pix_medio = valor / quantidade)

# Média e Mediana do Pix médio por PF e PJ
# Ordenado por pag_pfpj
df |> 
  group_by(pag_pfpj) |> 
  summarise(pix_medio_medio = mean(pix_medio),
            pix_medio_mediano = median(pix_medio))

# Dados agem como o esperado
# 1. Regressão logística ----
df_dummy <- fastDummies::dummy_cols(df)

df_dummy <- df_dummy |> 
  janitor::clean_names()

view(df_dummy)

is.na(df)

df <- read_csv("estatisticas_de_transações_pix.csv")
df |> 
  janitor::clean_names() |> 
  mutate(pag_pfpj == 'nao disponivel') |> 
  mutate()
