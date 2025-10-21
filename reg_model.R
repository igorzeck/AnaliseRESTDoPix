# Modelo Regressão ----
# 0. Setup ---- 
source("processos_estatisticos_alt.R")

df <- carregar("estatisticas_de_transações_pix.csv")

# 1. Transformar certas variáveis em NA ----
df <- df |> 
  mutate(pag_pfpj = if_else(pag_pfpj == "nao_disponivel",
                           NA,
                           pag_pfpj),
         rec_pfpj = if_else(rec_pfpj == "nao_disponivel",
                           NA,
                           rec_pfpj)) |>
  drop_na() |> 
  mutate(pag_pfpj = if_else(pag_pfpj == "pj", 1, 0)) |>  
  mutate(rec_pfpj = if_else(rec_pfpj == "pj", 1, 0))

str(df)

# 2. 