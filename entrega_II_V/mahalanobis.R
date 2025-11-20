# Análise pela distância de Mahalanobis ----
## 0. Setup ----
source("processos_estatisticos_alt.R")
df <- carregar("estatisticas_de_transações_pix.csv")

str(df)

## 1. Função de distância de Mahalanobis (para duas n variáveis)
calc_mahalanobis <- function(x_mat, vetor_medio = NULL) {
  x_mat <- as.matrix(x_mat)
  cov_m <- cov(x_mat)
  # Caso não se forneceça um vetor médio
  if (is.null(vetor_medio)) {
    vetor_medio <- colMeans(x_mat)
  }
  inv_m = solve(cov_m)
  
  # Para a operação row-wise
  # Conceitualmente a coluna seria uma coordenada
  x_dif <- sweep(x_mat, 2, vetor_medio)
  return(sqrt(rowSums((x_dif %*% inv_m) * x_dif)))
}

## 2. KNN com distância de Mahalanobis ----
# Olha K mais próximos
set.seed(42)

start_time <- Sys.time()

total <- 1e4
indices <- sample(1:nrow(df), total)
k <- 20
corretos <- 0

for (ix in indices) {
  # Escolhendo um ponto médio de forma arbitrária
  mh <- calc_mahalanobis(df[-ix,c('quantidade', 'valor')], 
                         vetor_medio = unlist(df[ix, c('quantidade', 'valor')]))
  
  df_temp <- df[-ix,] |> 
    mutate(mh = mh) |> 
    arrange(mh)
  
  categoria <- df_temp |>
    slice(1:k) |> 
    group_by(pag_pfpj) |> 
    summarise(n = n()) |> 
    slice_max(order_by = n, n = 1) |> 
    dplyr::select(pag_pfpj)
  
  if (categoria[[1]] == df[ix,"pag_pfpj"]) {
    corretos = corretos + 1
  }
}

end_time <- Sys.time()
delta_t <- end_time - start_time
cat(paste0("Delta T (Mahalanobis): ",round(delta_t, 3), "s"))
cat(corretos, "/", total)
paste0(corretos/total * 100, "%")
# Análise:
# Precisão de por volta de 85.2% para 1.000 elementos.
# Inferior ao do KNN usando distância euclidiana.

## 3. Mahalanobis pela função do R ----
set.seed(42)

# Como é a mesma seed garante ser os mesmos índices
start_time <- Sys.time()
total <- 1e3
indices <- sample(1:nrow(df), total)
k <- 20
corretos <- 0

for (ix in indices) {
  cov_df <- cov(df[-ix,c('quantidade', 'valor')])
  mh <- mahalanobis(df[-ix,c('quantidade', 'valor')], 
                    unlist(df[ix, c('quantidade', 'valor')]),
                    cov_df)
  
  df_temp <- df[-ix,] |> 
    mutate(mh = mh) |> 
    arrange(mh)
  
  categoria <- df_temp |>
    slice(1:k) |> 
    group_by(pag_pfpj) |> 
    summarise(n = n()) |> 
    slice_max(order_by = n, n = 1) |> 
    dplyr::select(pag_pfpj)
  
  if (categoria[[1]] == df[ix,"pag_pfpj"]) {
    corretos = corretos + 1
  }
}


end_time <- Sys.time()
delta_t <- end_time - start_time
cat(paste0("Delta T (Mahalanobis): ",round(delta_t, 3), "s"))
cat(corretos, "/", total, "\n")
paste0(corretos/total * 100, "%")
# Foi pior ainda: "82.9%". (829 elementos)
