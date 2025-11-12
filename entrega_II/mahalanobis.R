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

## 2. Knnn com distância de Mahalanobis ----
# Olha K mais próximos
set.seed(42)

total <- 1000
indices <- sample(1:nrow(df), total)
k <- 20
corretos <- 0
for (ix in indices) {
  # Escolhendo um ponto médio de forma arbitrária
  df[-ix,c('quantidade', 'valor')]
  mh <- calc_mahalanobis(df[-ix,c('quantidade', 'valor')], 
                         vetor_medio = unlist(df[ix, c('quantidade', 'valor')]))
  
  df_temp <- df |> 
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
cat(corretos, "/", total)
paste0(corretos/total * 100, "%")
# Análise:
# Precisão de por volta de 85% para 1e4 elementos.
# Inferior ao do KNN usando distância euclidiana.

