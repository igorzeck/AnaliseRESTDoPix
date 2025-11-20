# K-nearest neighbour ----
# Script de implementação do algoritmo KNN
## 0. Setup ----
library(FNN)
source("processos_estatisticos_alt.R")

df <- carregar("estatisticas_de_transações_pix.csv")

## 1. Normalizção dos dados ----
qte_norm <- znorm(df$quantidade)
valor_norm <- znorm(df$valor)

## 2. KNN ----
set.seed(42)
index_alvo = sample(1:nrow(df), 1)

# Prever se pagador é PF, PJ ou nao disponivel
knn_pred <- knn(
  train = tibble(qte = qte_norm, valor = valor_norm)[-index_alvo,],
  test = tibble(qte = qte_norm, valor = valor_norm)[index_alvo,],
  cl = df$pag_pfpj[-index_alvo],
  k = 20
)

# Acurácia
knn_pred == df$pag_pfpj[index_alvo]
# Acertou o alvo!


## 3. KNN iterativamente ----
set.seed(42)

start_time <- Sys.time()

correto <- c()
max_iter <- 1e4
for (i in 1:max_iter) {
  index_alvo = sample(1:nrow(df), 1)
  
  # Prever se pagador é PF, PJ ou nao disponivel
  knn_pred <- knn(
    train = tibble(qte = qte_norm, valor = valor_norm)[-index_alvo,],
    test = tibble(qte = qte_norm, valor = valor_norm)[index_alvo,],
    cl = df$pag_pfpj[-index_alvo],
    k = 20
  )
  correto <- append(correto, knn_pred == as.character(df[index_alvo, "pag_pfpj"]))
}

end_time <- Sys.time()
delta_t <- end_time - start_time
cat(paste0("Delta T (KNN): ",round(delta_t, 3), "s"))

# Acurácia
cat(paste0("Corretos: ", sum(correto), "/",max_iter))
sum(correto)/length(correto)
# Acurácia de 86.7% para 1.000 entradas.