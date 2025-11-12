# Random Forest ----
## Setup ----
# install.packages("randomForest")
source("processos_estatisticos_alt.R")
library(randomForest)

df <- carregar("estatisticas_de_transações_pix.csv") |> 
  mutate(pag_pfpj = as.factor(pag_pfpj))

## Treino ----
# Treino
set.seed(42)

# 80/20 para treino/teste
indices_treino <- sample(1:nrow(df), (nrow(df)*0.8))
df_treino <- df[indices_treino,]
df_teste <- df[-indices_treino,]

modelo <- randomForest(
  formula = pag_pfpj ~ quantidade + valor,
  data = df_treino,
  ntrees = 5,
  importance=TRUE
)
