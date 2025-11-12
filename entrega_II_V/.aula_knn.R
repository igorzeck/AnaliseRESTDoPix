# Aula de near neighbour (NN) ----
# Requerimentos:
# Pacotes RNN
# Tutorial:
#  Pega primeiro elemento df (1) com a coluna 2 e 3 (2:3)
#  E quer resultado em data.frame (drop = False)
#  novo_df <- df[1, 2:3, drop=FALSE]
#  knn_pred <- knn(
#   # Dados de treinamento
#   train=df[-1,2:3]  # Pega tudo menos linha 1
#   # O caso que queremos classificar
#   test = novo_df,
#   # A classe de cada observação
#   cl = df[-1, 1], # Exceto para primeira linha, pega coluna 1
#   # O número de vizinhos a serem considerados
#   k = 20
#   )
#   knn_pred == 'default'  # É esse valor? True ou False
#   knn_pred == 'paid off' # É esse valor? True ou False
# Procurar sobre distância "Mahalanobis"
## 0. Setup ----
# install.packages("FNN")
library(FNN)
source("processos_estatisticos_alt.R")

df <- carregar("estatisticas_de_transações_pix.csv")

## 1. KNN ----
set.seed(42)
index_alvo = sample(1:nrow(df), 1)
df[index_alvo, "pag_pfpj"]
# Prever se pagador é PF, PJ ou nao disponivel
knn_pred <- knn(
  train = df[-index_alvo, c("quantidade", "valor")],
  test = df[index_alvo, c("quantidade", "valor")],
  cl = df$pag_pfpj[-index_alvo],
  k = 20
)

knn_pred


### 1.1. KNN para 100 amostras ----
set.seed(42)
correto <- c()
for (i in 1:100) {
  index_alvo = sample(1:nrow(df), 1)
  
  # Prever se pagador é PF, PJ ou nao disponivel
  knn_pred <- knn(
    train = df[-index_alvo, c("quantidade", "valor")],
    test = df[index_alvo, c("quantidade", "valor")],
    cl = df$pag_pfpj[-index_alvo],
    k = 20
  )
  correto <- append(correto, knn_pred == as.character(df[index_alvo, "pag_pfpj"]))
}

# Acurácia
sum(correto)/length(correto)
# Acurácia de 91%
# Por distorção de dados é uma boa calcular a média
# por Z-score
# z = (X - media(X)) / s