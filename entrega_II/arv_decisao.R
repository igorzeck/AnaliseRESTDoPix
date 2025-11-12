# Árvore de decisão ----
## 0. Setup ----
# install.packages("rpart")
# install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
source("processos_estatisticos_alt.R")
df <- carregar("estatisticas_de_transações_pix.csv")

## 1. Escolha de variáveis
glimpse(df)
df |> distinct(natureza)
df |> distinct(finalidade)

## 2. Idade do pagador ----
# (pag_idade ~ pag_regiao + natureza + finalidade)
### 2.1. Treino do modelo ----
set.seed(42)
# 80/20 para treino/teste
indices_treino <- sample(1:nrow(df), (nrow(df)*0.8))
df_treino <- df[indices_treino,]
df_teste <- df[-indices_treino,]
# Tenta prever idade do pagante a partir de variáveis chaves
modelo <- rpart(
  pag_idade ~ pag_regiao + natureza + finalidade,
  data = df_treino,
  method = "class",  # Classificação
  parms = list(split = "gini"),  # Usa o índice de Gini
  control = rpart.control(cp = 0.00001, minsplit = 2, minbucket = 1)  # Relativo ao spit
)

### 2.2. Visualização do modelo ----
summary(modelo)
# As classes estão bem balaceadas
pdf("entrega_II/arvore_decisão_pag_idade.pdf", width = 8, height = 6)
rpart.plot(modelo)
dev.off()

### 2.3. Previsão dos valores ----
preds <- predict(modelo, df_teste, type = "class")
# Verifica se os valores estão corretos
sum(preds == df_teste$pag_idade) / length(preds)
# Análise:
# Acurácia de 31%

## 3. Previsão de PF/PJ para pagador ----
# (pag_pfpj ~ quantidade + valor)
### 3.1. Treino do modelo ----
set.seed(42)
# 80/20 para treino/teste
indices_treino <- sample(1:nrow(df), (nrow(df)*0.8))
df_treino <- df[indices_treino,]
df_teste <- df[-indices_treino,]

# Tenta prever se é pf/pj por quantidade e valor dos pix
modelo <- rpart(
  pag_pfpj ~ quantidade + valor,
  data = df_treino,
  method = "class",  # Classificação
  parms = list(split = "gini"),  # Usa o índice de Gini
  control = rpart.control(cp = 0.00001,
                          minsplit = 2,
                          minbucket = 1,
                          maxdepth = 4)  # Limitou a 6 para exibir a árvore 
)

### 3.2. Visualização do modelo ----
summary(modelo)

# As classes estão bem balaceadas
pdf("entrega_II/arvore_decisão_pag_pfpj.pdf", width = 8, height = 6)
rpart.plot(modelo)
dev.off()

### 3.3. Previsão dos valores ----
preds <- predict(modelo, df_teste, type = "class")
# Verifica se os valores estão corretos
sum(preds == df_teste$pag_pfpj) / length(preds)
# Análise:
# Acurácia de 84%
# Não fugiu muito disso para maiores profundiades

## 4. Previsão de região para pagador ----
# (pag_regiao ~ quantidade + valor)
### 3.1. Treino do modelo ----
set.seed(42)
# 80/20 para treino/teste
indices_treino <- sample(1:nrow(df), (nrow(df)*0.8))
df_treino <- df[indices_treino,]
df_teste <- df[-indices_treino,]

# Tenta prever se é pf/pj por quantidade e valor dos pix
modelo <- rpart(
  pag_regiao ~ quantidade + valor,
  data = df_treino,
  method = "class",  # Classificação
  parms = list(split = "gini"),  # Usa o índice de Gini
  control = rpart.control(cp = 0.00001,
                          minsplit = 2,
                          minbucket = 1,
                          maxdepth = 4)  # Limitou a 6 para exibir a árvore 
)

### 3.2. Visualização do modelo ----
summary(modelo)

# As classes estão bem balaceadas
pdf("entrega_II/arvore_decisão_pag_regiao.pdf", width = 8, height = 6)
rpart.plot(modelo)
dev.off()

### 3.3. Previsão dos valores ----
preds <- predict(modelo, df_teste, type = "class")
# Verifica se os valores estão corretos
sum(preds == df_teste$pag_regiao) / length(preds)
# Análise:
# Acurácia de 22%
# Horrível

