# Random Forest ----
## Setup ----
# install.packages("randomForest")
# install.packages("caret")
source("processos_estatisticos_alt.R")
library(randomForest)
library(caret)
library(kableExtra)

df <- carregar("estatisticas_de_transações_pix.csv") |> 
  mutate(pag_pfpj = as.factor(pag_pfpj))

df |> 
  filter(pag_pfpj == "nao disponivel") |> 
  nrow()

# 42 casos de não disponível
df |> 
  filter(pag_pfpj != "nao disponivel") |> 
  nrow()
# Casos com algum valor 546268
# Por questões de facilitar o treino, removeu-se os valores "nao disponivel"
# De pag_pfph
df <- df |> 
  filter(pag_pfpj != "nao disponivel") |> 
  mutate(pag_pfpj = droplevels(pag_pfpj))

## Treino só com Random Forest ----
# Treino
set.seed(42)

# 80/20 para treino/teste
indices_treino <- sample(1:nrow(df), nrow(df) * 0.8)
df_treino <- df[indices_treino,]
df_teste <- df[-indices_treino,]

# ### Melhor mtry ----
# # Procura-se achar o melhor mtry
# set.seed(42)
# tuneRF(
#   x = df_treino[, c("quantidade", "valor", "rec_regiao", "pag_regiao", "rec_idade", "finalidade", "natureza")],
#   y = df_treino$pag_pfpj,
#   stepFactor = 1.5,
#   improve = 0.01,
#   ntreeTry = 20,
#   trace = TRUE,
#   plot = TRUE
# )
# Não funcionando, por alguma razão...

### Treino em si ----

# Colunas
glimpse(df)

# Configurações do modelo:
modelo <- randomForest(
  formula = pag_pfpj ~ quantidade + valor + rec_regiao + pag_regiao + rec_idade + finalidade + natureza,
  data = df_treino,
  ntree = 20,
  mtry = 1,
  # Teste para não verificar acurácia do modelo
  xtest = df_teste |> dplyr::select(quantidade, valor, rec_regiao, pag_regiao, rec_idade, finalidade, natureza),
  ytest = df_teste$pag_pfpj,
  # Para não descartar a flores já que tô usando o test
  keep.forest = TRUE,
  importance=TRUE
)

# Não vão bater exatamente por causa do jeito 
# que a função do RF funciona internatmente
identical(
  modelo$test$predicted,
  predict(modelo, df_teste)
)

all(levels(df_treino$pag_pfpj) == levels(df_teste$pag_pfpj))

### Análise do modelo ----
#### Sumário ----
modelo

#### Predição ----
preds <- predict(modelo, df_teste, type = "class")

# Verifica se os valores estão corretos - Acurácia de teste
acc = sum(preds == df_teste$pag_pfpj) / length(preds)
cat("Predição:", round(acc * 100, 2), "%")
# Precisão de 99.2%

#### Matriz de confusão ----
# Para treino
conf_tab <- modelo$confusion

kbl(
  conf_tab,
  format = "latex",
  caption = "Matriz de confusão para treino",
  label = "rf_conf_m_treino",
  booktabs = TRUE,
  digits = 3
) |> 
  kable_styling(latex_options = c("scale_down", "hold_position"))

acc_treino = (360220 + 66434) / nrow(df_treino)
paste0(round(acc_treino * 100, 2), "%")
# Acurácia de 97.63% no treino
# Para teste
conf_tab <- modelo$test$confusion

kbl(
  conf_tab,
  format = "latex",
  caption = "Matriz de confusão para teste",
  label = "rf_conf_m_teste",
  booktabs = TRUE,
  digits = 3
) |> 
  kable_styling(latex_options = c("scale_down", "hold_position"))

acc_teste = (90004 + 18377) / nrow(df_teste)
paste0(round(acc_teste * 100, 2), "%")
# Acurácia de 99.2%% no treino

conf_m <- confusionMatrix(df_teste$pag_pfpj, preds)
conf_m$table
### Importância das variáveis ----
imp <- importance(modelo)
imp
varImpPlot(modelo)

# Normaliza importância
imp_norm <- 100 * imp[, 4] / max(imp[, 4])
imp_df <- data.frame(
  Variable = rownames(imp),
  RelativeImportance = imp_norm
)

# Plota importância relativa
ggplot(imp_df, aes(x = reorder(Variable, RelativeImportance), 
                   y = RelativeImportance)) +
  geom_col(fill = "grey") +
  coord_flip() +
  labs(
    x = NULL,
    y = "Importância relativa"
  ) +
  theme_minimal(base_size = 13)
