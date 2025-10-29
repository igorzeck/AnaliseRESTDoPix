# Arquivo de Regressão logística ----
# Explicativas: Quantidade e Valor
## 0. Setup ----
source("processos_estatisticos_alt.R")
library(fastDummies)

df <- carregar("estatisticas_de_transações_pix.csv") |> 
  mutate(ano_mes = as.factor(ano_mes)) |> 
  filter(pag_pfpj != "nao disponivel") |> 
  filter(rec_pfpj != "nao disponivel")

glimpse(df)
unique(df$pag_pfpj)
unique(df$rec_pfpj)
## 1. Transformar em Dummy ----
df_dummy <- df |>
  fastDummies::dummy_cols(select_columns = c("pag_pfpj"),
                          remove_first_dummy = TRUE,
                          remove_selected_columns = TRUE) |> 
  janitor::clean_names()

glimpse(df_dummy)

## 2. Regressão logística ----
set.seed(42)
modelo <- glm(pag_pfpj_pj ~ quantidade + valor,
              data = df_dummy,
              family = binomial(link = "logit"))
summary(modelo)

predicoes <- predict(modelo)

df_dummy$pred <- predicoes
df_dummy <- df_dummy |> 
  mutate(class_pred = if_else(predicoes > 0, "pj", "pf")) |> 
  mutate(class_pred = as.factor(class_pred))

# Precisão
sum(df_dummy$class_pred == df$pag_pfpj) / nrow(df_dummy)

## 3. Graficamente ----
# O modelo obteve uma precisão de 82%
df <- df |> 
  mutate(qte_norm = min_max_normalizar(quantidade)) |> 
  mutate(valor_norm = min_max_normalizar(valor)) |> 
  mutate(tg = valor_norm / qte_norm) |> 
  mutate(angulo = as.integer(rad_para_grau(atan(tg)))) |> 
  drop_na()

df |> 
  ggplot() +
  geom_vline(aes(xintercept = angulo,
                 colour = class_pred),
             alpha = 0.9,
             size = 10) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()
  )

## 4. Regressão linear com todas as variáveis ----
set.seed(42)
# NEste banco de dados há apenas 2 variáveis quantitativas

# Treino do modelo
modelo <- glm(pag_pfpj_pf ~ valor,
              data = df_dummy,
              family = binomial(link = "logit"))

summary(modelo)
# Análise:
# O sumário do modelo revela que ambas têm um Pr extremamente baixo
# Portanto, ambos são significativos!
coef <- summary(modelo)$coefficients

df_dummy$pred <- predict(modelo)
# Olha as variáveis mais significativas

sum(df_dummy$class_pred == df$pag_pfpj) / nrow(df_dummy)

# Olha qual das daus varáveis é mais significativa
# 5. Pelo recebedor ----
df_dummy <- df |>
  fastDummies::dummy_cols(select_columns = c("rec_pfpj", "pag_idade"),
                          remove_first_dummy = TRUE,
                          remove_selected_columns = TRUE) |> 
  janitor::clean_names()

glimpse(df_dummy)

modelo <- glm(pag_idade_entre_20_e_29_anos ~ quantidade,
              data = df_dummy,
              family = binomial(link = "logit"))

summary(modelo)
# Em todos os casos as probabilidades aparentam maximizar
# Warning alertando sobre esse problema!
# Estranho!

# 6. Plotagem ----
df_dummy <- df |>
  fastDummies::dummy_cols(select_columns = c("pag_pfpj"),
                          remove_first_dummy = TRUE,
                          remove_selected_columns = TRUE) |> 
  janitor::clean_names()

modelo <- glm(pag_pfpj_pj ~ quantidade,
              data = df_dummy,
              family = binomial(link = "logit"))

df |> 
  ggplot() +
  geom_point(aes(x = modelo$fitted.values,y = pag_pfpj))
