# Teste de funlão de linear model (lm())
# No dataset
source("processos_estatisticos.R")
set.seed(42)
max_entradas <- 500

df <- carregar("estatisticas-de-transações-pix.csv")

# - Plotagem -
plot(df$quantidade, df$valor)

# -- Erro --
alfa <- achar_alfa(df$quantidade, df$valor)
beta <- achar_beta(df$quantidade, df$valor)

# Não tá funcionando
erro <- y - alfa * x - beta

# - Plot do erro -
plot(erro)

mean(erro)

# -- Modelo --
modelo <- df |> 
  lm(
    formula = valor ~ quantidade,
    na.action = na.omit
  )

plot(modelo)

modelo
summary(modelo)
fitted(modelo)
