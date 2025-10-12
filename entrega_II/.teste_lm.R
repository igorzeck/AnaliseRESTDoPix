# Teste de funlão de linear model (lm())
set.seed(42)
max_entradas <- 500

x <- 1:max_entradas
y <- 2 * 0.5 * x + rnorm(max_entradas, mean = 0, sd = x)
df <- data.frame(x, y)

df$y[c(10, 20, 30)] <- NA

# - Plotagem -
plot(x, y)

# -- Erro --
alfa <- achar_alfa(x, y)
beta <- achar_beta(x, y)

erro <- y - alfa * x - beta

# - Plot do erro -
plot(erro)

mean(erro)

modelo <- lm(
  formula = y ~ x,
  data = df,
  subset = x <= 400,
  weights = 1/x,
  na.action = na.omit
)

modelo
summary(modelo)
fitted(modelo)

# 1. Gráfico de dispersão das variáveis
# 2. Gráfico dos errs
