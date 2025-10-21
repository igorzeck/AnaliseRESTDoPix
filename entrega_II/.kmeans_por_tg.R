# KMEANs por tangente ----
# 1. Setup
source("processos_estatisticos_alt.R")

df <- carregar("estatisticas_de_transações_pix.csv") |> 
  mutate(qte_norm = min_max_normalizar(quantidade)) |> 
  mutate(valor_norm = min_max_normalizar(valor)) |> 
  mutate(tg = valor_norm / qte_norm)

# 2. Gráfico da tangente por índice ----
plot(df$tg)
# Conclusão: muito ruidoso!
# Talvez seja melhor separar pelo ângulo

# 3. Gráfico do ângulo por índice ----
df <- df |> 
  mutate(angulo = as.integer(rad_para_grau(atan(tg))))

df$index <- seq_along(df$angulo)

# 3.1. Plot do ângulo dos valores por índice ----
ggplot(df, aes(x = index, y = angulo)) +
  geom_point(alpha = 0.01) +
  labs(title = "Dispersão por índice",
       x = "Índice",
       y = "Ânguulo")
# Análise: Note a existência de ângulo ou muito altos ou muito baixos
# predominando duas faixas: A superior e a inferiors

# 3.2. PLot do ângulo por variáveis explicativas ----
ggplot(df, aes(x = valor, y = angulo)) +
  geom_point(alpha = 0.1) +
  labs(title = "Dispersão por valor",
       x = "Ângulo",
       y = "Valor")
# Análise:
# Aparente haver um padrão, mas estão todos presos a uma única reta

ggplot(df, aes(x = quantidade, y = angulo)) +
  geom_point(alpha = 0.1) +
  labs(title = "Dispersão por quantidade",
       x = "Ângulo",
       y = "Quantidade")
# Análise:
# Aparente haver um padrão, mas estão todos presos a uma única reta

# 4. Kmeans para tangente ----
# 4.1. KMeans por Ângulo X Quantidade X Valor ----
df_k <- na.omit(df[,c("angulo", "quantidade", "valor")])

set.seed(42)
resultados <- kmeans(x = df_k, centers = 2)

str(resultados)

df_k |> 
  ggplot(aes(x = quantidade, y = valor, colour = as.factor(resultados$cluster))) +
  geom_point()
# Análise: Separação meio estranha dos grupo

# 4.2. Kmeans por ângulo e índice ----
df_k <- na.omit(df[,c("angulo", "index")])

set.seed(42)
resultados <- kmeans(x = df_k, centers = 2)

str(resultados)

na.omit(df) |> 
  ggplot(aes(x = quantidade, y = valor, colour = as.factor(resultados$cluster))) +
  geom_point()
# Análise: Definitivamente pior!

# 4.3. Kmeans por Ângulo X Quantidade X Valor para 4 grupos
df_k <- na.omit(df[,c("angulo", "quantidade", "valor")])

set.seed(42)
resultados <- kmeans(x = df_k, centers = 4)

str(resultados)

df_k |> 
  ggplot(aes(x = quantidade, y = valor, colour = as.factor(resultados$cluster))) +
  geom_point(alpha = 0.1)
# Ele praticamente separou levando em conta apenas o Valor!
# 4.4. Kmeans por Ângulo apenas?
df_k <- na.omit(df[,c("angulo")])

set.seed(42)
resultados <- kmeans(x = df_k, centers = 4)

str(resultados)

na.omit(df) |> 
  ggplot(aes(x = quantidade, y = valor, colour = as.factor(resultados$cluster))) +
  geom_point()
# Análise: Aqui faz sentido essa separação! Mas, ainda tá meio caótico...

# 4.5. Kmeans para 2 ângulos acredito que seja o melhor
df_k <- na.omit(df[,c("angulo")])

set.seed(42)
resultados <- kmeans(x = df_k, centers = 2)

str(resultados)

na.omit(df) |> 
  ggplot(aes(x = quantidade, y = valor, colour = as.factor(resultados$cluster))) +
  geom_point()
# Análise: Os resultados são mais sensíveis!