# KMEANs por ângulo ----
# A escolha do ângulo ao invés da tangente
# se deve ao fato de ele estar restrito a um ranjo de valores menor
# 1. Setup
source("processos_estatisticos_alt.R")
library(kableExtra)

df <- carregar("estatisticas_de_transações_pix.csv") |> 
  mutate(qte_norm = min_max_normalizar(quantidade)) |> 
  mutate(valor_norm = min_max_normalizar(valor)) |> 
  mutate(tg = valor_norm / qte_norm) |> 
  mutate(angulo = as.integer(rad_para_grau(atan(tg)))) |> 
  drop_na()

# 2. Gráfico da tangente por índice ----
plot(df$tg)
# Conclusão: muito ruidoso!
# Talvez seja melhor separar pelo ângulo

# 3. Gráfico do ângulo por índice ----
df$index <- seq_along(df$angulo)

ggplot(df, aes(x = index, y = angulo)) +
  geom_point(alpha = 0.01) +
  labs(title = "Dispersão por índice",
       x = "Índice",
       y = "Ânguulo")
# Análise: Note a existência de ângulo ou muito altos ou muito baixos
# predominando duas faixas: A superior e a inferiors

# 4. Kmeans ----
## 4.1. KMeans por ângulo (2 clusters) ----
set.seed(42)
resultados <- kmeans(x = df$angulo, centers = 2)

str(resultados)

df$cluster <- as.factor(resultados$cluster)

df |> 
  ggplot(aes(x = quantidade, y = valor, colour = cluster)) +
  geom_point()
# Análise: Definitivamente uma melhor separação visualmente.
# Resultados promissores!
# Contudo, por questões de comparação, vai se ainda uitlizar
# O de 4 clusters
## 4.2. KMeans por tangente (4 clusters) ----
set.seed(42)
resultados <- kmeans(x = df$angulo, centers = 4)
# Caso dê problema com a tangente ser "infinita"
# é uma boa olhar pelo ângulo

str(resultados)

df$cluster <- as.factor(resultados$cluster)

df |> 
  ggplot(aes(x = quantidade, y = valor, colour = cluster)) +
  geom_point()
# Análise: A separação em 4 grupos foi bem imprecisa
# Essa separação meio estranha pode, dentre outras coisas,
# Ser resultante da densidade de dados maior nos grupos inferiores
# Optou-se por utilizar então 2 grupos, para maior precisão!

# 5. Análise dos clusters ----
## 5.1. Dados por cluster ----

### 5.1.2. Para 2 clusters ----
set.seed(42)
resultados <- kmeans(x = df$angulo, centers = 2)

str(resultados)

df$cluster <- as.factor(resultados$cluster)

df |> 
  group_by(cluster) |> 
  summarise(freq_absoluta = n()) |> 
  mutate(freq_relativa = freq_absoluta / nrow(df) * 100) |> 
  kbl(
    format = "latex",
    caption = "Tabela de frequência para cada 2 clusters",
    label = "tab_freq_2_cluster",
    booktabs = TRUE,
    digits = 3
  ) |> 
  kable_styling(latex_options = c("scale_down", "hold_position"))
# Ainda existe um desbalancemanete nos dados, com a maioria sendo do cluster 2

### 5.1.2. Para 4 clusters ----
set.seed(42)
resultados <- kmeans(x = df$angulo, centers = 4)

str(resultados)

df$cluster <- as.factor(resultados$cluster)

# -- Tab 1: Frequência absoltua e relativa para cada cluster --
df |> 
  group_by(cluster) |> 
  summarise(freq_absoluta = n()) |> 
  mutate(freq_relativa = freq_absoluta / nrow(df) * 100) |> 
  kbl(
    format = "latex",
    caption = "Tabela de frequência para 4 clusters",
    label = "tab_freq_4_cluster",
    booktabs = TRUE,
    digits = 3
  ) |> 
  kable_styling(latex_options = c("scale_down", "hold_position"))


# Ainda existe um desbalancemanete nos dados, com a maioria sendo do cluster 2

# 6. Regressão linear

## 6.1. Modelo de regressão linear (4 clusters) ----
set.seed(42)
resultados <- kmeans(x = df_k, centers = 4)

str(resultados)

df$cluster <- as.factor(resultados$cluster)

tbl_resultados <- tibble()
for (k in sort(unique(df$cluster))) {
  cat(paste("K: ", k, " | Cor: "))
  df_sub <- df |> 
    filter(df$cluster == k)
  
  modelo <- lm(data = df_sub,
               formula = valor ~ quantidade)
  
  cor <- cor(modelo$fitted.values, df_sub$valor) * 100
  tbl_resultados <- bind_rows(tbl_resultados,
                              tibble(k = k, correlacao = cor)
                              )
  cat(paste0(round(cor, 2), "%\n"))
  rm(cor)
  rm(df_sub)
  rm(modelo)
}
# Todos, exceto a classe 2, exibem alta correlação!

tbl_resultados |> 
  kbl(
    format = "latex",
    caption = "Tabela de correlação para 4 clusters",
    label = "tab_cor_4_cluster",
    booktabs = TRUE,
    digits = 3
  ) |> 
  kable_styling(latex_options = c("scale_down", "hold_position"))

### 6.1.1 Gráfico para as retas ajustadas
modelo_1 <- lm(data = df,
               subset = df$cluster == 1,
             formula = valor ~ quantidade)
modelo_2 <- lm(data = df,
               subset = df$cluster == 2,
               formula = valor ~ quantidade)
modelo_3 <- lm(data = df,
               subset = df$cluster == 3,
               formula = valor ~ quantidade)
modelo_4 <- lm(data = df,
               subset = df$cluster == 4,
               formula = valor ~ quantidade)

linhas_df <- data.frame(
  intercepto = c(modelo_1$coefficients["(Intercept)"],
                modelo_2$coefficients["(Intercept)"],
                modelo_3$coefficients["(Intercept)"],
                modelo_4$coefficients["(Intercept)"]),
  coef_ang = c(modelo_1$coefficients["quantidade"],
            modelo_2$coefficients["quantidade"],
            modelo_3$coefficients["quantidade"],
            modelo_4$coefficients["quantidade"]),
  cluster = factor(c(1, 2, 3, 4))
)

ggplot(df, aes(x = quantidade, y = valor, colour = cluster)) +
  geom_point() +
  geom_abline(data = linhas_df,
              aes(intercept = intercepto, slope = coef_ang),
              linewidth = 0.5, linetype = "dashed")
# Análise:
# As retas aparentam, visualmente, estarem bem ajustadas
# ao cluster da qual pertencem. Mas, ainda assim,
# parece melhor utilizar apenas dois clusters.

rm(modelo_3)
rm(modelo_4)
  
## 6.2. Modelo de regressão linear (2 clusters) ----
set.seed(42)
resultados <- kmeans(x = df_k, centers = 2)

str(resultados)

df$cluster <- as.factor(resultados$cluster)

for (k in sort(unique(df$cluster))) {
  cat(paste("K: ", k, " | Cor: "))
  df_sub <- df |> 
    filter(df$cluster == k)
  
  modelo <- lm(data = df_sub,
               formula = valor ~ quantidade)
  
  cor <- cor(modelo$fitted.values, df_sub$valor) * 100
  cat(paste0(round(cor, 2), "%\n"))
  rm(cor)
  rm(df_sub)
  rm(modelo)
}
# Análise:
# Todos, exceto a classe 2, exibem alta correlação!
# A classe 2 é aquela com maior quantidade de elemntos aqui...


### 6.1.1 Gráfico para as retas ajustadas
modelo_1 <- lm(data = df,
               subset = df$cluster == 1,
               formula = valor ~ quantidade)
modelo_2 <- lm(data = df,
               subset = df$cluster == 2,
               formula = valor ~ quantidade)

ggplot(df, aes(x = quantidade, y = valor, colour = cluster)) +
  geom_point() +
  geom_abline(intercept = modelo_1$coefficients["(Intercept)"], slope = modelo_1$coefficients["quantidade"]) +
  geom_abline(intercept = modelo_2$coefficients["(Intercept)"], slope = modelo_2$coefficients["quantidade"])
# Análise:
# As retas aparentam, visualmente, estarem bem ajustadas
# ao cluster da qual pertencem. Mas, ainda assim,
# parece melhor utilizar apenas dois clusters.

# 7. Modelo com n Ks:
# Talvez o melhor jeito seja encontrar N Ks que produzam,
# maior aumento de correlação total dos lms
# 7.1. Achar n Ks
# Procura-se maximizar a correlação média (não ponderada) dos grupos:
cor_media <- tibble()
for (n in seq(1, 50, 2)) {
  set.seed(42)
  resultados <- kmeans(x = df$angulo, centers = n)
  
  df$cluster <- as.factor(resultados$cluster)
  
  cat("N = ", n, "\n")
  
  vec_cors <- c()
  min_cor <- 100
  max_cor <- 0
  
  for (k in sort(unique(df$cluster))) {
    df_sub <- df |> 
      filter(df$cluster == k)
    
    modelo <- lm(data = df_sub,
                 formula = valor ~ quantidade)
    
    cor <- cor(modelo$fitted.values, df_sub$valor)
    vec_cors <- append(vec_cors, cor)
    
    if (cor < min_cor) {
      min_cor <- cor
    }
    if (cor > max_cor) {
      max_cor <- cor
    }
    
    rm(cor)
    rm(df_sub)
    rm(modelo)
  }
  
  # Olha correlação média dos valores
  
  # Média ponderada por número de lementos no cluster
  num_por_cluster <- df |>
    group_by(cluster) |>
    summarise(num = n())
  media_pond <- sum(vec_cors * num_por_cluster$num) / nrow(df)
  
  # Média normal (aritmética)
  media_aritm <- sum(vec_cors) / n
  cor_media <- cor_media |> 
    bind_rows(tibble(n = n,
                     media_aritm = media_aritm,
                     media_pond = media_pond,
                     max_cor = max_cor,
                     min_cor = min_cor))
  cat("Correlação média: ", paste0(round(media_aritm * 100, 2), "%\n"))
  cat("Correlação média ponderada: ", paste0(round(media_pond * 100, 2), "%\n"))
  rm(num_por_cluster)  
}

cor_media |> 
  kbl(
    format = "latex",
    caption = "Tabela de correlações para cada categoria",
    label = "tab_cor_n_cluster",
    booktabs = TRUE,
    digits = 3
  ) |> 
  kable_styling(latex_options = c("scale_down", "hold_position"))


# 7.2. Plot da correlação média
# 7.2.1. Plot geral
cor_media |> 
  pivot_longer(cols = c("media_pond", "media_aritm"),
               names_to = "tipo_media",
               values_to = "medias") |> 
  ggplot(aes(x = n, y = medias, colour = tipo_media)) +
  geom_point() +
  geom_line()

# 7.2.2 Plot a partir de 2 grupos
cor_media |> 
  pivot_longer(cols = c("media_pond", "media_aritm"),
               names_to = "tipo_media",
               values_to = "medias") |> 
  slice(-1, -2) |> 
  ggplot(aes(x = n, y = medias, colour = tipo_media)) +
  geom_point() +
  geom_line()
# Análise:
# Atinge relativa estabilidade com 15 retas

# 7.3. Correlação com 15 retas ----
# Procura-se o ponto em que todas correlaçoes são >= 0.9
cor_media |> 
  filter(min_cor >= 0.89)
# 8 entradas, a partir do n = 13
# n = 13: Menor = 0.947
# Logo, tmos o limite inferior de 13 clusters

# 8. Ângulo visto unidmensionalmente ----
df |> 
  count(angulo) |>
  ggplot(aes(x = angulo, y = n)) +
  geom_col(fill = "steelblue", color = "black") +
  labs(x = "Angulo", y = "Count")

set.seed(42)
df |> 
  ggplot(aes(x = angulo, y = 0)) +
  geom_jitter(width = 0, height = 1, alpha = 0.2)
# Análise:
# O problema do ângulo é que existe muito mais tangentes
# entre o mesmo intervalo a medidade que se aproxima
# do ângulo de de 90º!


set.seed(42)
df |> 
  ggplot(aes(x = angulo, y = 0)) +
  geom_jitter(width = 0, size = 3, height = 1, alpha = 0.01) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()
  )
# Análise:
# Era uma ilusão pelas linhas de grid de fundo...
# Sem padrão

set.seed(42)
df |> 
  ggplot(aes(x = tg, y = 0)) +
  geom_jitter(width = 0, height = 1, alpha = 0.2)
# A tangente não foi muito promissora...

df |> 
  ggplot(aes(x = min_max_normalizar(tg), y = 0)) +
  geom_jitter(width = 0, height = 1, alpha = 0.2)
# A normalização não funciona tão bem também...

df |> 
  ggplot(aes(x = scale(tg), y = 0)) +
  geom_jitter(width = 0, height = 1, alpha = 0.2)
# Nope também...

# 9. Contagem de valores infinitos ----
df |> 
  filter(tg == Inf)
# ... Muitos dados ...

# 10. Gráfico agrupado por pf e pj e recebedor/pagador ----
## 10.1. Recebedor ----
df |> 
  ggplot(aes(x = quantidade, y = valor, color = rec_pfpj)) +
  geom_point()
# Análise:
# Não aparente ser tão bem segmentada nas duas retas

### 10.1.1. Categoria: pf ---
ggplot() +
  geom_point(data = df |> filter(rec_pfpj == "pf"), mapping = aes(x = quantidade, y = valor), color = "lightgreen") +
  geom_point(data = df |> filter(rec_pfpj == "pj"), mapping = aes(x = quantidade, y = valor), color = "lightblue", alpha = 0)
# Análise:
# Puramente parte da reta horizontal dois.


### 10.1.2. Categoria: pj ---
ggplot() +
  geom_point(data = df |> filter(rec_pfpj == "pf"), mapping = aes(x = quantidade, y = valor), color = "lightgreen", alpha = 0) +
  geom_point(data = df |> filter(rec_pfpj == "pj"), mapping = aes(x = quantidade, y = valor), color = "lightblue")
# Análise:
# Vaza para ambas as retas.

## 10.2. Pagador ----
df |> 
  ggplot(aes(x = quantidade, y = valor, color = pag_pfpj)) +
  geom_point()
# Grupos bem delimitados, com separação aparentemente clara entre eles
# Contudo é uma ilusão

### 10.2.1. Categoria: pf ---
ggplot() +
  geom_point(data = df |> filter(rec_pfpj == "pf"), mapping = aes(x = quantidade, y = valor), color = "lightgreen") +
  geom_point(data = df |> filter(rec_pfpj == "pj"), mapping = aes(x = quantidade, y = valor), color = "lightblue", alpha = 0)
# Análise:
# Apenas parte do grupo dois.

### 10.2.2. Categoria: pj ---
ggplot() +
  geom_point(data = df |> filter(rec_pfpj == "pf"), mapping = aes(x = quantidade, y = valor), color = "lightgreen", alpha = 0) +
  geom_point(data = df |> filter(rec_pfpj == "pj"), mapping = aes(x = quantidade, y = valor), color = "lightblue")
# Análise:
# Há vazamento de dados!

# 10.3. Plotando com wrap ----
# 10.3.1. Para Recebedor ----
ggplot(df) +
  geom_point(aes(x = quantidade, y = valor)) +
  facet_wrap(~rec_pfpj)
# Análise:
# Como pode-se ao separar pelas categorias, os grupos de pf
# se mantém na reta horziontal: muitos PIXs e pequenos valores

# 10.3.2. Para pagador ----
ggplot(df) +
  geom_point(aes(x = quantidade, y = valor)) +
  facet_wrap(~pag_pfpj)
# Análise:
# Idem para o anterior

# 11. Tangente, unidimensional, colorida por categoria
# Usa-se "pag_pfpj' por este ser de melhor segmentação
set.seed(42)
df |> 
  ggplot(aes(x = angulo, y = 0, color = pag_pfpj)) +
  geom_jitter(width = 0, height = 1, alpha = 0.2)

# Análise:
# Os espaços entres as retas "sumiram"

set.seed(42)
df |> 
  ggplot(aes(x = angulo, y = 0, color = rec_pfpj)) +
  geom_jitter(width = 0, size = 2, height = 1, alpha = 0.01)+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()
  )
# Análise:
# Idêntico ao anterior
# As retas quase certamente são ilusórias...

# 11. Ângulo 1d com reta vertical ----
df |> 
  ggplot() +
  geom_vline(aes(xintercept = angulo), alpha = 0.02, size = 10) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()
  )

# 12. Reta vertical colorida por cluster
set.seed(42)
resultados <- kmeans(x = df$angulo, centers = 2)

df$cluster <- as.factor(resultados$cluster)

df |> 
  ggplot() +
  geom_vline(aes(xintercept = angulo, colour = cluster), alpha = 0.2, size = 10) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()
  )
# Análise:
# Percebe-se onde a divisão foi feita!  