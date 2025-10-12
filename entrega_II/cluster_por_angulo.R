# Clusterização em R para achar o ângulo das retas
# Aqui foi feito utilizando o tidyverse e o ggplot2
# -- Setup --
source("processos_estatisticos_alt.R")

# Opcional se já tiver o dataset!
df <- read_csv("estatisticas_de_transações_pix.csv",
               locale = locale(decimal_mark = ","),
               n_max = 1e4) |> 
  janitor::clean_names() # limpa nome das variáveis

# Normalização dos dados
str(df)
df |> select(valor)

angulos <- seq(90, 0, by = -10) # Ângulo das retas

rads <- grau_para_rad(angulos)
tgs <-round(tan(rads), 2) # Pega tangente das retas

r <- 1  # Raio das retas

# Dataframe (tibble) com retas
linhas <- data_frame(
  angulo = angulos,
  x_origem = 0,
  y_origem = 0,
  x_ponta = r * cos(grau_para_rad(angulos)),
  y_ponta = r * sin(grau_para_rad(angulos))
)
# -- Código --
# 0. Normalização dos dados
# Os plots no R são 'distorcidos' para que o eixo X e Y
# tenham escala visual semelhante, mas na prática
qte_norm <- min_max_normalizar(df$quantidade)
valor_norm <- min_max_normalizar(df$valor)

# 1. Separação pela reta identidade (tangente = 1 e ângulo = 45º)
# Primeiro, pode-se perceber que a reta identidade divide
# o espaço em 2!
ggplot() +
  geom_point(aes(x = qte_norm, y = valor_norm)) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1),
               linewidth = 0.4, color = "blue", alpha = 0.7,
               linetype = "dotted") +
  coord_fixed()

# A reta identidade é aquela cuja a fóruma é y = x, ou seja,
# A sua tangente é igual a 1!
# Retas abaixo dela têm tangente MENOR que 1
# Retas acima dela têm tangente MAIOR que 1
# Visualizado:

# Ângulos
# Figura 11
ggplot() +
  geom_point(aes(x = qte_norm, y = valor_norm)) +
  # Segmento das flechas
  geom_segment(data = linhas,
               aes(x = x_origem,
                   y = y_origem,
                   xend = x_ponta,
                   yend = y_ponta),
               arrow = arrow(length = unit(0.08, "inches")),
               linewidth = 0.4,
               color = "blue",
               alpha = 0.7,
               linetype = "dotted") + # Highlight na reta identidade
  geom_segment(
    aes(x = 0,
        y = 0,
        xend = cos(grau_para_rad(45)),
        yend = sin(grau_para_rad(45))),
    arrow = arrow(length = unit(0.08, "inches")),
    linewidth = 0.4,
    color = "red",
    alpha = 0.7
  ) + # Texto na ponta das flechas
  geom_text(aes(x = cos(grau_para_rad(45)),
                y = sin(grau_para_rad(45)),
                label = paste0("1")),
            vjust = -0.3,
            hjust = 0.5,
            size = 4.5,
            color = "red") +
  geom_text(data = linhas,
            aes(x = x_ponta,
                y = y_ponta,
                label = paste0(tgs)),
            vjust = -0.3,
            hjust = 0.5,
            size = 3.5,
            color = "black") + # Para manter coordenanadas fixas
  coord_fixed()  +
  labs(title = "Tangente das retas!")

# "Acima" da identidade as retas tem tg > 1 e abaixo tg < 1

# 2. Exemplo de tangente da reta que passa por um ponto
# Sorteia um ponto qualquer (outlier)
set.seed(42)
ponto <- sample_n(tibble(qte_norm, valor_norm) |> filter(qte_norm > 0.35 & valor_norm > 0.05), size = 1)
tg_ponto <- round(ponto$valor_norm / ponto$qte_norm, 3)

labs_aux_x = sort(c(seq(0,1,0.25), round(ponto$qte_norm, 2)))
labs_aux_y = sort(c(seq(0,1,0.25), round(ponto$valor_norm, 2)))

ggplot() +
  geom_point(aes(x = qte_norm, y = valor_norm), alpha = 0.3) + # Ponto especial
  geom_point(aes(x = ponto$qte_norm, y = ponto$valor_norm), colour = "blue", size = 3) + # Reta horizontal
  # geom_vline(xintercept = ponto$qte_norm, alpha = 0.3) + 
  # geom_hline(yintercept = ponto$valor_norm, alpha = 0.3) + 
  geom_segment(aes(x = ponto$qte_norm,
                   y = 0,
                   xend = ponto$qte_norm,
                   yend = ponto$valor_norm),
               linetype = "dotted") + # Reta vertical
  geom_segment(aes(x = 0,
                   y = ponto$valor_norm,
                   xend = ponto$qte_norm,
                   yend = ponto$valor_norm),
               linetype = "dotted") + # Reta diagonal
  geom_abline(slope = tg_ponto, colour = "blue") + # coordenada Y
  scale_x_continuous(breaks = labs_aux_x, labels = labs_aux_x) + # coordenada X
  scale_y_continuous(breaks = labs_aux_y, labels = labs_aux_y) + # Texto
  geom_text(aes(x = ponto$qte_norm, y = ponto$valor_norm),
            label = (paste0("tg(",
                            round(ponto$qte_norm,2),
                            ", ",
                            round(ponto$valor_norm,2),
                            ") = ",
                            tg_ponto,
                            " ~ ",round(rad_para_grau(atan(tg_ponto)),3), "º")),
            vjust = -1.9,
            hjust = 0.5,
            size = 4.5,
            color = "black")

rm(qte_norm)
rm(valor_norm)

# -- Com dados totais --
df <- carregar("estatisticas_de_transações_pix.csv") |> 
  mutate(qte_norm = min_max_normalizar(quantidade)) |> 
  mutate(valor_norm = min_max_normalizar(valor))

# 3. Análise da distribuição dos dados
# -- Redefine os ângulos para inspeção visual --
angulos <- seq(90, 0, by = -5) # Ângulo das retas

r <- 1  # Raio das retas

# Dataframe (tibble) com retas
linhas <- tibble(
  angulo = angulos,
  x_origem = 0,
  y_origem = 0,
  x_ponta = r * cos(grau_para_rad(angulos)),
  y_ponta = r * sin(grau_para_rad(angulos))
)

# Ângulos
# Figura 13
ggplot() +
  geom_point(data = df, aes(x = qte_norm, y = valor_norm)) +
  geom_segment(data = linhas,
               aes(x = x_origem,
                   y = y_origem,
                   xend = x_ponta,
                   yend = y_ponta),
               arrow = arrow(length = unit(0.08, "inches")),
               linewidth = 0.4,
               color = "blue",
               alpha = 0.7,
               linetype = "dotted") +
  geom_text(data = linhas,
            aes(x = x_ponta,
                y = y_ponta,
                label = paste0(angulo, "º")),
            vjust = -0.3,
            hjust = 0.5,
            size = 3.5,
            color = "black") + # Para manter coordenanadas fixas
  coord_fixed()  +
  labs(title = "Ângulo das retas")

# Por inspeção visual, existem "4" retas
# 1 - A reta dos valores > 77º
# 2 - A reta dos valores entre 77º e 45º
# 3 - A reta dos valores de 45º até 5º
# 4 - A reta dos valores menores que 5º

# 4. Agrupamento das retas
df <- df |> 
  mutate(tg = valor_norm / qte_norm) |> 
  mutate(angulo = as.integer(rad_para_grau(atan(tg)))) |> # Força arredondamento
  mutate(grupo = case_when(
    angulo >= 77 ~ "A",
    (angulo < 77 & angulo >= 45) ~ "B",
    (angulo < 45 & angulo >= 5) ~ "C",
    (angulo < 5) ~ "D",
    TRUE ~ "F" # Se não se encaixar de alguma forma
  ))

# 5. Plot final
ggplot(df) +
  geom_point(aes(x = qte_norm, y = valor_norm, color = grupo)) +
  coord_fixed()  +
  labs(title = "Grupos de Retas")
