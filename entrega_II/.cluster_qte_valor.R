# Olhar padrão por valor e quantidade
library("tidyverse")

df <- read_csv("estatisticas-de-transações-pix.csv")

df <- janitor::clean_names(df)

# Função de normalização
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

em_graus <- function(rad) {
  rad * 180 / pi
}

# Separação baseado em ângulo
# Pego a reta contendo cada ponto
scaled_df <- df |> 
  mutate(qte_norm = min_max_norm(quantidade)) |> 
  mutate(valor_norm = min_max_norm(valor))

# plot(scaled_df$quantidade, scaled_df$valor, main = "Normal")
# plot(scaled_df$qte_norm, scaled_df$valor_norm, main = "Normalizado")

scaled_df <- scaled_df |> 
  mutate(tg = valor_norm / qte_norm) |> 
  mutate(angulo = em_graus(atan(tg))) |> 
  mutate(angulo = if_else(angulo == Inf, 90, as.integer(angulo)))

summary(scaled_df$tg)
summary(scaled_df$angulo)


scaled_df <- scaled_df |> 
  mutate(m_45 = if_else(angulo > 45, TRUE, FALSE))

angles <- seq(90, 0, by = -5)

r <- 1  

lines <- data.frame(
  angle = angles,
  x = 0,  # origin
  y = 0,
  xend = r * cos(pi * (90 - angles) / 180),
  yend = r * sin(pi * (90 - angles) / 180)
)

scaled_df |> 
  ggplot() +
  geom_point(aes(x = qte_norm, y = valor_norm, color = m_45))

scaled_df |> 
  ggplot() +
  geom_segment(
    data = lines,
    aes(x = x, y = y, xend = xend, yend = yend),
    arrow = arrow(length = unit(0.08, "inches")),
    linewidth = 0.8,
    color = "black",
    alpha = 0.4
  ) +
  geom_text(
    data = lines,
    aes(x = xend, y = yend, label = paste0(angle, "°")),
    vjust = -0.3,   # vertical adjustment
    hjust = 0.5,    # horizontal alignment
    size = 3.5,
    color = "black"
  ) +
  geom_point(aes(x = qte_norm, y = valor_norm, color = m_45)) +
  coord_fixed()

# Plot
ggplot(lines) +
  geom_segment(aes(x = 0, y = 0, xend = xend, yend = yend),
               arrow = arrow(length = unit(0.1, "inches")),
               linewidth = 1.2, color = "blue") +
  coord_fixed() +
  theme_minimal() +
  labs(title = "Radial lines with different inclinations",
       x = "X", y = "Y")

# Pegar o ângulo pelo arcos
df <- df |> 
  mutate(angulo = atan(scale(df$valor) / scale(df$quantidade)))

df |>  2 * pi

df |> 
  ggplot(aes(x = quantidade, y = valor, color = angulo)) +
  geom_point()

# Teste de min-max
# Create a sample vector
set.seed(42)
x <- rnorm(400, 24, 3)
y <- runif(400, 23, 30)

# Create the normalization function
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

# Apply the function
normalized_x <- min_max_norm(x)
normalized_y <- min_max_norm(y)
plot(normalized_x, normalized_y)
plot(x, y)
