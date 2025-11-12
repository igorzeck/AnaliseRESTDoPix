# Análise de KMeans recursivo, ângulo e ângulo / distância ----
# TODO: tg (ou ângulo) / distância
# TODO: kmeans dividndo espaço 
# TODO: zoom no gráfico do ângulo (FEITO)
## 0. Setup ----
source("processos_estatisticos_alt.R")

df <- carregar("estatisticas_de_transações_pix.csv") |> 
  mutate(qte_norm = min_max_normalizar(quantidade)) |> 
  mutate(valor_norm = min_max_normalizar(valor)) |> 
  mutate(tg = valor_norm / qte_norm) |> 
  mutate(angulo = as.integer(rad_para_grau(atan(tg)))) |> 
  drop_na()
  

## 1. Zoom no gráfico do ângulo ----
# Normal
df |> 
  ggplot() +
  geom_vline(aes(xintercept = angulo), alpha = 0.02, linewidth = 10) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()
  )

# Com zoom
# Normal
df |> 
  ggplot(aes(x = angulo, y = 0)) +
  geom_jitter(width = 0, size = 2, height = 1, alpha = 0.01)+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()
  )

# Com Zoom (0º até 10º)
df |> 
  filter(angulo < 30) |> 
  ggplot(aes(x = angulo, y = 0)) +
  geom_jitter(width = 0, size = 6, height = 1, alpha = 0.002)+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()
  )
# Análise:
# Sem grande melhoras, muito denso próximos dos valores de 0º
# Outros valores no zoom sem ser 30º não meloram muito
df |> 
  filter(angulo < 30) |> 
  ggplot() +
  geom_vline(aes(xintercept = angulo), alpha = 0.09, size = 10) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()
  )
## 2. ang / hipotenusa ----
df <- df |> 
  mutate(distancia = hipot(qte_norm, valor_norm)) |> 
  mutate(ang_dist = angulo / hipot(qte_norm, valor_norm))

# Plotando angulo por hipotenusa
df |> 
  ggplot(aes(angulo, distancia)) +
  geom_point()
# Análise:
# 

# Plot 1D da distância dividida pelo ângulo
df |> 
  ggplot(aes(ang_dist, 0)) +
  geom_jitter(width = 0, size = 2, height = 1, alpha = 0.02) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()
  )
# Análise:
# Aparenta haver, fracamente, alguns grupos

# Com zoom
# Olhando segmentado por quartil
df |> 
  mutate(quartil = ntile(ang_dist, 4)) |> 
  ggplot(aes(ang_dist, 0)) +
  geom_jitter(width = 0, size = 2, height = 1, alpha = 0.02) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()
  ) +
  facet_wrap(~ quartil, scales = "free_x")
