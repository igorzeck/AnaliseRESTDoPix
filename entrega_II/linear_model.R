# Linear Model
# Separação do modelo por agrupamento
# 0. Setup
source("processos_estatisticos_alt.R")

df <- carregar("estatisticas_de_transações_pix.csv") |> 
      mutate(qte_norm = min_max_normalizar(quantidade)) |> 
      mutate(valor_norm = min_max_normalizar(valor)) |> 
      mutate(tg = valor_norm / qte_norm) |> 
      mutate(angulo = as.integer(rad_para_grau(atan(tg)))) |> # Força arredondamento
      mutate(grupo = case_when(
        angulo >= 77 ~ "A",
        (angulo < 77 & angulo >= 45) ~ "B",
        (angulo < 45 & angulo >= 5) ~ "C",
        (angulo < 5) ~ "D",
        TRUE ~ "F" # Se não se encaixar de alguma forma
      )) |> # Por otimização de memória "factoriza" textos
      mutate(across(where(is.character), ~as.factor(.)))
  

# -- Características estatísticas de cada grupo --
# - Correlação entre variáveis quantitativas -
cat("Correlação",
    cor(df$quantidade, df$valor),
    "\n")

for (gp in unique(df$grupo)) {
  df_GP <- df |> 
    filter(grupo == gp)
  print(paste0("Grupo: ", gp))
  # print(summary(df_GP))
  cat("Correlação",
      cor(df_GP$quantidade, df_GP$valor),
      "\n")
}

# Por sumarização
df |> 
  group_by(grupo) |> 
  summarise(n = n()) |> 
  mutate(freq = paste0(round(n / sum(n) * 100, 2), "%"))

# -- Criação do linear model --
modelo_total  <- df |> lm(
    formula = valor ~ quantidade,
    na.action = na.omit
  )

cor(resid(modelo_total)**2, df$quantidade)
# Correlação = 0.07242869 (Muito baixa)

cor(modelo_total$fitted.values, df$valor)
# Correlação para os valores é de 0.2614237

# Gráfico com linha de regressão do modelo completo
# Figura 10
df |> 
  ggplot() +
  geom_point(aes(x = quantidade, y = valor), alpha = 0.25) +
  geom_abline(intercept = modelo_total$coefficients["(Intercept)"],
              slope = modelo_total$coefficients["quantidade"],
              colour = "blue")

# Gráfico por grupo
df |> 
  ggplot(aes(x = quantidade, y = valor, colour = grupo)) +
  geom_point()

# -- Reta A --
modelo_A <- df |> lm(
    formula = valor ~ quantidade,
    subset = grupo == "A",
    na.action = na.omit
  )

cor(resid(modelo_A)**2, (df |> filter(grupo == "A"))$quantidade)
# Correlação = 0.663357 (Moderada/Alta)

plot(modelo_A)

# -- Reta A --
modelo_A <- df |> lm(
  formula = valor ~ quantidade,
  subset = grupo == "A",
  na.action = na.omit
)

cor(resid(modelo_A)**2, (df |> filter(grupo == "A"))$quantidade)
# Correlação = 0.663357 (Moderada/Alta)

# -- Generalizando --
# Para todos os grupos
for (gp in sort(unique(df$grupo))) {
  if (gp == "F") {
    break
  }
  df_sub <- df |> 
    filter(grupo == gp)
  modelo_GP <- df_sub |> lm(
    formula = valor ~ quantidade,
    na.action = na.omit
  )
  print(paste0(gp,
              ": ",
              round(cor(modelo_GP$fitted.values, df_sub$valor), 3)))
  
  rm(modelo_GP)
  rm(df_sub)
}

# -- Erro --
# -- Total --
alfa <- achar_alfa(df$quantidade, df$valor)
beta <- achar_beta(df$quantidade, df$valor)

erros <- tibble(erro = df$valor - (beta * df$quantidade + alfa),
                quantidade = df$quantidade)

erros <- erros |>
  mutate(indice = as.numeric(row.names(erros)))

# -- Erro é ligeiramente assimétrico --
summary(erros)

erros |> 
  ggplot() +
  geom_point(aes(x = indice, y = erro))
# O Erro total é bem assimétrico (problema de escala entre x e y)!

erros |>
  ggplot() +
  geom_point(aes(x = quantidade, y = erro))

# - Em escala logarítimica -
alfa <- achar_alfa(log(df$quantidade), log(df$valor))
beta <- achar_beta(log(df$quantidade), log(df$valor))

erros <- tibble(erro = log(df$valor) - (beta * log(df$quantidade) + alfa),
                quantidade = log(df$quantidade))

erros |> 
  ggplot() +
  geom_point(aes(x = quantidade, y = erro))
# Uma bagunça!

# Isso pode também ser observado pelos resíduos
plot(modelo_total)

erros_totais <- tibble()
summ_totais <- tibble()

# -- Erros por grupo --
for (gp in sort(unique(df$grupo))) {
  if (gp == "F") {
    break
  }
  df_GP <- df |> 
    filter(grupo == gp)
  alfa <- achar_alfa(df_GP$quantidade, df_GP$valor)
  beta <- achar_beta(df_GP$quantidade, df_GP$valor)
  
  erros <- tibble(erro = df_GP$valor - (beta * df_GP$quantidade + alfa),
                  grupo = gp,
                  quantidade = df_GP$quantidade)
  print(paste("Grupo", gp))
  print(summary(erros$erro))
  
  summ <- tibble()
  summ <- bind_rows(summ, summary(erros$erro, digits = 2)) |> 
    mutate(grupo = gp) |> 
    relocate(grupo)
  
  summ_totais <- bind_rows(summ_totais, summ)
  erros_totais <- bind_rows(erros_totais, erros)
  rm(summ)
}

view(summ_totais)

erros_totais <- erros_totais |>
  mutate(indice = as.numeric(row.names(erros_totais)))

erros_totais |> 
  ggplot() +
  geom_point(aes(x = quantidade, y = erro)) +
  facet_wrap(~ grupo, scales = "free")

# -- Erros por resíduos dos modelos --

df |> 
  group_by(grupo) |> 
  summarise(n = n())

res_totais <- tibble()

for (gp in sort(unique(df$grupo))) {
  if (gp == "F") {
    break
  }
  modelo_GP <- df |> lm(
    formula = valor ~ quantidade,
    subset = grupo == gp,
    na.action = na.omit
  )
  
  # Correlação entre o erro e a variável explicativa (Figura 18)
  cat(gp,
      round(
          cor(residuals(modelo_GP)**2,  (df |> filter(grupo == gp))$quantidade), 3
        ),
      "\n")
  
  res_totais <- bind_rows(res_totais, tibble(res = residuals(modelo_GP), grupo = gp))
}

res_totais <- res_totais |>
  mutate(indice = as.numeric(row.names(res_totais)))

# Gráfico das retas por grupo (Figura 15)
res_totais |> 
  ggplot() +
  geom_point(aes(x = indice, y = res)) +
  facet_wrap(~ grupo, scales = "free")

# -- Exploração do erro total --
set.seed(42)
df_redux <- sample_n(df, 5e4)
ggplot(df_redux) +
  geom_point(aes(x = quantidade, y = valor, color = grupo))

alfa_redux <- achar_alfa(df_redux$quantidade, df_redux$valor)
beta_redux <- achar_beta(df_redux$quantidade, df_redux$valor)

erro <- df_redux$valor - (beta_redux * df_redux$quantidade + alfa_redux)
# Plot do erro: Sofre de assimetria!
plot(erro)

# Plotando em função do máximo ou míniomo (0 no meio)
plot(erro, ylim = c(-max(erro), max(erro)))
plot(erro, ylim = c(-min(erro), min(erro)))

# Note que essa assimetria ocorre menos a média sendo zero
summary(erro)

# -- Retas no gráfico --
# Análise das retas
# -- Total --
df_redux |> 
  ggplot() +
  geom_point(aes(x = quantidade, y = valor)) + # Reta A
  geom_abline(intercept = alfa_redux, slope = beta_redux)

# -- Por grupo --
# - Pela função lm -
modelo_A <- df |> lm(
  formula = valor ~ quantidade,
  subset = grupo == "A",
  na.action = na.omit
)
modelo_B <- df |> lm(
  formula = valor ~ quantidade,
  subset = grupo == "B",
  na.action = na.omit
)
modelo_C <- df |> lm(
  formula = valor ~ quantidade,
  subset = grupo == "C",
  na.action = na.omit
)
modelo_D <- df |> lm(
  formula = valor ~ quantidade,
  subset = grupo == "D",
  na.action = na.omit
)

df |> 
  ggplot() +
  geom_point(aes(x = quantidade, y = valor, colour = grupo)) +
  geom_abline(intercept = modelo_A$coefficients["(Intercept)"], slope = modelo_A$coefficients["quantidade"]) +
  geom_abline(intercept = modelo_B$coefficients["(Intercept)"], slope = modelo_B$coefficients["quantidade"]) +
  geom_abline(intercept = modelo_C$coefficients["(Intercept)"], slope = modelo_C$coefficients["quantidade"]) +
  geom_abline(intercept = modelo_D$coefficients["(Intercept)"], slope = modelo_D$coefficients["quantidade"])

# - Pela fórmula -
coefs <- tibble()
for (gp in unique(df$grupo)) {
  if (gp == "F") {
    break
  }
  df_gp <- df |> 
    filter(grupo == gp)
  
  alfa <- achar_alfa(df_gp$quantidade, df_gp$valor)
  beta <- achar_beta(df_gp$quantidade, df_gp$valor)
  
  coefs_aux <- tribble(
    ~modelo, ~beta, ~alfa,
    gp, beta, alfa
  )
  
  coefs <- bind_rows(coefs, coefs_aux)
}

df |> 
  ggplot(aes(colour = grupo)) +
  geom_point(aes(x = quantidade, y = valor)) +
  geom_abline(data = coefs,
              aes(slope = beta,
                  intercept = alfa,
                  colour = modelo),
              linewidth = 0.7)

# -- Modelo ajustado com o inverso do erro --
# Pela variância do erro
modelo_var <- df |> 
  lm(formula = log(valor) ~ quantidade)

var_estimada <- exp(fitted(modelo_var))

modelo_ajustado <- df |> 
  lm(
    formula = valor ~ quantidade,
    weights = 1/var_estimada,
    na.action = na.omit
  )

print(modelo_ajustado)
# Com estes pesos a correlação entre os valores ajustados se mantém
cor(modelo_ajustado$fitted.values, df$valor)

# No entanto, a correlação do quadrado dos residuos para com a quantidade aumenta
cor(resid(modelo_total)**2, df$quantidade)
# Menor correlação = melhor para o caso de erros e a explicativa

# Plot dos erros do modelo total
plot(modelo_total$residuals)

# Plot dos erros do modelo ajustado
plot(modelo_ajustado$residuals)

# Plot do erro por variáveis previstas
plot(modelo_ajustado$fitted.values, modelo_ajustado$residuals)

# Logaritminzando ambas variáveis (para compensar escala distorcida)
modelo_ajustado <- df |> 
  lm(
    formula = log(valor) ~ log(quantidade),
    weights = 1/var_estimada,
    na.action = na.omit
  )

plot(modelo_ajustado$fitted.values, modelo_ajustado$residuals)
# Ainda apresenta um padrão, com menores erros par maiores valores
plot(modelo_ajustado$residuals)

# -- Por grupo --
# - Com log -
df_a <- df |> 
  filter(grupo == "A")
modelo_A <- df |> 
  lm(
    formula = log(valor) ~ log(quantidade),
    subset = grupo == "A",
    na.action = na.omit
  )

cor(resid(modelo_A)**2, log(df_a$quantidade))
# Baixa correação entre o erro e a variável explicativa
cor(modelo_A$fitted.values, log(df_a$valor))
# Correlação de 89% (Alta)

plot(modelo_A$fitted.values, modelo_A$residuals)
# Apresenta reta diagonal, demonstrando haver padrão

plot(modelo_ajustado$residuals)
# Erro constante em uma reta

# - Sem log -
modelo_A <- df |> 
  lm(
    formula = valor ~ quantidade,
    subset = grupo == "A",
    na.action = na.omit
  )

cor(resid(modelo_A)**2, df_a$quantidade)
# Grande correlação, indicativo de tendência

cor(modelo_A$fitted.values, df_a$valor)
# Correlação de 99.7% (Muito Alta)

plot(modelo_A$fitted.values, modelo_A$residuals)
# Apresenta reta diagonal, demonstrando haver padrão
# Sinal claro de homocedasticidade

# Arrumando os pesos
modelo_var <- df_a |> 
  lm(formula = log(valor) ~ quantidade)

var_estimada <- exp(fitted(modelo_var))

modelo_ajustado <- df_a |> 
  lm(
    formula = valor ~ quantidade,
    weights = 1/var_estimada,
    na.action = na.omit
  )

cor(resid(modelo_ajustado)**2, df_a$quantidade)
# Grande correlação, indicativo de tendência

cor(modelo_ajustado$fitted.values, df_a$valor)
# Correlação de 99.7% (Muito Alta)

plot(modelo_ajustado$fitted.values, modelo_ajustado$residuals)
# Apresenta reta diagonal, demonstrando haver padrão
# Sinal claro de homocedasticidade

# Isso prova grande homocedasticidade dos dados!
# -- Aferimento dos valores em log --
res_totais <- tibble()
for (gp in sort(unique(df$grupo))) {
  if (gp == "F") {
    break
  }
  modelo_GP <- df |> lm(
    formula = log(valor) ~ log(quantidade),
    subset = grupo == gp,
    na.action = na.omit
  )
  
  # Correlação entre o erro e a variável explicativa (Figura 18)
  cat(gp,
      round(
        cor(residuals(modelo_GP)**2,  log((df |> filter(grupo == gp))$quantidade)), 3
      ),
      "\n")
  
  res_totais <- bind_rows(res_totais, tibble(res = residuals(modelo_GP), grupo = gp))
}

res_totais <- res_totais |>
  mutate(indice = as.numeric(row.names(res_totais)))
# Aparente redução na correlação dos erros para com a variável explicativa

# -- Plotando (em log) modelo e sua reta --
alfa <- achar_alfa(log(df$quantidade), log(df$valor))
beta <- achar_beta(log(df$quantidade), log(df$valor))

df |> 
  ggplot() +
  geom_point(aes(x = log(quantidade), y = log(valor))) + # Reta A
  geom_abline(aes(intercept = alfa, slope = beta, colour = "red"))

# -- Erros por modelo --
for (gp in sort(unique(df$grupo))) {
  if (gp == "F") {
    break
  }
  df_gp <- df |> 
    filter(grupo == gp)
  
  modelo_GP <- df_gp |> 
    lm(formula = valor ~ quantidade,
       na.action = na.omit)
  
  cat(gp,
      round(
        cor(residuals(modelo_GP)**2,  df_gp$quantidade), 3
      ),
      "\n")
  rm(df_gp)
}

# -- Erros por modelo ajustado --
res_totais <- tibble()

for (gp in sort(unique(df$grupo))) {
  if (gp == "F") {
    break
  }
  df_GP <- df |> 
    filter(grupo == gp)
  
  modelo_var <- df_GP |> 
    lm(formula = log(valor) ~ quantidade,
       na.action = na.omit)
  
  var_estimada <- exp(fitted(modelo_var))
  
  modelo_GP <- df_GP |> lm(
    formula = valor ~ quantidade,
    weights = 1/var_estimada,
    na.action = na.omit
  )
  
  cat(gp,
      round(
        cor(residuals(modelo_GP)**2,  df_GP$quantidade), 3
      ),
      "\n")
  
  res_totais <- bind_rows(res_totais, tibble(res = residuals(modelo_GP),
                                             grupo = gp,
                                             quantidade = df_GP$quantidade))
  rm(modelo_GP)  
}

# Apresenta alta correlação entre os erros e a quantidade,
# Incluside com os valores do erro sendo maiores

res_totais <- res_totais |>
  mutate(indice = as.numeric(row.names(res_totais)))

res_totais |> 
  ggplot() +
  geom_point(aes(x = quantidade, y = res)) +
  facet_wrap(~ grupo, scales = "free")

res_totais |> 
  ggplot() +
  geom_point(aes(x = indice, y = res)) +
  facet_wrap(~ grupo, scales = "free")
