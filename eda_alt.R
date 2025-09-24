# Arquivo de análise alternativo - V2
# Feito com auxílio do Tidyverse
# Aqui está contido os códigos para os gráficos:
# ...
# E para as tabelas:
# ...
# O nome das variáveis aqui seguem o padrão "snake_case"
# 0. -- Importações --
library(tidyverse)
library(fitdistrplus)

df <- read_csv('estatisticas-de-transações-pix.csv')

# 1. Exploração dos dados --
str(df)
view(df)


# 2. Limpeza dos dados --

# A biblioteca utilizada (janitor) pode não vir instalada com o tidyverse
# Nesse caso: install.packages("janitor")
df <- janitor::clean_names(df)

# Deixa em minúsculos os valores de texto com ao longo (across) todas as colunas
df <- df |> 
  mutate(across(where(is.character), ~tolower(.)))


# 3. Preparação dos dados --
# 3.1 NAs --

# Valores 'Nao informado' serão retirados
df <- df |> 
  mutate(across(where(is.character), ~na_if(., "nao informado")))

# 'Nao disponivel' é retirado para finalidade
df <- df |> 
  mutate(finalidade = na_if(finalidade, "nao disponivel"))

# 3.2 Conversões de tipo
# 3.2.1 Factorização

# Torna-se todas as colunas de texto (character) em factor
df <- df |> 
  mutate(across(where(is.character), ~as.factor(.)))

# Mudança do nome dos levels de 'pag_regiao' para siglas
levels(df$pag_regiao) = c('CO', 'NE', 'N', 'SE', 'S')

# Converte a coluna ano_mes em Data (contando dia 1 para todas as observações)
# Para que a conversão funcione corretamente é necessário ler ano_mes como character
# Daí o uso do paste0 que faz essa conversão automagicamente
# (Uma alternativa seria utilizar o ym() da library lubridate)
df <- df |> 
  mutate(ano_mes = as.Date(paste0(ano_mes, "01"), format = "%Y%m%d"))

# 3.2.2 Numerização
# Converte-se valor e quantidade para numeric e integer respectivamente
df <- df |> 
  mutate(
    valor = as.numeric(valor),
    quantidade = as.integer(quantidade))

# 4. Exploração estatística
# 4.1 Análise descritiva
summary(df)

# 4.1.1 Quantidade
# Por summary
summary(df$quantidade)

# Por funções:
# [1] -> min
# [2] -> 2º
# ...
# [5] -> maxs
quant_val <- quantile(df$quantidade)
quant_val

# Confesso que esse print está um tanto exagerado, e dava para colocar num função...
# Podia também ter sido colocado em um data.frame
cat(
  sprintf("\t-------- Por funções --------"),
  sprintf("\t|%-14s |%12d%s", "Mínimo:", min(df$quantidade), "|"),
  sprintf("\t|%-14s |%12.2f%-12s", "1º Quartil:", quantile(df$quantidade)[2], "|"),
  sprintf("\t|%-14s|%12.2f%-12s", "Mediana:", mean(df$quantidade), "|"),
  sprintf("\t|%-14s |%12.2f%-12s", "Média:", mean(df$quantidade), "|"),
  sprintf("\t|%-14s |%12.2f%s", "3º Quartil:", quantile(df$quantidade)[4], "|"),
  sprintf("\t|%-14s |%12.2f%.19s", "Máximo: ", max(df$quantidade), "|"),
  sprintf("\t-------- Por funções --------"),
  sep = "\n"
)

# Espaço interquartil (Considerando o mínimo como quartil '0')
sprintf("Valor entre quartis 0 e 1: %.2f", quant_val[2] - quant_val[1])
sprintf("Valor entre quartis 1 e 2: %.2f", quant_val[3] - quant_val[2])

sprintf("Diferença relativa entre interquartil 0-1 e interquartil 1-2: %.2f", (quant_val[3] - quant_val[2]) / (quant_val[2] - quant_val[1]))
 # Conclusão: A "distância" entre os quartis 1 e 2 é 24 vezes maior do que distância entre os quartis 0 e 1


# 4.1.2 Valor
# Por summary
summary(df$valor)

# Por funções:
# [1] -> min
# [2] -> 2º
# ...
# [5] -> maxs
quant_val <- quantile(df$valor)
quant_val

# Confesso que esse print está um tanto exagerado, e dava para colocar num função...
# Podia também ter sido colocado em um data.frame
cat(
  sprintf("\t----------- Por funções -----------"),
  sprintf("\t|%-14s |%18d%s", "Mínimo:", min(df$valor), "|"),
  sprintf("\t|%-14s |%18.2f%-12s", "1º Quartil:", quantile(df$valor)[2], "|"),
  sprintf("\t|%-14s|%18.2f%-12s", "Mediana:", mean(df$valor), "|"),
  sprintf("\t|%-14s |%18.2f%-12s", "Média:", mean(df$valor), "|"),
  sprintf("\t|%-14s |%18.2f%s", "3º Quartil:", quantile(df$valor)[4], "|"),
  sprintf("\t|%-14s |%18.2f%.19s", "Máximo: ", max(df$valor), "|"),
  sprintf("\t----------- Por funções -----------"),
  sep = "\n"
)

# Espaço interquartil (Considerando o mínimo como quartil '0')
sprintf("Valor entre quartis 0 e 1: %.2f", quant_val[2] - quant_val[1])
sprintf("Valor entre quartis 1 e 2: %.2f", quant_val[3] - quant_val[2])

sprintf("Diferença relativa entre interquartil 0-1 e interquartil 1-2: %.2f", (quant_val[3] - quant_val[2]) / (quant_val[2] - quant_val[1]))
# Conclusão: A "distância" entre os quartis 1 e 2 é 24 vezes maior do que distância entre os quartis 0 e 1
# 4.2 Tabelas de frequência absoluta e relativa
# O script desta subseção tem que ser executado nesta ordem
# Por questões de recursos computacionais utilizou-se as mesmas variáveis

# pag_idade X pag_pfpj
table(df$pag_idade, df$pag_pfpj)
# Conclusão: PJs nao tem idade aplicável

tab_reg_pfpj_original <- table(df$pag_regiao, df$pag_pfpj)

# pag_regiao X pag_pfpj
tab_reg_pfpj <- rbind(
  cbind(tab_reg_pfpj_original,margin.table(tab_reg_pfpj_original,1)),
  c(margin.table(tab_reg_pfpj_original,2),
  sum(margin.table(tab_reg_pfpj_original,1)))
)
dimnames(tab_reg_pfpj)[[1]][6] <- "Total_linha"
dimnames(tab_reg_pfpj)[[2]][[4]] <- "Total_coluna"
view(tab_reg_pfpj)

# Conclusão: O número total de entradas é corrobordado bate com o esperado
# Vale notar que para todas as regiões o relação pf/pj é praticamente constante (5:1)

# pag_regiao X pag_pfpj (%)
tab_reg_pfpj_percent <- round(100*prop.table(tab_reg_pfpj_original),2)
tab_reg_pfpj_percent <- rbind(
  cbind(tab_reg_pfpj_percent,margin.table(tab_reg_pfpj_percent,1)),
  c(margin.table(tab_reg_pfpj_percent,2),
  sum(margin.table(tab_reg_pfpj_percent,1)))
)
dimnames(tab_reg_pfpj_percent)[[1]][6] <- "Total_linha"
dimnames(tab_reg_pfpj_percent)[[2]][[4]] <- "Total_coluna"
view(tab_reg_pfpj_percent)

# pag_pfpj X pag_regiao
tab_reg_pfpj_original <- table(df$pag_pfpj, df$pag_regiao)
tab_reg_pfpj <- rbind(
  cbind(tab_reg_pfpj_original,margin.table(tab_reg_pfpj_original,1)),
  c(margin.table(tab_reg_pfpj_original,2),
    sum(margin.table(tab_reg_pfpj_original,1)))
)
dimnames(tab_reg_pfpj)
dimnames(tab_reg_pfpj)[[2]][[6]] <- "Total coluna"
dimnames(tab_reg_pfpj)[[1]][4] <- "Total linha"
view(tab_reg_pfpj)

# pag_regiao X pag_pfpj (%)
tab_reg_pfpj_percent <- round(100*prop.table(tab_reg_pfpj_original),2)
tab_reg_pfpj_percent <- rbind(
  cbind(tab_reg_pfpj_percent,margin.table(tab_reg_pfpj_percent,1)),
  c(margin.table(tab_reg_pfpj_percent,2),
    sum(margin.table(tab_reg_pfpj_percent,1)))
)

dimnames(tab_reg_pfpj_percent)[[2]][[6]] <- "Total coluna"
dimnames(tab_reg_pfpj_percent)[[1]][4] <- "Total linha"
view(tab_reg_pfpj_percent)

rm(tab_reg_pfpj_percent)
rm(tab_reg_pfpj_original)

# 4.3 Pix mensal médio
df$pix_medio <- df$valor / df$quantidade

summ <- list()
summ$pixm <- summary(df$pix_medio)
summ$valor <- summary(df$valor)
summ$quantidade <- summary(df$quantidade)

names(summ$pixm)

# 4.4 Fit
fits <- list()
fits$pix_medio <- fitdist(df$pix_medio[df$pix_medio < summ$pixm["3rd Qu."]], "lnorm")
fits$valor <- fitdist(df$valor, "lnorm")
fits$quantidade <- fitdist(df$quantidade, "lnorm")

df_3rd <- data.frame(
  pix_medio = df$pix_medio[df$pix_medio < summ$pixm["3rd Qu."]],
  valor = df$valor[df$valor < summ$valor["3rd Qu."]],
  quantidade = df$quantidade[df$quantidade < summ$quantidade["3rd Qu."]]
)

fits
head(df_3rd)

# 5 Gráficos
# Generalizados
plot(fits$pix_medio)

# 5.1 Histogramas (Frequência)
ggplot(df, aes(x = pix_medio)) +
  geom_histogram() +
  xlim(c(0,summ$pixm["3rd Qu."]))

ggplot(df, aes(x = valor)) +
  geom_histogram() +
  xlim(c(0,summ$valor["3rd Qu."]))

ggplot(df, aes(x = quantidade)) +
  geom_histogram() +
  xlim(c(0,summ$quantidade["3rd Qu."]))

# 5.2 Histogramas (Densidade)
x_seq <- seq(min(df_3rd$pix_medio), max(df_3rd$pix_medio), length.out=500)

hist(df_3rd$pix_medio, probability = TRUE, col = 'lightgreen', main='Histograma da variável Pix médio', xlab = 'Pix médio', ylab='Densidade')
lines(x_seq, dlnorm(x_seq, meanlog = fits$pix_medio$estimate["meanlog"], sdlog = fits$pix_medio$estimate["sdlog"]))

x_seq <- seq(min(df_3rd$valor), max(df_3rd$valor), length.out=500)

hist(df_3rd$valor, probability = TRUE, col = 'lightgreen', main='Histograma da variável Valor', xlab = 'Valor', ylab='Densidade')
lines(x_seq, dlnorm(x_seq, meanlog = fits$valor$estimate["meanlog"], sdlog = fits$valor$estimate["sdlog"]))

x_seq <- seq(min(df_3rd$quantidade), max(df_3rd$quantidade), length.out=500)

hist(df_3rd$quantidade, probability = TRUE, col = 'lightgreen', main='Histograma da variável Quantidade', xlab = 'Quantidade', ylab='Densidade')
lines(x_seq, dlnorm(x_seq, meanlog = fits$quantidade$estimate["meanlog"], sdlog = fits$quantidade$estimate["sdlog"]))

# 5.2 Box-plot
# Seed configurada por reprodutibilidade
set.seed(123)

# Boxplot a parte por questão de visibilidade
boxplot(df$quantidade, df$valor,
        main="Box-plot - Quantidade e Valor",
        xlab="Quantidade e Valor",
        col="lightblue",
        border="darkblue",
        notch = Ts)

# Até 3º quartil
boxplot(df$quantidade,
        main="Box-plot - Quantidade",
        xlab="Quantidade",
        col="lightblue",
        border="darkblue",
        notch = T,
        ylim=c(0,summ$quantidade["3rd Qu."] * 2.5))

boxplot(df$valor,
        main="Box-plot - Valor",
        xlab="Valor",
        col="lightblue",
        border="darkblue",
        notch = T,
        ylim=c(0,summ$valor["3rd Qu."] * 2.5))

# 5.3 Dispersão
# Quantidade X Valor
ggplot(df) +
  geom_point(aes(x = quantidade, y = valor))

# 5.4 Gráfico de Barras
# Região X Quantidade
ggplot(df) + 
  geom_col(
    aes(y = quantidade,
        x = reorder(pag_regiao, quantidade),
        fill = pag_regiao,
        color = pag_regiao)
    ) +
  xlab("pag_regiao")

# Região X Valor
ggplot(df) + 
  geom_col(
    aes(y = valor,
        x = reorder(pag_regiao, valor),
        fill = pag_regiao,
        color = pag_regiao)
    ) +
  xlab("pag_regiao")

# Região X Pix médio
df |> 
  group_by(pag_regiao) |> 
  summarise(pix_medio_regional = mean(pix_medio, na.rm = TRUE), .groups = "drop") |> 
  ggplot(aes(x = reorder(pag_regiao, pix_medio_regional), y = pix_medio_regional, fill = pag_regiao)) +
  geom_col(position = "dodge") +
  labs(
    x = "Região",
    y = "Pix médio",
    fill = "Região"
  ) +
  theme_minimal()

# Quantidade de PIX de PJ e PF por Região
df |> 
  group_by(pag_regiao, pag_pfpj) |> 
  summarise(total_quantidade = sum(quantidade, na.rm = TRUE), .groups = "drop") |> 
  ggplot(aes(x = pag_regiao, y = total_quantidade, fill = pag_pfpj)) +
  geom_col(position = "dodge") +
  labs(
    x = "Região",
    y = "Total de Quantidade",
    fill = "Tipo de Pagador"
  ) +
  theme_minimal()
