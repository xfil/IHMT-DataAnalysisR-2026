
  
#instalar
install.packages("dplyr")
install.packages("ggplot2")
install.packages("openxlsx")
install.packages("janitor")
install.packages("car")

#carregar

library(dplyr)
library(ggplot2)
library(openxlsx)
library(janitor)
library(car)

#set wd
getwd()
setwd("/Users/sebasgoncalves/Desktop/R")

#importar dados e criar objeto 
df <- read.xlsx("Dataset_new_course.xlsx", sheet = 1, startRow = 1, colNames = TRUE)
df

#verificar dados e estrutura - variáveis cataegóricas estão como chr
head(df)
tail(df)
names(df)
str(df)
dim(df)
summary(df)

#O nome de algumas variáveis não é limpo e obriga ao uso de crasses. Ex: df$`Weight.(g)`. Por isso talvez seja recomendado mudar os nomes.
#criar uma versão limpa do dataframe, mantendo o original intacto
df_clean <- df %>%
  clean_names()

#verificar nomes originais
names(df)

#verificar nomes limpos. Nomes limpos são: "weight_g" "cuvier_response" "cuvier_present"
names(df_clean)

#verificar estrutura novamente
str(df_clean)


#confirmar se existe NAs nos dados: Não há NA no peso, há 162 na variável "cuvier_present" - anatómico
colSums(is.na(df_clean))

# esatistica descritiva, Como não há NA no peso, não é necessário o argumento na.rm
mean(df_clean$weight_g)
median(df_clean$weight_g)


#converter as variaveis categóricas que estavam como chr em factor
df_clean$cuvier_response <- as.factor(df_clean$cuvier_response)
df_clean$cuvier_present <- as.factor(df_clean$cuvier_present)


#verificar estrutura
str(df_clean$cuvier_response)
str(df_clean$cuvier_present)

# 2 níveis em cada, Absent e Present
levels((df_clean$cuvier_response)) 
levels(df_clean$cuvier_present)

#ver quantas amostras de absent e present em cuvier_response
table(df_clean$cuvier_response)

#ver quantas amostras de absent e present em cuvier_present. Tem menos amostras porque tem NA's
table(df_clean$cuvier_present)


#criar tabela de estatísticas para o peso global
table_statistics <- df_clean %>%
  summarise(
    n = n(),
    mean_weight = mean(weight_g, na.rm = TRUE),
    sd_weight = sd(weight_g, na.rm = TRUE),
    median_weight = median(weight_g, na.rm = TRUE),
    min_weight = min(weight_g, na.rm = TRUE),
    max_weight = max(weight_g, na.rm = TRUE),
    variancia = var(weight_g, na.rm = TRUE)
  )
table_statistics


#Criar tabela agrupada de estatísticas para o peso na variável response
tabela_response <- df_clean %>%
  group_by(cuvier_response) %>%
  summarise(
    n = n(),
    media_response = mean(weight_g, na.rm = TRUE),
    mediana_response = median(weight_g, na.rm = TRUE),
    sd_response = sd(weight_g, na.rm = TRUE),
    min_response = min(weight_g, na.rm = TRUE),
    max_response = max(weight_g, na.rm = TRUE),
    variancia = var(weight_g, na.rm = TRUE)
  )
tabela_response


#Criar tabela agrupada de estatisticas para o peso na variável response, omitindo os NA através da função filter
tabela_presence <- df_clean %>%
  filter(!is.na(cuvier_present)) %>%
  group_by(cuvier_present) %>%
  summarise(
    n = n(),
    media_presence = mean(weight_g, na.rm = TRUE),
    mediana_presence = median(weight_g, na.rm = TRUE),
    sd_presence = sd(weight_g, na.rm = TRUE),
    min_presence = min(weight_g, na.rm = TRUE),
    max_presence = max(weight_g, na.rm = TRUE),
    variancia = var(weight_g, na.rm = TRUE)
  )
tabela_presence


## Avaliação dos pressupostos para testes paramétricos
# Antes de escolher os testes estatísticos para comparar o peso entre grupos,
# avaliou-se se os dados cumprem os principais pressupostos dos testes paramétricos.
# A normalidade da distribuição do peso foi analisada visualmente através de histogramas
# e QQ plots, e formalmente através do teste de Shapiro-Wilk.
# A homogeneidade de variâncias foi avaliada visualmente através de boxplots
# e formalmente através do teste de Levene.
# Esta etapa permite decidir se é adequado usar testes paramétricos, como o t-test,
# ou se será mais apropriado recorrer a testes não paramétricos, como o Wilcoxon.

# histograma com funções do R
hist_w <- hist(df_clean$weight_g)


# histograma do peso com ggplot
hist_w_gg <- ggplot(df_clean, aes(x = weight_g)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black", na.rm = TRUE) +
  labs(
    x = "Weight (g)",
    y = "Frequência",
    title = "Weight of sea cucumbers"
  )
hist_w_gg


# criar histograma empilhado por resposta (absent e present) com ggplot
hist_response_gg_empilhado <- ggplot(df_clean, aes(x = weight_g, fill = cuvier_response)) +
  geom_histogram(binwidth = 1, color = "black", alpha = 0.7, na.rm = TRUE) +
  labs(
    x = "Weight (g)",
    y = "Frequência",
    title = "Distribuição do peso por resposta Cuvier",
    fill = "Resposta Cuvier"
  )
hist_response_gg_empilhado


# criar 2 histogramas para cada resposta com ggplot
hist_response_gg <- ggplot(df_clean, aes(x = weight_g)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black", na.rm = TRUE) +
  facet_wrap(~ cuvier_response) +
  labs(
    x = "Weight (g)",
    y = "Frequência",
    title = "Distribuição do peso por resposta Cuvier"
  )
hist_response_gg


#Histograma por cuvier_present
hist_presence_gg_empilhado <- df_clean %>%
  filter(!is.na(cuvier_present)) %>%
  ggplot(aes(x = weight_g, fill = cuvier_present)) +
  geom_histogram(binwidth = 1, color = "black", alpha = 0.7, na.rm = TRUE) +
  labs(
    x = "Weight (g)",
    y = "Frequência",
    title = "Distribuição do peso por presença anatómica de Cuvier",
    fill = "Presença Cuvier"
  )
hist_presence_gg_empilhado


#Histograma por cuvier_present separado
hist_presence_gg <- df_clean %>%
  filter(!is.na(cuvier_present)) %>%
  ggplot(aes(x = weight_g)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black", na.rm = TRUE) +
  facet_wrap(~ cuvier_present) +
  labs(
    x = "Weight (g)",
    y = "Frequência",
    title = "Distribuição do peso por presença anatómica de Cuvier"
  )
hist_presence_gg



#QQ Plot para cuvier_response. Aqui não usei o dplyr com o pipe por oção estética e para fazer o exercício. Faz mais sentido usar %>% quando quero fazer alguma operação antes do gráfico
qqplot_response <- ggplot(df_clean, aes(sample = weight_g)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ cuvier_response) +
  labs(
    title = "QQ plots do peso por resposta de Cuvier",
    x = "Quantis teóricos",
    y = "Quantis observados"
  )
qqplot_response
#No Absent há um grande desvio à normalidade.
#No present não tanto, mas ainda assim há desvio nas extremidades -> problemas nas caudas da distribuição outliers, caudas assimétricas, distribuição muito alongada

#QQ plot para cuvier_present
qqplot_present <- df_clean %>%
  filter(!is.na(cuvier_present)) %>%
  ggplot(aes(sample = weight_g)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ cuvier_present) +
  labs(
    title = "QQ plots do peso por presença anatómica de Cuvier",
    x = "Quantis teóricos",
    y = "Quantis observados"
  )
qqplot_present

# Teste de normalidade global para o peso. caracterização geral da distribuição do peso
shapiro.test(df_clean$weight_g)

# Teste de normalidade do peso por resposta de Cuvier
shapiro_response <- df_clean %>%
  group_by(cuvier_response) %>%
  summarise(
    n = n(),
    shapiro_W = shapiro.test(weight_g)$statistic,
    shapiro_p = shapiro.test(weight_g)$p.value
  )
shapiro_response


# Teste de normalidade do peso por presença anatómica de Cuvier
shapiro_presence <- df_clean %>%
  filter(!is.na(cuvier_present)) %>% #remove NA
  group_by(cuvier_present) %>%
  summarise(
    n = n(),
    shapiro_W = shapiro.test(weight_g)$statistic,
    shapiro_p = shapiro.test(weight_g)$p.value
  )
shapiro_presence


#Boxplot

# Boxplot do peso por presença anatómica de Cuvier

box_presence_gg <- df_clean %>%
  filter(!is.na(cuvier_present)) %>%
  ggplot(aes(x = cuvier_present, y = weight_g, fill = cuvier_present)) +
  geom_boxplot(na.rm = TRUE) +
  labs(
    x = "Presença anatómica de Cuvier",
    y = "Weight (g)",
    title = "Peso por presença anatómica de Cuvier",
    fill = "Presença Cuvier"
  )
box_presence_gg


#Com dados individuais - argumento jitter, onde o argumento alpha dá a transparência dos pontos, sendo 1 totalmente opacos e 0 totalmente transparentes.
box_presence_jitter_gg <- df_clean %>%
  filter(!is.na(cuvier_present)) %>%
  ggplot(aes(x = cuvier_present, y = weight_g, fill = cuvier_present)) +
  geom_boxplot(na.rm = TRUE) +
  geom_jitter(width = 0.15, alpha = 0.4, na.rm = TRUE) +
  labs(
    x = "Presença anatómica de Cuvier",
    y = "Weight (g)",
    title = "Peso por presença anatómica de Cuvier",
    fill = "Presença Cuvier"
  )

box_presence_jitter_gg


# Boxplot do peso por resposta de Cuvier
box_response_gg <- df_clean %>%
  ggplot(aes(x = cuvier_response, y = weight_g, fill = cuvier_response)) +
  geom_boxplot(na.rm = TRUE, alpha = 0.7) +
  labs(
    x = "Resposta de Cuvier",
    y = "Weight (g)",
    title = "Peso por resposta de Cuvier",
    fill = "Resposta Cuvier"
  )
box_response_gg

#Com dados individuais - argumento jitter
box_response_jitter_gg <- df_clean %>%
  ggplot(aes(x = cuvier_response, y = weight_g, fill = cuvier_response)) +
  geom_boxplot(na.rm = TRUE, alpha = 0.7) +
  geom_jitter(width = 0.15, alpha = 0.4, na.rm = TRUE) +
  labs(
    x = "Resposta de Cuvier",
    y = "Weight (g)",
    title = "Peso por resposta de Cuvier",
    fill = "Resposta Cuvier"
  )
box_response_jitter_gg

# Teste de Levene para homogeneidade de variâncias
# Peso por resposta de Cuvier

levene_response <- leveneTest(weight_g ~ cuvier_response, data = df_clean)

levene_response


# Teste de Levene para homogeneidade de variâncias
# Peso por presença anatómica de Cuvier

levene_presence <- leveneTest(
  weight_g ~ cuvier_present,
  data = df_clean %>%
    filter(!is.na(cuvier_present))
)

levene_presence

# Conclusão da avaliação dos pressupostos
# A inspeção visual dos histogramas e QQ plots, em conjunto com os testes de Shapiro-Wilk,
# indicou que a distribuição do peso não segue uma distribuição normal nos grupos analisados.
# Além disso, o teste de Levene indicou ausência de homogeneidade de variâncias
# entre os grupos, ou seja, os dados não são homocedásticos.
# Assim, os principais pressupostos para a aplicação de testes paramétricos
# não estão cumpridos.
# Por este motivo, a comparação do peso entre grupos será realizada através
# de testes não paramétricos, nomeadamente o teste de Wilcoxon/Mann-Whitney (não emparelhados)
# para comparações entre dois grupos independentes.


#teste não paramétrico para comparar as medianas da variável cuvier_response (Present e Absent)
teste_w_response <- wilcox.test(weight_g ~ cuvier_response, data = df_clean)
teste_w_response


#teste não paramétrico para comparar as medianas da variável Cuvier.Present (Present e Absent)
teste_w_presence <- wilcox.test(
  weight_g ~ cuvier_present,
  data = df_clean %>%
    filter(!is.na(cuvier_present))
)
teste_w_presence



#criando novo data frame em que filtramos os NA, removendo.
wilcox.test(
  weight_g ~ cuvier_present,
  data = df_clean %>% filter(!is.na(cuvier_present))
)

# Conclusão dos testes não paramétricos
# Foram realizados testes de Wilcoxon/Mann-Whitney para comparar o peso entre
# os grupos de resposta de Cuvier e entre os grupos de presença anatómica de Cuvier.
# Em ambos os casos, os resultados foram estatisticamente significativos (p < 0.05).
# Assim, rejeita-se a hipótese nula de que não existem diferenças no peso entre os grupos.
# Conclui-se, portanto, que o peso difere significativamente entre os indivíduos
# com resposta de Cuvier ausente e presente, e também entre os indivíduos
# com presença anatómica de Cuvier ausente e presente.
# Considerando as estatísticas descritivas e os gráficos exploratórios,
# os indivíduos com resposta/presença de Cuvier tendem a apresentar pesos inferiores,
# enquanto os indivíduos sem resposta/presença de Cuvier tendem a apresentar pesos superiores.
# Estes resultados apoiam a hipótese de que a resposta e a presença anatómica
# dos túbulos de Cuvier estão associadas ao tamanho/peso dos indivíduos.


# Escolha do modelo
# Como as variáveis resposta são binárias (Absent/Present), o objetivo da modelação
# é estimar a probabilidade de resposta ou presença de Cuvier em função do peso.
# Assim, uma regressão linear não seria adequada, porque poderia produzir valores
# fora do intervalo 0-1 e não modela diretamente probabilidades.
# Por este motivo, será utilizado um modelo linear generalizado (GLM)
# com distribuição binomial e função de ligação logit, ou seja, uma regressão logística.

#Questões:
#1. A probabilidade de resposta/libertação muda com o peso?
#2. A probabilidade de presença anatómica muda com o peso?



# Criar variáveis binárias para modelação com a função mutate e ifelse (condições)
# 0 = Absent
# 1 = Present

df_model <- df_clean %>%
  mutate(
    cuvier_response_bin = ifelse(cuvier_response == "Present", 1, 0),
    cuvier_present_bin = ifelse(cuvier_present == "Present", 1, 0)
  )
df_model


# Escolha do modelo
# Como as variáveis resposta são binárias (Absent/Present), o objetivo da modelação
# é estimar a probabilidade de resposta ou presença de Cuvier em função do peso.
# Assim, uma regressão linear não seria adequada, porque poderia produzir valores
# fora do intervalo 0-1 e não modela diretamente probabilidades.
# Por este motivo, será utilizado um modelo linear generalizado (GLM)
# com distribuição binomial e função de ligação logit, ou seja, uma regressão logística.

#Questões:
#1. A probabilidade de resposta/libertação muda com o peso?
#2. A probabilidade de presença anatómica muda com o peso?


# Modelo logístico para resposta de Cuvier
model_response <- glm(
  cuvier_response_bin ~ weight_g,
  data = df_model,
  family = binomial
)

# Coeficientes do modelo
intercept_response <- coef(model_response)[1]
slope_response <- coef(model_response)[2]

# Calcular W50: peso em que a probabilidade prevista de resposta é 50%
w50_response <- (log(0.50 / (1 - 0.50)) - intercept_response) / slope_response

# Calcular W10: peso em que a probabilidade prevista de resposta é 10%
w10_response <- (log(0.10 / (1 - 0.10)) - intercept_response) / slope_response

# Ver valores
w50_response
w10_response

#Ver caracteristicas do modelo
model_response
summary(model_response) #More info
deviance_explained_response <- 1 - model_response$deviance / model_response$null.deviance
deviance_explained_response # 0.21 ou ~21%

# Visualização da relação entre peso e resposta de Cuvier
plot_model_response <- df_model %>%
  ggplot(aes(x = weight_g, y = cuvier_response_bin)) +
  geom_jitter(height = 0.05, width = 0, alpha = 0.4) +
  stat_smooth(
    method = "glm",
    method.args = list(family = "binomial"),
    se = TRUE
  ) +
  
  # Linha vertical do W50
  geom_vline(
    xintercept = w50_response,
    color = "red",
    linetype = "dashed",
    linewidth = 1
  ) +
  
  # Linha vertical do W10
  geom_vline(
    xintercept = w10_response,
    color = "blue",
    linetype = "dotted",
    linewidth = 1
  ) +
  
  # Texto do W50
  annotate(
    "text",
    x = w50_response,
    y = 0.5,
    label = paste0("W50 = ", round(w50_response, 2), " g"),
    color = "red",
    angle = 90,
    vjust = -0.5
  ) +
  
  # Texto do W10
  annotate(
    "text",
    x = w10_response,
    y = 0.1,
    label = paste0("W10 = ", round(w10_response, 2), " g"),
    color = "blue",
    angle = 90,
    vjust = -0.5
  ) +
  
  scale_y_continuous(
    breaks = c(0, 0.1, 0.5, 1),
    labels = c("Absent", "10%", "50%", "Present")
  ) +
  
  labs(
    x = "Weight (g)",
    y = "Resposta de Cuvier",
    title = "Probabilidade de resposta de Cuvier em função do peso"
  ) +
  
  theme_classic()

plot_model_response

# Conclusão do modelo logístico para a resposta de Cuvier
# Foi ajustado um modelo linear generalizado com distribuição binomial
# para avaliar se o peso dos indivíduos influencia a probabilidade de resposta de Cuvier.
# O efeito do peso foi negativo e estatisticamente significativo
# (Estimate = -0.8332; p < 0.001), indicando que a probabilidade de resposta
# de Cuvier diminui significativamente com o aumento do peso.
# Assim, indivíduos mais leves apresentam maior probabilidade de resposta,
# enquanto indivíduos mais pesados tendem a não apresentar resposta de Cuvier.
# O modelo estimou um W50 de 2.79 g, correspondente ao peso em que a probabilidade
# prevista de resposta é de 50%.
# O W10 foi estimado em 5.43 g, indicando o peso em que a probabilidade prevista
# de resposta desce para 10%, ou seja, um ponto a partir do qual a resposta de Cuvier
# se torna pouco provável.
# Estes resultados apoiam a hipótese de que a resposta de Cuvier é dependente
# do tamanho/peso dos indivíduos, diminuindo progressivamente durante o crescimento.



# Modelo logístico para presença anatómica de Cuvier
# 0 = Absent
# 1 = Present

model_presence <- df_model %>%
  filter(!is.na(cuvier_present_bin)) %>%
  glm(
    cuvier_present_bin ~ weight_g,
    data = .,
    family = binomial
  )

summary(model_presence)
deviance_explained_presence <- 1 - model_presence$deviance / model_presence$null.deviance
deviance_explained_presence #0.36 ou ~36%

# Coeficientes do modelo
intercept_presence <- coef(model_presence)[1]
slope_presence <- coef(model_presence)[2]

# Calcular W50: peso em que a probabilidade prevista de presença anatómica é 50%
w50_presence <- (log(0.50 / (1 - 0.50)) - intercept_presence) / slope_presence

# Calcular W10: peso em que a probabilidade prevista de presença anatómica é 10%
w10_presence <- (log(0.10 / (1 - 0.10)) - intercept_presence) / slope_presence

# Ver valores
w50_presence
w10_presence

# Gráfico do modelo logístico para presença anatómica de Cuvier

plot_model_presence <- df_model %>%
  filter(!is.na(cuvier_present_bin)) %>%
  ggplot(aes(x = weight_g, y = cuvier_present_bin)) +
  geom_jitter(height = 0.05, width = 0, alpha = 0.4) +
  stat_smooth(
    method = "glm",
    method.args = list(family = "binomial"),
    se = TRUE
  ) +
  
  # Linha vertical do W50
  geom_vline(
    xintercept = w50_presence,
    color = "red",
    linetype = "dashed",
    linewidth = 1
  ) +
  
  # Linha vertical do W10
  geom_vline(
    xintercept = w10_presence,
    color = "blue",
    linetype = "dotted",
    linewidth = 1
  ) +
  
  # Texto do W50
  annotate(
    "text",
    x = w50_presence,
    y = 0.5,
    label = paste0("W50 = ", round(w50_presence, 2), " g"),
    color = "red",
    angle = 90,
    vjust = -0.5
  ) +
  
  # Texto do W10
  annotate(
    "text",
    x = w10_presence,
    y = 0.1,
    label = paste0("W10 = ", round(w10_presence, 2), " g"),
    color = "blue",
    angle = 90,
    vjust = -0.5
  ) +
  
  scale_y_continuous(
    breaks = c(0, 0.1, 0.5, 1),
    labels = c("Absent", "10%", "50%", "Present")
  ) +
  
  labs(
    x = "Weight (g)",
    y = "Presença anatómica de Cuvier",
    title = "Probabilidade de presença anatómica de Cuvier em função do peso"
  ) +
  
  theme_classic()

plot_model_presence

# Conclusão do modelo logístico para a presença anatómica de Cuvier
# Foi ajustado um modelo linear generalizado com distribuição binomial
# para avaliar se o peso dos indivíduos influencia a probabilidade de presença
# anatómica de túbulos de Cuvier.
# O efeito do peso foi negativo e estatisticamente significativo
# (Estimate = -0.9084; p < 0.001), indicando que a probabilidade de presença
# anatómica de Cuvier diminui significativamente com o aumento do peso.
# Assim, indivíduos mais leves apresentam maior probabilidade de possuir
# anatomicamente túbulos de Cuvier, enquanto indivíduos mais pesados tendem
# a não os apresentar.
# O modelo estimou um W50 de aproximadamente 4.17 g, correspondente ao peso
# em que a probabilidade prevista de presença anatómica é de 50%.
# O W10 foi estimado em aproximadamente 6.59 g, indicando o peso em que
# a probabilidade prevista de presença anatómica desce para 10%.
# Estes resultados sugerem que a presença anatómica dos túbulos de Cuvier
# diminui progressivamente com o crescimento dos indivíduos, tornando-se
# pouco provável em indivíduos de maior peso.

# Comparação entre os modelos logísticos de resposta e presença anatómica de Cuvier
# Ambos os modelos indicaram um efeito negativo e estatisticamente significativo do peso,
# mostrando que a probabilidade de resposta de Cuvier e a probabilidade de presença
# anatómica dos túbulos diminuem com o aumento do peso dos indivíduos.
# No entanto, os limiares estimados diferiram entre os dois processos.
# Para a resposta de Cuvier, o W50 foi estimado em 2.79 g e o W10 em 5.43 g.
# Para a presença anatómica de Cuvier, o W50 foi estimado em aproximadamente 4.17 g
# e o W10 em aproximadamente 6.59 g.
# Assim, a perda da resposta funcional ocorre a pesos mais baixos do que a perda
# da presença anatómica dos túbulos.
# Estes resultados sugerem que os indivíduos deixam primeiro de libertar/responder
# com túbulos de Cuvier e só posteriormente deixam de os apresentar anatomicamente.
# Esta diferença entre resposta funcional e presença anatómica apoia a hipótese
# de uma transição ontogenética progressiva, na qual a função defensiva dos túbulos
# diminui antes da sua perda anatómica completa.

# O peso tem poder explicativo nos dois processos,
# mas explica melhor a presença anatómica do que a resposta funcional.
# Isto faz sentido biologicamente, porque a presença anatómica
# pode estar mais diretamente ligada ao tamanho/peso do animal,
# enquanto a resposta funcional pode depender também de outros fatores,
# como condição individual, comportamento, estímulo, energia disponível ou
# variabilidade experimental.
