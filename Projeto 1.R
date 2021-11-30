# Objetivo: determinar o melhor modelo de previsão

# Passo 1: baixar e visualizar os dados

carro <- read.csv("CarPrice_Assignment.csv")

View(carro)

# Passo 2: realizar o Data Wrangling

library(tidyverse)

carro <- carro[, -1]

vetor <- c(1, 2, 3, 4, 5, 6, 7, 8, 14, 15, 17)

carro[vetor] <- lapply(carro[vetor], factor)

sapply(carro, class)

carro <- carro %>% rename(seguro = "symboling",
                          nome = "CarName",
                          tipo_combustivel = "fueltype",
                          aspiracao = "aspiration",
                          numero_portas = "doornumber",
                          formato_carro = "carbody",
                          direcao = "drivewheel",
                          localizacao_motor = "enginelocation",
                          distancia_eixos = "wheelbase",
                          comprimento = "carlength",
                          largura = "carwidth",
                          altura = "carheight",
                          peso = "curbweight",
                          tipo_motor = "enginetype",
                          numero_cilindro = "cylindernumber",
                          tamanho_motor = "enginesize",
                          sistema_combustivel = "fuelsystem",
                          razao_pistao = "boreratio",
                          volume_motor = "stroke",
                          razao_compressao = "compressionratio",
                          potencia = "horsepower",
                          maximo_rpm = "peakrpm",
                          consumo_cidade = "citympg",
                          consumo_rodovia = "highwaympg",
                          preco = "price")

lapply(carro[vetor], unique)

carro <- carro %>% mutate(aspiracao = recode(aspiracao,
                                             "std" = "padrao"),
                          numero_portas = recode(numero_portas,
                                                 "two" = 2,
                                                 "four" = 4),
                          formato_carro = recode(formato_carro,
                                                 "convertible" = "conversivel"),
                          localizacao_motor = recode(localizacao_motor,
                                                     "front" = "frente",
                                                     "rear" = "atras"),
                          numero_cilindro = recode(numero_cilindro,
                                                   "four"= 4,
                                                   "six" = 6,
                                                   "five" = 5,
                                                   "three" = 3,
                                                   "twelve" = 12,
                                                   "two" = 2,
                                                   "eight" = 8))
carro$numero_portas <- as.integer(carro$numero_portas)

carro$numero_cilindro <- as.integer(carro$numero_cilindro)

carro[!complete.cases(carro),]

carro <- carro %>% relocate(nome, .before = seguro)

# Passo 3: regressão linear simples (transformar variáveis qualitativas em
# dummies)

library(fastDummies)

sapply(carro, class)

carro_dummies <- carro %>% dummy_columns(select_columns = c("seguro",
                                                            "tipo_combustivel",
                                                            "aspiracao",
                                                            "formato_carro",
                                                            "direcao",
                                                            "localizacao_motor",
                                                            "tipo_motor",
                                                            "sistema_combustivel"),
                                         remove_selected_columns = T,
                                         remove_most_frequent_dummy = T)

View(carro_dummies)

modelo_simples <- lm(formula = preco ~ . - nome,
                     carro_dummies)

summary(modelo_simples)

modelo_simples_AIC <- extractAIC(modelo_simples)[2]

modelo_simples_AIC

# Passo 4: regressão linear simples com Stepwise

modelo_simples_step <- step(modelo_simples,
                            k = qchisq(0.05,
                                       df = 1,
                                       lower.tail = F))

summary(modelo_simples_step)

modelo_simples_step_AIC <- extractAIC(modelo_simples_step)[2]

modelo_simples_step_AIC

# Passo 5: testar aderência à normalidade dos resíduos dos dois modelos
# anteriores (todos rejeitam a hipótese nula, indicando a não-aderência)

library(nortest)

sf.test(modelo_simples$residuals)

sf.test(modelo_simples_step$residuals)

# Passo 6: regressão não-linear (transformação Box-Cox na variável dependente)

library(car)

lambda <- powerTransform(carro_dummies$preco)
lambda <- lambda$lambda

carro_nao_linear <- carro_dummies %>% mutate(preco = ((preco ^ lambda) - 1)/
                                               lambda) 

View(carro_nao_linear)

modelo_nao_linear <- lm(formula = preco ~ . - nome,
                        carro_nao_linear)

summary(modelo_nao_linear)

modelo_nao_linear_AIC <- extractAIC(modelo_nao_linear)[2]

modelo_nao_linear_AIC

# Passo 7: regressão não-linear com Stepwise

modelo_nao_linear_step <- step(modelo_nao_linear,
                               k = qchisq(0.05,
                                          df = 1,
                                          lower.tail = F))

summary(modelo_nao_linear_step)

modelo_nao_linear_step_AIC <- extractAIC(modelo_nao_linear_step)[2]

modelo_nao_linear_step_AIC

# Passo 8: testar aderência à normalidade dos resíduos dos dois modelos
# anteriores (todos rejeitam a hipótese nula, indicando a não-aderência)

sf.test(modelo_nao_linear$residuals)

sf.test(modelo_nao_linear_step$residuals)

# Passo 9: realizar análise de cluster hierárquica para a criação de regressão
# linear multinível

carro_padronizado <- scale(carro_dummies[, -1])

View(carro_padronizado)

distancia <- dist(carro_padronizado, method = "euclidean")

cluster <- hclust(distancia, method = "ward.D")

library(dendextend)

grupo <- cutree(cluster, k = 10)

carro_hierarquico <- bind_cols(carro_dummies, grupo)

colnames(carro_hierarquico)[45] <- "grupo"

View(carro_hierarquico)

# Passo 10: regressão linear multinível (realização passo-a-passo)

library(nlme)

modelo_multinivel <- lme(fixed = preco ~ 1,
                         random = ~ 1 | grupo,
                         data = carro_hierarquico,
                         method = "REML")

summary(modelo_multinivel)

modelo_multinivel <- lme(fixed = preco ~ . - nome - grupo,
                         random = ~ 1 | grupo,
                         data = carro_hierarquico,
                         method = "REML")

summary(modelo_multinivel)

modelo_multinivel <- lme(fixed = preco ~ . - nome - grupo,
                         random = ~ . - nome - grupo | grupo,
                         data = carro_hierarquico,
                         method = "REML")

summary(modelo_multinivel)

modelo_multinivel_AIC <- AIC(modelo_multinivel)

modelo_multinivel_AIC

# Passo 11: testar aderência à normalidade dos resíduos do modelo anterior
# (rejeitada a hipótese nula, indicando a não-aderência)

sf.test(modelo_multinivel$residuals)

# Passo 12: regressão não-linear multinível (transformação Box-Cox na variável
# dependente)

carro_hierarquico_nao_linear <- bind_cols(carro_nao_linear, grupo)

colnames(carro_hierarquico_nao_linear)[45] <- "grupo"

modelo_multinivel_nao_linear <- lme(fixed = preco ~ 1,
                                    random = ~ 1 | grupo,
                                    data = carro_hierarquico_nao_linear,
                                    method = "REML")

summary(modelo_multinivel_nao_linear)

modelo_multinivel_nao_linear <- lme(fixed = preco ~ . - nome - grupo,
                                    random = ~ 1 | grupo,
                                    data = carro_hierarquico_nao_linear,
                                    method = "REML")

summary(modelo_multinivel_nao_linear)

modelo_multinivel_nao_linear <- lme(fixed = preco ~ . - nome - grupo,
                                    random = ~ . -nome - grupo | grupo,
                                    data = carro_hierarquico_nao_linear,
                                    method = "REML")

summary(modelo_multinivel_nao_linear)

modelo_multinivel_nao_linear_AIC <- AIC(modelo_multinivel_nao_linear)

modelo_multinivel_nao_linear_AIC

# Passo 13: relacionando AICs e identificando melhor modelo

aic <- data.frame(nome = c("modelo_simples",
                           "modelo_simples_step",
                           "modelo_nao_linear",
                           "modelo_nao_linear_step",
                           "modelo_multinivel",
                           "modelo_multinivel_nao_linear"),
                  aic = c(modelo_simples_AIC,
                          modelo_simples_step_AIC,
                          modelo_nao_linear_AIC,
                          modelo_nao_linear_step_AIC,
                          modelo_multinivel_AIC,
                          modelo_multinivel_nao_linear_AIC))

aic <- aic[order(aic$aic),]

View(aic)

library(ggplot2)

aic %>% 
  ggplot(aes(y = aic,
             color = nome,
             fill = nome)) +
  geom_bar(aes(x = reorder(nome, aic)),
           stat = "identity") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# Identifica-se que o modelo não linear com Stepwise é o melhor modelo
# preditivo

predicao <- ((modelo_nao_linear_step$fitted.values * lambda) + 1) ^
  (1 / lambda)

carro_predicao <- bind_cols(carro, predicao)

colnames(carro_predicao)[26] <- "predicao"

View(carro_predicao)

carro_predicao %>% 
  ggplot() +
  geom_point(aes(x = 1:dim(carro_predicao)[1],
                 y = preco),
             color = "red",
             size = 2) +
  geom_line(aes(x = 1:dim(carro_predicao)[1],
                y = predicao),
            color = "black",
            size = 1) +
  labs(x = "carro",
       y = "preco/predicao") +
  theme_bw()
