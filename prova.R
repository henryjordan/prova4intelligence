# Projeto de análise referente ao processo seletivo na 4intelligence

# Etapa 1: Preparação

# Denominando o diretório dos arquivos
setwd("~/DataScience/4intelligence")
getwd()


# Instalando pacotes (se necessário)
install.packages("tidyr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("xlsx")
install.packages("reshape2")
install.packages("scales")

# Carregando os pacotes
library(tidyr)
library(dplyr)
library(ggplot2)
library(xlsx)
library(reshape2)
library(scales)

dados_base <- read.xlsx("Bases_Final_ADS_Jun2021.xlsx",sheetName="dados")

# Etapa 2: Pré-Processamento

summary(dados_base)

# Como há missing values nas colunas massa_r e renda_r, iremos preenchê-las
# com os valores de suas respectivas médias

dados_base <- dados_base %>% replace_na(list(renda_r = mean(dados_base$renda_r, na.rm = TRUE),
                               massa_r = mean(mean(dados_base$massa_r, na.rm = TRUE))))

# Agora precisamos separar os dados_base em dados e dados futuros

dados <- dados_base[1:206,]

# Etapa 2: Análise Descritiva

# Separando os dados das datas e criando um array com os nomes das regiões
regiao <- c('Centro-Oeste','Norte','Nordeste','Sul','Sudeste')
datas <- dados[,1]

# Criando subsets para análise de consumos de energia comercial, industrial e residencial

comercio <- subset(dados[,2:6])
colnames(comercio) <- regiao
comercio$categoria <- "Comércio"
comercio$data <- datas

industria <- subset(dados[,7:11])
colnames(industria) <- regiao
industria$categoria <- "Indústria"
industria$data <- datas

residencial <- subset(dados[,12:16])
colnames(residencial) <- regiao
residencial$categoria <- "Residencial"
residencial$data <- datas

dados_consumo <- do.call("rbind", list(comercio, industria, residencial))

# Histograma do Consumo de Energia Comercial por Região
reshape2::melt(dados_consumo[,1:6]) %>% filter(categoria == "Comércio") %>%
  ggplot(aes(x=value)) + facet_wrap( ~ variable, scales="free") +
  xlab('Consumo de Energia (Gwh)') + ylab('Número de Ocorrências') + ggtitle('Histograma de Consumo de Energia Comercial por Região') +
  theme(plot.title = element_text(hjust = 0.5)) + geom_histogram(aes(y=..density..),fill='white',colour='black') +
  geom_density(alpha = .2, fill="#FF6655")

# Histograma do Consumo de Energia Industrial por Região
reshape2::melt(dados_consumo[,1:6]) %>% filter(categoria == "Indústria") %>%
  ggplot(aes(x=value)) + facet_wrap( ~ variable, scales="free") +
  xlab('Consumo de Energia (Gwh)') + ylab('Número de Ocorrências') + ggtitle('Histograma de Consumo de Energia Industrial por Região') +
  theme(plot.title = element_text(hjust = 0.5)) + geom_histogram(aes(y=..density..),fill='white',colour='black') +
  geom_density(alpha = .2, fill="#FF6655")

# Histograma do Consumo de Energia Residencial por Região
reshape2::melt(dados_consumo[,1:6]) %>% filter(categoria == "Residencial") %>%
  ggplot(aes(x=value)) + facet_wrap( ~ variable, scales="free") +
  xlab('Consumo de Energia (Gwh)') + ylab('Número de Ocorrências') + ggtitle('Histograma de Consumo de Energia Residencial por Região') +
  theme(plot.title = element_text(hjust = 0.5)) + geom_histogram(aes(y=..density..),fill='white',colour='black') +
  geom_density(alpha = .2, fill="#FF6655")

# Boxplot do Consumo de Energia Comercial por Região
reshape2::melt(dados_consumo[,1:6]) %>% filter(categoria == "Comércio") %>%
  ggplot(aes(x=variable, y=value, color=variable)) + facet_wrap( ~ variable, scales="free") +
  xlab('Região') + ylab('Consumo de Energia (Gwh)') + ggtitle('Consumo de Energia Comercial por Região (01/2004-02/2021)') +
  theme(plot.title = element_text(hjust = 0.5)) + geom_boxplot() + scale_color_discrete(name="Legenda")

# Boxplot do Consumo de Energia Industrial por Região
reshape2::melt(dados_consumo[,1:6]) %>% filter(categoria == "Indústria") %>%
  ggplot(aes(x=variable, y=value, color=variable)) + facet_wrap( ~ variable, scales="free") +
  xlab('Região') + ylab('Consumo de Energia (Gwh)') + ggtitle('Consumo de Energia Industrial por Região (01/2004-02/2021)') +
  theme(plot.title = element_text(hjust = 0.5)) + geom_boxplot() + scale_color_discrete(name="Legenda")

# Boxplot do Consumo de Energia Residencial por Região
reshape2::melt(dados_consumo[,1:6]) %>% filter(categoria == "Residencial") %>%
  ggplot(aes(x=variable, y=value, color=variable)) + facet_wrap( ~ variable, scales="free") +
  xlab('Região') + ylab('Consumo de Energia (Gwh)') + ggtitle('Consumo de Energia Residencial por Região (01/2004-02/2021)') +
  theme(plot.title = element_text(hjust = 0.5)) + geom_boxplot() + scale_color_discrete(name="Legenda")

# Série Temporal de Consumo de Energia Comercial por Região
reshape2::melt(comercio[,-6],id="data") %>% ggplot(aes(x=data, y=value)) + geom_line(aes(colour=variable, group=variable)) +
  geom_point(aes(colour=variable),size=2) + scale_x_date(date_labels = "%b/%Y") +
  xlab('Mês/Ano') + ylab('Consumo de Energia (Gwh)') + ggtitle('Energia Comercial Consumida por Região (01/2004-02/2021)') +
  theme(plot.title = element_text(hjust = 0.5)) + scale_color_discrete(name="Região")

# Série Temporal de Consumo de Energia Industrial por Região
reshape2::melt(industria[,-6],id="data") %>% ggplot(aes(x=data, y=value)) + geom_line(aes(colour=variable, group=variable)) +
  geom_point(aes(colour=variable),size=2) + scale_x_date(date_labels = "%b/%Y") +
  xlab('Mês/Ano') + ylab('Consumo de Energia (Gwh)') + ggtitle('Energia Industrial Consumida por Região (01/2004-02/2021)') +
  theme(plot.title = element_text(hjust = 0.5)) + scale_color_discrete(name="Região")

# Série Temporal de Consumo de Energia Residencial por Região
reshape2::melt(residencial[,-6],id="data") %>% ggplot(aes(x=data, y=value)) + geom_line(aes(colour=variable, group=variable)) +
  geom_point(aes(colour=variable),size=2) + scale_x_date(date_labels = "%b/%Y") +
  xlab('Mês/Ano') + ylab('Consumo de Energia (Gwh)') + ggtitle('Energia Residencial Consumida por Região (01/2004-02/2021)') +
  theme(plot.title = element_text(hjust = 0.5)) + scale_color_discrete(name="Região")

# Correlação entre as variáveis

library(ggcorrplot)
corr <- cor(dados[,2:16])
ggcorrplot(corr, hc.order = FALSE, type = "lower", lab = TRUE) +
  ggtitle('Matriz de Correlação entre as Variáveis de Consumo de Energia') +
  theme(plot.title = element_text(hjust = 0.5))

# Etapa 3: Elaborando os modelos de machine learning

library(caret)
library(randomForest)
library(e1071)
library(xgboost)

dados$data_tidy <- NULL

sample1 <- sample(1:nrow(dados), 165)
sample2 <- sample(166:nrow(dados), 41)
dados_treino <- dados[sample1,]
dados_teste <- dados[sample2,]

# Modelo Inicial

modelo_v1 <- train(ind_se ~ ., data = dados_treino, method = 'lm')
varImp(modelo_v1)
plot(varImp(modelo_v1))

# Pode-se perceber que as variáveis mais importantes para o modelo com a variável
# "ind_se" são "pim_se", "ind_s", "ind_ne" "com_se"

# Modelo Regressão Linear

modelo_v1 <- train(log(ind_se) ~ log(com_se) + log(ind_s) + log(pim_se) + log(ind_ne),
                   data = dados_treino, method = 'lm')
summary(modelo_v1)

previsao <- predict(modelo_v1, dados_teste)
previsao <- round(previsao,1)
mean(previsao==round(log(dados_teste$ind_se),1))

# Modelo randomForest

modelo_v2 <- randomForest(ind_se ~ com_se + ind_s + pim_se + ind_ne, data = dados_treino)

previsao2 <- predict(modelo_v2,dados_teste)
RMSE(previsao2,dados_teste$ind_se)

# Modelo GradientBoost

set.seed(123)
modelo_v3 <- train(ind_se ~ com_se + ind_s + ind_ne + pim_se, data = dados_treino, method = "xgbTree",
                 trControl = trainControl("cv", number = 10),
                 objective = "reg:squarederror", verbose=FALSE)

previsao3 <- predict(modelo_v3,dados_teste)
RMSE(previsao3, dados_teste$ind_se)

# Modelo GLM

modelo_v4 <- train(log(ind_se) ~ log(com_se) + log(ind_s) + log(pim_se) + log(ind_ne),
                   data = dados_treino, method = 'glm')

previsao4 <- predict(modelo_v4, dados_teste)
previsao4 <- round(previsao4,1)
mean(previsao4==round(log(dados_teste$ind_se),1))

# Modelo SVM

modelo_v5 <- svm(log(ind_se) ~ log(com_se) + log(ind_s) + log(pim_se) + log(ind_ne),
                     data = dados_treino,
                     type = 'eps-regression',
                     kernel = 'linear')

pred_test <- predict(modelo_v5, dados_teste)
mean(round(pred_test,1) == round(log(dados_teste$ind_se),1))
