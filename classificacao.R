# 1. Preparação do problema
# a) Carga de pacotes

library(ggplot2) # visualização dos dados 
library(dplyr) # Usado para as funções de filtro e tratamento de dados
library(reshape2) # Converter a matriz de correlação em um data frame
library(caret) # Usada para usar a função createDataPartition (partição dos dados)
library(corrplot) # Visualizar as variaveis correlacionadas
library(C50) # pacote para arvore de binaria (classificação)
library(class)# pacote para modelo KNN 
library(e1071) # pacote para modelo  naiveBayes
library(kernlab) # pacote para modelo  SVM
library(mlbench) # pacote para modelo  SVM

install.packages("class")
install.packages("corrplot")
install.packages("caret")
install.packages("C50")
install.packages("e1071")
install.packages("kernlab")
install.packages("mlbench")


# b) Carga de dataset
tabela = read.table("Student_performance_data.csv", sep = ";", dec = ",", header = TRUE)

# Hipotese
# "Qual será a classificação de nota (GradeClass) de um aluno novo, dado seu perfil (por exemplo, idade, tempo de estudo semanal, faltas, etc.)?"

# Resumo estatisticos dos dados
summary(tabela)

#Atraves do resumo estatistico é possivel perceber que:

# idade: apesar de os alunos estarem entre 15 e 18 anos a média é 16 
# A média do nivel de educação dos pais é 1.74. Isso significa que todos possui ensino médio. Além disso,
# a mediana é 2 que significa que os pais passuem tambem alguma faculdade. é possivel perceber tbm que 
# mais de 75% possui alguma faculdade, pois o nivel 2 esta concentrado no terceiro quartil
# As variaveis  StudyTimeWeekly, GPA e  GradeClass possuem valores muito discrepantes. O que pode ser 
# necessário uma normalização das variaveis
# A variavel Absences possui muitos numeros aleatorios. Isso poderia ser resolvido com
# uma escala de notas. É possível perceber tbm que a média de faltas é muita alta. Pois,
# 75% dos alunos tiveram 22 faltas


# Para entender melhor a distribuição dos dados. 

# 1) Identificar as variaveis numericas e categoricas na base de dados

str(tabela)

# É possivel perceber que as variaveis: StudyTimeWeekly, GPA e GradeClass são do tipo
# caracter e as demais são do tipo inteiros

# 2) Transformar as variaveis de caracteres para numericos. 


# 2.1) StudyTimeWeekly
# Verificando valores únicos para entender melhor a natureza dos dados e identificar
# padrões de formatação ou possíveis erros nos dados. Utilizaremos a função unique
unique(tabela$StudyTimeWeekly)

# Limpeza dos dados
# Remover pontos e transformar para numericos 
tabela$StudyTimeWeekly <- gsub("\\.", "", tabela$StudyTimeWeekly) # Remove os pontos
tabela$StudyTimeWeekly <- as.numeric(tabela$StudyTimeWeekly) # Converte para numérico

# Verificar e validar os dados
# Verificar se a conversão foi bem sucedida
summary(tabela$StudyTimeWeekly)  # # Resumo estatístico da variavel

# Observando o resultado é possivel observar que os valores são extremamente grandes.
# Sendo assim, será necessario fazer uma normalização para ajustar os valores para
# um intervalo especifico. Neste caso será feito um Escalamento Logarítmico pois os dados
# seguem uma distribuição exponencial que pode ser util para reduzir a dispersão

# Aplicar escalamento logarítmico à coluna StudyTimeWeekly
tabela$StudyTimeWeekly_log_scaled <- log(tabela$StudyTimeWeekly)

# Visualizando o boxplot da variavel StudyTimeWeekly

ggplot(tabela, aes(y = StudyTimeWeekly_log_scaled)) +
  geom_boxplot() +
  labs(title = "Boxplot de StudyTimeWeekly", y = "StudyTimeWeekly") +
  theme(plot.title = element_text(size = 20),  # Tamanho do título
        axis.title.y = element_text(size = 15))  # Tamanho do rótulo do eixo y


# 2.2) GPA (será feito o mesmo procedimento na variavel StudyTimeWeekly)
# Verificando valores únicos 
unique(tabela$GPA)

# Limpeza dos dados
# Remover pontos e transformar para numericos 
tabela$GPA <- gsub("\\.", "", tabela$GPA) # Remove os pontos
tabela$GPA <- as.numeric(tabela$GPA) # Converte para numérico

# Verificar e validar os dados
# Verificar se a conversão foi bem sucedida
summary(tabela$GPA)  # Resumo estatístico dos dados

# Aplicar escalamento logarítmico à coluna GPA
tabela$GPA_log_scaled <- log(tabela$GPA)

# Visualizando os valores escalados com boxplot
# Visualizando o boxplot da variavel GPA

ggplot(tabela, aes(y = GPA_log_scaled)) +
  geom_boxplot() +
  labs(title = "Boxplot de GPA", y = "GPA") +
  theme(plot.title = element_text(size = 20),  # Tamanho do título
        axis.title.y = element_text(size = 15))  # Tamanho do rótulo do eixo y
+ options(repr.plot.width = 8, repr.plot.height = 6)

# Verificar resumo dos valores de GPA
summary(tabela$GPA_log_scaled)
# É possível perceber que a média apresentou valor -inf. Isso significa que
# a presença desses valores afetou o calculo da media

# Resolver o Problema com Valores -Inf
# Identificar os índices onde os valores são -Inf
indices_inf <- which(is.infinite(tabela$GPA_log_scaled))

# Exibir os índices ou valores problemáticos
tabela$GPA_log_scaled[indices_inf]

# Filtrando os valores -Inf
tabela_inf <- tabela %>%
  filter(is.infinite(GPA_log_scaled))

# Exibir o resultado
print(tabela_inf)

# É possivel perceber que somente 16 linhas contem o valor -Inf. Sendo assim,
# será necessario excluir esses valores. 

# Remover valores -Inf na coluna GPA_log_scaled
tabela_filtrada <- tabela %>%
  filter(!is.infinite(GPA_log_scaled))

#removendo a coluna GPA
nova_tabela <- tabela_filtrada %>%
  select(-GPA)

#removendo a coluna StudyTimeWeekly
nova_tabela <- nova_tabela %>%
  select(-StudyTimeWeekly)

# Renomeando variavel GPA_log_scaled
nova_tabela <- nova_tabela %>%
  rename(GPA = GPA_log_scaled)


# Renomeando nova_tabela
# Renomeando variavel GPA_log_scaled
nova_tabela <- nova_tabela %>%
  rename(StudyTimeWeekly = StudyTimeWeekly_log_scaled)


# Como não vamos precisar do ID dos estudantes, também será removido da base de dados
#removendo a coluna StudentID
nova_tabela <- nova_tabela %>%
  select(-StudentID)


# 2.3) GradeClass
# Convertendo a coluna GradeClass para formato numérico
nova_tabela$GradeClass <- as.numeric(nova_tabela$GradeClass)

# Verificar quantos valores foram convertidos para NA
sum(is.na(nova_tabela$GradeClass))

# Verificar estatísticas descritivas
summary(nova_tabela$GradeClass)

# 3) Análise de Correlação

# Verificar as variaveis correlacionada
correlation_matrix <- cor(nova_tabela)

# Converter a matriz de correlação em um data frame longo (formato tidy)
melted_correlation_matrix <- melt(correlation_matrix)

# Transformar em formato de matriz (PARA VISUALIZAR O image)
cor_matrix <- as.matrix(correlation_matrix)

# VISUALIZAR O image
image(t(cor_matrix), col = colorRampPalette(c("blue", "white", "red"))(100),
      axes = TRUE, main = "Matriz de Correlação", xlab = "Variáveis", ylab = "Variáveis")

# Transformar em formato de dados (PARA VISUALIZAR NA BIBLIOTECA ggplot)
melted_cor_matrix <- melt(cor_matrix)

# VISUALIZAR O ggplot2

library(ggplot2)
ggplot(data = melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0,
                       limit = c(-1, 1), name = "Correlação") +
  labs(title = "Matriz de Correlação") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, margin = margin(t = 0.6, r = 0, b = 0, l = 0)),
        axis.text.y = element_text(margin = margin(t = 0.6, r = 0, b = 0, l = 0))) +
  coord_fixed()

# Visualizar as variaveis correlacionadas usando a biblioteca corrplot
library(corrplot)
correlations <- cor(nova_tabela[,1:14])
corrplot(correlations, method="circle",  add = FALSE)

# Identificar as variaveis mais relacionadas
# - Como podemos observar no gráfico acima, podemos observar que a variavel 
# Absences que representa o número de faltas é a variavel mais correlacionada com o GPA do que outras variaveis


# 3) Divisão dos dados
# 3.1) Dividir os dados em dois conjunto: treino e teste

# Definindo a proporção de divisão
set.seed(123)  # Definindo uma seed para reprodutibilidade
proporcao_treino <- 0.8

# Criando uma partição estratégica
indice_treino <- createDataPartition(nova_tabela$GradeClass, p = proporcao_treino, list = FALSE)

# Criando conjuntos de treinamento e teste
dados_treino <- nova_tabela[indice_treino, ]
dados_teste <- nova_tabela[-indice_treino, ]

# Nessa próxima etapa é importante utilizar mais de um modelo de classificação para fazer a comparação. Sendo assim, foi escolhido
# os seguintes modelos: Arvore de classificação,KNN (K-Nearest Neighbours), Bayes  (Naïve Bayes), SVM  (Support Vector Machine)

# 3.2) Arvore de classificação

# Separar as variáveis preditoras e a variável de resposta

# Identificar o índice da coluna de resposta 
response_col_index <- which(colnames(nova_tabela) == "GradeClass")

# Separar as variáveis preditoras e a variável de resposta para os dados de treino
train_target <- dados_treino[, response_col_index] # Variável de resposta
train_features <- dados_treino[, -response_col_index] # Variáveis preditoras

# Separar as variáveis preditoras e a variável de resposta para os dados de teste
test_target <- dados_teste[, response_col_index] # Variável de resposta
test_features <- dados_teste[, -response_col_index] # Variáveis preditoras

# No R, um fator é usado para representar variáveis categóricas. Sendo assim, é necessario
# garantir que a variável de resposta seja tratada como um fator. 

# Convertendo a variável de resposta para fator
train_target <- factor(dados_treino[, response_col_index])
test_target <- factor(dados_teste[, response_col_index])

# Criar o modelo
c50_model <- C5.0(train_features, train_target)

# Fazer previsões nos dados de teste
predictions_c50 <- predict(c50_model, test_features)

# Avaliar o desempenho
confusion_matrix_c50 <- table(predictions_c50, test_target)
print(confusion_matrix_c50)

accuracy_c50 <- sum(diag(confusion_matrix_c50)) / sum(confusion_matrix_c50)
print(paste("Acurácia arvore:", accuracy_c50 * 100))


# 3.3) KNN (K-Nearest Neighbours)
# Separar as características (features) e o alvo (target)
train_x <- dados_treino[, -response_col_index]  # Todas as colunas exceto GradeClass
train_y <- dados_treino$GradeClass  # Apenas a coluna GradeClass
test_x <- dados_teste[, -response_col_index]  # Todas as colunas exceto GradeClass
test_y <- dados_teste$GradeClass  # Apenas a coluna GradeClass


# Ajustar o modelo KNN
k <- 5
predict_knn <- knn(train = train_x, test = test_x, cl = train_y, k = k)


# Avaliar o modelo
confusion_matrix_knn <- table(Predicted = predict_knn, Actual = test_y)
accuracy_knn <- sum(diag(confusion_matrix_knn)) / sum(confusion_matrix_knn)
print(confusion_matrix_knn)
print(paste("Acurácia KNN:", accuracy_knn * 100))

# 3.4) Bayes  (Naïve Bayes)

# Ajustar o modelo Naive Bayes
model <- naiveBayes(GradeClass ~ ., data = dados_treino)

# Fazer previsões
predictions_bayes <- predict(model, dados_teste)

# Avaliar o modelo
confusion_matrix_bayes <- table(dados_teste$GradeClass, predictions_bayes)
accuracy_bayes <- sum(diag(confusion_matrix_bayes)) / sum(confusion_matrix_bayes)
print(confusion_matrix_bayes)
print(paste("Acurácia Naïve Bayes:", accuracy_bayes * 100))


# 3.5) SVM  (Support Vector Machine)

# Treinamento do Modelo SVM
svm_model <- ksvm(GradeClass ~ ., data = dados_treino, kernel = "rbfdot")

# Predição com o Modelo Treinado
predictions_svm <- predict(svm_model, dados_teste, type = "response")

# Avaliação do Modelo
confusion_matrix_svm <- table(Predicted = predictions_svm, Actual = dados_teste$GradeClass)
accuracy_svm <- sum(diag(confusion_matrix_svm)) / sum(confusion_matrix_svm)
print(confusion_matrix_svm)
print(paste("Acurácia modelo SVM:", accuracy_svm * 100))



# Proximos passos
# Comparar os modelos de todos os modelos e dizer qual é o melhor. Para comparar os modelos use histograma































