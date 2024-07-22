# 1. Preparação do problema
# a) Carga de pacotes

library(ggplot2)
# b) Carga de dataset
tabela = read.table("Student_performance_data.csv", sep = ";", dec = ",", header = TRUE)
# c) Divisão do dataset em conjunto de treino e teste (validação final)

# 2. Analise exploratoria dos dados
# a) Estatistica Descritiva
# Para entender melhor a distribuição de cada variavel no data frame utilizo a função summary()
summary(tabela)
# Analisando os resultados é possivel perceber que:
  # Menos de 25% dos alunos tem 15 anos
  
  
# b) Visualizações dos dados
# Para visualizar melhor os dados utilizarei o boxplots, um tipo de gráfico usado para 
# avaliar a distribuição empírica dos dados.

p <- ggplot(tabela, aes(x=factor(Gender), y=Age, fill=factor(Gender))) + 
  geom_boxplot(alpha=0.3) +
  theme(legend.position="none") +
  labs(x="Sexo (0 = Masculino, 1 = Feminino)", y="Idade")


# 3. Pré-processamento
# a) Limpeza de Dados
# b) Seleção de atributos
# c) Transformação de Dados

# 4. Modelagem e Inferencia
# a) Escolha de procedimentos e métricas de avaliação
# b) Criação de algoritimos
# c) Comparação de Algoritimos
# d) Melhoria de desempenho

# 5. Pós-processamento
# a) Escolha e construção do modelo final com todo o conjunto de treinamento
# b) Predições no conjunto de teste (validação final)
# c) Salvamento do modelo para uso superior




