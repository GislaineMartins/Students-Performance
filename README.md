# Students-Performance

## Descrição do Projeto de Análise de Dados

### Título: Análise e Classificação de Dados Students Performance

### Introdução
Este projeto foi desenvolvido com o objetivo de responder à pergunta Qual será a classificação de nota (GradeClass) de um aluno novo, dado seu perfil (por exemplo, idade, tempo de estudo semanal, faltas, etc.)? Utilizando um modelo de classificação. Os dados foram retirados da plataforma Kaggle, um repositório popular de datasets para análise de dados e aprendizado de máquina.

### Pergunta de Pesquisa
A pergunta que buscamos responder neste projeto é: Qual será a classificação de nota (GradeClass) de um aluno novo, dado seu perfil (por exemplo, idade, tempo de estudo semanal, faltas, etc.)?

### Metodologia
 #### 1) Coleta de Dados:
 Os dados foram obtidos da plataforma Kaggle, a partir do dataset Students Performance Dataset.
 O dataset inclui contém informações abrangentes sobre 2.392 estudantes do ensino médio, detalhando seus dados demográficos, hábitos de estudo, envolvimento dos pais, atividades extracurriculares e desempenho acadêmico. Possui 2392 linhas, 17 colunas e algumas variaveis como: idade, Genero, etinia, eduação parental, habitos de estudo entro outros. 
 
 #### 2) Pré-processamento dos Dados:
- Limpeza dos dados, incluindo tratamento de valores ausentes e inconsistências.
- Transformação de variáveis categóricas em variáveis numéricas (codificação).
- Normalização/Padronização dos dados para otimizar o desempenho do modelo de classificação.

#### 3) Análise Exploratória dos Dados (EDA):
   - Visualização dos dados para identificar padrões, tendências e possíveis outliers.
   - Estatísticas descritivas para entender melhor a distribuição das variáveis.

#### 4) Modelagem e Treinamento:
  - Seleção do modelo de classificação adequado (Árvore de Decisão, K-Nearest Neighbours, Naïve Bayes e Support Vector Machine).
  - Divisão dos dados em conjuntos de treinamento e teste.
  - Treinamento do modelo utilizando o conjunto de treinamento.

#### 5) Avaliação do Modelo:
  - Avaliação do desempenho do modelo utilizando a métrica de Acurácia.

#### 6) Implementação e Resultados:
  - Implementação do modelo de classificação para prever GradeClass que é o classificador de estudantes.
  - Interpretação dos resultados e resposta à pergunta de pesquisa.

## Resultados

- O modelo de classificação atingiu uma acurácia de **94%** no conjunto de teste.
- Observando a acurácia é possivel perceber que a proporção de previsões corretas feitas pelo modelo em relação ao total de previsões foi de quase 100%.
- Ao inserir novos dados de novos alunos podemos concluir que o modelo será capaz de prever a classificação de grupos com 94% de acerto.

  ## Futuras Direções
  Para melhorar ainda mais este projeto, futuras análises podem incluir:
  - Outras métrica como precisão, Precisão, Recall, F1-Score, entre outras.




  


