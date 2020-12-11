#1 OBJETIVO

##Utilizar o dataset disponibilizado para criar um modelo preditivo dos valores de vendas e verificar os pressupostos do modelo de regressão linear.

#2 BIBLIOTECAS E DADOS

library(dagitty)##Análises gráficas de modelos causais
library(ggdag) ##Análises e plots de directed acyclic graphs (DAGs)
#library(car) #Utilizada pontualmente para cálculo do FIV e executar teste de indepência dos erros
#library(corrplot) É utilizada pontualmente para plotar matrizes de correlação  

data("marketing",package="datarium")

datarium_df<- marketing
str(datarium_df)

#Traduzir as variáveis para o português
colnames(datarium_df)[3:4]<- c("jornais","vendas")
 
#3 ANÁLISES EXPLORATÓRIAS

##Análises de medidas de posição e tendência central
summary(datarium_df)

##Análise de Missings
NAs <- is.na(datarium_df);which(TRUE == NAs)

#Avaliação da correlação entre variáveis
###Forma nativa
cor_datarium<- cor(datarium_df)
cor_datarium

###Representação visual 
corrplot::corrplot(cor(datarium_df), method = "circle")
corrplot::corrplot(cor(datarium_df), method = "number")

##Nota: Verificamos a colinearidade entre as variáveis, observando que entre os possíveis preditores do modelo de vendas (facebook, jornais e youtube) não há alta colineraidade aparente

#4 CONSTRUÇÃO E REPRESENTAÇÃO DE HIPÓTESES

## A partir de dados de estudos anteriores, opiniões de especialistas, por exemplo, podemos construir hipóteses sobre a relação das variáveis da dataset "datarium" estabelecendo entre as variáveis youtube, facebook e jornais qual ou quais explicam melhor predizem a variação das vendas
##Um modo de representar os nossos palpites sobre a relações de causalidade, confundimento, mediação entre variáveis é utilizar os Directed Acyclic Graph (DAG)

##Aqui representamos um palpite da relação entre as variáveis através de um DAG
hipotese_1 <- dagify(vendas ~ youtube + facebook + jornais,
                     exposure = "youtube",
                     outcome ="vendas")
##Análise visual
DAG_datarium <- ggdag(hipotese_1) + theme_dag_blank()
DAG_datarium

##Análise "descritiva"
impliedConditionalIndependencies(hipotese_1) #Traz por escrito a interpretação que temos pela análise visual

##Interpretação --> "Facebook" é independente de "Jornais"; "Facebook é independente de "Youtube" e "Jornais" é independente de "Youtube"

adjustmentSets(hipotese_1) ##Função ggdag_adjustment_set informa quais conjuntos de covariáveis podemos incluir  para obter estimativas não enviesadas
plot_1 <- ggdag_adjustment_set(hipotese_1,outcome="youtube", exposure= "vendas")
plot_1 ##Função ggdag_adjustment_set informa visualmente quais caminhos estamos fechando ao condicionarmos um grupo de covariáveis

##Considerando as correlações entre as variáveis podemos construir a seguinte hipótese para predição de vendas utilizando a representação através de DAG

hipotese_2 <- dagify(vendas ~ youtube + facebook + jornais,jornais ~ facebook,
                     exposure = "youtube",
                     outcome ="vendas")

##Análise visual
DAG2_datarium <- ggdag(hipotese_2) + theme_dag_blank()
DAG2_datarium

##Análise "descritiva"
impliedConditionalIndependencies(hipotese_2)
adjustmentSets(hipotese_2)

plot_2 <- ggdag_adjustment_set(hipotese_2,outcome="youtube", exposure= "vendas")
plot_2

#Nota: Os caminhos informados no DAG apenas indicam uma possível ligação causal, que ainda deverá ser testada e estimada por meio dos dados.

#5 CONSTRUIR MODELO DE PREDIÇÃO DE VENDAS

##Construção de um modelo de regressão linear simples com a variável preditora de maior influência nas vendas

modelo_1<-lm(vendas ~ youtube, data=datarium_df)

summary(modelo_1) #Conseguimos avaliar os coeficiêntes do modelo e seu grau de significância estatística, Estatística F e R²


##Análise do efeito de outliers, observaçoes influentes e avaliação de possíveis violações dos pressupostos do modelo de regressão linear

plot(modelo_1) #A medida que você aperta "Enter" novos gráficos são plotados e é possível avaliar a presença de outliers, valores influentes e violação de pressupostos do modelo


#6 AJUSTE DO MODELO AS VARIÁVEIS FACEBOOK E JORNAIS

modelo_2<- lm(vendas ~ youtube + facebook +jornais, data=datarium_df)

summary(modelo_2) #Avaliamos se o acréscimo das duas variáveis trouxe mudanças significantes na predição das vendas

plot(modelo_2) #Avaliamos se o acréscimo das duas variáveis resultou em outliers, valores influentes e violação de pressupostos do modelo

#7 AVALIAÇÃO DE COLINEARIDADE E INDEPENDÊNCIA DOS RESÍDUOS

## Calculamos o FIV para confirmar a não violação do pressuposto de ausência de alta-colinearidade entre as variáveis
car::vif(lm(vendas ~ youtube + facebook + jornais,datarium_df)) #Nota: FIV abaixos de 10 indicavam não violação do pressuposto

##Calculamos a Independência dos resíduos através do teste de Durbin Watson

car::durbinWatsonTest(modelo_2) ##Nota: Hipótese nula é de independência dos resíduos




  
