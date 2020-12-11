#1 OBJETIVO 
##Usar o dataset disponibilizado para avaliar se algum dos tratamentos foi eficaz na redução do número de reações adversas da doença de Crohn.

#2 BIBLIOTECAS E DADOS
library(ggplot2) #Alternativa aos plots de gráficos utilizando o default do R
#library(nortest) É utilizada pontualmente para executar o teste de Kolmogorov Smirnov

crohn_df<-read.table (file="Data_set_Crohn_LAC.txt",header=T,sep="\t")
head(crohn_df)
str(crohn_df) ## Ao executar a função "str" verifica-se que algumas variáveis que devem ser lidas como fatores estão classificadas como caracterer.

crohn_df[,c("pais","sexo","tratamento")]<-lapply(crohn_df[,c("pais","sexo","tratamento")],as.factor)
str(crohn_df)

#3 QUADRO DE HIPÓTESES 

#Definição a priori do quadro de hipóteses para a associação do tratamento administrado com o número de reações adversas.

#Hipótese nula: Não há associação entre tipo de tratamento e o número de reaçõees adversas.

#Hipótese alternativa: Há associaçãoo entre o tipo de tratamento e o númeoro de reações adversas.

#Classificação das variaveis:

#Variável independente --> Tipo de tratamento
#Variável dependente --> Número de reações adversas
##Tipo de tratamento --> Categórica policotômicas
##Número de reações adversas --> Quantitativa Discreta


#4 ANÁLISE EXPLORATÓRIA

##Avaliamos a mediana do número de reações adversas para cada tratamento administrado
##Nota: A escolha pela mediana leva-se em consideração que a variável "tratamento administrado" é quantitativa discretiva. Portanto a mediana é a medida de tendência central mais adequada.

median.by.groups <- aggregate(x=chron_df$nrAdvE,by=list(chron_df$tratamento),FUN=median)
median.by.groups

#Avaliação da dispersão do número de reações adversas para cada tratamento através de Boxplot

par(mar=c(5,10,5,5))

boxplot(chron_df$nrAdvE ~ chron_df$tratamento, names=F ,xlab="Grupo de tratamento",ylab="Número de reações adversas",cex.axis=1.4, cex.lab=1.5, col=c("red","darkgreen","blue"),ylim=c(0,15),main= "Relação do número de reaçõees adversas vs tratamento")

legend("toprigh",pch=15,col=c("red","darkgreen","blue"), legend=c("Droga 1", "Droga 2", "Placebo"),horiz=T,bty = "n")

dev.off()

##Alternativa de box plot utilizando a biblioteca ggplot 
##Nota: Acrescentamos a estratificação para variável sexo

ggplot(chron_df,aes(x=chron_df$tratamento,y=chron_df$nrAdvE,fill=chron_df$sexo)) + geom_boxplot() + labs(x="Grupo de tratamento",y="Número de reações adversas",title= "Relação do número de reações adversas vs Grupo de tratamento")

##Para avaliar se a moda dos número de reações adversas varia entre os grupos de tratamento plotamos um Barplot para contagem da frequencia do número de reações adversas por grupo de tratamento.

##Criando uma tabela para avaliaçãoo da frequência do número de reações adversas por grupo de tratamento 

y<-as.data.frame(table(chron_df$nrAdvE,chron_df$tratamento))

ggplot(y,aes(x=Var1,y=Freq,fill=Var2))+ geom_bar(stat= "identity", position="stack") + labs(x="Grupo de tratamento",y="Frequência de número de reações adversas",title= "Relação do número de reações adversas vs Grupo de tratamento")+ tema + facet_wrap(~Var2)

#Notamos que 0 é a moda no número de reações adversas nos três grupos de tratamento o que aumenta a possibilidade da diferença entre as medianas ser aleatória.

#5 TESTE DE HIPÓTESES

##Analisamos se "número de reações adversas" mesmo sendo uma variável discreta possui distribuição gaussiana

x<- crohn_df$nrAdvE

##Análise visual 
##Gráfico qqnorm x hist

par(mfrow=c(1,2))
qqnorm(x);qqline(x)
hist(x)

dev.off()

##Reforçamos a análise visual com um teste estatístico para avaliação de normalidade. O teste escolhido é o de Kolmogorov Smirnov.

nortest::lillie.test(crohn_df$nrAdvE)

##Uma vez que "número de reações adversas" não segue a distribuição gaussiana utilizamos o teste não paramétrico de Kruskall Wallis para avaliar se a diferença entre as medianas deve-se ao acaso.

kruskal.test(crohn_df$tratamento~crohn_df$nrAdvE)


