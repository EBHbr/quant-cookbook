# Teste de Hipóteses
### 2.1.1.Baixar e Instalar Bibliotecas
Para esta aula utilizaremos apenas* funções nativas do R, sem necessidade de instalar ou carregar bibliotecas adicionais.
  
### 2.1.2.Banco de dados e variaveis
####  2.1.2.1.Ler o banco de dados em excel
``` {r}
BD <- readxl::read_excel("data/lbw.xlsx")
```
#### 2.1.2.2.Visualizar o banco de dados no RStudio
``` {r}
View(BD)
```
#### 2.1.2.3.Ajustar o BD para as classes das variáveis
``` {r}      
BD[,c("low","race","smoke","ptl","ht","ui","ftv")] <- lapply(BD[,c("low","race","smoke","ptl","ht","ui","ftv")], as.factor)    
BD$low <- factor(BD$low, levels = c(0,1), labels = c("Não", "Sim"))
BD <- transform(BD, lwt=round(lwt*0.453592,1))
BD$race <- factor(BD$race, levels = c(1,2,3), labels = c("Branca", "Negra","Outra"))
BD$smoke <- factor(BD$smoke, levels = c(0,1), labels = c("Não", "Sim"))
BD$ht <- factor(BD$ht, levels = c(0,1), labels = c("Não", "Sim"))
BD$ui <- factor(BD$ui, levels = c(0,1), labels = c("Não", "Sim"))
```     
#### 2.1.2.4.Verificar as propriedades das variáveis 
(verifica dados como ocorrência, quartis, média, mediana, limites etc):
``` {r}
summary(BD)
```
(verificar existência de dados faltantes (NA) no banco de dados):
``` {r}
NAs <- is.na(BD);which(TRUE == NAs)
``` 
   
### 2.1.3. Hipótese I
Para o primeiro exemplo, vamos supor que estamos tentando investigar a relação do tabagismo na gravidez com o peso do bebê ao nascer.
Hipótese nula: Não há diferença no peso ao nascer de bebês de mães fumantes em em comparação com o peso ao nascer de bebês de mães não fumantes.
Hipótese alternativa: Há diferença no peso ao nascer de bebês de mães fumantes em em comparação com o peso ao nascer de bebês de mães não fumantes.
Nesse caso, temos uma variável categórica dicotômica como independente e uma variável contínua como dependente. 
Qual seria a opção de teste paramétrico? E de teste não-paramétrico?

#### 2.1.3.1. Pressupostos (Teste T)
A utilização do Teste T depende de algumas condições.
##### Homocedasticidade
A cada nível das variáveis previsoras, a variância do termo residual deve ser constante. Isso significa que os resíduos a cada nível dos previsores devem ter a mesma variância (homocedasticidade): quando as variâncias são desiguais diz-se que existe heterocedasticidade. 
###### Teste de Levene
Função: LeveneTest(dados$vardependente~dados$varindependente)
Df: Degrees of Freedom
F value: Quanto mais distante de 1, maior é a diferença detectada entre as variâncias. O valor de p informa quando essa diferença é significativa ou não.
Pr(>F): Valor de p para o teste de Levene em que a Ho é que as variâncias são iguais.
``` {r}
library(car)
leveneTest(BD$bwt~BD$smoke)
```

##### 2.1.3.2. Normalidade
###### Aspecto visual
``` {r}
hist(BD$bwt)
plot(density(BD$bwt), ylab='Densidade', xlab='Peso ao Nascer', main='')
```
###### Shapiro-wilk
Função: Shapiro.test(dados$vardependente)
W: Quanto mais próximo de 1, maior é a semelhança da curva estudada com a distribuição normal. O valor de p informa quando essa semelhança é significativa ou não.
P-value: Valor de p para o teste SW em que a Ho é que não há diferença entre a distribuição estudada e uma distribuição normal.
``` {r}
shapiro.test(BD$bwt)
```  
###### Kolmogorov-Smirnov
Função: Ks.test(dados$vardependente,"pnorm", mean(dados$vardependente), sd(dados$vardependente))
pnorm identifica que o KS vai comparar a distribuição da variável dependente com a normal
D: Representa a distância vertical máxima entre a curva estudada e a curva de referência.
Quando mais próximo de zero, maior a semelhança entre as duas distribuições.
P-value: Valor de p para o teste KS em que a Ho é que não há diferença entre a distribuição estudada e uma distribuição normal.
``` {r}
library(dgof)
ks.test(BD$bwt,"pnorm",mean(BD$bwt),sd(BD$bwt))
``` 

##### 2.1.3.3. Teste de hipótese - T não pareado
Função: t.test(dados$varindependente~BD$vardependente)
t: A diferença calculada entre as médias em unidades de desvio padrão;
Df: Degrees of freedom;
P-value: Valor de p para o teste KS em que a Ho é que não há diferença entre as médias dos dois grupos.
``` {r}
t.test(BD$bwt~BD$smoke)
```  


### 2.1.4. Hipótese II
Para o segundo exemplo, vamos supor que estamos tentando investigar a relação da raça da mãe com o peso ao nascer.
Hipótese nula: Não há diferença no peso ao nascer de bebês de segundo a raça de suas mães.
Hipótese alternativa: Há diferença no peso ao nascer de bebês de segundo a raça de suas mães.
Nesse caso, temos uma variável categórica policotômica como independente e uma variável contínua como dependente.
Quais seriam as opções para testes paramétricos e não-paramétricos?
  
#### 2.1.4.1 Pressupostos (One-way ANOVA)
##### Homocedasticidade
``` {r}
leveneTest(BD$bwt~BD$race)
```
##### 2.1.4.2. Normalidade
###### Aspecto visual
Distribuição da variável independente
``` {r}
hist(BD$bwt)
plot(density(BD$bwt), ylab='Densidade', xlab='Peso ao Nascer', main='')
```  
Variável independente x variável dependente
``` {r}
plot(BD$bwt~BD$race, ylab ='Peso ao nascer', xlab='Raça', main='')
```  
###### Shapiro-wilk
``` {r}
shapiro.test(BD$bwt)
ks.test(BD$bwt,"pnorm",mean(BD$bwt),sd(BD$bwt))
```
##### 2.1.4.3. Teste de hipótese - One-way ANOVA
###### Opção I - Função lm()
Função: 
``` {r}
summary(lm(BD$bwt~BD$race))
```
A função lm() é utilizada para construir modelos lineares de regressão.
Sintaxe: lm(formula, data, weights, subset, na.action)
A função summary() fornece os principais parâmetros do objeto.
Retorno: Em termos de ANOVA, a parte de interesse do retorno é o F-statistic, que dá informação do F-value, dos DFs e p-value para a Ho de que todos os grupos têm médias iguais entre si.
  
###### Opção II - Função anova()
Função:
``` {r}
anova(lm(BD$bwt~BD$race))
```  
A função anova() é utilizada para construir uma tabela ANOVA do objeto fornaecido.
Sintaxe: Anova(mod, ...) em que mod pode ser lm para a "linear model", aov para "análise de variância" e outras funções.
Retorno: Informa Df, Sum sq, Mean Sq, F-value e p-value para a Ho de que todos os grupos têm médias iguais entre si.
  
##### Teste post-hoc (teste de Tuckey)
Função:
``` {r}
  TukeyHSD(aov(BD$bwt~BD$race))
```   
A função TukeyHSD() realiza o teste de Tukey no mod especificado (no caso, aov).
Sintaxe: TukeyHSD(x, which, ordered = FALSE, conf.level = 0.95, …)
Retorno: O elemento de maior interesse vai ser o p-value para identificar os grupos nos quais as diferenças são analisadas.
  


### 2.1.5. Hipótese III
Para o último exemplo, vamos supor que estamos tentando investigar a relação do peso da mãe no último ciclo menstrual com o hábito de fumar.
Hipótese nula: Não há diferença significativa no peso da mãe no último ciclo menstrual entre entre os grupos de mães fumantes e não fumantes.
Hipótese alternativa: Há diferença significativa no peso da mãe no último ciclo menstrual entre entre os grupos de mães fumantes e não fumantes.
Nesse caso, temos uma variável categórica dicotômica como independente e uma variável contínua como dependente. 
Quais são algumas das opções teste de hipótese que podemos usar?

#### 2.1.5.1 Pressupostos (One-way ANOVA)
##### Homocedasticidade
``` {r}
leveneTest(BD$lwt~BD$smoke)
```
##### 2.1.5.2. Normalidade
###### Aspecto visual
Distribuição da variável independente
``` {r}
hist(BD$lwt)
plot(density(BD$lwt), ylab='Densidade', xlab='Peso da mãe no último ciclo menstrual', main='')
```
###### Shapiro-wilk
``` {r}
shapiro.test(BD$lwt)
ks.test(BD$lwt,"pnorm",mean(BD$lwt),sd(BD$lwt))
```
Aqui temos que o pressuposto da normalidade não é atendido. Precisamos recorrer a um teste não paramétrico. O teste de Teste de Wilcoxon-Mann-Whitney é uma opção quando o pressuposto da normalidade não é atendido.
  
##### 2.1.5.3. Teste de hipótese - Wilcoxon-Mann-Whitney
``` {r} 
  wilcox.test(BD$lwt~BD$smoke)
```
Valor de p para o teste WMW em que a Ho é que não há diferença entre as médias dos dois grupos.