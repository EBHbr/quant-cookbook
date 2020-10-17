## Medidas de tendência central  

As medidas de tendência central se referem a formas de definir um valor central dentro de uma distribuição probabilística, elas podem ser uma média, mediana ou moda. A média é a forma mais utilizada, ela representa um ponto que minimiza a distância total em relação a outros pontos.  

#### 1.2.3.1.Média
##### 1.2.3.1.1.Média simples
      Função: 
``` {r,eval=F}
mean(x, na.rm = T/F)
```
x é a variável de entrada
na.rm é utilizado para remover entradas com valores faltando.
    
##### 1.2.3.1.2.Média entre variáveis
Função: 
``` {r,eval=F}
tapply(x, index, mean)
```
x é a variável a ser tirada a média.
index é a variável de categorização.
mean é a função de média.

#### 1.2.3.2.Mediana
##### 1.2.3.2.1.Mediana simples
Função: 
``` {r,eval=F}
median(x, na.rm = T/F)
```
x é a variável de entrada.
na.rm é utilizado para remover entradas com valores faltando.
    
##### 1.2.3.2.2.Mediana entre variáveis
Função: 
``` {r,eval=F}
tapply(x, index, median)
```
x é a variável a ser tirada a mediana.
index é a variável de categorização.
mean é a função de mediana.
    
#### 1.2.3.3.Moda
##### 1.2.3.3.1.Moda simples
No R puro não existe a função moda, então uma opção é criar uma função que avalie os valores, e identifique o de maior ocorrência.
Função:
``` {r,eval=F}
y <- BD$age
moda <- function(y, na.rm = T/F) {
  if(na.rm){
    y = y[!is.na(y)] 
    }
  uy <- unique(y)
  tab <- tabulate(match(y, uy))
  uy[tab == max(tab)]
}
moda(y)
```
x é a variável de entrada
na.rm é utilizado para remover entradas com valores faltando.
Outra opção é instalar bibliotecas que contenham ferramentas semelhantes.A modeest é uma opção. Vamos instalá-la e carregá-la:
``` {r,eval=F}
library(modeest)
mfv(x)
```
x é a variável de entrada

        
#####    #3.3.2.Moda entre variáveis
Função: 
``` {r,eval=F}
tapply(x, index, mfv(x))
```
x é a variável a ser tirada a moda.
index é a variável de categorização.
mean é a função de moda, podendo ser tanto a "moda" quanto a "mfv".

