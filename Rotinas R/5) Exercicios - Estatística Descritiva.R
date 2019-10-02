########################################################################
###########                      Exercicios                  ###########
###########             Eric Vinicius Vieira Silva           ###########
###########              ericvinicius.vs@gmail.com           ###########
###########                     28/08/2019                   ###########
########################################################################


#1) Simule uma distribuição Normal com 100 elementos, média 10 e variância 4

a<-rnorm(n=100, mean=10, sd = 2); a


#2) Desta distribuição calcule:
      
#MÉDIA

SOMA<-sum(a) # E(Xi)
SOMA
N<-length(a)

media<-SOMA/N; media # E(Xi)/N
mean(a) # Utilizar a função do R para conferir


#MODA
      # A Moda não possui um comando especifico no R. Assim é necessario criar
      # uma função que realize o procedimento da moda (Valor que mais se repete)
      # Neste caso, retirei essa função de: https://www.tutorialspoint.com/r/r_mean_median_mode.htm

# Criando a função Getmode

getmode <- function(a) {
  uniqa <- unique(a)
  uniqa[which.max(tabulate(match(a, uniqa)))]
}

getmode(a) # basta utilizar a função no objeto a


#MEDIANA

a<-a[(order(a, decreasing = TRUE))] # Ordenar os elementos de a
mediana<-(((a[N/2])+(a[(N+2)/2]))/2); mediana #N par
mediana<-((a[(N+1)/2])); mediana #N Impar

median(a) #Conferir

#MINIMO
(a[1]) # Se o ordenamento for crescente
(a[N]) # Se o ordenamento for decrescente
min(a) # Conferir
      
#MÁXIMO
(a[1]) # Se o ordenamento for decrescente
(a[N]) # Se o ordenamento for crescente
max(a) # Conferir


#AMPLITUDE
(a[1])-(a[N]) # Ordenamento decrescente
(a[N])-(a[1]) # Ordenamento crescente
max(a) - min(a) # Conferir

      
#VARIÂNCIA

sum((a[1:N]-mean(a))^2)/(N-1) #VARIÂNCIA = (E(xi - Xmedia)^2)/N-1
var(a) # Conferir
      

#DESVIO PADRÃO
sqrt(sum((a[1:N]-mean(a))^2)/(N-1)) #Raiz quadrada da variância    
sd(a) #Conferir

#COEFICIENTE DE VARIAÇÃO


CV<-sqrt(sum((a[1:N]-mean(a))^2)/(N-1))/(sum(a)/N)*100 ; CV

sd(a)/mean(a)*100 #Conferir


#GRÁFICO DA DISTRIBUIÇÃO

install.packages("fBasics")
library(fBasics)
histPlot(x = as.timeSeries(a))

#Compare a curva do objeto a com os objetos b, c, d, e
b<-rnorm(n=5, mean=10, sd = 2) #N = 5
histPlot(x = as.timeSeries(b))

c<-rnorm(n=10, mean=10, sd = 2) #N = 10
histPlot(x = as.timeSeries(c))

d<-rnorm(n=20, mean=10, sd = 2) #N = 20
histPlot(x = as.timeSeries(d))

e<-rnorm(n=50, mean=10, sd = 2) #N = 50
histPlot(x = as.timeSeries(e))

# A medida que N aumenta, o que acontece?


#GRÁFICO BOXPLOT

boxplot(a)
boxplot(b)
boxplot(c)
boxplot(d)
boxplot(e)
boxplot(a,b,c,d,e,main="BoxPlot",names=c(LETTERS[1:5]),xlab="Objetos",ylab="Eixo Y")

#Interprete o gráfico, quais informações este gráfico fornece?

# https://www.youtube.com/watch?v=1fMCymE0ZPE   (Grupo Voitto)


###--------------------------------  FIM  -------------------------------###

