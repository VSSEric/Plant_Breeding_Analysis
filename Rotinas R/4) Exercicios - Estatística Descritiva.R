1) Simular uma distribuição Normal com 100 elementos, média 20 e variância 25


a<-rnorm(n=100, mean=20, sd = 5); a

SOMA<-sum(a)
SOMA

N<-length(a)




menor<-min(a)
maior<-max(a)

amplitude<-maior-menor; amplitude


b<-a[(order(a, decreasing = FALSE))]
(b[1])
(b[100])

media<-SOMA/N; media
mean(a)

sum((a[1:100]-mean(a))^2)/(N-1) 
var(a)

sd(a)
sqrt(var(a))

sqrt(sum((a[1:100]-mean(a))^2)/(N-1))


cv<-sd(a)/mean(a)*(100); cv


desvio<-a[1:100]-mean(a)
desvio
d2<-desvio^2
somatorio<-sum(d2)
vari<-somatorio/(N-1)
  
2) Desta distribuição calcule:
      Média
      Minimo
      Máximo
      Amplitude
      Variância
      Desvio Padrão
      Coeficiente de Variação
      Gráfico da distribuição
      BoxPlot
      
      moda???


      
      
      
####---------------------------------------------------------------------------####
      
      
      
      
      
      
      
      
      
      
      
      

a<-rnorm(100, 20,5)
a





E<-sum(a); E
N<-length(a); N

X<-E/N; X
mean(a)


sum((a[1:100]-mean(a))^2)/(N-1) 
var(a)

DesP<-sqrt((sum(((a[1:100]-mean(a))^2)))/(N-1)); DesP
sd(a)

CV<-(DesP/X)*100; CV
(sd(a)/mean(a))*100


install.packages("fBasics")
library(fBasics)
histPlot(x = as.timeSeries(b))
boxplot(a,b)

b<-rnorm(100, 20, 20)
c<-rnorm(100, 10, 12)
histPlot(x = as.timeSeries(b))


###-------------------------------  MODA  ----------------------------------------###

# Create the function.

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Create the vector with numbers.
v <- c(2,1,2,3,1,2,3,4,1,5,5,3,2,3)

# Calculate the mode using the user function.
result <- getmode(v)
print(result)

# Create the vector with characters.
charv <- c("o","it","the","it","it")

# Calculate the mode using the user function.
result <- getmode(charv)
print(result)
