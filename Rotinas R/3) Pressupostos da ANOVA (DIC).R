########################################################################
###########                Rotina elaborada por              ###########
###########             Eric Vinicius Vieira Silva           ###########
###########              ericvinicius.vs@gmail.com           ###########
###########                     22/08/2019                   ###########
########################################################################


##--------------------- PRESSUPOSTOS DA ANOVA ------------------------##

# A Análise de variância é uma das ferramentas estátistica mais emprega
# na experimentação agricola de forma geral, seja experimentos fitotecnicos
# ou genéticos.

# Entretanto, para que a mesma seja realizada é necessário que alguns 
# pressupostos sejam atendidos, de forma a permitir/validar a análise

# Os pressupostos são:

# 1) Normalidade dos erros (Os erros devem seguir distribuição Normal)
# 2) Homocedasticidade dos erros (Os erros devem ser homogeneos)
# 3) Independência dos erros (Os erros devem ser independentes)
# 4) Aditividade do modelo (O modelo deve conter apenas efeitos aditivos)

# OBS.: Nesta rotina será utilizado o exemplo 3


##---------------------------- Normalidade ---------------------------##

setwd("C:\\Users\\Usuario\\Desktop\\Plant_Breeding_Analysis") #Definir diretório ou Ctrl+Shift+H
dir()
dados<-read.table("exemplo3.txt", h=T)
str(dados) #Estrutura da tabela --> Trat INT?!?!
dados<-transform(dados, Trat=factor(Trat)) #Transformando Trat em fator!
str(dados) #Conferindo os fatores
summary(dados) #Média e médiana dos dados.

# Para realizar as análises dos pressupostos é necessário extrair primeiramente os ERROS.
# Neste exemplo vamos trabalhar com um conjuto de dados oriundos de um experimento em DIC

AOVDados<-aov(ABS ~ Trat, data = dados)
AOVDados$residuals #Extraindo os residuos/erros do nosso conjunto de dados

shapiro.test(AOVDados$residuals) # O teste de Shapiro-Wilk é um dos teste para analisar a 
                                 # Normalidade dos erros.
                                 # Aqui testamos a seguinte hipotese:
                                 # H0: Os erros seguem distribuição Normal. Ou seja, queremos
                                 # aceitar a hipotese de nulidade, assim, desejamos o p-value
                                 # superior ao nivel de significância, exemplo p-value > 0.05


# Neste exemplo, de acordo com o teste de Shapiro-Wilk os erros seguem Distribuição Normal

# É possivel análisar a Normalidade por meio de gráficos. Utilizaremos o pacote fBasics

install.packages("fBasics")
library(fBasics)

qqnormPlot(AOVDados$residuals) #Plotar o gráfico com os residuos. 

histPlot(x = as.timeSeries(AOVDados$residuals)) #observar a distribuição dos dados


##------------------------- Homocedasticidade ------------------------##

# O segundo pressuposto a ser verificado é a homogeneidade dos erros.

getwd() # Verificar o diretório que se está trabalhando
dados

bartlett.test(ABS~Trat,data=dados) # O teste de Bartlett é um dos teste utilizados
                                   # para aferir a homocedasticidade dos erros. 
                                   # Semelhante ao teste de Normalidade, desejamos
                                   # aceitar a hipótese de nulidade
                                   # H0: Os erros são homogêneos, assim, 
                                   # desejamos o p-value superior ao nivel de 
                                   # significância, exemplo p-value > 0.05

bartlett.test(AOVDados$residuals ~ Trat,data=dados)

# Observer que utilizando os dados e os residuos temos o mesmo resultado? Por quê?
# O mesmo não ocorre em DBC!!!

# Neste exemplo, de acordo com o teste de Bartlett os erros são homogêneos


##--------------------------- Independência --------------------------##

# Para verificar a Independência dos erros pode-se utilizar o pacote car

install.packages("car")    
library("car")

getwd() # Verificar o diretório que se está trabalhando
dados

dwt(lm(AOVDados)) # Neste caso será utilizado o teste de Durbin-Watson
                  # a função dentro do pacote car é dwt. 
                  # Deseja-se que os erros não apresentem autocorrelação
                  # ou seja, os erros devem ser independentes
                  # H0: Os erros são independentes
                  # Dessa forma desejamos o p-value superior ao nivel de 
                  # significância, exemplo p-value > 0.05


# Os erros são independentes


##---------------------------- Aditividade ---------------------------##

# Em DIC o pressuposto da Aditividade não é necessario ser testado, uma vez
# que a aditividade está relacionado aos efeitos principais. Em dic tem-se
# Yij = m + Ti + eij, sendo apenas o efeito de tratamento considerado como
# principal.


##--------------------- Pressupostos não atendidos --------------------##


# Caso os pressupostos não sejam atendidos pode-se adotar algumas estrátegias
# para submeter os dados à Análise de Variância, uma delas é a transformação 
# de dados. 

##-------------------------------  FIM  -------------------------------##
