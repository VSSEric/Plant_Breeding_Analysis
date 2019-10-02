########################################################################
###########                Rotina elaborada por              ###########
###########             Eric Vinicius Vieira Silva           ###########
###########              ericvinicius.vs@gmail.com           ###########
###########                     22/08/2019                   ###########
########################################################################


##------------------ PRESSUPOSTOS DA ANOVA PARA DBC ------------------##

# Os testes para as pressupossições do DBC são semelhantes aos do DIC.
# Entretanto, necessário transformar as nossas repetições em um fator 
# principal. De modo que:  Yij = m + Ti + Bj + eij

##---------------------------- Normalidade ---------------------------##

setwd("C:\\Users\\Usuario\\Desktop\\Plant_Breeding_Analysis") #Definir diretório ou Ctrl+Shift+H
dir()
dados<-read.table("exemplo3.txt", h=T)
str(dados) #Estrutura da tabela --> Trat INT?!?!
dados<-transform(dados, Trat=factor(Trat), Rep=factor(Rep)) #Transformando Trat e Rep em fatores!
str(dados) #Conferindo os fatores
summary(dados) #Média e médiana dos dados.

# Para realizar as análises dos pressupostos é necessário extrair primeiramente os ERROS.

AOVDados<-aov(ABS ~ Trat + Rep, data = dados)
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


bartlett.test(AOVDados$residuals~Trat,data=dados) # O teste de Bartlett é um dos teste utilizados
                                                  # para aferir a homocedasticidade dos erros. 
                                                  # Semelhante ao teste de Normalidade, desejamos
                                                  # aceitar a hipótese de nulidade
                                                  # H0: Os erros são homogêneos, assim, 
                                                  # desejamos o p-value superior ao nivel de 
                                                  # significância, exemplo p-value > 0.05

# Observa-se que para DBC o teste de homogeneidade de Bartlett deve ser realizado
# somente com os residuos, diferentemente do DIC onde a análise dos residuos ou
# dos dados retornam a mesma informação. Por quê??

# Outra forma de verificar a homogeneidade é atraves do teste de ONeill e Mathews.
# Para tal, pode-se utilzar o pacote ExpDes.pt

install.packages("ExpDes.pt")
library("ExpDes.pt")

oneilldbc(dados$ABS, dados$Trat, dados$Rep) #Pressuposto atendido!

# Neste exemplo, de acordo com o teste de Bartlett e ONeill e Mathews os erros são homogêneos


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

# No modelo em DBC, além dos tratamentos tem-se o efeito de blocos como
# um dos efeitos principais do modelo. Sendo necessário realizar o teste
# da aditividade!

# Para verificar a Aditividade do modelo pode-se utilizar o pacote asbio

install.packages("asbio")
library(asbio)

getwd() # Verificar o diretório que se está trabalhando
dados

tukey.add.test(dados$ABS,  dados$Rep, dados$Trat) # Neste caso será utilizado o 
                                                  # teste de Aditividade de Tukey
                                                  # H0: O modelo é aditivo
                                                  # Dessa forma desejamos o p-value 
                                                  # superior ao nivel de significância
                                                  # exemplo: p-value > 0.05



attach(dados) # Utilizando a função attach/detach podemos fixar o objeto dados, simplificando
# para:

tukey.add.test(ABS, Rep, Trat)

detach(dados)

# O modelo é aditivo

##--------------------- Pressupostos não atendidos --------------------##

# Caso os pressupostos não sejam atendidos pode-se adotar algumas estrátegias
# para submeter os dados à Análise de Variância, uma delas é a transformação 
# de dados.

##-------------------------------  FIM  -------------------------------##