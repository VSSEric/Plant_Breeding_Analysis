########################################################################
###########                Rotina elaborada por              ###########
###########             Eric Vinicius Vieira Silva           ###########
###########              ericvinicius.vs@gmail.com           ###########
###########                     20/09/2019                   ###########
########################################################################

##---------------- DELINEAMENTOS EXPERIMENTAIS BÁSICOS ---------------##

# Nesta rotina serão abordados a análises de dados de delineamentos
# experimentais básicos como o Inteiramente Casualizado (DIC) e o
# blocos completos casualizados (DBC ou DBCC).

# OBS.: Nesta rotina será utilizado os exemplos 4,5 e 6



##-------------- DELINEAMENTOS INTEIRAMENTE CASUALIZADO --------------##

# O DIC possui algumas vantagens como maior flexibilidade uma vez que
# não se tem nenhuma restrição quanto a casualização dos tratamento.
# Outra grande vantagem é que este delineamento provê um maior número de
# graus de liberdade para o resíduo.

# Embora apresente essas vantagens, este delineamento é pouco robusto quando
# se lida com áreas heterogeneas. Dessa forma, toda variação que não é devido
# ao efeito dos tratamentos em estudo será adicionado ao resíduo, dessa forma,
# este delineamento requer um maior rigor experimental. Sendo, por fim, não
# indicado para experimentos fitotecnicos ou genéticos a nivel de campo.


# Mais informações a respeito do Delineamento Inteiramento Casualizado podem ser
# encontradas no livro base: 

# RAMALHO, M. A. P.; FERREIRA, D. F.; OLIVEIRA, A. C. de. Experimentação 
# em Genética e Melhoramento de Plantas: 3.ed. Lavras: Editora UFLA, 2012 

setwd("C:\\Users\\Usuario\\Desktop\\Plant_Breeding_Analysis") #Definir diretório ou Ctrl+Shift+H
dir()
dados<-read.table("exemplo4.txt", h=T)
str(dados) #Observar que LINHAGENS (LIN) estão sendo consideradas como int e não como fator 
dados<-transform(dados, LIN=factor(LIN)) #Transformando LINHAGENS em fator.
str(dados) #Check!

##----> Modelo DIC:   Yij = m + Ti + eij
# Em que:
# Yij: observação da parcela que recebeu o i-esimo tratamento na j-esima repetição
# m: média geral associada a todas as repetições
# Ti: efeito ("Fixo ou Aleatorio") do i-esimo tratamento
# eij: erro aleatorio associado a observação ij.

# Saber o modelo dos delineamentos é importante, pois indicaremos para o R como é o
# modelo a ser utilizado.

#Função aov
#Objeto<-aov(VariavelResposta ~ Tratamento, data = ??) # Aqui indica-se para o R que
                                                       # a nossa variavel resposta deve
                                                       #ser análisada em função ( ~ ) 
                                                       #dos nossos tratamentos
#Exemplo 1 --> Fator Qualitativo

AOVDados<-aov(DIAM ~ LIN, data = dados) 

##--->> AOVDados$residuals <<---## Sempre testar os pressupostos antes das análises

# Uma vez atendidos aos pressupostos, procede-se a a análise de variancia


#### DADOS BALANCEADOS

AOVDados<-aov(DIAM ~ LIN, data = dados)# Yij = m + Ti + eij
anova(AOVDados)    #Quadro de Análise de Variância
cv.model(AOVDados) #CV%

library(agricolae)
TesteTukey<-HSD.test(y=dados$DIAM, trt=dados$LIN, DFerror=(df.residual(AOVDados)), MSerror=((deviance(AOVDados)/df.residual(AOVDados))), alpha=0.05)
TesteTukey

library(ScottKnott)
sk <- SK(x=dados, y=dados$DIAM, model="y~LIN", which="LIN", sig.level=0.05, id.trim=10)
summary(sk)


# UTILIZANDO O PACOTE ExpDes.pt

# O Pacote ExpDes.pt é um excelente pacote para realizar as análises dos delineamentos básicos
# Possui diversas análises, e é possivel solicitar o teste de comparação de médias ou definir
# reta de regressão. Além disso, a função testa os pressupostos da Normalidade e Homocedasticidade


library(ExpDes.pt)

dic(trat=dados$LIN, resp=dados$DIAM, quali = TRUE, mcomp = "tukey", nl = FALSE,
    hvar='bartlett', sigT = 0.05, sigF = 0.05) # PRESSUPOSTOS ATENDIDOS  


#Vamos pedir para escrever um arquivo .txt com toda a nossa análise

sink("Análise de Variância - DIC.txt", type = c ("output"))

dic(trat=dados$LIN, resp=dados$DIAM, quali = TRUE, mcomp = "tukey", nl = FALSE,
    hvar='bartlett', sigT = 0.05, sigF = 0.05)

sink()


#EXEMPLO 2 --> Fator Quantitativo!!

dados2<-read.table("exemplo5.txt", h=T)
str(dados2)  #Efeito quantitativo --> INT = ok!


analise<-dic(dados2$DOSE, dados2$ALTURA, quali = FALSE) # PRESSUPOSTOS ATENDIDOS

# Linear
graficos(analise, grau = 1, mod = TRUE, main = "Gráfico Regressão ", sub = " ",
         xlab = "DOSE", ylab = "ALTURA", pch = 19,
         xlim = NULL, ylim = NULL, bty = "o")

# Quadratica
graficos(analise, grau = 2, mod = TRUE, main = "Gráfico Regressão ", sub = " ",
         xlab = "DOSE", ylab = "ALTURA", pch = 19,
         xlim = NULL, ylim = NULL, bty = "o")


#Vamos pedir para escrever um arquivo .txt com toda a nossa análise

sink("Análise de Variância - DIC Quantitativo.txt", type = c ("output"))

dic(trat=dados2$DOSE, resp=dados2$ALTURA, quali = FALSE)

sink()


##-------------- DELINEAMENTOS EM BLOCOS CAUSALIZADOS ---------------##

# O DBC utiliza-se do principio de controle local como forma de atenuar
# os efeitos residuais. Este delineamento visa controlar fontes de variação
# conhecidas a partir do efeito de blocagem

# O DBC apresenta então restrições quando a sua casualização. Ou seja,
# em cada bloco deve-se ter todos os tratamentos à serem avaliados
# de forma a promover comparações justas entre os tratamentos.

# Espera-se aqui controlar essas fontes de variações conhecidas e
# fornecer condições homogeneas à todos os tratamentos.

# O preço pela utilização da blocagem é refletido na redução dos graus
# de liberdade do resíduo em detrimento dos graus de liberdade dos blocos
# GLblocos = (r-1)

# Este é um dos delineamentos mais utilizados na fitotecnia e melhoramento
# de plantas. 

# Mais informações a respeito do Delineamento Em Blocos Casualizados podem ser
# encontradas no livro base: 

# RAMALHO, M. A. P.; FERREIRA, D. F.; OLIVEIRA, A. C. de. Experimentação 
# em Genética e Melhoramento de Plantas: 3.ed. Lavras: Editora UFLA, 2012 

setwd("C:\\Users\\Usuario\\Desktop\\Plant_Breeding_Analysis")
dir()
dados<-read.table("exemplo6.txt", h=T)

str(dados) # Neste caso, deseja-se epocas e blocos como fator

dados<-transform(dados, Epoca=factor(Epoca), Bloco=factor(Bloco)) #Transformando Epoca e Bloco em fatores.
str(dados) #Check!

##----> Modelo DBC:   Yij = m + Ti + bj + eij
# Em que:
# Yij: observação da parcela que recebeu o i-esimo tratamento no j-esimo bloco
# m: média geral associada a todas as repetições
# Ti: efeito ("Fixo ou Aleatorio") do i-esimo tratamento
# bj: efeito ("Fixo ou Aleatorio") do j-esimo bloco
# eij: erro aleatorio associado a observação ij.


#Exemplo 1 --> Fator Qualitativo

AOVDados<-aov(Brix ~ Epoca + Bloco, data = dados) 

##--->> AOVDados$residuals <<---## Sempre testar os pressupostos antes das análises

# Uma vez atendidos aos pressupostos, procede-se a a análise de variancia


#### DADOS BALANCEADOS

AOVDados<-aov(Brix ~ Epoca + Bloco, data = dados)# Yij = m + Ti + bj + eij
anova(AOVDados)    #Quadro de Análise de Variância
cv.model(AOVDados) #CV%

# UTILIZANDO O PACOTE ExpDes.pt

library(ExpDes.pt)

dbc(trat=dados$Epoca, bloco=dados$Bloco, resp=dados$Brix, quali = TRUE, mcomp = "tukey", nl=FALSE,
    hvar='oneillmathews', sigT = 0.05, sigF = 0.05) # PRESSUPOSTOS ATENDIDOS


#Vamos pedir para escrever um arquivo .txt com toda a nossa análise

sink("Análise de Variância - DBC.txt", type = c ("output"))

dbc(trat=dados$Epoca, bloco=dados$Bloco, resp=dados$Brix, quali = TRUE, mcomp = "tukey", nl=FALSE,
    hvar='oneillmathews', sigT = 0.05, sigF = 0.05)


sink()


#EXEMPLO 2 --> Fator Quantitativo!!

dados2<-read.table("exemplo6.txt", h=T)
str(dados2)  # Efeito quantitativo (Epoca)--> INT = ok!
             # Transformar bloco em fator!

dados2<-transform(dados2, Bloco=factor(Bloco)) #Transformando Bloco em fator.
str(dados2) #Check!

analiseDBC<-dbc(trat=dados2$Epoca, bloco=dados2$Bloco, resp=dados2$Brix, quali = FALSE) # PRESSUPOSTOS ATENDIDOS


# Linear
graficos(analiseDBC, grau = 1, mod = TRUE, main = "Gráfico Regressão ", sub = " ",
         xlab = "Epoca", ylab = "Brix", pch = 19,
         xlim = NULL, ylim = NULL, bty = "o")

# Quadratica
graficos(analiseDBC, grau = 2, mod = TRUE, main = "Gráfico Regressão ", sub = " ",
         xlab = "Epoca", ylab = "Brix", pch = 19,
         xlim = NULL, ylim = NULL, bty = "o")


#Vamos pedir para escrever um arquivo .txt com toda a nossa análise

sink("Análise de Variância - DBC Quantitativo.txt", type = c ("output"))

dbc(trat=dados2$Epoca, bloco=dados2$Bloco, resp=dados2$Brix, quali = FALSE)

sink()

##------------------------------- FIM --------------------------------##
