########################################################################
###########                Rotina elaborada por              ###########
###########             Eric Vinicius Vieira Silva           ###########
###########              ericvinicius.vs@gmail.com           ###########
###########                     17/09/2019                   ###########
########################################################################


##--------------------- TRANSFORMAÇÃO DE DADOS ------------------------##
-
# Caso os pressupostos da ANOVA não sejam atingindos, os dados do experimento
# não podem ser submetidos a Análise de Variância.

# Visando atender aos pressupostos algumas estrategias podem ser utilizadas

# Como a transformação de dados. 

# OBS.: Nesta rotina será utilizado o exemplo 2

##--------------------- Verificando os pressupostos -------------------##

setwd("C:\\Users\\Usuario\\Desktop\\Plant_Breeding_Analysis") #Definir diretório ou Ctrl+Shift+H
dir()
dados<-read.table("exemplo2.txt", h=T)
str(dados) 
AOVDados<-aov(VarResp ~ Trat + Rep, data = dados)

AOVDados$residuals               # Extraindo os residuos/erros do nosso conjunto de dados

shapiro.test(AOVDados$residuals) # Residuos significativos de acordo com Shapiro-Wilk test  
                                 # Necessário transformação para atender a Normalidade

bartlett.test(residuals(AOVDados)~dados$Trat) # Significativo para Homocedasticidade
                                              # Necessário transformação para atender esse pressuposto

library("car")
dwt(lm(AOVDados)) #Os erros são independentes

library(asbio)
tukey.add.test(dados$VarResp,  dados$Rep, dados$Trat)#Modelo não pode ser considerado aditivo



##------------------- Testando Transformações Comuns -------------------##

# Raiz Quadrada
dados$RaizQuadVarResp<-dados$VarResp^0.5 # Adicionando uma nova coluna aos nossos dados
                                         # Denominado VarResp0.18 que receberar a transformação
                                         # VarResp^0.5

AOVTransformado<- aov(RaizQuadVarResp~Trat+Rep, data=dados) # AOV dos dados transformados para
                                                            # extrair os residuos.

shapiro.test(residuals(AOVTransformado)) # Pressuposto da Normalidade atendido
bartlett.test(residuals(AOVTransformado)~dados$Trat) # Pressuposto da Homocedasticidade atendido
dwt(lm(AOVTransformado)) #Os erros são independentes
tukey.add.test(dados$RaizQuadVarResp,  dados$Rep, dados$Trat) #Pressuposto da Aditividade atendido


# Raiz Cúbica

dados$RaizCubVarResp<-dados$VarResp^(1/3)

AOVTransformado<- aov(RaizCubVarResp~Trat+Rep, data=dados) 

shapiro.test(residuals(AOVTransformado)) # Pressuposto da Normalidade atendido
bartlett.test(residuals(AOVTransformado)~dados$Trat) # Pressuposto da Homocedasticidade atendido
dwt(lm(AOVTransformado)) #Os erros são independentes
tukey.add.test(dados$RaizCubVarResp,  dados$Rep, dados$Trat) #Pressuposto da Aditividade atendido

#Log
dados$LogVarResp<-log(dados$VarResp)

AOVTransformado<- aov(LogVarResp~Trat+Rep, data=dados) 

shapiro.test(residuals(AOVTransformado)) # Pressuposto da Normalidade atendido
bartlett.test(residuals(AOVTransformado)~dados$Trat) # Pressuposto da Homocedasticidade atendido
dwt(lm(AOVTransformado)) #Os erros são independentes
tukey.add.test(dados$LogVarResp,  dados$Rep, dados$Trat) #Pressuposto da Aditividade atendido


#Potência
dados$VarResp2<-dados$VarResp^2  

AOVTransformado<- aov(VarResp2~Trat+Rep, data=dados)

shapiro.test(residuals(AOVTransformado)) # Pressuposto da Normalidade NÃO atendido
bartlett.test(residuals(AOVTransformado)~dados$Trat) # Pressuposto da homocedasticidade NÃO atendido
dwt(lm(AOVTransformado)) #Os erros são independentes
tukey.add.test(dados$LogVarResp,  dados$Rep, dados$Trat) #Pressuposto da Aditividade atendido


##------------------------- VERIFICANDO VIA BOXCOX ---------------------##

install.packages("MASS")
library("MASS")
# ou require(MASS)

bc<-boxcox(AOVDados)
bc

locator(n=1) #Clicando

lambda <- boxcox(AOVDados)$x[which(boxcox(AOVDados)$y==max(boxcox(AOVDados)$y))]
# ou lambda <- bc$x[which(bc$y==max(bc$y))]
lambda

# usando a tranformação indicada -> 0.1818182 ~ 0.18
dados$VarResp0.18<-dados$VarResp^0.18  # Adicionando uma nova coluna aos nossos dados
                                       # Denominado VarResp0.18 que receberar a transformação
                                       # VarResp^0.18

AOVTransformado<- aov(VarResp0.18~Trat+Rep, data=dados) # AOV dos dados transformados para extrair
                                                        # os residuos.

shapiro.test(residuals(AOVTransformado)) # Pressuposto da Normalidade atendido

bartlett.test(residuals(AOVTransformado)~dados$Trat) # Pressuposto da Homocedasticidade atendido
 
dwt(lm(AOVTransformado)) #Os erros são independentes
tukey.add.test(dados$VarResp0.18,  dados$Rep, dados$Trat) #Pressuposto da Aditividade atendido

# Uma vez atendido aos pressupostos. Pode-se submeter os dados a ANOVA
# Nas próximas rotinas serão destacados as análises dos delineamentos
# básicos Inteirameira Casualizado (DIC) e Blocos Completos Casualizados (DBC)


##---------------------------------- FIM -------------------------------##
