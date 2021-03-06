########################################################################
###########                Rotina elaborada por              ###########
###########             Eric Vinicius Vieira Silva           ###########
###########              ericvinicius.vs@gmail.com           ###########
###########                     28/11/2019                   ###########
########################################################################

##---------------- DELINEAMENGOS EM BLOCOS INCOMPLETOS ---------------##

# OBS.: Nesta rotina ser�o utilizados os exemplos 8 e 9


##------------------------- LATICE BALANCEADO ------------------------##


# O para o latice ser balanceado temos a restri��o r = k + 1, onde r � o 
# n�mero de repeti��es e k o n�mero de blocos

setwd("C:\\Users\\Usuario\\Desktop\\Plant_Breeding_Analysis")
dir()
dados<-read.table("exemplo8.txt", h=T)
str(dados)
dados<-transform(dados, Rep=factor(Rep), Bloco=factor(Bloco), Trat=factor(Trat))
str(dados) #Observe que Rep possui 5 niveis e Blocos 4, atendendo a restri��o r = k + 1

m1<-aov(terms(Resp ~ Rep + Bloco%in%Rep + Trat, keep.order = TRUE), data=dados) # Quando se tem desbalanceamento
                                                                               # � necessario trabalhar com a SQ
                                                                               # tipo II ou III. O padr�o aov � a tipo I
                                                                               # sendo necessario especificar a ordem
                                                                               # o comando Anova do pacote car, permite 
                                                                               # trabalhar com SQ tipo I, II e III. Basta
                                                                               # especificar.
summary(m1)
car::Anova(m1, type = 'II') # Resultados iguais. 
car::Anova(m1, type = 'III') # SQRes e SQtrat permanecem iguais as analises anteriores!
install.packages("lsmeans")
library("lsmeans")
lsmeans(m1)
(majus<-lsmeans(m1, "Trat"))


# Para realizar as an�lises ser� utilizado o pacote Agricolae

library(agricolae)
library(ExpDes)

dados$block <- factor(paste(dados$Rep, dados$Bloco, sep="")) # Unindo as colunas de Rep e Bloco
                                                             # em uma unica coluna chamada block
head(dados)
str(dados)
block <- dados$block
trt <- dados$Trat
y <- dados$Resp
k<-nlevels(dados$Bloco)

# An�lise INTRABLOCOS - BLOCOS INCOMPLETOS BALANCEADOS - M�dias fenot�picas ajustadas
analiseBIB <- BIB.test(block, trt, y, test = "waller", alpha = 0.05, group=T, console=TRUE)
bar.group(analiseBIB$groups,col="blue",density=4,ylim=c(0,max(y))) #Gr�fico! Facil visualiza��o!!

# Considerando an�lise em DBC 
car::Anova(m1, type = 'III') 
DBC<-aov(terms(Resp ~ Rep + Trat, keep.order = TRUE), data=dados)
car::Anova(DBC, type = 'III') 
(QMRESDBC<-deviance(DBC)/DBC$df.residual)
(QMRESPBIB<-deviance(m1)/m1$df.residual)

#Eficiencia Relativa
(ER<-(2/nlevels(dados$Rep))*(QMRESDBC)/(2/nlevels(dados$Rep)*((1+1/k)*QMRESBIB))*100) #18% mais efetivo

#Agrupamento de m�dias (Scott-knott)
adjmeans <- lsmeans::lsmeans(m1, "Trat") # retorna as m�dias ajustadas em fun��o do tratamento
(QMRES<-anova(m1)[4,3])
(GLRES<-m1$df.residual)
medias<-summary(adjmeans)
x<-cbind(medias[,1], medias[,2]) # Selecionando as colunas de tratamento e m�dias ajudastadas somente
mult<-matrix(1,nlevels(dados$Rep),1) # Uma vez que as m�dias foram ajustadas para as repeti��es
                                     # � necessario que se tenha uma nova matrix com os dados
                                     # das m�dias ajutadas por r repeti��es
xk<-kronecker(x,mult)
colnames(xk)<-c("TRAT", "MEDIAS_AJ") # Renomeando a colunas
xk # Observe que agora a m�dia ajustada do tratamento 1 foi repetida 4 vezes (r=4)

ExpDes::scottknott(xk[,2], xk[,1],GLRES,QMRES*GLRES) #Dois grupos

#Problemas com o teste tukey -> Muito rigoroso -> N�o significancia 
ExpDes::tukey(xk[,2], xk[,1],GLRES,QMRES*GLRES) #Todas as medias iguais


# AN�LISE INTERBLOCOS - Efeito de "blocos dentro de repeti��es" como aleat�rio

# Para essa an�lise ser� utilizado o pacote lme4 e lmerTest

library(lme4) 
library(lmerTest)

# No pacote lme4 os efeitos aleatorios devem ser indicados, para tal, utiliza-se 
# (1|Efeito aleatorio)

modelo1 <- lmer(Resp ~ Rep + (1|Rep:Bloco) + Trat, data = dados)
summary(modelo1) # Ao pedir o resumo do modelo, pode-se observar os componentes de vari�ncia
                 # dos efeitos aleatorios, neste caso o efeito de blocos e o residuo

ranova(modelo1)  # Diferentemente dos efeitos fixos, os efeitos aleatorios n�o s�o testados
                 # pelo F, e sim pela metodo da raz�o de maxima verossimilhan�a - Likelihood ratio (LRT).
                 # Observa-se efeito de blocos significativos

(ANAVA<-anova(modelo1)) # Os efeitos fixos podem ser testados pelo comando de ANOVA normalmente
                        # SQ tipo III

GLTRAT<-ANAVA[2,3] # Graus de Liberdade do tratamento
GLRES<-m1$df.residual # Graus de Liberdade do residuo
FcTrat<-ANAVA[2,5]
(pf(FcTrat, GLTRAT, GLRES, lower.tail = FALSE)) #Significativo ao nivel de 5%

#Agrupamento de m�dias (Scott-knott)
adjmeans <- lsmeans::lsmeans(modelo1, "Trat") # retorna as m�dias ajustadas em fun��o do tratamento
ve<-sigma(modelo1)^2 # Variancia do erro = E(QMRes) = VarResiduo
medias<-summary(adjmeans)
z<-cbind(medias[,1], medias[,2]) # Selecionando as colunas de tratamento e m�dias ajudastadas somente
multz<-matrix(1,nlevels(dados$Rep),1)
zt<-kronecker(z,multz)
colnames(zt)<-c("TRAT", "MEDIAS_AJ") # Renomeando a colunas
zt
ExpDes::scottknott(zt[,2],zt[,1],GLRES,ve*GLRES) # Utilizando a fun��o do pacote ExpDes para o ranqueamento
ExpDes::tukey(zt[,2],zt[,1],GLRES,ve*GLRES)

# AN�LISE INTERBLOCOS E GEN�TIPOS ALEATORIOS

dados<-read.table("exemplo8.txt", h=T)
str(dados)
dados<-transform(dados, Rep=factor(Rep), Bloco=factor(Bloco), Trat=factor(Trat))
modelo2 <- lmer(Resp ~ Rep + (1|Rep:Bloco) + (1|Trat),data = dados) # Matriz � singular! N�o possui
                                                                    # inversa! Problemas! 
summary(modelo2)
ranova(modelo2) # LRT efeitos aleatorios -> Vari�ncia de TRAT SIGNIFICATIVO
anova(modelo2)  # Teste efeitos fixos
blups<-ranef(modelo2, condVar = TRUE) #Desvios
names(blups)
library(lattice)
dotplot(blups) #  gr�fico para observar os desvios de cada tratamento
(media<-mean(dados$Resp,na.rm=T))
(mediablup<-media+blups$Trat) #M�dias Blups!
(ma<-as.matrix(mediablup))
M_BLUPS<-ma[(order(ma, na.last = TRUE, decreasing = TRUE))]
TRAT<-(order(ma[,1], na.last = TRUE, decreasing = TRUE))
(Resultado<-cbind(TRAT, M_BLUPS))


rm(list=ls())



##------------------ LATICE PARCIALMENTE BALANCEADO ------------------##


setwd("C:\\Users\\Usuario\\Desktop\\Plant_Breeding_Analysis")
dir()
dados<-read.table("exemplo9.txt", h=T)
str(dados)
dados<-transform(dados, Rep=factor(Rep), Bloco=factor(Bloco), Trat=factor(Trat))
str(dados) # Observe que Rep possui 2 niveis e Blocos 6, portanto Parcialmente desbalanceado
m1<-aov(terms(Resp ~ Rep + Bloco%in%Rep + Trat, keep.order = TRUE), data=dados)

# An�lise INTRABLOCOS - BLOCOS INCOMPLETOS PARCIALMENTE BALANCEADOS - M�dias fenot�picas ajustadas

# Para realizar as an�lises ser� utilizado o pacote Agricolae

library(agricolae)
library(ExpDes)

car::Anova(m1, type = 'III') #Exp desbalanceado -> SQ tipo III mais indicado
install.packages("lsmeans")
library("lsmeans")
lsmeans::lsmeans(m1, pairwise ~ Trat)
(majus<-lsmeans(m1, "Trat"))


# An�lise INTERBLOCOS - BLOCOS INCOMPLETOS PARCIALMENTE BALANCEADOS - M�dias fenot�picas ajustadas

block <- dados$Bloco
trt <- dados$Trat
replication <- dados$Rep
y <- dados$Resp
b<-length(unique(block))
r<-length(unique(replication))
k<-b/r # tamanho do bloco

analisePBIB <- PBIB.test(block, trt, replication, y, k, method=c("REML"), test = c("lsd"), alpha = 0.05, group=T, console=TRUE)
bar.group(analisePBIB$groups,col="blue",density=4,ylim=c(0,max(y))) #Gr�fico! Facil visualiza��o!!

names(analisePBIB) #Para os nomes dos arquivos que a fun��o possui
analisePBIB$means
analisePBIB$comparison
analisePBIB$groups

#An�lise Interblocos via lme4

library(lme4) 
library(lmerTest)

modelo1 <- lmer(Resp ~ Rep + (1|Rep:Bloco) + Trat, data = dados)

summary(modelo1) # Observar componente de vari�ncia dos efeitos aleatorios!

ranova(modelo1) # Teste dos efeitos aleat�rios -> Vari�ncia de Rep:Bloco n�o significativa

anova(modelo1) # Teste dos efeitos fixos -> Trat n�o significativo a 5%

car::Anova(modelo1, test.statistic="F") #recomendada para objetos lme

lsmeans::lsmeans(modelo1, pairwise ~ Trat, test=("Satterthwaite")) # M�dias ajustadas

(medias.ajus<-lsmeans(modelo1, "Trat")) # M�dias ajustadas


#Agrupamento de m�dias (Scott-knott)
ve<-sigma(modelo1)^2 # Variancia do erro = E(QMRes) = VarResiduo
medias.ajustadas<-summary(medias.ajus)
z<-cbind(medias.ajustadas[,1], medias.ajustadas[,2]) # Selecionando as colunas de tratamento e m�dias ajudastadas somente
multz<-matrix(1,nlevels(dados$Rep),1)
zt<-kronecker(z,multz)
colnames(zt)<-c("TRAT", "MEDIAS_AJ") # Renomeando a colunas
zt
ExpDes::scottknott(zt[,2],zt[,1],GLRES,ve*GLRES) # Utilizando a fun��o do pacote ExpDes para o ranqueamento
ExpDes::tukey(zt[,2],zt[,1],GLRES,ve*GLRES)




# An�lise INTERBLOCOS e INTERTRATAMENTOS

library(lme4)
modelo2 <- lmer(Resp ~ Rep + (1|Rep:Bloco) + (1|Trat), data = dados)

summary(modelo2) # Observar os componentes de vari�ncia

anova(modelo2) # Teste efeitos fixos

ranova(modelo2) # Teste efeitos aleat�rios -> Trat n�o significativo

blups <- ranef(modelo2, condVar = TRUE) 
names(blups)
library(lattice)
dotplot(blups) #  gr�fico para observar os desvios de cada tratamento
hist(blups$Trat[,]) # Histograma blups

(media<-mean(dados$Resp,na.rm=T))
(mediablup<-media+blups$Trat)
hist(mediablup[,])  #Histograma m�dias blups

(ma<-as.matrix(mediablup))
M_BLUPS<-ma[(order(ma, na.last = TRUE, decreasing = TRUE))]
TRAT<-(order(ma[,1], na.last = TRUE, decreasing = TRUE))
(Resultado<-cbind(TRAT, M_BLUPS))


##------------------------------- FIM --------------------------------##