########################################################################
###########                Rotina elaborada por              ###########
###########             Eric Vinicius Vieira Silva           ###########
###########              ericvinicius.vs@gmail.com           ###########
###########                     04/11/2019                   ###########
########################################################################

##-------------------------- ANÁLISE CONJUNTA ------------------------##

# Nesta rotina será abordado a analise conjunta. Uma das abordadem de
# análises de experimentos mais utilizadas na genética e melhoramento de plantas

# O delineamento utilizado foi o de Blocos Completos Casualizados (DBCC). Entretanto
# no melhoramento de plantas o Delineamento em Blocos Incompletos (DBI) é bastante
# utilizado devido ao elevado número de tratamentos. Análises envolvendo DBI serão 
# abordados nas próximas rotinas.

# OBS.: Nesta rotina será utilizado o exemplo 7

# Leitura da tabela de dados

setwd("C:/Users/Usuario/Desktop/Plant_Breeding_Analysis")
dir()
dados<-read.table("exemplo7.txt", h=T)
str(dados)
dados<-transform(dados, Amb=factor(Amb), Trat=factor(Trat), Rep=factor(Rep))
str(dados)

#Modelos análise individuais: Yij = m + Ti + Bj + eij
#Modelo para análise conjunda: Yijk = m + B/Aj(k) + Ti + Ak + TAik + ej(ik)

sink("Resultados Análise Conjunta.txt",type = c("output"))
i<-nlevels(dados$Amb)
for(i in 1:nlevels(dados$Amb)) {
  cat("Análise de Variância do ambiente = ", i, "\n")
  print(anova(model<-aov(VR ~ Trat + Rep, data=dados[dados$Amb==i,])))
  print(QMR<-deviance(model)/df.residual(model))}

cat("
    ", "\n")#somente para saltar espaço

cat("#--------------------------------------------------------------#", "\n")
cat("Análise de Variância Conjunta", "\n")
conj<-aov(VR ~ Trat + Amb + Trat*Amb + Rep%in%Amb, data=dados)
print(anova(conj))

cat("
    ", "\n")#somente para saltar espaço

cat("# OBSERVAÇÃO -> VALORES DE F CALCULADO SOMENTE PARA G E A FIXOS" , "\n")

cat("
    ", "\n")#somente para saltar espaço

cat("#-------- GL e QM dos Fatores --------#", "\n") 

options(digits=5)

cat("
    ", "\n")#somente para saltar espaço
cat("GL e QMTRAT", "\n")
print(QMT<- summary(conj)[[1]][1,3])
print(GLT<- summary(conj)[[1]][1,1])
cat("GL e QMAMB", "\n")
print(QMA<-summary(conj)[[1]][2,3])
print(GLA<- summary(conj)[[1]][2,1])
cat("GL e QMINT", "\n")
print(QMInt<-summary(conj)[[1]][3,3])
print(GLInt<- summary(conj)[[1]][3,1])
cat("GL e QMBLOCO/AMB", "\n")
print(QMBinA<-summary(conj)[[1]][4,3])
print(GLBinA<- summary(conj)[[1]][4,1])
cat("GL e QMRES", "\n")
print(QMRes<-summary(conj)[[1]][5,3])
print(GLRes<- summary(conj)[[1]][5,1])

cat("
    ", "\n")#somente para saltar espaço

cat("#-------- F calculado (Considerando Bloco sempre aleatório) --------#" , "\n") 

cat("
    ", "\n")#somente para saltar espaço

cat("Genótipo e Ambiente Fixos:", "\n")
cat("Fcalc Tratamento", "\n")
(FcTrat<-QMT/QMRes)
(pf(FcTrat, GLT, GLRes, lower.tail = FALSE))
cat("Fcalc Ambiente", "\n")
(FcAmb<-QMA/QMRes)
(pf(FcAmb, GLA, GLRes, lower.tail = FALSE))
cat("Fcalc Interação GxA", "\n")
(FcInt<-QMInt/QMRes)
(pf(FcInt, GLInt, GLRes, lower.tail = FALSE))
cat("Fcalc Bloco/Ambiente", "\n")
(FcBinA<-QMBinA/QMRes)
(pf(FcBinA, GLBinA, GLRes, lower.tail = FALSE))


cat("
    ", "\n")#somente para saltar espaço

cat("Genótipo (Aleatório) e Ambiente (Fixo):", "\n")
cat("Fcalc Tratamento", "\n")
(FcTrat<-QMT/QMRes)
(pf(FcTrat, GLT, GLRes, lower.tail = FALSE))
cat("Fcalc Ambiente", "\n")
(FcAmb<-QMA/QMInt)
(pf(FcAmb, GLA, GLInt, lower.tail = FALSE))
cat("Fcalc Interação GxA", "\n")
(FcInt<-QMInt/QMRes)
(pf(FcInt, GLInt, GLRes, lower.tail = FALSE))
cat("Fcalc Bloco/Ambiente", "\n")
(FcBinA<-QMBinA/QMRes)
(pf(FcBinA, GLBinA, GLRes, lower.tail = FALSE))

cat("
    ", "\n")#somente para saltar espaço

cat("Genótipo (Fixo) e Ambiente (Aleatório):", "\n")
cat("Fcalc Tratamento", "\n")
(FcTrat<-QMT/QMInt)
(pf(FcTrat, GLT, GLInt, lower.tail = FALSE))
cat("Fcalc Ambiente", "\n")
(FcAmb<-QMA/QMBinA)
(pf(FcAmb, GLA, GLBinA, lower.tail = FALSE))
cat("Fcalc Interação GxA", "\n")
(FcInt<-QMInt/QMRes)
(pf(FcInt, GLInt, GLRes, lower.tail = FALSE))
cat("Fcalc Bloco/Ambiente", "\n")
(FcBinA<-QMBinA/QMRes)
(pf(FcBinA, GLBinA, GLRes, lower.tail = FALSE))

cat("
    ", "\n")#somente para saltar espaço

cat("Genótipo e Ambiente Aleatórios:", "\n")
cat("Fcalc Tratamento", "\n")
(FcTrat<-QMT/QMInt)
(pf(FcTrat, GLT, GLInt, lower.tail = FALSE))
cat("Fcalc Ambiente", "\n")
(FcAmb<-QMA/QMInt)
(pf(FcAmb, GLA, GLInt, lower.tail = FALSE))
cat("Fcalc Interação GxA", "\n")
(FcInt<-QMInt/QMRes)
(pf(FcInt, GLInt, GLRes, lower.tail = FALSE))
cat("Fcalc Bloco/Ambiente", "\n")
(FcBinA<-QMBinA/QMRes) 
(pf(FcBinA, GLBinA, GLRes, lower.tail = FALSE))


cat("
    ", "\n")#somente para saltar espaço
  
library(ScottKnott)  
library(agricolae)
cat("#----------Testes de média e Agrupamento---------------#", "\n")
cat("
    ", "\n")
for(i in 1:nlevels(dados$Amb)) {
  cat("Teste de Scott-Knott para o ambiente = ", i, "\n")
  sk<-SK(dados[dados$Amb==i,], y=NULL, model=VR ~ Trat + Rep, which="Trat")
  summary(sk)
  cat("
    ", "\n")
    cat("Teste de Tukey para o ambiente = ", i, "\n")
  tk<-HSD.test(aov(VR ~ Trat + Rep, data=dados[dados$Amb==i,]), "Trat", group=TRUE)
  print(tk$groups)
  cat("
    ", "\n")}

sink()

##------------------------------- FIM --------------------------------##
