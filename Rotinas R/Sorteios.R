########################################################################
###########                Rotina elaborada por              ###########
###########             Eric Vinicius Vieira Silva           ###########
###########              ericvinicius.vs@gmail.com           ###########
###########                     22/08/2019                   ###########
########################################################################


##------------------------  EXPERIMENTAÇÃO  --------------------------##

# O planejamento de um experimento deve ser pautado em respeitar os 
# principios da experimentação

#1) Repetição
#2) Casualização
#3) Controle local

# Dessa forma a forma e qualidade das análise dependem sobretudo do planejamento 
# e da condução do experimento.

# Neste rotina serão abordados temas relacionados ao planejamento experimental 
# e análise dos presupostos da Análise de variância (ANOVA)


##---------------------- Sorteio (Casualização) ----------------------##

# A Casualização é um dos principios básicos e obrigatórios da ANOVA
# Este principio promove independência dos erros (embora não o garanta)
# um dos presupostos da análise de variância. A casualização é realizada
# a partir do sorteio do tratamentos ao longo do experimento.


## O pacote agricolae é uma boa ferramenta para realizar sorteios

install.packages("agricolae") #Instalar o pacote 
library("agricolae") #Carregar o pacote

?`design.X` # Existem diversos delineamentos experimentais que podem/devem 
            # ser utilizados, delineamentos no inglês é conhecido como design
            # X deve ser substituido por:
                                # crd para Inteiramente casualizado
                                # rcdb para Blocos Completos casualizado
                                # ab para Esquema fatorial
                                # alpha para Alpha Latice
                                # bib para Blocos Incompletos
                                # split para Parcela Subdividida
                                # dau para Blocos Aumentados
                                # lsd para Quadrado Latino
                                # lattice para Latice
                                # graeco para Quadrado Greco-Latino
                                # strip para experimentos em faixas


# PRIMEIRO PASSO - PROCURAR A FUNÇÃO E COPIAR/COLAR O COMANDO 
 
# design.crd(trt, r, serie = 2, seed = 0, kinds = "Super-Duper",randomization=TRUE)
# design.rcbd(trt, r, serie = 2, seed = 0, kinds = "Super-Duper", first=TRUE,continue=FALSE,randomization=TRUE)
# design.ab(trt, r, serie = 2, design=c("rcbd","crd","lsd"),seed = 0, kinds = "Super-Duper",first=TRUE,randomization=TRUE)
# design.split(trt1, trt2,r=NULL, design=c("rcbd","crd","lsd"),serie = 2, seed = 0, kinds = "Super-Duper", first=TRUE,randomization=TRUE)
# design.lattice(trt, r=3, serie = 2, seed = 0, kinds = "Super-Duper",randomization=TRUE)

#EXEMPLOS:
setwd("C:\\Users\\Usuario\\Desktop\\Plant_Breeding_Analysis") #Definir diretório ou Ctrl+Shift+H

#DIC, 25 Tratamentos, 3 Repetições

trt<-(1:25) #Número de tratamentos
r<-3 #Número de repetições

sorteioDIC<-design.crd(trt, r, serie = 3, seed = 0, kinds = "Super-Duper",randomization=TRUE)
sorteioDIC

write.table(sorteioDIC$book, file='SorteioDIC.csv', 
            row.names=FALSE, col.names=c("Parcela", "Rep", "Trat"), sep =" ")

#Configurando a planilha
# 1)Abrir o arquivo CSV no Excel
# 2)Selecionar a coluna dos dados -> Clicar em dados -> Opção texto para colunas
#   Delimitado -> avançar -> Opção Espaços -> Concluir

#DBCC, 25 Tratamentos, 3 Repetições

trt<-(1:25)
r<-3

sorteioDBCC<-design.rcbd(trt, r, serie = 3, seed = 0, kinds = "Super-Duper",randomization=TRUE)
sorteioDBCC

write.table(sorteioDBCC$book, file='SorteioDBCC.csv', 
            row.names=FALSE, col.names=c("Parcela", "Bloco", "Trat"), sep =" ")

#Obs: Configurar a planilha

#FATORIAL, PRIM. FATOR 3 TRATS, 
#SEGU. FATOR 12 TRATS -> TOTAL 3 x 12 = 48 trats , 3 Repetições

trt<-c(3,12)#Se tiver 3 fatores, basta separar os niveis de cada fator com virgula.
            #exemplo   c(5,3,2)

r<-3        #número de repetições

# O Fatorial é na verdade um esquema e não um delineamento, dessa forma, pode-se ter
# o esquema fatorial em diferentes delineamentos como DIC, DBCC, Latice entre outros

sorteioFATDIC<-design.ab(trt, r, serie = 3, design=c("crd"),seed = 0, kinds = "Super-Duper",first=TRUE,randomization=TRUE)
sorteioFATDIC

sorteioFATDBCC<-design.ab(trt, r, serie = 3, design=c("rcbd"),seed = 0, kinds = "Super-Duper",first=TRUE,randomization=TRUE)
sorteioFATDBCC

write.table(sorteioFATDIC$book, file='SorteioFATDIC.csv', 
            row.names=FALSE, col.names=c("Parcela", "Rep", "Fator A", "Fator B"), sep =" ")


write.table(sorteioFATDBCC$book, file='SorteioFATDBCC.csv', 
            row.names=FALSE, col.names=c("Parcela", "Bloco", "Fator A", "Fator B"), sep =" ")

#Obs:Caso deseja-se dar nomes aos fatores basta nomear Fator A e Fator B com os nomes desejados

#Obs2:Configurar a planilha


#PARCELA SUBDIVIDIDA->PRIM.FAT. 3TRATS, SEG.FAT. 12TRATS->TOTAL 3x12=48 trats , 3 Repetições

#Semelhante ao fatorial.
trt1<-(1:3)
trt2<-(1:12)
r<-3

sorteioSPLITDIC<-design.split(trt1, trt2,r, design=c("crd"),serie = 3, seed = 0, kinds = "Super-Duper", first=TRUE,randomization=TRUE)
sorteioSPLITDIC

write.table(sorteioSPLITDIC$book, file='SorteioSPLITDIC.csv', 
            row.names=FALSE, col.names=c("Parcela", "Subparcela", "Rep", "Fator A", "Fator B"), sep =" ")

sorteioSPLITDBCC<-design.split(trt1, trt2,r, design=c("rcbd"),serie = 3, seed = 0, kinds = "Super-Duper", first=TRUE,randomization=TRUE)
sorteioSPLITDBCC

write.table(sorteioSPLITDBCC$book, file='SorteioSPLITDBCC.csv', 
            row.names=FALSE, col.names=c("Parcela", "Subparcela", "Bloco", "Fator A", "Fator B"), sep =" ")

#Obs:Caso deseja-se dar nomes aos fatores basta nomear Fator A e Fator B com os nomes desejados

#Obs2:Configurar a planilha


#LATICE SIMPLES 12 X 12 -> 12 bloquinhos, 2 repetições, 144 Tratamentos

trt<-(1:144)
r=2 # para látice triplo basta alterar o número de repetições para 3

sorteioLATICE<-design.lattice(trt, r, serie = 3, seed = 0, kinds = "Super-Duper",randomization=TRUE)
sorteioLATICE

write.table(sorteioLATICE$book, file='SorteioLATICE.csv', 
            row.names=FALSE, col.names=c("Parcela", "Rep", "Bloco", "Trat"), sep =" ")

write.table(sorteioLATICE$book, file='SorteioLATICE.csv')

##---------------------------------------------------------------------##

# Dessa forma, realizando o sorteio do experimento os principios obrigatórios
# da experimentação (Repetição e Casualização, quando necessário controle local)
# estão sendo respeitados

# Este é o primeiro passo para uma boa experimentação!!!


##-------------------------------  FIM  -------------------------------##