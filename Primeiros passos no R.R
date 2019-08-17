########################################################################
###########                Rotina elaborada por              ###########
###########             Eric Vinicius Vieira Silva           ###########
###########              ericvinicius.vs@gmail.com           ###########
###########                     17/08/2019                   ###########
########################################################################


##------------------------ PRIMEIROS PASSOS ---------------------------##


##A " # " é utilizada para escrever comentarios, 
##assim o Software R não "ler" o que está escrito após a #. 
##Isso é util pois auxilia no entendimento das rotinas.

##comandos basicos no R -> ?; help; 
##help.search; instal.package; library, setwd, getwd() dir()

?TukeyHSD #Buscando o termo -> ou help("TukeyHSD")
??TukeyHSD #Buscando o termo dentro de pacotes -> help.search("TukeyHSD")

citation() #Como citar o software R

#No R, decimal é representado por ponto " . " Se desejamos escrever
#quatro virgula cinco, a representação correta é 4.5
#virgula será utilizada para separar valores, linhas, colunas, e funções

#Instalando Pacotes

install.packages("NOME DO PACOTE") #Instalar pacotes
install.packages("ExpDes.pt")
install.packages("lme4")
install.packages("lmerTest")
install.packages("laercio")
install.packages("agricolae")
library("NOME DO PACOTE") #Carregando pacotes
library("ExpDes.pt")

#Operações matematicas básicas no R
2+2
3-1
2*3 
4/2
2^2
sqrt(100) #Raiz quadrada
sin(45) #Seno
cos(45) #Coseno
tan(45) #Tangente

4<7	  #x menor que y?
2<=3 	#x menor ou igual a y?
5>2	  #x maior que y?
4>=3	#x maior ou igual a y?
2==3	#x igual a y?
6!=7	#x diferente de y?


##-------------------------- CRIANDO OBJETOS -------------------------##
#CRIANDO OBJETOS

a<-7 # O comando <- indica que o objeto "a" receberá o valor 7; 
     #Esse objeto fica armazenado no ambiente de trabalho 

a    #Chamando o objeto "a" o R apresentará o valor atribuido ao objeto

b<-5

a+b  #Quando o comando "recebe" (<-) não é utilizado, o mesmo 
     #não será armazenado

c<-a+b  #Caso queira armazenar o resultado, é necessario criar um objeto 

c

#Atenção com sobrepossição. Sempre que um objetivo já existente
#recebe um novo valor, o anterior será substituido

a #O objeto "a" é igual a 7

a<-19 #Caso o mesmo objeto receba um novo valor, o mesmo substituirá o antigo

a

a<-7
a2<-19 #Caso não deseje apagar o objeto anterior. Deve ser criado um novo objeto, no caso "a2"
a
a2


#Objetos armazenados 
ls()

#Remoção de objeto. Observe que somente a2 será removido do workspace
rm(a2)


#Deleta tudo que está armazenado no workspace
rm(list=ls())

#Simulando dados

#No software R é possivel simular dados

?rnorm #Ajuda a respeito da função rnorm -> Gera aleatoriamente um
       #conjunto de dados com distribuição normal

#rnorm(n, mean = 0, sd = 1) é a função base

Dados<-rnorm(n=100,mean=10, sd=3); Dados

a<-sd(Dados); a #Estatisticamente igual a 3
b<-mean(Dados); b #Estatisticamente igual a 10

rm(list=ls())


##---------------------- ENTRANDO COM DADOS NO R ----------------------##

#É possivel entrar com dados de diversas formas no R. Serão mostradas
#as duas formas mais comuns.


#1) Entrando com dados diretamente no R
a<- c(10, 20, 30, 40, 50, 60) #criando Vetor A
a
b <- c(2, 4, 6, 10, 8, 12) #Criando Vetor B
b

a+b
a*b
a/b

sum(a) #Somatório de "a"
sum(a+b)

length(a+b) # número de elementos em a+b
media<-sum(a+b)/length(a+b); media #A média pode ser encontrada por 
                                   #sum/length, ou simplesmente
                                   #utilizando a função mean(objeto)
mean(a+b) 
          

names(b)<-c("A","B","C","D", "E", "F");b #Nomeando o vetor b
mean(b)

(order(b, na.last = TRUE, decreasing = TRUE)) #Ordenando b

novob<-b[(order(b, na.last = TRUE, decreasing = TRUE))]; novob

b2<-b[1:3];b2  #retirar os números do vetor b nas posições 1, 2 e 3
b3<-b[-5]; b3  #retirar o quinta observação
b[6]<-100; b #Inserir o valor 100 na posição 6

a
names(a)<-c("A","B","C","D", "E", "F");a
mean(a)
a2<-a[a>mean(a)]; a2 #retirar os maiores que a média
mean(a2)

ab<-data.frame(a,b); ab #Criando data.frame composto pelos vetores a e b

names(ab)<-c("Amb1", "Amb2");ab #Renomeando as a e b para ambientes

ab$Amb1 #Observar somente o Ambiente 1

mean(ab$Amb1) #Média do ambiente 1

var(ab$Amb1) #Variância do ambiente 1

length(ab[1,]) #[x,y] Neste caso x representa as linhas, e y as colunas
               #Estamos interessados em verificar quantos valores temos
               #Para a linha 1, considerando todas as colunas.
               #Observa-se que temos 2 valores associados a essa linha

length(ab[,2]) #Quantos valores associados a coluna 2 ?

sum(ab[1,])    #Somando os valores da linha 1. 
sum(ab[,2])    #Somando os valores da coluna 2

mA<-sum(ab[1,])/length(ab[1,]); mA #media de A -> 12/2 = 6
mAmb2<-sum(ab[,2])/length(ab[,2]); mAmb2 #media do Amb2 -> 13/6 = 21.67

#É possivel aplicar a função apply
#apply(X, MARGIN, FUN, ...) X=objeto, Margin = Lin ou Col? FUN = função

apply(ab,1,mean) #média de todos os genotipos (1 para linhas)
apply(ab,2,mean) #média de todos os ambientes (2 para colunas)

ab$Amb3<- c(30, 60, 90, 100, 120, 760); ab #Adicionando o Ambiente 3
mean(ab$Amb3) #média ambiente 3

#É possivel conectar comandos utilizando &
ab[ab$Amb1 > mean(ab$Amb1)
   & ab$Amb2 > mean(ab$Amb2)
   & ab$Amb3 > mean(ab$Amb3), ] #Genótipos acima da média em 
                                #todos os Ambientes

ab2<-ab[order(ab$Amb2, decreasing = TRUE), ]; ab2 #Ordenando de acordo
                                                  #com o ambiente 2


#2) Importando planilha de dados txt

#Primeiro é necessário definir o diretório (pasta onde estão os arquivos)
#Para definir o diretório basta utilizar a função setwd
#O caminho deve está entre " e as barras duplicadas \\

setwd("C:\\Users\\Usuario\\Desktop\\Plant_Breeding_Analysis")
#Ctrl+Shift+H -> DEFINIR DIRETORIO #Abrir a pasta diretamente

getwd() #Verifica qual diretório está sendo utilizado
dir() #Exibe todos os arquivos dentro do diretorio

#Para importar o txt vamos utilizar a função read.table. Lembrando que
#deve-se usar o ponto " . " para decimais no txt
ex1<-read.table("exemplo.txt", h=T) #h=T informa que cabeçalho

ex1
head(ex1) #Primeiras linhas do txt
tail(ex1) #Ultimas linhas do txt
str(ex1) #Mostra a estrutura da tabela

#Toda manipulação de dados feita anteriormente é possivel de ser realizada
#quando se importa a planilha

#Exemplos
rownames(ex1)<-c(LETTERS[1:20])#Nomeando as linhas. O Comando
                               #LETTERS indica que eu quero nomear as
                               #linhas com letras MAIUSCULAS da 1a Letra
                               #do alfabeto (a) até a 20a (T). letters p/
                               #minusculas
ex1

ex1$Amb1 #Observar dados somente do ambiente 1

mean(ex1$Amb2) #Média do ambiente 2

var(ex1$Amb3) #Variância do ambiente 3

apply(ex1,1,mean) #média de todos os genotipos
apply(ex1,2,mean) #média de todos os ambientes


ex1[ex1$Amb1 > mean(ex1$Amb1)
   & ex1$Amb2 > mean(ex1$Amb2)
   & ex1$Amb3 > mean(ex1$Amb3), ] #Genótipos acima da média em 
                                  #todos os Ambientes

ab2<-ab[order(ab$Amb2, decreasing = TRUE), ]; ab2


##------------------- ESTATÍSTICA DESCRITIVA BÁSICA -------------------##



apply(ab,1,mean) #1 para linhas e 2 para colunas
apply(ab,1,median) 
apply(ab,1,max) 
apply(ab,1,min)
apply(ab,1, summary) #Resumo de todos acima
apply(ab,1,sd)
apply(ab,1,var) 

apply(ex1,1,mean) #1 para linhas e 2 para colunas
apply(ex1,1,median) 
apply(ex1,1,max) 
apply(ex1,1,min)
apply(ex1,1, summary) #Resumo de todos acima
apply(ex1,1,sd)
apply(ex1,1,var) 


##------------------------- CRIANDO MATRIZES --------------------------##


ex<-matrix(c(42,21,37,180,160,177,80,59,90), nrow=3,ncol=3,byrow=FALSE ) 
#valores que estarão em uma matrix com 3 linhas e 3 colunas
ex
colnames(ex)<-c("Idade", "Altura", "Peso")
row.names(ex)<-c("Paulo","Larissa", "Luiz")
ex
ex[1,2]
ex["Larissa", "Altura"]
ex["Luiz", 1:2]
ex["Luiz", 1-3]
ex["Luiz", 1:3] # ou ex["Luiz", ]

#Combinando Vetores para formar uma matriz
a<- c(10, 20, 30, 40, 50, 60) 
b <- c(2, 4, 6, 8, 10, 12) 
c <-c(30, 60, 90, 100, 120, 760)

z<-cbind(a,b,c) #combina os vetores em uma matriz
z #6x3
class(z)
t(z) #transposta = 3x6

colnames(z)<-c("Amb1", "Amb2","Amb3");z
row.names(z)<-c(LETTERS[1:6]);z

mean(z[ , "Amb1"])#Media Amb1
mean(z["A", ])#Média A
mean(z["D", 1-3])#Média de D Amb 1 e 3

#simulando uma matriz
m<-matrix(rnorm(20, 0, 1), 5, 4) #matriz com 20 dados,5 lin e 4 col
m
mean(m)
sd(m) 

#Criando diretamento um data.frame
A<-data.frame(Cultivares=1:4, Producao=rnorm(4,10,2),Nota=letters[1:4]) 
A #Data frame A

B<-data.frame(Tr=rep(1:3,times=3),Bl=rep(1:3,each=3),Prod=rnorm(9,10,1))

#Observar o que a função rep(x, times), rep(x, each) fazem
Tr=rep(1:3,times=3); Tr
Bl=rep(1:3,each=3);Bl

#Possivel combinar (x, times=?, each=?)
Trat2=rep(1:3,times=3, each=3); Trat2

B #Data frame B

DADOS<-edit(data.frame()) #possibilida a digitação direta em uma planilha
DADOS


rm(list=ls()) #Limpar workspace



##----------------------------- EXEMPLOS ------------------------------##


#Exemplo 1

#Diretamente
(TRAT<-rep(c("A", "B", "C", "D", "E"), each=6))
(REP<-rep(1:6,5))
(VR<-c(2370, 1687, 2592, 2283, 2910, 3020,
       1282, 1527, 871, 1025, 825, 920,
       562, 321, 636, 317, 485, 842,
       173, 127, 132, 150, 129, 227,
       193, 71, 82, 62, 96, 44))
(dados2<- data.frame(Trat = TRAT, Rep = REP, VarResp = VR))
edit(dados2) #Editar os dados
head(dados2)
tail(dados2)
str(dados2)
summary(dados2)
#Aplicando Análise de Variância nos dados
ANOVA<-aov(VarResp ~ Trat, data=dados2); summary(ANOVA)

#Importando dados txt
setwd("C:\\Users\\Usuario\\Desktop\\Plant_Breeding_Analysis")
#Ctrl+Shift+H -> DEFINIR DIRETORIO
dir()
ex3<-read.table("exemplo2.txt", h=T)  
ex3
str(ex3)
summary(ex3)
#Utilizando o pacote ExpDes.pt
library("ExpDes.pt") #Após baixar o pacote, é necessário carregá-lo
                     #função library("Nome do pacote")

?ExpDes.pt #Ajuda sobre o pacote. Deve carregá-lo primeiro
           #No final da página de ajuda temos os delineamentos.
           #Neste caso será utilizado o Inteiramente ao Acaso (DIC)
           #Ao clicar em dic podemos ver a função e como usá-la

dic(trat=ex3$Trat, resp=ex3$VarResp, quali = TRUE, mcomp = "tukey", nl = FALSE,
    hvar='bartlett', sigT = 0.05, sigF = 0.05)

#Para facilitar, podemos utilizar a função attach
attach(ex3) #Fixa a planilha de trabalho. Assim, não é necessário indicar
            #O que são os tratamentos, nem a resposta. Basta utilizar
            #O nomes dos fatores do txt

dic(Trat, VarResp, quali = TRUE, mcomp = "tukey", nl = FALSE,
    hvar='bartlett', sigT = 0.05, sigF = 0.05)

#Note problemas com Normalidade dos residuos e homogeneidade de variância
#Esse tópicos serão abortados mais a frente.

detach(ex3) #Remove. Sempre utilizar essa função antes de analisar 
            #um novo conjunto de dados.
            


##-------------------------------  FIM  -------------------------------##

