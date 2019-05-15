##MOBILIDADE SOCIAL NO BRASIL: EVIDENCIAS DA PNAD 2014##

###PACOTES
library("dplyr")
library(ggplot2)
library("plyr")
library("survey")
library("magrittr")
library(stats)

###IMPORTANDO A PNAD
a <- c(4, 2, 11, 1, 8, 3, 1, 
       2, 1, 35, 1, 84,
       1, 4, 4, 5,
       512,2, 20, 12, 54, 5, 41,
       1, 1, 1, 2, 1, 
       1, 4, 5, 1, 1, 1, 1, 1,
       2, 1, 1, 1, 1, 1, 1, 4, 
       5, 1, 1, 1, 
       1, 1, 2, 1, 1, 1, 1, 1, 1, 4, 5, 1, 1, 1,
       5, 5, 91 )

name <- c("ano", "UF", "skip1", "sexo", "skip2", "idade", "V0401", 
          "skip0", "V0404", "skip3", "V0601", "skip4", 
          "v9001", "skip5", "V9906", "V9907", 
          "skip6", "V4803","skip7", "V4718", "skip000", "V4729", "skip0000",
          "V0305", "skip00","V32001", "V32002", "V32003",
          "V32004", "V32005", "V32006", "V32007", "V32008", "V32009", "V32010","V32011",
          "V32012", "V32013", "V32014", "V32015", "V32016", "V32017", "V32018","V32019", 
          "V32020", "V32021", "V32022", "V32023", 
          "V32024", "V32025", "V32026", "V32027", "V32028", "V32029", "V32030", "V32031", "V32032", "V32033", "V32034", "V32035", "V32036", "V32037",
          "V32038", "V32039", "skip9" )

base <- read.fwf("PES2014.txt", widths = a)

names(base) <- name

excluir <- c("skip0", "skip00","skip000", "skip0000", "skip1", "skip2", "skip3", "skip4","skip5", "skip6", "skip7", "skip8", "skip9")
base <- base[, !names(base) %in% excluir]

###DESCRITIVA
ocup <- subset(base, base$v9001==1 )
ocup <- data.frame(ocup$idade, ocup$V4729, ocup$sexo)
names(ocup) <- c("idade", "peso", "sexo")

ocup$categoria <- c(0.0)
linhas <- seq(1, nrow(ocup), 1)
i=0

for ( i in linhas ){
  if(ocup$idade[i]<20)
    ocup$categoria[i] <- "a"
  
}

i=0
for ( i in linhas ){
  if(ocup$idade[i]<30)
    if(ocup$idade[i]>19)
      ocup$categoria[i] <- "b"
    
}

i=0
for ( i in linhas ){
  if(ocup$idade[i]<40)
    if(ocup$idade[i]>29)
      ocup$categoria[i] <- "c"
    
}

i=0
for ( i in linhas ){
  if(ocup$idade[i]<50)
    if(ocup$idade[i]>39)
      ocup$categoria[i] <- "d"
    
}


i=0
for ( i in linhas ){
  if(ocup$idade[i]<60)
    if(ocup$idade[i]>49)
      ocup$categoria[i] <- "e"
    
}

i=0
for ( i in linhas ){
  if(ocup$idade[i]>59)
    ocup$categoria[i] <- "f"
  
}

output1 <- ddply(ocup, c("sexo", "categoria"), summarise, total = sum(peso))

chefe <- subset(base, base$v9001==1 & base$V0401==1)
chefe <- data.frame(chefe$idade, chefe$V4729, chefe$sexo)
names(chefe) <- c("idade", "peso", "sexo")

chefe$categoria <- c(0.0)
linhas <- seq(1, nrow(chefe), 1)
i=0

for ( i in linhas ){
  if(chefe$idade[i]<20)
    chefe$categoria[i] <- "a"
  
}

i=0
for ( i in linhas ){
  if(chefe$idade[i]<30)
    if(chefe$idade[i]>19)
      chefe$categoria[i] <- "b"
    
}

i=0
for ( i in linhas ){
  if(chefe$idade[i]<40)
    if(chefe$idade[i]>29)
      chefe$categoria[i] <- "c"
    
}

i=0
for ( i in linhas ){
  if(chefe$idade[i]<50)
    if(chefe$idade[i]>39)
      chefe$categoria[i] <- "d"
    
}


i=0
for ( i in linhas ){
  if(chefe$idade[i]<60)
    if(chefe$idade[i]>49)
      chefe$categoria[i] <- "e"
    
}

i=0
for ( i in linhas ){
  if(chefe$idade[i]>59)
    chefe$categoria[i] <- "f"
  
}

output2 <- ddply(chefe, c("sexo"), summarise, total=sum(peso) )
output3 <- ddply(chefe, c("sexo", "categoria"), summarise, total=sum(peso) )


#CONSTRUINDO A ESCALA

i=0
linhas <- seq(1, nrow(base), 1)
base$gen <- c(0.0)
for (i in linhas){
  if(base$sexo[i]==2)
    base$gen[i] <- 1
  
}

escalavl <- subset(base, base$idade<65 & base$idade>14 & base$V0401==1 & base$V4718!=999999999999)

linhas <- seq(1, nrow(escalavl), 1)

i=0
escalavl$A <- c(0.0)
for (i in linhas){
  if(escalavl$idade[i]>14)
    if(escalavl$idade[i]<20)
      escalavl$A[i] <- 1
}

i=0

escalavl$B <- c(0.0)
for (i in linhas){
  if(escalavl$idade[i]>19)
    if(escalavl$idade[i]<25)
      escalavl$B[i] <- 1
}

i=0
escalavl$C <- c(0.0)
for (i in linhas){
  if(escalavl$idade[i]>24)
    if(escalavl$idade[i]<30)
      escalavl$C[i] <- 1
}

i=0
escalavl$D <- c(0.0)
for (i in linhas){
  if(escalavl$idade[i]>29)
    if(escalavl$idade[i]<35)
      escalavl$D[i] <- 1
}

i=0
escalavl$E <- c(0.0)
for (i in linhas){
  if(escalavl$idade[i]>34)
    if(escalavl$idade[i]<40)
      escalavl$E[i] <- 1
}

i=0
escalavl$F <- c(0.0)
for (i in linhas){
  if(escalavl$idade[i]>39)
    if(escalavl$idade[i]<45)
      escalavl$F[i] <- 1
}

i=0
escalavl$G <- c(0.0)
for (i in linhas){
  if(escalavl$idade[i]>44)
    if(escalavl$idade[i]<50)
      escalavl$G[i] <- 1
}

i=0
escalavl$H <- c(0.0)
for (i in linhas){
  if(escalavl$idade[i]>49)
    if(escalavl$idade[i]<55)
      escalavl$H[i] <- 1
}

i=0
escalavl$I <- c(0.0)
for (i in linhas){
  if(escalavl$idade[i]>54)
    if(escalavl$idade[i]<60)
      escalavl$I[i] <- 1
}

i=0
escalavl$J <- c(0.0)
for (i in linhas){
  if(escalavl$idade[i]>59)
    if(escalavl$idade[i]<65)
      escalavl$J[i] <- 1
}

escalavl$esc <- escalavl$V4803-1

#REGRESSOAES PARA CALCULAR O ESCORE

#Geral: o ajuste em log e melhor
Zero <- subset(escalavl,  V4718!=0 & gen==1 & esc!=16)
reg0 <- lm(log(Zero$V4718) ~ Zero$esc)
summary(reg0)

#1- 15 - 19
A <- subset(escalavl, escalavl$A=="1" & V4718!=0 & gen==1 & esc!=16)
A$lV4718 <- log(A$V4718)
regA <- lm(A$lV4718 ~ A$esc)
summary(regA)
A$inter <- c(coef(regA)[1])
A$beta <- c(coef(regA)[2])
A$result <- exp(A$V4803*A$beta + A$inter)

outputI <- ddply(A, c("esc"), summarise, total=mean(result) )
print(outputI)

#2 - 20- 24
B <- subset(escalavl, escalavl$B=="1" & V4718!=0 & gen==1 & esc!=16)
B$lV4718 <- log(B$V4718)
regB <- lm(B$lV4718 ~ B$esc )
summary(regB)
B$inter <- c(coef(regB)[1])
B$beta <- c(coef(regB)[2])
B$result <- exp(B$V4803*B$beta + B$inter)

outputII <- ddply(B, c("esc"), summarise, total=mean(result) )
print(outputII)

#3 - 25 - 29
C <- subset(escalavl, escalavl$C=="1" & V4718!=0 & gen==1 & esc!=16)
C$lV4718 <- log(C$V4718)
regC <- lm(C$lV4718 ~ C$esc )
summary(regC)
C$inter <- c(coef(regC)[1])
C$beta <- c(coef(regC)[2])
C$result <- exp(C$V4803*C$beta + C$inter)

outputIII <- ddply(C, c("esc"), summarise, total=mean(result))
print(outputIII)

#4 - 30-34
D <- subset(escalavl, escalavl$D=="1" & V4718!=0 & gen==1 & esc!=16)
D$lV4718 <- log(D$V4718)
regD <- lm(D$lV4718 ~ D$esc )
summary(regD)
D$inter <- c(coef(regD)[1])
D$beta <- c(coef(regD)[2])
D$result <- exp(D$V4803*D$beta + D$inter)

outputIV <- ddply(D, c("esc"), summarise, total=mean(result) ) 
print(outputIV)

#5 - 35 - 39
E <- subset(escalavl, escalavl$E=="1" & V4718!=0 & gen==1 & esc!=16)
E$lV4718 <- log(E$V4718)
regE <- lm(E$lV4718 ~ E$esc )
summary(regE)
E$inter <- c(coef(regE)[1])
E$beta <- c(coef(regE)[2])
E$result <- exp(E$V4803*E$beta + E$inter)

outputV <- ddply(E, c("esc"), summarise, total=mean(result) ) 
print(outputV)

#6 - 40 - 44
F <- subset(escalavl, escalavl$F=="1" & V4718!=0 & gen==1 & esc!=16)
F$lV4718 <- log(F$V4718)
regF <- lm(F$lV4718 ~ F$esc )
summary(regF)
F$inter <- c(coef(regF)[1])
F$beta <- c(coef(regF)[2])
F$result <- exp(F$V4803*F$beta + F$inter)

outputVI <- ddply(F, c("esc"), summarise, total=mean(result) ) 
print(outputVI)

#7 45-49
G <- subset(escalavl, escalavl$G=="1" & V4718!=0 & gen==1 & esc!=16)
G$lV4718 <- log(G$V4718)
regG <- lm(G$lV4718 ~ G$esc )
summary(regG)
G$inter <- c(coef(regG)[1])
G$beta <- c(coef(regG)[2])
G$result <- exp(G$V4803*G$beta + G$inter)

outputVII <- ddply(G, c("esc"), summarise, total=mean(result) ) 
print(outputVII)

#8 - 50 - 54
H <- subset(escalavl, escalavl$H=="1" & V4718!=0 & gen==1 & esc!=16)
H$lV4718 <- log(H$V4718)
regH <- lm(H$lV4718 ~ H$esc )
summary(regH)
H$inter <- c(coef(regH)[1])
H$beta <- c(coef(regH)[2])
H$result <- exp(H$V4803*H$beta + H$inter)

outputVIII <- ddply(H, c("esc"), summarise, total=mean(result) ) 
print(outputVIII)

#9 - 55 - 59
I <- subset(escalavl, escalavl$I=="1" & V4718!=0 & gen==1 & esc!=16)
I$lV4718 <- log(I$V4718)
regI <- lm(I$lV4718 ~ I$esc )
summary(regI)
I$inter <- c(coef(regI)[1])
I$beta <- c(coef(regI)[2])
I$result <- exp(I$V4803*I$beta + I$inter)

outputIX <- ddply(I, c("esc"), summarise, total=mean(result) ) 
print(outputIX)

#10 - 60- 64
J <- subset(escalavl, escalavl$J=="1" & V4718!=0 & gen==1 & esc!=16)
J$lV4718 <- log(J$V4718)
regJ <- lm(J$lV4718 ~ J$esc )
summary(regJ)
J$inter <- c(coef(regJ)[1])
J$beta <- c(coef(regJ)[2])
J$result <- exp(J$V4803*J$beta + J$inter)

outputX <- ddply(J, c("esc"), summarise, total=mean(result) ) 
print(outputX)

#Plotando os resultados
outputI$Faixa <- c("15-19")
outputII$Faixa <- c("20-24")
outputIII$Faixa <- c("25-29")
outputIV$Faixa <- c("30-34")
outputV$Faixa <- c("35-39")
outputVI$Faixa <- c("40-44")
outputVII$Faixa <- c("45-49")
outputVIII$Faixa <- c("50-54")
outputIX$Faixa <- c("55-59")
outputX$Faixa <- c("60-64")

result <- rbind.data.frame(outputI, outputII, outputIII, outputIV, outputV, outputVI, outputVII, outputVIII, outputIX, outputX)
as.factor(result$Faixa)

ggplot(result, aes(x=esc, y=total, color=Faixa))+ geom_point()+geom_line()+xlab("Anos de estudo")+ylab("Rendimento")+theme_bw()

#Plotando o valor indicado pela pnad
escalavl <- subset(escalavl, V4718!=0 & esc!=16 & gen==1)
real <- ddply(escalavl, c("esc"), summarise, total= mean(V4718))

ggplot(real, aes(x= esc, y=total ))+ geom_point()+geom_line()+xlab("Anos de estudo")+ylab("Rendimento")+theme_bw()

#Filtrando para pessoas maiores de 19
A <- subset(A, idade>19)
B<- subset(B, idade>19)
C<- subset(C, idade>19)
D <- subset(D, idade>19)
E <- subset(E, idade>19)
F <- subset(F, idade>19)
G <- subset(G, idade>19)
H <- subset(H, idade>19)
I <- subset(I, idade>19)
J  <- subset(J, idade>19)

#Definindo o status socioeconomico: e a media entre o retorno esperado por nivel de educacao e o salario recebido
i=0
linhas <- seq(1, nrow(A), 1)
A$sse <- c(0.0)
for (i in linhas){
  A$sse[i] <- (A$V4718[i] + A$result[i])/2
}

i=0
linhas <- seq(1, nrow(B), 1)
B$sse <- c(0.0)
for (i in linhas){
  B$sse[i] <- (B$V4718[i] + B$result[i])/2
}

i=0
linhas <- seq(1, nrow(C), 1)
C$sse <- c(0.0)
for (i in linhas){
  C$sse[i] <- (C$V4718[i] + C$result[i])/2
}

i=0
linhas <- seq(1, nrow(D), 1)
D$sse <- c(0.0)
for (i in linhas){
  D$sse[i] <- (D$V4718[i] + D$result[i])/2
}

i=0
linhas <- seq(1, nrow(E), 1)
E$sse <- c(0.0)
for (i in linhas){
  E$sse[i] <- (E$V4718[i] + E$result[i])/2
}

i=0
linhas <- seq(1, nrow(F), 1)
F$sse <- c(0.0)
for (i in linhas){
  F$sse[i] <- (F$V4718[i] + F$result[i])/2
}

i=0
linhas <- seq(1, nrow(G), 1)
G$sse <- c(0.0)
for (i in linhas){
  G$sse[i] <- (G$V4718[i] + G$result[i])/2
}

i=0
linhas <- seq(1, nrow(H), 1)
H$sse <- c(0.0)
for (i in linhas){
  H$sse[i] <- (H$V4718[i] + H$result[i])/2
}

i=0
linhas <- seq(1, nrow(I), 1)
I$sse <- c(0.0)
for (i in linhas){
  I$sse[i] <- (I$V4718[i] + I$result[i])/2
}

i=0
linhas <- seq(1, nrow(J), 1)
J$sse <- c(0.0)
for (i in linhas){
  J$sse[i] <- (J$V4718[i] + J$result[i])/2
}

# Juntando os resultados em um unico dataframe
socup = rbind.data.frame( B, C, D, E, F, G, H, I, J)
core <- ddply(socup, c("V9906"), summarise, total=mean(sse)) 
escore$p <-((escore$total-min(escore$total))/(max(escore$total)-min(escore$total)))*100

#Atribuindo classe
esc <- read_xlsx("ocup_CBO.xlsx")
linhas <- seq(1, nrow(escore), 1)
d <- c(escore$V9906)
esc <- esc[esc$cod %in% d, ]
names(escore)[1] <- c("cod")

escores <- merge.default(escore, esc, by= "cod")

descritiva <- ddply(escores, c("classe"), summarise, media= mean(p) , desvio= sd(p))


###INDICADORES DE MOBILIDADE
amostra_mob <- subset(base, base$idade>19 & base$idade<65 & base$sexo==2 & base$V0401==1  & base$v9001==1 & base$V0305==2 & base$V32019!="NA" )

#Identificando as classe

#####Para os pais
#Superior
superior <- subset(escala, escala$classe=="superior")
sup <-c(superior$cod)
superior <- amostra_mob[amostra_mob$V32019 %in% sup, ]
superior$classe_pai <- c("superior")

#Medio superior
ms <- subset(escala, escala$classe=="medio superior")
medsup <-c(ms$cod)
ms <- amostra_mob[amostra_mob$V32019 %in% medsup, ]
ms$classe_pai <- c("medio superior")

#Medio medio
mm <- subset(escala, escala$classe=="medio medio")
medm <-c(mm$cod)
mm <- amostra_mob[amostra_mob$V32019 %in% medm, ]
mm$classe_pai <- c("medio medio")

#Medio inferior
mi <- subset(escala, escala$classe=="medio inferior")
medinf <-c(mi$cod)
mi <- amostra_mob[amostra_mob$V32019 %in% medinf, ]
mi$classe_pai <- c("medio inferior")

#Baixo superior
bs <- subset(escala, escala$classe=="baixo superior")
bsup <-c(bs$cod)
bs <- amostra_mob[amostra_mob$V32019 %in% bsup, ]
bs$classe_pai <- c("baixo superior")

#Baixo inferior
bi <- subset(escala, escala$classe=="baixo inferior")
binf <-c(bi$cod)
bi <- amostra_mob[amostra_mob$V32019 %in% binf, ]
bi$classe_pai <- c("baixo inferior")

######Para os filhos
#Superior
superiorf <- subset(escala, escala$classe=="superior")
supf <-c(superiorf$cod)
superiorf <- amostra_mob[amostra_mob$V9906 %in% supf, ]
superiorf$classe_filho <- c("superior")

#Medio superior
msf <- subset(escala, escala$classe=="medio superior")
medsupf <-c(msf$cod)
msf <- amostra_mob[amostra_mob$V9906 %in% medsupf, ]
msf$classe_filho <- c("medio superior")

#Medio medio
mmf <- subset(escala, escala$classe=="medio medio")
medmf <-c(mmf$cod)
mmf <- amostra_mob[amostra_mob$V9906 %in% medmf, ]
mmf$classe_filho <- c("medio medio")

#Medio inferior
mif <- subset(escala, escala$classe=="medio inferior")
medinf <-c(mif$cod)
mif <- amostra_mob[amostra_mob$V9906 %in% medinf, ]
mif$classe_filho <- c("medio inferior")

#Baixo superior
bsf <- subset(escala, escala$classe=="baixo superior")
bsupf <-c(bsf$cod)
bsf <- amostra_mob[amostra_mob$V9906 %in% bsupf, ]
bsf$classe_filho <- c("baixo superior")

#Baixo inferior
bif <- subset(escala, escala$classe=="baixo inferior")
binff <-c(bif$cod)
bif <- amostra_mob[amostra_mob$V9906 %in% binff, ]
bif$classe_filho <- c("baixo inferior")

pais <- rbind.data.frame(superior, ms, mm, mi, bs, bi)
filhos <- rbind.data.frame(superiorf, msf, mmf, mif, bsf, bif)

amostra <- merge(pais, filhos )


table(amostra$classe_pai, amostra$classe_filho)
table(amostra$classe_pai, amostra$esc)
table(amostra$classe_filho)
mean(amostra[amostra$esc!=16 & 
               amostra$classe_filho=="baixo inferior", 
             "esc"])

table(amostra$classe_filho, amostra$UF)
as.factor(amostra$V32012)
table(amostra$classe_filho, amostra$V32012)

table(amostra$classe_filho, amostra$faixa)

n <- c(11:17, 21:28)
nneco <- amostra[amostra$UF %in% n,]
table(nneco$classe_pai, nneco$classe_filho)


s <- c(31:35, 41:43, 50:53)
sse <- amostra[amostra$UF %in% s, ]
table(sse$classe_pai, sse$classe_filho)


###Com peso
#Superior
ss <- subset(sse, sse$classe_pai=="superior" & sse$classe_filho=="superior")
sum(ss$V4729)

sms <- subset(sse, sse$classe_pai=="superior" & sse$classe_filho=="medio superior")
sum(sms$V4729)

smm <- subset(sse, sse$classe_pai=="superior" & sse$classe_filho=="medio medio")
sum(smm$V4729) 

smi <- subset(sse, sse$classe_pai=="superior" & sse$classe_filho=="medio inferior")
sum(smi$V4729)

sbs <- subset(sse, sse$classe_pai=="superior" & sse$classe_filho=="baixo superior")
sum(sbs$V4729)

sbi <- subset(sse, sse$classe_pai=="superior" & sse$classe_filho=="baixo inferior")
sum(sbi$V4729)

#Medio superior
ss <- subset(sse, sse$classe_pai=="medio superior" & sse$classe_filho=="superior")
sum(ss$V4729)

sms <- subset(sse, sse$classe_pai=="medio superior" & sse$classe_filho=="medio superior")
sum(sms$V4729)

smm <- subset(sse, sse$classe_pai=="medio superior" & sse$classe_filho=="medio medio")
sum(smm$V4729) 

smi <- subset(sse, sse$classe_pai=="medio superior" & sse$classe_filho=="medio inferior")
sum(smi$V4729)

sbs <- subset(sse, sse$classe_pai=="medio superior" & sse$classe_filho=="baixo superior")
sum(sbs$V4729)

sbi <- subset(sse, sse$classe_pai=="medio superior" & sse$classe_filho=="baixo inferior")
sum(sbi$V4729)

#Medio medio
ss <- subset(sse, sse$classe_pai=="medio medio" & sse$classe_filho=="superior")
sum(ss$V4729)

sms <- subset(sse, sse$classe_pai=="medio medio" & sse$classe_filho=="medio superior")
sum(sms$V4729)

smm <- subset(sse, sse$classe_pai=="medio medio" & sse$classe_filho=="medio medio")
sum(smm$V4729) 

smi <- subset(sse, sse$classe_pai=="medio medio" & sse$classe_filho=="medio inferior")
sum(smi$V4729)

sbs <- subset(sse, sse$classe_pai=="medio medio" & sse$classe_filho=="baixo superior")
sum(sbs$V4729)

sbi <- subset(sse, sse$classe_pai=="medio medio" & sse$classe_filho=="baixo inferior")
sum(sbi$V4729)

#Medio inferior
ss <- subset(sse, sse$classe_pai=="medio inferior" & sse$classe_filho=="superior")
sum(ss$V4729)

sms <- subset(sse, sse$classe_pai=="medio inferior" & sse$classe_filho=="medio superior")
sum(sms$V4729)

smm <- subset(sse, sse$classe_pai=="medio inferior" & sse$classe_filho=="medio medio")
sum(smm$V4729) 

smi <- subset(sse, sse$classe_pai=="medio inferior" & sse$classe_filho=="medio inferior")
sum(smi$V4729)

sbs <- subset(sse, sse$classe_pai=="medio inferior" & sse$classe_filho=="baixo superior")
sum(sbs$V4729)

sbi <- subset(sse, sse$classe_pai=="medio inferior" & sse$classe_filho=="baixo inferior")
sum(sbi$V4729)

#Baixo superior
ss <- subset(sse, sse$classe_pai=="baixo superior" & sse$classe_filho=="superior")
sum(ss$V4729)

sms <- subset(sse, sse$classe_pai=="baixo superior" & sse$classe_filho=="medio superior")
sum(sms$V4729)

smm <- subset(sse, sse$classe_pai=="baixo superior" & sse$classe_filho=="medio medio")
sum(smm$V4729) 

smi <- subset(sse, sse$classe_pai=="baixo superior" & sse$classe_filho=="medio inferior")
sum(smi$V4729)

sbs <- subset(sse, sse$classe_pai=="baixo superior" & sse$classe_filho=="baixo superior")
sum(sbs$V4729)

sbi <- subset(sse, sse$classe_pai=="baixo superior" & sse$classe_filho=="baixo inferior")
sum(sbi$V4729)

#Baixo inferior
ss <- subset(sse, sse$classe_pai=="baixo inferior" & sse$classe_filho=="superior")
sum(ss$V4729)

sms <- subset(sse, sse$classe_pai=="baixo inferior" & sse$classe_filho=="medio superior")
sum(sms$V4729)

smm <- subset(sse, sse$classe_pai=="baixo inferior" & sse$classe_filho=="medio medio")
sum(smm$V4729) 

smi <- subset(sse, sse$classe_pai=="baixo inferior" & sse$classe_filho=="medio inferior")
sum(smi$V4729)

sbs <- subset(sse, sse$classe_pai=="baixo inferior" & sse$classe_filho=="baixo superior")
sum(sbs$V4729)

sbi <- subset(sse, sse$classe_pai=="baixo inferior" & sse$classe_filho=="baixo inferior")
sum(sbi$V4729)


#Criando categorias para faixas de idade de 5 em 5 anos
linhas <- seq(1, nrow(amostra), 1)
i=0
amostra$faixa <- c(0.0)
for (i in linhas){
  if (amostra$idade[i]>19)
    if(amostra$idade[i]<25){
      amostra$faixa[i] <- c("20-24")
    }else if(amostra$idade[i]>24)
      if(amostra$idade[i]<30){
        amostra$faixa[i] <- c("25-29")
      }else if(amostra$idade[i]>29)
        if(amostra$idade[i]<35){
          amostra$faixa[i] <- c("30-34")
        }else if(amostra$idade[i]>34)
          if(amostra$idade[i]<40){
            amostra$faixa[i] <- c("35-39")
          }else if(amostra$idade[i]>39)
            if(amostra$idade[i]<45){
              amostra$faixa[i] <- c("40-44")
            }else if(amostra$idade[i]>44)
              if(amostra$idade[i]<50){
                amostra$faixa[i] <- c("45-49")
              }else if(amostra$idade[i]>49)
                if(amostra$idade[i]<55){
                  amostra$faixa[i] <- c("50-54")
                }else if(amostra$idade[i]>54)
                  if(amostra$idade[i]<60){
                    amostra$faixa[i] <- c("55-59")
                  }else if(amostra$idade[i]>54)
                    if(amostra$idade[i]<60){
                      amostra$faixa[i] <- c("55-59")
                    }else if(amostra$idade[i]>59)
                      if(amostra$idade[i]<65){
                        amostra$faixa[i] <-c("60-64")
                      }
}

#Alterando as variveis
amostra$esc <- amostra$V4803-1


### Mobilidade para chefas de familia
amostra_mob <- subset(base, base$idade>19 & base$idade<65 & base$V0401==1  & base$v9001==1 & base$V0305==2 & base$V32033!="NA" )


#Identificando as classe

#####Para as maes
#Superior
superior <- subset(escala, escala$classe=="superior")
sup <-c(superior$cod)
superior <- amostra_mob[amostra_mob$V32033 %in% sup, ]
superior$classe_mae <- c("superior")

#Medio superior
ms <- subset(escala, escala$classe=="medio superior")
medsup <-c(ms$cod)
ms <- amostra_mob[amostra_mob$V32033 %in% medsup, ]
ms$classe_mae <- c("medio superior")

#Medio medio
mm <- subset(escala, escala$classe=="medio medio")
medm <-c(mm$cod)
mm <- amostra_mob[amostra_mob$V32033 %in% medm, ]
mm$classe_mae <- c("medio medio")

#Medio inferior
mi <- subset(escala, escala$classe=="medio inferior")
medinf <-c(mi$cod)
mi <- amostra_mob[amostra_mob$V32033 %in% medinf, ]
mi$classe_mae <- c("medio inferior")

#Baixo superior
bs <- subset(escala, escala$classe=="baixo superior")
bsup <-c(bs$cod)
bs <- amostra_mob[amostra_mob$V32033 %in% bsup, ]
bs$classe_mae <- c("baixo superior")

#Baixo inferior
bi <- subset(escala, escala$classe=="baixo inferior")
binf <-c(bi$cod)
bi <- amostra_mob[amostra_mob$V32033 %in% binf, ]
bi$classe_mae <- c("baixo inferior")

######Para os filhos
#Superior
superiorf <- subset(escala, escala$classe=="superior")
supf <-c(superiorf$cod)
superiorf <- amostra_mob[amostra_mob$V9906 %in% supf, ]
superiorf$classe_filho <- c("superior")

#Medio superior
msf <- subset(escala, escala$classe=="medio superior")
medsupf <-c(msf$cod)
msf <- amostra_mob[amostra_mob$V9906 %in% medsupf, ]
msf$classe_filho <- c("medio superior")

#Medio medio
mmf <- subset(escala, escala$classe=="medio medio")
medmf <-c(mmf$cod)
mmf <- amostra_mob[amostra_mob$V9906 %in% medmf, ]
mmf$classe_filho <- c("medio medio")

#Medio inferior
mif <- subset(escala, escala$classe=="medio inferior")
medinf <-c(mif$cod)
mif <- amostra_mob[amostra_mob$V9906 %in% medinf, ]
mif$classe_filho <- c("medio inferior")

#Baixo superior
bsf <- subset(escala, escala$classe=="baixo superior")
bsupf <-c(bsf$cod)
bsf <- amostra_mob[amostra_mob$V9906 %in% bsupf, ]
bsf$classe_filho <- c("baixo superior")

#Baixo inferior
bif <- subset(escala, escala$classe=="baixo inferior")
binff <-c(bif$cod)
bif <- amostra_mob[amostra_mob$V9906 %in% binff, ]
bif$classe_filho <- c("baixo inferior")

mae <- rbind.data.frame(superior, ms, mm, mi, bs, bi)
filhos <- rbind.data.frame(superiorf, msf, mmf, mif, bsf, bif)

amostra_maes <- merge(mae, filhos)
table(amostra_maes$classe_mae, amostra_maes$classe_filho)


###Com peso
#Superior
ss <- subset(amostra_maes, amostra_maes$classe_mae=="superior" & amostra_maes$classe_filho=="superior")
sum(ss$V4729)

sms <- subset(amostra_maes, amostra_maes$classe_mae=="superior" & amostra_maes$classe_filho=="medio superior")
sum(sms$V4729)

smm <- subset(amostra_maes, amostra_maes$classe_mae=="superior" & amostra_maes$classe_filho=="medio medio")
sum(smm$V4729) 

smi <- subset(amostra_maes, amostra_maes$classe_mae=="superior" & amostra_maes$classe_filho=="medio inferior")
sum(smi$V4729)

sbs <- subset(amostra_maes, amostra_maes$classe_mae=="superior" & amostra_maes$classe_filho=="baixo superior")
sum(sbs$V4729)

sbi <- subset(amostra_maes, amostra_maes$classe_mae=="superior" & amostra_maes$classe_filho=="baixo inferior")
sum(sbi$V4729)

#Medio superior
ss <- subset(amostra_maes, amostra_maes$classe_mae=="medio superior" & amostra_maes$classe_filho=="superior")
sum(ss$V4729)

sms <- subset(amostra_maes, amostra_maes$classe_mae=="medio superior" & amostra_maes$classe_filho=="medio superior")
sum(sms$V4729)

smm <- subset(amostra_maes, amostra_maes$classe_mae=="medio superior" & amostra_maes$classe_filho=="medio medio")
sum(smm$V4729) 

smi <- subset(amostra_maes, amostra_maes$classe_mae=="medio superior" & amostra_maes$classe_filho=="medio inferior")
sum(smi$V4729)

sbs <- subset(amostra_maes, amostra_maes$classe_mae=="medio superior" & amostra_maes$classe_filho=="baixo superior")
sum(sbs$V4729)

sbi <- subset(amostra_maes, amostra_maes$classe_mae=="medio superior" & amostra_maes$classe_filho=="baixo inferior")
sum(sbi$V4729)

#Medio medio
ss <- subset(amostra_maes, amostra_maes$classe_mae=="medio medio" & amostra_maes$classe_filho=="superior")
sum(ss$V4729)

sms <- subset(amostra_maes, amostra_maes$classe_mae=="medio medio" & amostra_maes$classe_filho=="medio superior")
sum(sms$V4729)

smm <- subset(amostra_maes, amostra_maes$classe_mae=="medio medio" & amostra_maes$classe_filho=="medio medio")
sum(smm$V4729) 

smi <- subset(amostra_maes, amostra_maes$classe_mae=="medio medio" & amostra_maes$classe_filho=="medio inferior")
sum(smi$V4729)

sbs <- subset(amostra_maes, amostra_maes$classe_mae=="medio medio" & amostra_maes$classe_filho=="baixo superior")
sum(sbs$V4729)

sbi <- subset(amostra_maes, amostra_maes$classe_mae=="medio medio" & amostra_maes$classe_filho=="baixo inferior")
sum(sbi$V4729)

#Medio inferior
ss <- subset(amostra_maes, amostra_maes$classe_mae=="medio inferior" & amostra_maes$classe_filho=="superior")
sum(ss$V4729)

sms <- subset(amostra_maes, amostra_maes$classe_mae=="medio inferior" & amostra_maes$classe_filho=="medio superior")
sum(sms$V4729)

smm <- subset(amostra_maes, amostra_maes$classe_mae=="medio inferior" & amostra_maes$classe_filho=="medio medio")
sum(smm$V4729) 

smi <- subset(amostra_maes, amostra_maes$classe_mae=="medio inferior" & amostra_maes$classe_filho=="medio inferior")
sum(smi$V4729)

sbs <- subset(amostra_maes, amostra_maes$classe_mae=="medio inferior" & amostra_maes$classe_filho=="baixo superior")
sum(sbs$V4729)

sbi <- subset(amostra_maes, amostra_maes$classe_mae=="medio inferior" & amostra_maes$classe_filho=="baixo inferior")
sum(sbi$V4729)

#Baixo superior
ss <- subset(amostra_maes, amostra_maes$classe_mae=="baixo superior" & amostra_maes$classe_filho=="superior")
sum(ss$V4729)

sms <- subset(amostra_maes, amostra_maes$classe_mae=="baixo superior" & amostra_maes$classe_filho=="medio superior")
sum(sms$V4729)

smm <- subset(amostra_maes, amostra_maes$classe_mae=="baixo superior" & amostra_maes$classe_filho=="medio medio")
sum(smm$V4729) 

smi <- subset(amostra_maes, amostra_maes$classe_mae=="baixo superior" & amostra_maes$classe_filho=="medio inferior")
sum(smi$V4729)

sbs <- subset(amostra_maes, amostra_maes$classe_mae=="baixo superior" & amostra_maes$classe_filho=="baixo superior")
sum(sbs$V4729)

sbi <- subset(amostra_maes, amostra_maes$classe_mae=="baixo superior" & amostra_maes$classe_filho=="baixo inferior")
sum(sbi$V4729)

#Baixo inferior
ss <- subset(amostra_maes, amostra_maes$classe_mae=="baixo inferior" & amostra_maes$classe_filho=="superior")
sum(ss$V4729)

sms <- subset(amostra_maes, amostra_maes$classe_mae=="baixo inferior" & amostra_maes$classe_filho=="medio superior")
sum(sms$V4729)

smm <- subset(amostra_maes, amostra_maes$classe_mae=="baixo inferior" & amostra_maes$classe_filho=="medio medio")
sum(smm$V4729) 

smi <- subset(amostra_maes, amostra_maes$classe_mae=="baixo inferior" & amostra_maes$classe_filho=="medio inferior")
sum(smi$V4729)

sbs <- subset(amostra_maes, amostra_maes$classe_mae=="baixo inferior" & amostra_maes$classe_filho=="baixo superior")
sum(sbs$V4729)

sbi <- subset(amostra_maes, amostra_maes$classe_mae=="baixo inferior" & amostra_maes$classe_filho=="baixo inferior")
sum(sbi$V4729)
