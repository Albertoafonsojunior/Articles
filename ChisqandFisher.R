# Chisq and Fisher exact test (signiicance level = 0,05)

# install.packages("data.table")
library(data.table)
library(MASS)
library(dplyr)
library(daff)

#____________________________________________________________________


# framinghan
mydata <- read.csv(file = '~/ana/article_data.csv', header = TRUE)

# times 100
for (i in 1:nrow(mydata)){
ifelse(mydata$RISCO_DAC_10_ANOS[i]!="NA", 
mydata$RISCO_DAC_10_ANOS[i] <- 100*mydata$RISCO_DAC_10_ANOS[i], 
TRUE)}

HAS <- array(NA, dim = nrow(mydata))
for (i in 1: nrow(mydata)){
ifelse(mydata$PAS[i] >= 140 | mydata$PAD[i] >= 90, HAS[i] <- 1,
ifelse((mydata$PAS[i] >= 140 & mydata$PAD[i] >= 90), HAS[i] <- 1,
ifelse((mydata$PAS[i] < 140 & mydata$PAD[i] < 90) & mydata$Trato..HAS[i] == 0, 
HAS[i] <- 0,ifelse(mydata$Trato..HAS[i] == 1, HAS[i] <- 1,
ifelse(mydata$ID[i] == c(34,257), mydata$IMC.Classif.[i] <- "Sobrepeso",
TRUE)))))
}

DAC10ANOS <- array(NA, dim = nrow(mydata))
for (i in 1: nrow(mydata))
{
ifelse(mydata$RISCO_DAC_10_ANOS[i] < 10, DAC10ANOS[i] <- "BAIXO",
ifelse(mydata$RISCO_DAC_10_ANOS[i] >= 10 & mydata$RISCO_DAC_10_ANOS[i] <= 19.9, DAC10ANOS[i] <- "MODERADO",
ifelse(mydata$RISCO_DAC_10_ANOS[i] >= 20, DAC10ANOS[i] <- "ALTO", TRUE)))
}

DAC10ANOSIN <- array(NA, dim = nrow(mydata))
for (i in 1: nrow(mydata))
{
ifelse(mydata$RISCO_DAC_10_ANOS[i] < 10, DAC10ANOSIN[i] <- "BAIXO",
ifelse(mydata$RISCO_DAC_10_ANOS[i] >= 10, DAC10ANOSIN[i] <- "MODEALTO", TRUE))
}

mydata <- cbind(mydata, HAS, DAC10ANOS, DAC10ANOSIN)

#_____________________________________________________________________

# COLESTEROL TOTAL
ColTotal <- array(NA, dim = 230)
for (i in 1:230){
  ifelse(mydata$COLESTEROL[i]>=190, ColTotal[i] <- "SIM", 
         ifelse(ColTotal[i] <- "NAO", TRUE))
}

# TRIGLICERIDES
TriGlice <- array(NA, dim = 230)
for (i in 1:230){
  ifelse(mydata$TG[i]>=150, TriGlice[i] <- "SIM", 
         ifelse(TriGlice[i] <- "NAO", TRUE))
}

# HDL -COLTESTEROL ANORMAL
HDLCLTan <- array(NA, dim = 230)
for (i in 1:230){
  ifelse(mydata$HDL[i]<=40, HDLCLTan[i] <- "SIM", 
         ifelse(HDLCLTan[i] <- "NAO", TRUE))
}

# LDL - COLTESTEROL ELEVADO
LDLCLTel <- array(NA, dim = 230)
for (i in 1:230){
  ifelse(mydata$LDL[i]>130, LDLCLTel[i] <- "SIM", 
         ifelse(LDLCLTel[i] <- "NAO", TRUE))
}

# HIPERGLICEMIA
HiperGlicemia <- array(NA, dim = 230)
for (i in 1:230){
  ifelse(mydata$GLICEMIA[i]>130, HiperGlicemia[i] <- "SIM", 
         ifelse(HiperGlicemia[i] <- "NAO", TRUE))
}

# TABAGISMO
Tabag <- array(NA, dim = 230)
for (i in 1:230){
  ifelse(mydata$TABAGISMO[i]==1, Tabag[i] <- "SIM", 
         ifelse(Tabag[i] <- "NAO", TRUE))
}

mydata <- cbind(mydata, Tabag, ColTotal, HDLCLTan, LDLCLTel, HiperGlicemia, TriGlice)

#_______________________________________________________

tbl1 <- table(mydata$HAS, mydata$DAC10ANOSIN)
tbl2 <- table(mydata$Tabag, mydata$DAC10ANOSIN)
tbl3 <- table(mydata$ColTotal, mydata$DAC10ANOSIN)
tbl4 <- table(mydata$HDLCLTan, mydata$DAC10ANOSIN)
tbl5 <- table(mydata$LDLCLTel, mydata$DAC10ANOSIN)
tbl6 <- table(mydata$TriGlice, mydata$DAC10ANOSIN)
tbl7 <- table(mydata$HiperGlicemia, mydata$DAC10ANOSIN)
tbl8 <- table(mydata$Clas..Eco, mydata$DAC10ANOSIN)

ctbl <- rbind(tbl8["A1", ]+tbl8["B1", ]+
        tbl8["B2",], tbl8["C1", ]+tbl8["C2", ], tbl8["D-E",] )


# table larger than 2 x 2, chisq: 1, 3, 5, 6

hytest <- chisq.test(ctbl, simulate.p.value = T, B = 1e9)
hytest$expected
hytest$p.value

# fisher: 2, 4, 7
fisher.test(ctbl, simulate.p.value = TRUE, B=1e2)


# Extract rows from dataframe
one <- mydata %>% slice(30)

two <- mydata %>% filter(ID %in% c(34,257))

patern <- subset(mydata, Clas..Eco == "D-E" & DAC10ANOSIN == "MODEALTO",
          select = c(ID, SEXO, Clas..Eco, DAC10ANOSIN))

# the first 3 columns of the last two observations
tail(x = mydata, n = 3)[1:3]

# mydatahas <- mydata[, c(1:9)] 
# write.csv(x = mydatahas, "HAS.csv", row.names = FALSE)

#__________________________________________________________

sexo <- mydata %>%
  group_by(SEXO) %>%
  summarise(GENERO=n())

dac <- mydata %>%
  group_by(SEXO) %>%
  summarise(DAC = sum(RISCO_DAC_10_ANOS,na.rm = T))

# Rejeita-se a hipótese nula, ou seja, "Não há associação entre
# gênero e Doença arterial coronariana", pois 
# 1.261e-05 << 0,05.