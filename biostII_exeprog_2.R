# ALBERTO AFONSO JÚNIOR  enviar para o email: mcm5909@gmail.com
# CRISTIANO JOSÉ MENDES PINTO


# EXERCICIO PROGRAMA 2: ESCORE DE RISCO DE FRAMINGHAM (RISCO DE DAC EM 10 ANOS)

# definindo o directorio de trabalho
setwd('/media/afonso/Transcend/biostatistic_II/')

# lendo o banco de dados pelo nome "banco"
banco <- read.csv('Exercicio_programa_01.csv')

# RISCO NOS HOMENS

# Pontuacao para a variavel IDADE
IDADE_PONTOS <- array(NA, dim = 3000)
for (i in 1:1500){
ifelse (banco$IDADE[i] >= 30 & banco$IDADE[i] <= 34, IDADE_PONTOS[i] <- -1, 
ifelse (banco$IDADE[i] >= 35 & banco$IDADE[i] <= 39, IDADE_PONTOS[i] <- 0,
ifelse (banco$IDADE[i] >= 40 & banco$IDADE[i] <= 44, IDADE_PONTOS[i] <- 1, 
ifelse (banco$IDADE[i] >= 45 & banco$IDADE[i] <= 49, IDADE_PONTOS[i] <- 2,
ifelse (banco$IDADE[i] >= 50 & banco$IDADE[i] <= 54, IDADE_PONTOS[i] <- 3,
ifelse (banco$IDADE[i] >= 55 & banco$IDADE[i] <= 59, IDADE_PONTOS[i] <- 4,
ifelse (banco$IDADE[i] >= 55 & banco$IDADE[i] <= 64, IDADE_PONTOS[i] <- 5, 
ifelse (banco$IDADE[i] >= 65 & banco$IDADE[i] <= 69, IDADE_PONTOS[i] <- 6,
ifelse (banco$IDADE[i] >= 70 & banco$IDADE[i] <= 74, IDADE_PONTOS[i] <- 7, TRUE)))))))))}
banco <- cbind(banco, IDADE_PONTOS)

# Pontuacao para a variavel LDL
LDL_PONTOS <- array(NA, dim = 3000)
for(i in 1:1500){
ifelse(banco$LDL[i] < 100, LDL_PONTOS[i] <- -3, 
ifelse(banco$LDL[i] >= 100 & banco$LDL[i] <= 129, LDL_PONTOS[i] <- 0, 
ifelse(banco$LDL[i] >= 130 & banco$LDL[i] <= 159, LDL_PONTOS[i] <- 0,
ifelse(banco$LDL[i] >= 160 & banco$LDL[i] <= 190, LDL_PONTOS[i] <- 1, 
ifelse(banco$LDL[i] >= 190, LDL_PONTOS[i] <- 2, TRUE)))))}
banco <- cbind(banco, LDL_PONTOS)

# Pontuacao para a variavel HDL
HDL_PONTOS <- array(NA, dim = 3000)
for(i in 1:1500){
ifelse(banco$HDL[i] < 35, HDL_PONTOS[i] <- 2,  
ifelse(banco$HDL[i] >= 35 & banco$HDL[i] <= 44, HDL_PONTOS[i] <- 1,
ifelse(banco$HDL[i] >= 45 & banco$HDL[i] <= 49, HDL_PONTOS[i] <- 0,  
ifelse(banco$HDL[i] >= 50 & banco$HDL[i] <= 59, HDL_PONTOS[i] <- 0,
ifelse(banco$HDL[i] >= 60, HDL_PONTOS[i] <- -1, TRUE)))))}
banco <- cbind(banco, HDL_PONTOS)

# Pontuacao para a variavel PRESSAO ARTERIAL
PS_PONTOS <- array(NA, dim = 3000)
for(i in 1:1500){
ifelse(banco$PAS[i] < 120 & banco$PAD[i] < 80, PS_PONTOS[i] <- 0,
ifelse(banco$PAS[i] < 120 & (banco$PAD[i] >= 80 & banco$PAD[i] <= 84), PS_PONTOS[i] <- 0,
ifelse(banco$PAS[i] < 120 & (banco$PAD[i] >= 85 & banco$PAD[i] <= 89), PS_PONTOS[i] <- 1,
ifelse(banco$PAS[i] < 120 & (banco$PAD[i] >= 90 & banco$PAD[i] <= 99), PS_PONTOS[i] <- 2,
ifelse(banco$PAS[i] < 120 & banco$PAD[i] >= 100, PS_PONTOS[i] <- 3,
ifelse((banco$PAS[i] >= 120 & banco$PAS[i] <= 129) & (banco$PAD[i] >= 80 & banco$PAD[i] <= 84), PS_PONTOS[i] <- 0,
ifelse((banco$PAS[i] >= 120 & banco$PAS[i] <= 129) & (banco$PAD[i] >= 85 & banco$PAD[i] <= 89), PS_PONTOS[i] <- 1,
ifelse((banco$PAS[i] >= 120 & banco$PAS[i] <= 129) & (banco$PAD[i] >= 90 & banco$PAD[i] <= 99), PS_PONTOS[i] <- 2,
ifelse((banco$PAS[i] >= 120 & banco$PAS[i] <= 129) & banco$PAD[i] >= 100, PS_PONTOS[i] <- 3,
ifelse((banco$PAS[i] >= 130 & banco$PAS[i] <= 139) & (banco$PAD[i] >= 80 & banco$PAD[i] <= 84), PS_PONTOS[i] <- 1,
ifelse((banco$PAS[i] >= 130 & banco$PAS[i] <= 139) & (banco$PAD[i] >= 85 & banco$PAD[i] <= 89), PS_PONTOS[i] <- 1,
ifelse((banco$PAS[i] >= 130 & banco$PAS[i] <= 139) & (banco$PAD[i] >= 90 & banco$PAD[i] <= 99), PS_PONTOS[i] <- 2,
ifelse((banco$PAS[i] >= 130 & banco$PAS[i] <= 139) & banco$PAD[i] >= 100, PS_PONTOS[i] <- 3,
ifelse((banco$PAS[i] >= 140 & banco$PAS[i] <= 159) & (banco$PAD[i] >= 80 & banco$PAD[i] <= 84), PS_PONTOS[i] <- 2,
ifelse((banco$PAS[i] >= 140 & banco$PAS[i] <= 159) & (banco$PAD[i] >= 85 & banco$PAD[i] <= 89), PS_PONTOS[i] <- 2,
ifelse((banco$PAS[i] >= 140 & banco$PAS[i] <= 159) & (banco$PAD[i] >= 90 & banco$PAD[i] <= 99), PS_PONTOS[i] <- 2,
ifelse((banco$PAS[i] >= 140 & banco$PAS[i] <= 159) & banco$PAD[i] >= 100, PS_PONTOS[i] <- 3,
ifelse(banco$PAS[i] >= 160 & (banco$PAD[i] >= 80 & banco$PAD[i] <= 84), PS_PONTOS[i] <- 3,
ifelse(banco$PAS[i] >= 160 & (banco$PAD[i] >= 85 & banco$PAD[i] <= 89), PS_PONTOS[i] <- 3,
ifelse(banco$PAS[i] >= 160 & (banco$PAD[i] >= 90 & banco$PAD[i] <= 99), PS_PONTOS[i] <- 3,
ifelse(banco$PAS[i] >= 160 & banco$PAD[i] >= 100, PS_PONTOS[i] <- 3,
ifelse(banco$PAD[i] < 80 & (banco$PAS[i] >= 120 & banco$PAS[i] <= 129), PS_PONTOS[i] <- 0,
ifelse(banco$PAD[i] < 80 & (banco$PAS[i] >= 130 & banco$PAS[i] <= 139), PS_PONTOS[i] <- 1,
ifelse(banco$PAD[i] < 80 & (banco$PAS[i] >= 140 & banco$PAS[i] <= 159), PS_PONTOS[i] <- 2,
ifelse(banco$PAD[i] < 80 & banco$PAS[i] >= 160, PS_PONTOS[i] <- 3, TRUE)))))))))))))))))))))))))}
banco <- cbind(banco, PS_PONTOS)

# Pontuacao para a variavel DIABETES
DIABETES_PONTOS <- array(NA, dim = 3000)
for(i in 1:1500){
ifelse(banco$DIABETES[i] == 'Sim', DIABETES_PONTOS[i] <- 2,  
ifelse(banco$DIABETES[i] == 'Nao', DIABETES_PONTOS[i] <- 0, TRUE))}
banco <- cbind(banco, DIABETES_PONTOS)

# Pontuacao para a variavel TABAGISMO
TABAGISMO_PONTOS <- array(NA, dim = 3000)
for(i in 1:1500){
ifelse(banco$TABAGISMO[i] == 'Atual', TABAGISMO_PONTOS[i] <- 2,
ifelse(banco$TABAGISMO[i] == 'Nunca', TABAGISMO_PONTOS[i] <- 0, 
ifelse(banco$TABAGISMO[i] == 'Previo', TABAGISMO_PONTOS[i] <- 0, TRUE)))}
banco <- cbind(banco, TABAGISMO_PONTOS)

# Pontuacao total: ESCORE TOTAL
ESCORE_TOTAL <- array(NA, dim = 3000)
for(i in 1:1500){
ifelse(banco$HPDAC[i] == 'Sim', ESCORE_TOTAL[i] <- NA, 
       ifelse(banco$HPDAC[i] == 'Nao', ESCORE_TOTAL[i] <- sum(banco[i, c(19:24)]), TRUE))}
banco <- cbind(banco, ESCORE_TOTAL)

# PERCENTAGEM DE RISCO NOS HOMENS
RISCO_DAC_10_ANOS <- array(NA, 3000)
for(i in 1:1500){
ifelse(banco$ESCORE_TOTAL[i] < -3, RISCO_DAC_10_ANOS[i] <- 1,
ifelse(banco$ESCORE_TOTAL[i] == -2, RISCO_DAC_10_ANOS[i] <- 2,
ifelse(banco$ESCORE_TOTAL[i] == -1, RISCO_DAC_10_ANOS[i] <- 2,
ifelse(banco$ESCORE_TOTAL[i] == 0, RISCO_DAC_10_ANOS[i] <- 3,
ifelse(banco$ESCORE_TOTAL[i] == 1, RISCO_DAC_10_ANOS[i] <- 4,
ifelse(banco$ESCORE_TOTAL[i] == 2, RISCO_DAC_10_ANOS[i] <- 4,
ifelse(banco$ESCORE_TOTAL[i] == 3, RISCO_DAC_10_ANOS[i] <- 6,
ifelse(banco$ESCORE_TOTAL[i] == 4, RISCO_DAC_10_ANOS[i] <- 7,
ifelse(banco$ESCORE_TOTAL[i] == 5, RISCO_DAC_10_ANOS[i] <- 9,
ifelse(banco$ESCORE_TOTAL[i] == 6, RISCO_DAC_10_ANOS[i] <- 11,
ifelse(banco$ESCORE_TOTAL[i] == 7, RISCO_DAC_10_ANOS[i] <- 14,
ifelse(banco$ESCORE_TOTAL[i] == 8, RISCO_DAC_10_ANOS[i] <- 18,
ifelse(banco$ESCORE_TOTAL[i] == 9, RISCO_DAC_10_ANOS[i] <- 22,
ifelse(banco$ESCORE_TOTAL[i] == 10, RISCO_DAC_10_ANOS[i] <- 27,
ifelse(banco$ESCORE_TOTAL[i] == 11, RISCO_DAC_10_ANOS[i] <- 33,
ifelse(banco$ESCORE_TOTAL[i] == 12, RISCO_DAC_10_ANOS[i] <- 40,
ifelse(banco$ESCORE_TOTAL[i] == 13, RISCO_DAC_10_ANOS[i] <- 47,
ifelse(banco$ESCORE_TOTAL[i] >= 14, RISCO_DAC_10_ANOS[i] <- 56, TRUE))))))))))))))))))}
banco <- cbind(banco, RISCO_DAC_10_ANOS)


# RISCO NAS MULHERES

# Pontuacao para a variavel IDADE
for (i in 1501:3000){
ifelse (banco$IDADE[i] >= 30 & banco$IDADE[i] <= 34, banco$IDADE_PONTOS[i] <- -9, 
ifelse (banco$IDADE[i] >= 35 & banco$IDADE[i] <= 39, banco$IDADE_PONTOS[i] <- -4,
ifelse (banco$IDADE[i] >= 40 & banco$IDADE[i] <= 44, banco$IDADE_PONTOS[i] <- 0, 
ifelse (banco$IDADE[i] >= 45 & banco$IDADE[i] <= 49, banco$IDADE_PONTOS[i] <- 3,
ifelse (banco$IDADE[i] >= 50 & banco$IDADE[i] <= 54, banco$IDADE_PONTOS[i] <- 6,
ifelse (banco$IDADE[i] >= 55 & banco$IDADE[i] <= 59, banco$IDADE_PONTOS[i] <- 7,
ifelse (banco$IDADE[i] >= 55 & banco$IDADE[i] <= 64, banco$IDADE_PONTOS[i] <- 8, 
ifelse (banco$IDADE[i] >= 65 & banco$IDADE[i] <= 69, banco$IDADE_PONTOS[i] <- 8,
ifelse (banco$IDADE[i] >= 70 & banco$IDADE[i] <= 74, banco$IDADE_PONTOS[i] <- 8, TRUE)))))))))}

# Pontuacao para a variavel LDL
for(i in 1501:3000){
ifelse(banco$LDL[i] < 100, banco$LDL_PONTOS[i] <- -2, 
ifelse(banco$LDL[i] >= 100 & banco$LDL[i] <= 129, banco$LDL_PONTOS[i] <- 0, 
ifelse(banco$LDL[i] >= 130 & banco$LDL[i] <= 159, banco$LDL_PONTOS[i] <- 0,
ifelse(banco$LDL[i] >= 160 & banco$LDL[i] <= 190, banco$LDL_PONTOS[i] <- 2, 
ifelse(banco$LDL[i] >= 190, banco$LDL_PONTOS[i] <- 2, TRUE)))))}

# Pontuacao para a variavel HDL
for(i in 1501:3000){
ifelse(banco$HDL[i] < 35, banco$HDL_PONTOS[i] <- 5,  
ifelse(banco$HDL[i] >= 35 & banco$HDL[i] <= 44, banco$HDL_PONTOS[i] <- 2,
ifelse(banco$HDL[i] >= 45 & banco$HDL[i] <= 49, banco$HDL_PONTOS[i] <- 1,  
ifelse(banco$HDL[i] >= 50 & banco$HDL[i] <= 59, banco$HDL_PONTOS[i] <- 0,
ifelse(banco$HDL[i] >= 60, banco$HDL_PONTOS[i] <- -2, TRUE)))))}

# Pontuacao para a variavel PRESSAO SANGUINEA
for(i in 1501:3000){
ifelse(banco$PAS[i] < 120 & banco$PAD[i] < 80, banco$PS_PONTOS[i] <- -3,
ifelse(banco$PAS[i] < 120 & (banco$PAD[i] >= 80 & banco$PAD[i] <= 84), banco$PS_PONTOS[i] <- 0,
ifelse(banco$PAS[i] < 120 & (banco$PAD[i] >= 85 & banco$PAD[i] <= 89), banco$PS_PONTOS[i] <- 0,
ifelse(banco$PAS[i] < 120 & (banco$PAD[i] >= 90 & banco$PAD[i] <= 99), banco$PS_PONTOS[i] <- 2,
ifelse(banco$PAS[i] < 120 & banco$PAD[i] >= 100, banco$PS_PONTOS[i] <- 3,
ifelse((banco$PAS[i] >= 120 & banco$PAS[i] <= 129) & (banco$PAD[i] >= 80 & banco$PAD[i] <= 84), banco$PS_PONTOS[i] <- 0,
ifelse((banco$PAS[i] >= 120 & banco$PAS[i] <= 129) & (banco$PAD[i] >= 85 & banco$PAD[i] <= 89), banco$PS_PONTOS[i] <- 0,
ifelse((banco$PAS[i] >= 120 & banco$PAS[i] <= 129) & (banco$PAD[i] >= 90 & banco$PAD[i] <= 99), banco$PS_PONTOS[i] <- 2,
ifelse((banco$PAS[i] >= 120 & banco$PAS[i] <= 129) & banco$PAD[i] >= 100, banco$PS_PONTOS[i] <- 3,
ifelse((banco$PAS[i] >= 130 & banco$PAS[i] <= 139) & (banco$PAD[i] >= 80 & banco$PAD[i] <= 84), banco$PS_PONTOS[i] <- 0,
ifelse((banco$PAS[i] >= 130 & banco$PAS[i] <= 139) & (banco$PAD[i] >= 85 & banco$PAD[i] <= 89), banco$PS_PONTOS[i] <- 0,
ifelse((banco$PAS[i] >= 130 & banco$PAS[i] <= 139) & (banco$PAD[i] >= 90 & banco$PAD[i] <= 99), banco$PS_PONTOS[i] <- 2,
ifelse((banco$PAS[i] >= 130 & banco$PAS[i] <= 139) & banco$PAD[i] >= 100, PS_PONTOS[i] <- 3,
ifelse((banco$PAS[i] >= 140 & banco$PAS[i] <= 159) & (banco$PAD[i] >= 80 & banco$PAD[i] <= 84), banco$PS_PONTOS[i] <- 2,
ifelse((banco$PAS[i] >= 140 & banco$PAS[i] <= 159) & (banco$PAD[i] >= 85 & banco$PAD[i] <= 89), banco$PS_PONTOS[i] <- 2,
ifelse((banco$PAS[i] >= 140 & banco$PAS[i] <= 159) & (banco$PAD[i] >= 90 & banco$PAD[i] <= 99), banco$PS_PONTOS[i] <- 2,
ifelse((banco$PAS[i] >= 140 & banco$PAS[i] <= 159) & banco$PAD[i] >= 100, banco$PS_PONTOS[i] <- 3,
ifelse(banco$PAS[i] >= 160 & (banco$PAD[i] >= 80 & banco$PAD[i] <= 84), banco$PS_PONTOS[i] <- 3,
ifelse(banco$PAS[i] >= 160 & (banco$PAD[i] >= 85 & banco$PAD[i] <= 89), banco$PS_PONTOS[i] <- 3,
ifelse(banco$PAS[i] >= 160 & (banco$PAD[i] >= 90 & banco$PAD[i] <= 99), banco$PS_PONTOS[i] <- 3,
ifelse(banco$PAS[i] >= 160 & banco$PAD[i] >= 100, banco$PS_PONTOS[i] <- 3,
ifelse(banco$PAD[i] < 80 & (banco$PAS[i] >= 120 & banco$PAS[i] <= 129), banco$PS_PONTOS[i] <- 0,
ifelse(banco$PAD[i] < 80 & (banco$PAS[i] >= 130 & banco$PAS[i] <= 139), banco$PS_PONTOS[i] <- 0,
ifelse(banco$PAD[i] < 80 & (banco$PAS[i] >= 140 & banco$PAS[i] <= 159), banco$PS_PONTOS[i] <- 2,
ifelse(banco$PAD[i] < 80 & banco$PAS[i] >= 160, banco$PS_PONTOS[i] <- 3, TRUE)))))))))))))))))))))))))}

# Pontuacao para a variavel DIABETES
for(i in 1501:3000){
ifelse(banco$DIABETES[i] == 'Sim', banco$DIABETES_PONTOS[i] <- 4,  
ifelse(banco$DIABETES[i] == 'Nao', banco$DIABETES_PONTOS[i] <- 0, TRUE))}

# Pontuacao para a variavel TABAGISMO
for(i in 1501:3000){
ifelse(banco$TABAGISMO[i] == 'Atual', banco$TABAGISMO_PONTOS[i] <- 2,
ifelse(banco$TABAGISMO[i] == 'Nunca', banco$TABAGISMO_PONTOS[i] <- 0, 
ifelse(banco$TABAGISMO[i] == 'Previo', banco$TABAGISMO_PONTOS[i] <- 0, TRUE)))}

# Pontuacao total: ESCORE TOTAL
for(i in 1501:3000){
  ifelse(banco$HPDAC[i] == 'Sim', banco$ESCORE_TOTAL[i] <- NA, 
         ifelse(banco$HPDAC[i] == 'Nao', banco$ESCORE_TOTAL[i] <- sum(banco[i, c(19:24)]), TRUE))}

# PERCENTAGEM DE RISCO NAS MULHERES
for(i in 1501:3000){
ifelse(banco$ESCORE_TOTAL[i] <= -2, banco$RISCO_DAC_10_ANOS[i] <- 1,
ifelse(banco$ESCORE_TOTAL[i] == -1, banco$RISCO_DAC_10_ANOS[i] <- 2,
ifelse(banco$ESCORE_TOTAL[i] == 0, banco$RISCO_DAC_10_ANOS[i] <- 2,
ifelse(banco$ESCORE_TOTAL[i] == 1, banco$RISCO_DAC_10_ANOS[i] <- 2,
ifelse(banco$ESCORE_TOTAL[i] == 2, banco$RISCO_DAC_10_ANOS[i] <- 3,
ifelse(banco$ESCORE_TOTAL[i] == 3, banco$RISCO_DAC_10_ANOS[i] <- 3,
ifelse(banco$ESCORE_TOTAL[i] == 4, banco$RISCO_DAC_10_ANOS[i] <- 4,
ifelse(banco$ESCORE_TOTAL[i] == 5, banco$RISCO_DAC_10_ANOS[i] <- 5,
ifelse(banco$ESCORE_TOTAL[i] == 6, banco$RISCO_DAC_10_ANOS[i] <- 6,
ifelse(banco$ESCORE_TOTAL[i] == 7, banco$RISCO_DAC_10_ANOS[i] <- 7,
ifelse(banco$ESCORE_TOTAL[i] == 8, banco$RISCO_DAC_10_ANOS[i] <- 8,
ifelse(banco$ESCORE_TOTAL[i] == 9, banco$RISCO_DAC_10_ANOS[i] <- 9,
ifelse(banco$ESCORE_TOTAL[i] == 10, banco$RISCO_DAC_10_ANOS[i] <- 11,
ifelse(banco$ESCORE_TOTAL[i] == 11, banco$RISCO_DAC_10_ANOS[i] <- 13,
ifelse(banco$ESCORE_TOTAL[i] == 12, banco$RISCO_DAC_10_ANOS[i] <- 15,
ifelse(banco$ESCORE_TOTAL[i] == 13, banco$RISCO_DAC_10_ANOS[i] <- 17,
ifelse(banco$ESCORE_TOTAL[i] == 14, banco$RISCO_DAC_10_ANOS[i] <- 20,
ifelse(banco$ESCORE_TOTAL[i] == 15, banco$RISCO_DAC_10_ANOS[i] <- 24,
ifelse(banco$ESCORE_TOTAL[i] == 16, banco$RISCO_DAC_10_ANOS[i] <- 27,
ifelse(banco$ESCORE_TOTAL[i] >= 17, banco$RISCO_DAC_10_ANOS[i] <- 32, TRUE))))))))))))))))))))}

banco <- banco[, -c(19:24)]
banco$RISCO_DAC_10_ANOS <- banco$RISCO_DAC_10_ANOS/100
write.csv(banco, 'Exercicio_programa_02.csv', row.names = FALSE)