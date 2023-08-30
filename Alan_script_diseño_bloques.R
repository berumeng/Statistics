#Establecer ruta de trabajo siempre
setwd("/Users/berumen/Desktop/Estadistica_MIAP/")

#Cargar librería
library(readxl)
library(ggplot2)
library(agricolae)
library(psych)


ALTURA_ESTADISTICA <- read_excel("Altura listo.xlsx")
View(ALTURA_ESTADISTICA)
#ALTURA_ESTADISTICA <- ALTURA_ESTADISTICA
#establecer el factor
str(ALTURA_ESTADISTICA)
#ALTURA_ESTADISTICA$TRATAMIENTO <- as.factor(ALTURA_ESTADISTICA$TRATAMIENTO)
ALTURA_ESTADISTICA$ALTURA <- as.numeric(ALTURA_ESTADISTICA$ALTURA)
ALTURA_ESTADISTICA$BLOQUE <- as.factor(ALTURA_ESTADISTICA$BLOQUE)
str(ALTURA_ESTADISTICA)

#estadística descriptiva por grupo
describeBy(ALTURA_ESTADISTICA, group = ALTURA_ESTADISTICA$TRATAMIENTO)

#Modelo one-way ANOVA
block.anova <- aov(ALTURA ~ TRATAMIENTO + BLOQUE, data = ALTURA_ESTADISTICA)
summary(block.anova)
#mean(ALTURA_ESTADISTICA$ALTURA)
#Homogenidad de varianza y Normalidad de datos
#codigo para que aparezcan las 2
par(mfrow=c(1,2))
#Plot para homocedasticidad y normalidad (1=homocedasticidad, 2= normalidad)
plot(block.anova, which=1:2)

#Pruebas de normalidad homocedasticidad estadistica
shapiro.test(block.anova$residuals)
bartlett.test(block.anova$residuals ~ ALTURA_ESTADISTICA$BLOQUE)

##Vemos que en ambos casos el valor de probabilidad p > 0,05, 
#por lo que no rechazamos la hipótesis nula
#por tanto, sí hay normalidad en los datos

##barlett test
#si p<0.05 se rechaza H0 (varianza no es igual, no es homogenea)
#si p > 0.05 no se rechaza HO (varianza igual, sí es homogenea) 

#PRUEBA DE MEDIAS, GRUPOS CON LETRAS
HSD.test(block.anova, "TRATAMIENTO", console = TRUE)
LSD.test(block.anova, "TRATAMIENTO", console = TRUE)
duncan.test(block.anova, "TRATAMIENTO", console=TRUE)
