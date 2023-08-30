#Establecer ruta de trabajo siempre
setwd("/Users/berumen/Desktop/UAN/Clases/Posgrado/MIAP/Estadistica_MIAP/")

#Cargar librería
library(readxl)
library(ggplot2)
library(agricolae)
library(psych)

problema4.2<- read.table("Problema4.2.txt", header = TRUE)
head(problema4.2)
#Problema4.2 <- problema4.2
#establecer el factor
str(problema4.2)
#problema4.2$Solucion <- as.factor(problema4.2$Solucion)
problema4.2$Crecimiento <- as.numeric(problema4.2$Crecimiento)
problema4.2$Bloque <- as.factor(problema4.2$Bloque)
str(problema4.2)

#estadística descriptiva por grupo
describeBy(problema4.2, group = problema4.2$Solucion)

#Modelo one-way ANOVA
block.anova <- aov(Crecimiento ~ Solucion + Bloque, data = problema4.2)
summary(block.anova)
#mean(problema4.2$Crecimiento)
#Homogenidad de varianza y Normalidad de datos
#codigo para que aparezcan las 2
par(mfrow=c(1,2))
#Plot para homocedasticidad y normalidad (1=homocedasticidad, 2= normalidad)
plot(block.anova, which=1:2)

#Pruebas de normalidad homocedasticidad estadistica
shapiro.test(block.anova$residuals)
bartlett.test(block.anova$residuals ~ problema4.2$Bloque)

##Vemos que en ambos casos el valor de probabilidad p > 0,05, 
#por lo que no rechazamos la hipótesis nula
#por tanto, sí hay normalidad en los datos

##barlett test
#si p<0.05 se rechaza H0 (varianza no es igual, no es homogenea)
#si p > 0.05 no se rechaza HO (varianza igual, sí es homogenea) 

#PRUEBA DE MEDIAS, GRUPOS CON LETRAS
HSD.test(block.anova, "Solucion", console = TRUE)
LSD.test(block.anova, "Solucion", console = TRUE)
duncan.test(block.anova, "Solucion", console=TRUE)
