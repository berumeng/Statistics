#Hacer gráficas de expresión
setwd("/Users/berumen/Desktop/Estadistica_MIAP/")

#Cargar librería
library(readxl)
library(ggplot2)
library(agricolae)
library(psych)

problema5.1<- read.table("Problema5.1.txt", header = TRUE)
head(problema5.1)

#establecer el factor
str(problema5.1)
problema5.1$PRESION <- as.factor(problema5.1$PRESION)
problema5.1$TEMPERATURA <- as.factor(problema5.1$TEMPERATURA)
str(problema5.1)

#modelo two-way con INTERACCION
two.way.interaction <- aov(RENDIMIENTO ~ PRESION * TEMPERATURA, data = problema5.1)
summary (two.way.interaction)

#Hay interacción entre factores PRESION:TEMPERATURA
#Interacción significativa, solo interpretar interacción

#La presencia de una interacción entre tratamiento
#indica que el impacto de factor depende de los niveles del otro factor
#Por tanto, los efectos principales deben interpretarse con cuidado
model.tables(two.way.interaction, "means")
TukeyHSD(two.way.interaction)

#estadística descriptiva por grupo
describeBy(problema5.1, group = problema5.1$PRESION, problema5.1$TEMPERATURA)


#AGRICOLAE PARA GRUPOS CON INTERACCION
library(agricolae)
HSD.test(two.way.interaction, trt = c("PRESION", "TEMPERATURA"), console = TRUE)
LSD.test(two.way.interaction, trt = c("PRESION", "TEMPERATURA"), console= TRUE)

#model two-way SIN INTERACCION
two.way.no.inter <-aov(RENDIMIENTO ~ PRESION + TEMPERATURA, data = problema5.1)
summary (two.way.no.inter)
model.tables(two.way.no.inter, "means")
TukeyHSD(two.way.no.inter)
HSD.test(two.way.no.inter, trt = "PRESION", console = TRUE)

#VER CUAL ES EL MEJOR MODELO DE TODOS
library(AICcmodavg)
model.set <- list(two.way.interaction, two.way.no.inter)
model.names <- c("interaction", "no.interac")
aictab(model.set, modnames = model.names)

#el mejor fue el de interacción, ya que tiene menor AICc (4.44) y de acuerdo
#al Cum.Wt (1) significa que explica el 100% de la variacion total

#homogeneidad y normalidad de los datos
par(mfrow=c(1,2))
plot(two.way.interaction, which=1:2)

#Two-way interaction plot
interaction.plot(x.factor = problema5.1$PRESION, trace.factor = problema5.1$TEMPERATURA, 
                 response = problema5.1$RENDIMIENTO, fun = mean, 
                 type = "b", legend = TRUE, 
                 xlab = "TEMPERATURA", ylab="RENDIMIENTO",
                 pch=c(1,19), col = c("#00AFBB", "#E7B800"))

#PRESION a 215 y temperatura a 170ºC da el mejor rendimiento


# Calcular means for each treatment combination
problema5.1_stats <- 
  problema5.1 %>% 
  group_by(PRESION, TEMPERATURA) %>% # <- remember to group by the two factors
  summarise(Means = mean(RENDIMIENTO), SEs = sd(RENDIMIENTO)/sqrt(n()))
problema5.1_stats

# Plot these as an interaction plot
ggplot(problema5.1_stats, 
       aes(x = PRESION, y = Means, colour = TEMPERATURA,
           ymin = Means - SEs, ymax = Means + SEs)) +
  # this adds the mean
  geom_point(size = 3) +
  # this adds the error bars
  geom_errorbar(width = 0.1) +
  # controlling the appearance
  #scale_y_continuous(limits = c(2, 7)) + 
  xlab("PRESION") + ylab("RENDIMIENTOFC") + 
  # use a more professional theme
  theme_bw()

