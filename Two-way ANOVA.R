#Cargar el archivo, seleccionando los datos: numerico, texto, numerico
library(readxl)
Fenoles_bien <- read_excel("Fenoles_bien.xlsx")
View(Fenoles_bien) 

#summary
head(Fenoles_bien)
summary((Fenoles_bien))

#comando para observar el tipo de caracter (ejemplo = dia es num, Tratam es chr, fenol, num)
str(Fenoles_bien)

#Convertir nuestro caracter (num, chr) en FACTORES (dia y tratamiento)
Fenoles_bien$Dia <- as.factor(Fenoles_bien$Dia)
Fenoles_bien$Tratamiento <- as.factor(Fenoles_bien$Tratamiento)
str(Fenoles_bien)

#ANOVA SIN VER INTERACCION
anova1 <- aov(Fenoles ~ Tratamiento + Dia, data = Fenoles_bien)
summary (anova1)
model.tables(anova1, "means")
TukeyHSD(anova1)
plot(TukeyHSD(anova1))

#TWO-WAY ANOVA INTERACCION, (USAR) ES LO MISMO EN AMBOS CASOS. res.aov3 lo dejare en comentario
#res.aov3 <- aov(Fenoles ~ Dia + Tratamiento + Dia:Tratamiento, data = Fenoles_bien)
anova3 <- aov(Fenoles ~ Dia*Tratamiento, data = Fenoles_bien)
summary (anova3)
model.tables(anova3, "means")

#Prueba de medias 
TukeyHSD(anova3)
plot(TukeyHSD(anova3))

#codes para plots, Normal residues
par(mfrow=c(1,2))
plot(anova3, which=1:2)


#Two-way interaction plot
interaction.plot(x.factor = Fenoles_bien$Tratamiento, trace.factor = Fenoles_bien$Dia, 
                 response = Fenoles_bien$Fenoles, fun = mean, 
                 type = "b", legend = TRUE, 
                 xlab = "Tratamiento", ylab="Fenoles",
                 pch=c(1,19), col = c("#00AFBB", "#E7B800"))

interaction.plot(x.factor = Fenoles_bien$Dia, trace.factor = Fenoles_bien$Tratamiento, 
                 response = Fenoles_bien$Fenoles, fun = mean, 
                 type = "b", legend = TRUE, 
                 xlab = "Dia", ylab="Fenoles",
                 pch=c(1,19), col = c("#00AFBB", "#E7B800"))



###################    OTRA FORMA /OMITIR PARA EJERCICIO   ################
#MODELO D
model <- lm(Fenoles_bien$Fenoles~Fenoles_bien$Dia+Fenoles_bien$Tratamiento)

#ANOVA
anova<-anova(model)
anova

#LSD TEST
library(agricolae)
LSD <- LSD.test(anova3, "Dia")
LSD


