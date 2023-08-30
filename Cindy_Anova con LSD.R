#ANOVA#

library(readxl)
dieta_num <- read_excel("dieta1_copia.xlsx")
View(Dieta1_copia)
head(Dieta1_copia)

#grafica de caja o bigotes o box plot

boxplot(PP~Dieta, data= Dieta1_copia)
aov(PP~Dieta, data = Dieta1_copia)

#Anova en objeto
Anova1 <- aov(PP~Dieta, data= Dieta1_copia)

#Resultados en anova (resumen)
summary(Anova1)

#PRUEBA DE TUKEY 
TukeyHSD(Anova1)

#Graficar
plot(Anova1)

library(agricolae)

LSD= LSD.test(Anova1, "Dieta", console = TRUE)
hsd= HSD.test(Anova1, "Dieta", console = TRUE)

#grafica prueba de medias tukey
plot(TukeyHSD(Anova1))

#graficas normalidad
plot(anova)


        
