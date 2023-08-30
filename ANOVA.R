#ANOVA#
#Analizar si existen diferencias significativas entre 4 tipos de dietas (A,B,C,D)
#Variable de respuesta es PP

#Cargar la librería de excel
library(readxl)
#Crear objeto llamado dieta y cargar el archivo dieta1_copia
Dieta1 <- read_excel("Dieta1.xlsx")
#Ver
View(Dieta1)
#Encabezado
head(Dieta1)

#Realizar el digrama de caja o bigotes o box plot para ver el comportamiento de los datos
boxplot(PP~Dieta, data= Dieta1)

#Crear un objeto llamado ANOVA
#Establecer el modelo usando como variable de respuesta PP y Dieta es el factor
#aov es el comando para el ANOVA
Anova1 <- aov(PP~Dieta, data= Dieta1)

#Resultados en anova (resumen)
summary(Anova1)

#Graficar
plot(Anova1)

#PRUEBA DE TUKEY 
TukeyHSD(Anova1)

#Usar librería agricolae para ver si existen diferencias estadísticas significativas
library(agricolae)
#Usar pruebas de medias LSD y Tukey (HSD)
LSD= LSD.test(Anova1, "Dieta", console = TRUE)
hsd= HSD.test(Anova1, "Dieta", console = TRUE)

#grafica prueba de medias tukey
plot(TukeyHSD(Anova1))

#graficas normalidad
plot(anova)


        
