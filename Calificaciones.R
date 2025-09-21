#Instalar paqueterías (en caso no la tengan instalada)
install.packages(c("readxl", "dplyr", "ggplot2"))

#Cargar paqueterías
library(readxl)
library(dplyr)
library(ggplot2)

setwd("/Users/berumen/Desktop/YOUTUBE_NEW")
# Cargar datos desde Excel
calificaciones <- read_excel("Lista_Bioquimica.xlsx", sheet = "Datos_R")

# 2. Agregar columna de estado (Aprobado/Reprobado)
calificaciones <- calificaciones %>%
  mutate(Estado = ifelse(Calificacion >= 60, "Aprobado", "Reprobado"))

# 3. Calcular estadísticos descriptivos
#Media, desviación estandar, valores máximos y mínimos
resumen <- calificaciones %>%
  summarise(
    Media = mean(Calificacion, na.rm = TRUE),
    Desviacion_Estandar = sd(Calificacion, na.rm = TRUE),
    Maxima = max(Calificacion, na.rm = TRUE),
    Minima = min(Calificacion, na.rm = TRUE),
    Aprobados = sum(Calificacion >= 60, na.rm = TRUE),
    Reprobados = sum(Calificacion < 60, na.rm = TRUE),
    Total = n()
  )

print(resumen)


# 4. Histograma con curva normal escalada a frecuencia
ggplot(calificaciones, aes(x = Calificacion)) +
  # Histograma de frecuencia absoluta
  geom_histogram(binwidth = 1, fill = "#69b3a2", color = "white", alpha = 0.7) +
  
  # Curva normal teórica escalada (frecuencia, no densidad)
  stat_function(
    fun = function(x) {
      mu <- mean(calificaciones$Calificacion, na.rm = TRUE)
      sigma <- sd(calificaciones$Calificacion, na.rm = TRUE)
      n <- nrow(calificaciones)
      binwidth <- 1
      dnorm(x, mean = mu, sd = sigma) * n * binwidth
    },
    color = "red", linetype = "dashed", size = 1
  ) +
  
  # Línea vertical para la media
  geom_vline(aes(xintercept = mean(Calificacion, na.rm = TRUE)),
             color = "darkgreen", linetype = "dotted", size = 1) +
  
  labs(
    title = "Distribución de Calificaciones",
    subtitle = "Histograma con curva de distribución normal",
    x = "Calificación",
    y = "Frecuencia"
  ) +
  theme_minimal(base_size = 14)


#Hipótesis planteadas
#H₀ (nula): la media poblacional = 80
#H₁ (alternativa): la media poblacional < 80

# ¿La media es menor que 80?
t.test(calificaciones$Calificacion, mu = 80, alternative = "less")

# ¿La media es mayor que 80?
t.test(calificaciones$Calificacion, mu = 80, alternative = "greater")

###NOTAS
#¿Qué te da la prueba?
 # Valor t: estadístico t calculado.

#gl (df): grados de libertad.
#valor p: si es menor que 0.05 (o tu alfa), rechazas H₀.
#Intervalo de confianza de la media.
#Media muestral observada.

# Interpretación:
 # Si p < 0.05, rechazas H₀ → la media es significativamente diferente de 80.
#Si p ≥ 0.05, no hay evidencia suficiente para rechazar que la media es 80.
#Por tanto, no se rechaza H₀, las calificaciones están centradas alrededor de 80





# 2. Estadísticos
media <- mean(calificaciones$Calificacion, na.rm = TRUE)
desviacion <- sd(calificaciones$Calificacion, na.rm = TRUE)
binwidth <- 5

# 3. Crear histograma como base
ggplot(calificaciones, aes(x = Calificacion)) +
  
  # Histograma de frecuencia absoluta
  geom_histogram(binwidth = binwidth, fill = "#90CAF9", color = "white", alpha = 0.8) +
  
  # Polígono de frecuencia (misma agrupación que el histograma)
  geom_freqpoly(binwidth = binwidth, color = "darkblue", size = 1.2) +
  
  # Línea vertical para la media
  geom_vline(xintercept = media, color = "darkgreen", linetype = "dotted", size = 1) +
  
  # Etiquetas y diseño
  labs(
    title = "Distribución de Calificaciones",
    subtitle = paste0("Media = ", round(media, 2), 
                      ", Desviación estándar = ", round(desviacion, 2)),
    x = "Calificación",
    y = "Frecuencia"
  ) +
  theme_minimal(base_size = 14)


#3. Gráfico: Histograma + Curva Normal + Línea de Media
ggplot(calificaciones, aes(x = Calificacion)) +
  
  # Histograma con frecuencia absoluta
  geom_histogram(binwidth = binwidth, fill = "#90CAF9", color = "white", alpha = 0.8) +
  
  # Curva normal ajustada al histograma (escalada a frecuencia)
  stat_function(
    fun = function(x) {
      dnorm(x, mean = media, sd = desviacion) * n * binwidth
    },
    color = "red", linetype = "dashed", size = 1
  ) +
  
  # Línea vertical en la media
  geom_vline(xintercept = media, color = "darkgreen", linetype = "dotted", size = 1) +
  
  # Etiquetas y diseño
  labs(
    title = "Distribución de Calificaciones",
    subtitle = paste0("Media = ", round(media, 2), 
                      ", Desviación estándar = ", round(desviacion, 2)),
    x = "Calificación",
    y = "Frecuencia"
  ) +
  theme_minimal(base_size = 14)

