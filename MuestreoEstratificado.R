####===========================================================A.L.R.R.2021
# Muestreo Estratificado
# 1. Contexto:
# La base de datos utilizada para la presente actividad se tomó del...
# ...repositorio figshare.com (https://doi.org/10.6084/m9.figshare.767318.v1)
# ...y corresponde a los datos del experimento 2 de Germine, Duchaine, &...
# ...Nakayama (2011), cuyo estudio y resultados se reportan en la revista...
# ...Cognition (Germine, L. T., Duchaine, B., & Nakayama, K. (2011). Where...
# ...cognitive development and aging meet: Face learning ability peaks...
# ...after age 30. Cognition, 118(2), 201-210.).


# Install necessary packages ==============================================
pacman::p_load(ggplot2, dplyr, magrittr, TeachingSampling, gplots, gtools,
               sampling, GGally, RColorBrewer)


# Datos ===================================================================

# Un solo data frame
datos <- read.csv("germineExp2.csv")

# Total de datos
N <- nrow(datos)

# Estructura de los datos
str(datos)

# Ajustar algunas variables
# Volver factor la variable de gender
datos$gender[which(datos$gender == "f")] <- "F"
datos$gender <- as.factor(datos$gender)
# Reducir a dos decimales ambas variables numéricas
datos$FACESpropcorr <- round(datos$FACESpropcorr, 2)
datos$NAMESpropcorr <- round(datos$NAMESpropcorr, 2)

# Volver a revisar la estructura nuevamente
str(datos)

# Generar un resumen y una muestra de los datos
summary(datos)
head(datos)

# Gráficamente
ggplot(datos, aes(x = FACESpropcorr, group = gender, fill = gender)) +
        geom_histogram(position = "dodge") + theme_bw() +
        xlab("Proporción de aciertos") + ylab("Frecuencia") +
        ggtitle("Histograma Prueba de Rostros") + labs(fill = "Sexo") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
ggsave("Faces_hist.jpg", width = 15, height = 10, units = "cm")
ggplot(datos, aes(x = NAMESpropcorr, group = gender, fill = gender)) +
        geom_histogram(position = "dodge") + theme_bw() +
        xlab("Proporción de aciertos") + ylab("Frecuencia") +
        ggtitle("Histograma Prueba de Nombres") + labs(fill = "Sexo") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
ggsave("Names_hist.jpg", width = 15, height = 10, units = "cm")
ggplot(datos, aes(x = age, group = gender, fill = gender)) +
        geom_histogram(position = "dodge") + theme_bw() +
        xlab("Edad") + ylab("Frecuencia") +
        ggtitle("Histograma Distribución de Edad") + labs(fill = "Sexo") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
ggsave("Age_hist.jpg", width = 15, height = 10, units = "cm")


# Ajustar variables categóricas ===========================================

# Crear los estratos (L = 4)
datos$grupo <- ""
datos$grupo[which(
        datos$gender == "F" & datos$age <= median(datos$age))] <- "F joven"
datos$grupo[which(
        datos$gender == "F" & datos$age > median(datos$age))] <- "F mayor"
datos$grupo[which(
        datos$gender == "M" & datos$age <= median(datos$age))] <- "M joven"
datos$grupo[which(
        datos$gender == "M" & datos$age > median(datos$age))] <- "M mayor"

# Volver la nueva variable grupo y chequear los datos
datos$grupo <- as.factor(datos$grupo)
head(datos$grupo, 10)

# Resultado de la estratificación (y comprobación con la variable original)
table(datos$grupo)
table(datos$gender[which(datos$age <= 30)])
table(datos$gender[which(datos$age > 30)])


# Estratos ================================================================

# Crear distintos estratos con la variable "grupo"
# Estrato 1
estrato1 <- split(datos, datos$grupo)$`F joven`
head(estrato1)

# Estrato 2
estrato2 <- split(datos, datos$grupo)$`F mayor`
head(estrato2)

# Estrato 3
estrato3 <- split(datos, datos$grupo)$`M joven`
head(estrato3)

# Estrato 4
estrato4 <- split(datos, datos$grupo)$`M mayor`
head(estrato4)

# Unidades por estrato - Tamaños de N
N1 <- nrow(estrato1)
N2 <- nrow(estrato2)
N3 <- nrow(estrato3)
N4 <- nrow(estrato4)

# Total (verificación)
Nt <- N1 + N2 + N3 + N4


# Valores de los estratos =================================================

# Summaries
summary(estrato1)
summary(estrato2)
summary(estrato3)
summary(estrato4)

# Variable de interés: "FACES"
# Estrato mujeres jóvenes
faces1 <- estrato1$FACESpropcorr
# Estrato mujeres mayores
faces2 <- estrato2$FACESpropcorr
# Estrato hombres jóvenes
faces3 <- estrato3$FACESpropcorr
# Estrato hombres mayores
faces4 <- estrato4$FACESpropcorr

# Desviaciones poblacionales por estrato
sdEstra1 <- sd(faces1)
sdEstra2 <- sd(faces2)
sdEstra3 <- sd(faces3)
sdEstra4 <- sd(faces4)

# Visualizar las distribuciones por estrato
display.brewer.all()
colores <- brewer.pal(9, "Pastel1")
jpeg("boxplot_faces_por_grupo.jpeg", width = 1900, height= 1400, res = 300)
boxplot(datos$FACESpropcorr ~ datos$grupo,
        xlab = paste("Estratos (N = ", N, ")", sep = ""),
        ylab = "Proporción de aciertos (rostros)",
        col = colores, ylim = c(0, 1))
title("Boxplots por estratos")
#abline(h = 0.5, lty = 3, col = "darkgray")
dev.off()


# Prueba piloto ===========================================================

# Tamaño del error (entre 0 y 0.10)
error <- 0.05

# Valor de n para la prueba piloto
n <- round(error * nrow(datos), 0)
n

# Total de muestras por estrato
# Denominador
denominador <- (N1 * sdEstra1) + (N2 * sdEstra2) +
        (N3 * sdEstra3) + (N4 * sdEstra4)

# Tamaño de muestra, Estrato 1: mujer joven
n1 <- round(((n * N1 * sdEstra1) / denominador), 0)

# Tamaño de muestra, Estrato 2: mujer mayor
n2 <- round(((n * N2 * sdEstra2) / denominador), 0)

# Tamaño de muestra, Estrato 3: hombre joven
n3 <- round(((n * N3 * sdEstra3) / denominador), 0)

# Tamaño de muestra, Estrato 4: hombre mayor
n4 <- round(((n * N4 * sdEstra4) / denominador), 0)

# Chequear resultados
sum(n1, n2, n3, n4)
cbind(N1, N2, N3, N4)
cbind(n1, n2, n3, n4)

## Selección de muestra ====
# Crear copia por si se necesita
datos_orig <- datos

# Ordenar los datos según el grupo
datos <- datos[order(datos$grupo), ]
row.names(datos) <- NULL

# Extraer la muestra de acuerdo con los estratos
set.seed(810705)
estratos <- sampling:::strata(datos,
                   stratanames = "grupo",
                   size = c(n1, n2, n3, n4),
                   method = "srswor" )

# Obtener toda la base de datos
muestreo <- getdata(datos, estratos)

# Filtrar datos con la variable FACES
muestreo_faces <- getdata(datos$FACESpropcorr, estratos)
muestreo_names <- getdata(datos$NAMESpropcorr, estratos)

# Descripción de la muestra estratificada ====
# Chequear n por grupo
table(muestreo_faces$grupo)

# Guardar vector de estrato
A <- muestreo_faces$grupo

# Guardar bases de datos diferentes para cada estrato
SN1 <- split(muestreo_faces, A)$`F joven`
row.names(SN1) <- NULL
SN2 <- split(muestreo_faces, A)$`F mayor`
row.names(SN2) <- NULL
SN3 <- split(muestreo_faces, A)$`M joven`
row.names(SN3) <- NULL
SN4 <- split(muestreo_faces, A)$`M mayor`
row.names(SN4) <- NULL

# Medias para cada estrato ====
me1 <- mean(SN1$data)
me2 <- mean(SN2$data)
me3 <- mean(SN3$data)
me4 <- mean(SN4$data)
cbind(round(me1, 3), round(me2, 3), round(me3, 3), round(me4, 3))

# Media estimada de la muestra piloto ====
media.est <- (1 / N) * ((N1 * me1) + (N2 * me2) + (N3 * me3) + (N4 * me4))
media.est

# Tamaño de la muestra ====
E <- 0.10 * media.est # Error máximo permitido
NC <- 0.95
z <- -qnorm((1 - NC) / 2) 
V <- (E / z) ^ 2
Nt <- nrow(datos)

# Estimación de la varianza con corrección de la distribución de Neyman (allocation)
Var_st <- ((N1 / Nt) * var(SN1$data)) + ((N2 / Nt) * var(SN2$data)) +
                   ((N3 / Nt) * var(SN3$data)) + ((N4 / Nt) * var(SN4$data))

# Margen de error
MargenError. <- seq(0, E, by = E / 10)
V <- (MargenError. / z) ^ 2
N. <- nrow(datos)
n0 <- Var_st * (1 / V) # Población infinita o desconocida
n. <- n0 / (1 + (n0 / N.))
cbind(MargenError., n0, n.)
plot(MargenError., n0, main = "Población infinita o desconocida",
     ylab = "Tamaño de la muestra",
     xlab = "Nivel de precisión",
     xlim = c(0, E),
     ylim = c(0, n0[2]))
grid(NULL, NULL, lty = 3, col = "lightgray")
jpeg("MargenErrorFinita.jpg", width = 1700,
     height = 1300, res = 200)
plot(MargenError., n.,
     main = paste("Población con N = ", Nt, sep = ""),
     ylab = "Tamaño de la muestra",
     xlab = "Nivel de precisión",
     xlim = c(0, E),
     ylim = c(0, n0[2]))
grid(NULL, NULL, lty = 3, col = "lightgray")
#abline(v = MargenError.[2:11], col = "red")
dev.off()

# n seleccionada = 530, para un error de precisión < 0.01 (0.0084)


# Análisis con la muestra seleccionada ===========================
n <- 530

# Total de muestras por estrato ("denominador": definido arriba en
# la prueba piloto)

# Tamaño de muestra, Estrato 1: mujer joven
n1 <- round(((n * N1 * sdEstra1) / denominador), 0)

# Tamaño de muestra, Estrato 2: mujer mayor
n2 <- round(((n * N2 * sdEstra2) / denominador), 0)

# Tamaño de muestra, Estrato 3: hombre joven
n3 <- round(((n * N3 * sdEstra3) / denominador), 0)

# Tamaño de muestra, Estrato 4: hombre mayor
n4 <- round(((n * N4 * sdEstra4) / denominador), 0)

# Chequear resultados y graficar los tamaños de muestra
sum(n1, n2, n3, n4)
cbind(N1, N2, N3, N4)
cbind(n1, n2, n3, n4)
jpeg("Tamaños_muestra_estratos.jpg", width = 1700,
     height = 1300, res = 200)
bp <- barplot(c(n1, n2, n3, n4),
        names.arg = c("Mujeres <= 30",
                      "Mujeres > 30",
                      "Hombres <= 30",
                      "Hombres > 30"),
        beside = T,
        main = "Tamaños de n para cada estrato",
        xlab = "Estratos (por sexo y edad)",
        ylab = "Tamaño (n)", ylim = c(0, 165),
        col = c("#B3CDE3", "#CCEBC5", "#DECBE4", "#FED9A6"))
text(bp, 1, c(n1, n2, n3, n4), cex = 1, pos = 3) 
dev.off()

# Extraer la muestra de acuerdo con los estratos
estratos_piloto <- estratos # Guardar la anterior
set.seed(810705)
estratos <- sampling:::strata(datos,
                              stratanames = "grupo",
                              size = c(n1, n2, n3, n4),
                              method = "srswor" )
row.names(estratos) <- NULL

# Filtrar datos con la variable FACES
muestreo_faces_piloto <- muestreo_faces
muestreo_faces <- getdata(datos$FACESpropcorr, estratos)

# Descripción de la muestra estratificada ====
# Chequear n por grupo
table(muestreo_faces$grupo)

# Guardar vector de estrato
A <- muestreo_faces$grupo

# Guardar bases de datos diferentes para cada estrato
SN1 <- split(muestreo_faces, A)$`F joven`
row.names(SN1) <- NULL
SN2 <- split(muestreo_faces, A)$`F mayor`
row.names(SN2) <- NULL
SN3 <- split(muestreo_faces, A)$`M joven`
row.names(SN3) <- NULL
SN4 <- split(muestreo_faces, A)$`M mayor`
row.names(SN4) <- NULL

# Medias para cada estrato ====
me1 <- mean(SN1$data)
me2 <- mean(SN2$data)
me3 <- mean(SN3$data)
me4 <- mean(SN4$data)
cbind(round(me1, 3), round(me2, 3), round(me3, 3), round(me4, 3))

# Media estimada de la muestra ====
media.est <- (1 / N) * ((N1 * me1) + (N2 * me2) + (N3 * me3) + (N4 * me4))
media.est

# Varianza estimada de la muestra ====
# Varianza estimada en cada estrato de la muestra
var1 <- var(SN1$data)
var2 <- var(SN2$data)
var3 <- var(SN3$data)
var4 <- var(SN4$data)
# Varianza estimada corregida por el tamaño del estrato en
# la población y muestra
VAR.1 <- N1 ^ 2 * ((N1 - n1) / N1) * (var1 / n1)
VAR.2 <- N2 ^ 2 * ((N2 - n2) / N2) * (var2 / n2)
VAR.3 <- N3 ^ 2 * ((N3 - n3) / N3) * (var3 / n3)
VAR.4 <- N4 ^ 2 * ((N4 - n4) / N4) * (var4 / n4)
cbind(VAR.1, VAR.2, VAR.3, VAR.4)
# Varianza y desviación en toda la muestra
Var.est <- (1 / N ^ 2) * (VAR.1 + VAR.2 + VAR.3 + VAR.4)
DES.Est <- sqrt(Var.est)

# Descriptivos en la muestra e intervalo de confianza ====
cbind(Lim_inf = media.est - z * DES.Est,
      Media = media.est,
      Lim_sup = media.est + z * DES.Est,
      CV = DES.Est / media.est)

# Total (proporción de aciertos en la muestra)
TOTALmedia <- N * media.est
TOTALvar <- N ^ 2 * Var.est
