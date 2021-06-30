####===========================================================A.L.R.R.2021
# SCRIPT Evaluación de los mejores modelos de regresión múltiple
# Y de los supuestos de la regresión, así como pruebas de hipótesis


####===========================================================####
# Install necessary packages
library(dplyr)
library(leaps)
library(tidyverse)
library(car)
library(ggpubr)
library(lmtest)


####===========================================================####
# Set working directory
setwd(paste('/Users/adriana/Documents/default/',
            'default/', sep = ""))


####===========================================================####
# Load file
tabla1 <- read.csv("Tabla1_PreciosPropiedades.csv")


####===========================================================####
# Ajuste un modelo de regresion multiple que relacione el...
# ...precio de venta con los nueve variables regresoras.

# Revisar la tabla y los tipos de variables que incluye
View(tabla1)
str(tabla1)

# Crear una variable con los nombres de las variables del modelo...
# ...y reemplazar las columnas de tabla1 para que quede más fácil 

var_names <- c("precio_venta_1000",
               "impuestos_1000",
               "baños",
               "tamaño_lote_pies2_x1000",
               "espacio_vital_pies_2_x1000",
               "puestos_garaje",
               "habitaciones",
               "dormitorios",
               "edad_hogar_años",
               "chimeneas")

colnames(tabla1) <- var_names
attach(tabla1)

# Crear el modelo y generar la tabla de resumen
modelo <- lm(precio_venta_1000 ~ ., data = tabla1)
summary(modelo)


####===========================================================####
# Prueba de significancia de la regresion. ¿Qué conclusiones...
# ...puedes sacar?

alpha <- 0.05

# Suma de cuadrados total; df = n - 1
SCT <- sum((precio_venta_1000 - mean(precio_venta_1000)) ^ 2)
SCT_gl <- nrow(tabla1) - 1

# Suma de cuadrados de la regresión; df = k (predictores)
SCReg <- sum((fitted.values(modelo) - mean(precio_venta_1000)) ^ 2)
SCReg_gl <- length(tabla1) - 1

# Suma de cuadrados residual; df = n - p [p = k + 1]
SCRes <- sum((precio_venta_1000 - fitted.values(modelo)) ^ 2)
SCRes_gl <- nrow(tabla1) - length(tabla1)

# Fuentes de variabilidad (F = CMReg / CMErr)
Fuentes <- c("Regresion", "Residual", "Total")
Sumas_cuadrados <- c(SCReg, SCRes, SCT)
Grados_libertad <- c(SCReg_gl, SCRes_gl, SCT_gl)
Cuadrados_medios <- Sumas_cuadrados / Grados_libertad
Estadistico_F <- c(Cuadrados_medios[1] / Cuadrados_medios[2], NA, NA)
Tabla_ANOVA <- tibble(Fuentes, Sumas_cuadrados, Grados_libertad,
                      Cuadrados_medios, Estadistico_F)

# Valor crítico para F: cuantil en la distribución F
fcrit <- qf(alpha, SCReg_gl, SCRes_gl, lower.tail = F)

# Prueba de hipótesis
H0 <- "B1 = B2 = ... = B9 = 0"
H1 <- "Bj != 0 para al menos una j"
if (abs(Estadistico_F[1]) > fcrit){
  print(paste("Se rechaza H0, indicando que: ", H1,
              "con un nivel de significancia de",
              pf(Estadistico_F[1], SCReg_gl, SCRes_gl, lower.tail = F)))
} else {
  print(paste("No se rechaza H0. O sea, ", H0, " es verdadero"))
}


####===========================================================####
# Utilice las pruebas t para evaluar la contribucion de cada...
# ...variable regresora al modelo. Discute tus hallazgos.

# Tenemos que T = bj / SE(bj), con el SE = sqrt(CME * Cjj)

# Cuadrado medio del error
CME <- Cuadrados_medios[2]

# Elemento diagonal Cjj:
# Generar una matriz X
X <- as.matrix(tabla1[, 2:length(tabla1)])
# Añadir columna de 1s para B0
X <- cbind(1, X)
# Calcular la multiplicación con la transpuesta de X
mat <- t(X) %*% X
# Calcular la inversa de mat
invmat <- solve(mat)

# Crear un data frame donde se guardarán los valores t
# ...de todas las variables predictoras
valorest <- data.frame(predictor = colnames(
  tabla1)[2:length(tabla1)],
  b = NA, se = NA, valort = NA)
valorest

# Loop para calcular el numerador y denominador necesarios
# ...para calcular los estadísticos de prueba para cada
# ...variable predictora
for (i in valorest$predictor){
  valorest$b[which(valorest$predictor==i)] <-
    round(coefficients(modelo)[[i]], 5)
  Cjj <- diag(invmat)[[i]]
  valorest$se[which(valorest$predictor==i)] <-
    round(sqrt(CME * Cjj), 5)
}

# Cálculo del estadístico de prueba t para las variables
# ...predictoras
valorest$valort <- round((valorest$b / valorest$se), 3)

# Cálculo del valor p para cada variable predictora
valorest$valorp <-
  round((pt(q = abs(valorest$valort),
            df = (nrow(tabla1) - (length(tabla1))),
            lower.tail = F) * 2), 5)
valorest

# Valor p
H0 <- "Bj = 0"
H1 <- "Bj != 0"
for(t in valorest$valort){
  if(abs(t) > qt(alpha/2, (nrow(tabla1)-(length(tabla1))),
                 lower.tail = F)){
    print(paste("* Se rechaza H0 para la variable predictora:",
                valorest$predictor[which(
                  valorest$valort==t)], "*", "con un nivel de",
                "significancia (alpha) de",
                valorest$valorp[which(
                  valorest$valort==t)]))
  } else {
    print(paste("No se rechaza H0 para la variable predictora",
                valorest$predictor[which(
                  valorest$valort==t)]))
  }
}


####===========================================================####
# Encuentre el subconjunto de variables predictoras en el...
# ...conjunto de datos que da como resultado el “mejor” modelo...
# ...segun algun criterio de información.

# Método exhaustivo
best_subset <- regsubsets(precio_venta_1000 ~ ., tabla1,
                          method = "exhaustive")
results <- summary(best_subset)

# Método hacia adelante
forward <- regsubsets(precio_venta_1000 ~ ., tabla1,
                      method = "forward")
results <- summary(forward)

# Método hacia atrás
backward <- regsubsets(precio_venta_1000 ~ ., tabla1,
                       method = "backward")
results <- summary(backward)

# Gráfico
tibble(predictors = 1:8,
       adj_R2 = results$adjr2,
       Cp = results$cp,
       BIC = results$bic) %>%
  gather(statistic, value, -predictors) %>%
  ggplot(aes(predictors, value, color = statistic)) +
  geom_line(show.legend = F) +
  geom_point(show.legend = F) +
  facet_wrap(~ statistic, scales = "free")

# Valores exactos del "mejor" modelo (con 5 variables):
results$adjr2[5]
results$cp[5]
results$bic[5]
results$outmat[5,]


####===========================================================####
# Interprete los parámetros estimados del nuevo modelo.

mejormodelo <- lm(precio_venta_1000 ~ impuestos_1000 + baños +
                    puestos_garaje + edad_hogar_años +
                    chimeneas, data = tabla1)
summary(mejormodelo)


####===========================================================####
# Construya e interprete un gráfico de residuales contra la...
# ...respuesta estimada. ¿Es sensato pensar que el modelo cumple...
# ...los supuestos? Verifique esto a un nivel de significancia del 5%

# Residuales estudentizados externamente
estudentizados <- rstudent(mejormodelo)
estudentizados
range(estudentizados)

# Gráfico
jpeg("qqplot.jpeg", width = 2440, height = 1880, res = 300)
qqPlot(estudentizados, pch = 20); dev.off()

# Residuales contra la respuesta estimada (1, 1):
jpeg("resvsfitted.jpeg", width = 2440, height = 1880, res = 300)
plot(fitted.values(mejormodelo), estudentizados,
     xlab = "Respuesta estimada",
     ylab = "Residuales estudentizados",
     main = "Residuales vs. respuesta estimada",
     ylim = c(-2, 3), pch = 16)
abline(0, 0, lty = 2) # añadir línea de referencia
# verificar si los puntos siguen algún patrón
lines(lowess(fitted.values(mejormodelo), estudentizados), col = "red")
# añadir el índice a cada valor para identificarlo fácilmente
text(fitted.values(mejormodelo), estudentizados,
     labels = row.names(tabla1), cex = 0.7, pos = 3); dev.off()
# También se puede hacer de manera más pro con:
plot(mejormodelo)

# Supuestos
# Normalidad
# Prueba de normalidad Shapiro-Wilks. H0: Normalidad
shapiro.test(estudentizados)

# Homocedasticidad
# Prueba de Breusch-Pagan. H0: homocedasticidad (varianza constante)
bptest(mejormodelo)

# Independencia
# Prueba de Durbin-Watson. H0: Errores no correlacionados
# (autocorrelación = 0)
dwtest(mejormodelo)


####===========================================================####
# Realice un análisis de diagnóstico e identifique (si es...
# ...que existen) observaciones con alto leverage, influyentes...
# ...y extremas en la respuesta.

# Diagnóstico de residuales

# Observaciones de alto leverage o apalancamiento
# Función "Leverage.normal":
Leverage.normal <- function(objeto){
  y <- objeto$residuals + fitted(objeto)
  H <- lm.influence(objeto)$hat
  X <- model.matrix(objeto)
  maxy <- max(max(H), 2 * mean(H))
  plot(H, main = "Puntos de alto Leverage",
       xlab = "Índice",
       ylim = c(0, maxy), ylab = "h",
       cex = 0.3, lwd = 3)
  abline(2 * mean(H), 0, col = 2, lty = 3)
  text(H, cex = 0.7, pos = 1)
  alto.leverage = ifelse(H > 2 * mean(H), T, F)
  identificador = 1:length(y)
  identificador[alto.leverage]
}

# Evaluación de la función en este modelo
jpeg("alto_leverage.jpeg", width = 2440, height = 1880, res = 300)
Leverage <- Leverage.normal(mejormodelo)
tabla1[Leverage, ]; dev.off()

# Observaciones extremas en la respuesta
# Función "Residuos.normal":
Residuos.normal <- function(objeto){
  y <- objeto$residuals + fitted(objeto)
  r <- rstudent(objeto)
  maxy <- max(max(r), 3)
  miny <- min(min(r),-3)
  plot(fitted(objeto), r,
       main = "Observaciones extremas en la respuesta",
       xlab = "Media estimada", ylab = "Residuo estudentizado",
       cex = 0.3,
       lwd = 3, ylim = c(miny, maxy))
  text(fitted(objeto), r, cex = 0.7, pos = 1)
  abline(2, 0, col = 2,lty = 3)
  abline(0, 0, lty = 3)
  abline(-2, 0, col = 2, lty = 3)
  extremo.respuesta = ifelse(abs(r) > 2, T, F)
  identificador = 1:length(y)
  identificador[extremo.respuesta]
}

# Evaluación de la función en este modelo
jpeg("extremo_rpta.jpeg", width = 2440, height = 1880, res = 300)
Extremos <- Residuos.normal(mejormodelo)
tabla1[Extremos, ]
dev.off()

# Observaciones influyentes

# Función "Influyentes.normal":
Influyentes.normal <- function(objeto){
  y <- objeto$residuals + fitted(objeto)
  n <- length(y)
  h <- lm.influence(objeto)$hat
  p <- ncol(model.matrix(objeto))
  s <- sqrt(deviance(objeto)/(n - p))
  ti <- objeto$residuals / (s * sqrt(1 - h))
  # Distancia de Cook (H grande, leverage y extremas)
  DC <- (ti ^ 2 / p) * h / (1 - h)
  maxy <- max(max(DC), 3 * mean(DC))
  plot(DC, main = "Observaciones influyentes",
       xlab = "Índice",
       ylim = c(0, maxy), ylab = "Distancia de Cook",
       cex = 0.3, lwd = 3)
  text(DC, cex = 0.7, pos = 1)
  abline(3 * mean(DC), 0, col = 2, lty = 3)
  infl.glob = ifelse(DC > 3 * mean(DC), T, F)
  identificador = 1:length(y)
  identificador[infl.glob]
}

# Evaluación de la función en este modelo
jpeg("influyentes.jpeg", width = 2440, height = 1880, res = 300)
Global <- Influyentes.normal(mejormodelo)
tabla1[Global, ]
dev.off()

# Usando una función del paquete "car"
outlierTest(mejormodelo)
