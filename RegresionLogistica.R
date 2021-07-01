####===========================================================A.L.R.R.2021
# SCRIPT Regresión Logística


####===========================================================####
# Install necessary packages
library(ggplot2)


####===========================================================####
# Set working directory
setwd(paste('/Users/adriana/Documents/default/',
            'default/', sep = ""))


####===========================================================####
# Cargar los datos
dengue <- read.csv("dengue.csv")


####===========================================================####
# Mdelo logístico lineal para explicar la...
# ...probabilidad de que un individuo contraiga la enfermedad a...
# ...partir de las tres variables explicativas. Describa las...
# ...componentes aleatoria y sistematica del modelo propuesto.

# Revisar la base de datos y los tipos de variables que incluye:
View(dengue)
str(dengue)
attach(dengue)

# Convertir a factores las variables de 'nivel', 'sector' y
# ...'enfermedad' (edad sí es continua) y revisar que sí

dengue[, -which(colnames(dengue) == "edad")] <-
  lapply(dengue[, -which(colnames(dengue) == "edad")], as.factor)
str(dengue)

# Crear el modelo y generar la tabla de resumen
modelo <- glm(enfermedad ~ ., data = dengue, family = "binomial")
summary(modelo)

# Obtener los odds para cada variable predictora del modelo
# para su interpretación
exp(coef(modelo)[-1]) - 1

# Parte sistemática del modelo: son los estimados directos:
# = eta = beta estimados * X. Podemos obtener eta a través del
# logaritmo natural del odds, así:
# ln (Pi_i / (1 - Pi_i)) =
# intercepto + edad_i * X_i_edad + nivel2_i * X_i_nivel2 +
# nivel3_i * X_i_nivel3 + sector2_i * X_i_sector2

# Primero identificamos Pi_i, que sabemos son los valores predichos
# para Y_i:
Pi_i <- modelo$fitted.values
eta_i <- log(Pi_i / (1 - Pi_i))
eta_i

# Parte aleatoria del modelo: es la distribución de
# probabilidad de la variable respuesta Y, la cual
# sigue una distribución de Bernoulli:
# Pi_i cuando Y_i = 1 (éxito)
# y 1 - Pi_i cuando Y_i = 0 (fracaso)
head(modelo$fitted.values)
summary(modelo$fitted.values)
jpeg("hist_Yi_i.jpeg", width = 2440, height = 1880, res = 300)
hist(modelo$fitted.values,
     xlab = "Valores de Yi",
     ylab = "Frecuencia",
     main = "Histograma parte aleatoria del modelo")
dev.off()


####===========================================================####
# ¿La probabilidad de que un individuo contraiga la...
# ...enfermedad depende de su edad?

# Proponemos las hipótesis
H0 <- "Beta estimado = 0"
H1 <- "Beta estimado != 0"

# Obtenemos el estadístico de prueba para la variable predictora:
est_prueba_z <- summary(modelo)$coefficients["edad","Estimate"] /
  summary(modelo)$coefficients["edad", "Std. Error"]

# Calculamos el z crítico
z_crit <- qnorm(0.05/2, lower.tail = F)

if (abs(est_prueba_z) > z_crit){
  print(paste("Se rechaza la H0 de que", H0,
              "con un valor p =",
              round(pnorm(est_prueba_z, lower.tail = F) * 2, 6)))
} else {
  print(paste("No se rechaza H0, o sea que", H0))
}


####===========================================================####
# ¿La probabilidad de que un individuo contraiga la...
# ...enfermedad depende del sector de la ciudad en el que vive?

# Obtenemos el estadístico de prueba para la variable predictora:
est_prueba_z <- summary(modelo)$coefficients["sector2","Estimate"] /
  summary(modelo)$coefficients["sector2", "Std. Error"]

# Calculamos el z crítico
z_crit

if (abs(est_prueba_z) > z_crit){
  print(paste("Se rechaza la H0 de que", H0,
              "con un valor p =",
              round(pnorm(est_prueba_z, lower.tail = F) * 2, 6)))
} else {
  print(paste("No se rechaza H0, o sea que", H0))
}


####===========================================================####
# Segun el modelo estimado, ¿cuál es la probabilidad de...
# ...contraer dengue de una persona de 30 años, nivel...
# ...socioeconómico alto y que vive en el sector 2 de la ciudad?

# Creamos un vector con los nuevos datos, teniendo en cuenta
# los coeficientes del modelo (y su respectivo orden):
# (Intercept), edad, nivel2, nivel3, sector2 
X <- c(1, 30, 0, 0, 1)

# Calculamos la componente sistemática o el predictor lineal
# del modelo (esto es, eta):
eta <- sum(modelo$coefficients * X)

# Calculamos la componente aleatoria o Pi_i:
prob_ind <- exp(eta) / (1 + exp(eta))

# Con la función "predict" podemos comprobar este resultado:
X_predict <- data.frame(edad = 30, nivel = "1", sector = "2")
predict(modelo, newdata = X_predict, type = "response")


####===========================================================####
# Gráfico que muestra cómo cambia la probabilidad...
# ...de contraer la enfermedad al variar la edad y el sector de...
# ...la ciudad donde se vive, manteniendo el nivel socioeconomico...
# ...medio

# Crear dataframe del tamaño del número de sectores (2)
newdata1 <- with(dengue, data.frame(edad = mean(edad),
                                    nivel = factor(2),
                                    sector = factor(1:2)))

# Crear columna de probabilidades del tamaño de newdata1
newdata1$dengueP <- predict(modelo, newdata = newdata1,
                          type = "response")

# Crear un dataframe que represente cada nivel de edad para
# cada nivel de sector, de la misma n de dengue
newdata2 <- with(dengue,
                 data.frame(edad = rep(seq(from = min(edad),
                                           to = max(edad),
                                           length.out = 98), 2),
                            nivel = factor(2),
                                    sector = factor(rep(
                                      1:2, each = 98))))

# Unir el dataframe anterior con una predicción ('fit' o
# log odds) en la escala de los predictores lineales más el
# error estándar
newdata3 <- cbind(newdata2, predict(modelo,
                                    newdata = newdata2,
                                    type = "link",
                                    se = T))

# Añadirle al data frame anterior una columna que incluya
# la probabilidad del valor de log-odds y su intervalo de
# confianza, utilizando la función 'plogis'
newdata3 <- within(newdata3, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

# Crear el gráfico basado en el último data frame creado
ggplot(newdata3, aes(x = edad, y = PredictedProb)
       ) + geom_ribbon(aes(ymin = LL,
                           ymax = UL, fill = sector),
                       alpha = 0.2
                       ) + geom_line(aes(colour = sector),
                                     size = 1)
ggsave("prob.jpg")
