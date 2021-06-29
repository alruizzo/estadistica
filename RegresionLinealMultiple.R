####===========================================================A.L.R.R.2021
# SCRIPT REGRESIÓN LINEAL MÚLTIPLE, TABLA ANOVA Y PRUEBAS DE HIPÓTESIS


####===========================================================####
# Install necessary packages
install.packages("Hmisc"); install.packages("readxl")
library(Hmisc); library("readxl"); library("dplyr")


####===========================================================####
# Set working directory
setwd(paste('/Users/adriana/Documents/deafult/',
            'deafult/', sep = ""))


####===========================================================####
# Análisis descriptivo del conjunto de datos

# Read file
atletas <- read_excel("./Atletas.xlsx")
View(atletas)
attach(atletas)

# Get a sense of the data
head(atletas)
str(atletas)

# Descriptives based on the Hmisc package
describe(lbm) # masa corporal magra; lean body mass
sd(lbm) # Desviación estándar
describe(ht) # estatura en centímetros
sd(ht) # Desviación estándar
describe(wt) # peso en kilogramos
sd(wt) # Desviación estándar
describe(rcc) # conteo de glóbulos rojos; red cell counts
sd(rcc) # Desviación estándar

# Outliers
jpeg("outliers.jpeg", width = 2440, height = 1880, res = 300)
par(mfrow = c(2, 2))
boxplot(lbm, ylab = "Masa"); boxplot(ht, ylab = "Estatura (cm)")
boxplot(wt, ylab = "Peso (kg)")
boxplot(rcc, ylab = "Glóbulos rojos (conteo)"); dev.off()

# Scatter plot
jpeg("scatterpairs.jpeg", width = 2440, height = 1880, res = 300)
pairs(atletas); dev.off()

# Correlación (Hmisc package)
cor_mat <- rcorr(as.matrix(atletas))
round(cor_mat$r, 2); cor_mat$P; cor_mat$n


####===========================================================####
# Ajuste un modelo de regresión lineal múltiple que relacione...
# ...la masa corporal magra (lbm) con la estatura en centímetros...
# ...(ht), el peso en kilogramos (wt) y el conteo de glóbulos...
# ...rojos (rcc).

modelo <- lm(lbm ~ ht + wt + rcc)
summary(modelo)


####===========================================================####
# Prueba de hipótesis para las variables predictoras

# Primero calculamos la suma de cuadrados residual (SCRes)...
# ...necesaria para calcular el cuadrado medio residual (CMRes)...
# ...y el elemento diagonal Cjj (necesarios para el estadístico...
# ...de prueba)

# Datos necesarios para todos
n <- dim(atletas)[1] # número de observaciones
p <- dim(atletas)[2] # k (predictors) + 1 (intercepto)
gl <- n - p # grados de libertad
SCRes <- sum((lbm - fitted.values(modelo)) ^ 2)
CMRes <- SCRes / gl # Igual a la varianza

pendiente <- modelo$coefficients[["wt"]] # Bj
pendiente <- modelo$coefficients[["ht"]] # Bj
pendiente <- modelo$coefficients[["rcc"]] # Bj

# Elemento diagonal Cjj:
# Datos generales
X <- as.matrix(atletas[, 2:4]) # matriz X
X <- cbind(1, X) # Añadir la columna de unos de B0
mat <- t(X) %*% X # Calcular la multiplicación con la transpuesta
invmat <- solve(mat) # Calcular la inversa del paso anterior
diag(invmat) # Obtener la diagonal de dicha matriz inversa

Cjj <- diag(invmat)[["wt"]] # Obtener el elemento diagonal de Bj
ErrEst <- sqrt(CMRes * Cjj)

Cjj <- diag(invmat)[["ht"]] # Obtener el elemento diagonal de Bj
ErrEst <- sqrt(CMRes * Cjj)

Cjj <- diag(invmat)[["rcc"]] # Obtener el elemento diagonal de Bj
ErrEst <- sqrt(CMRes * Cjj)

# Cálculo del estadístico de prueba  
t0 <- pendiente / ErrEst

# Valor crítico para alpha/2
tcrit <- qt(0.05/2, gl, lower.tail = F)

# Prueba de hipótesis
if (abs(t0) > tcrit){
  print("Se rechaza H0")
} else {
  print("No se rechaza H0")
}

# Valor p de nuestro estadístico
pt(t0, gl, lower.tail = F)


####===========================================================####
# Construya la tabla de análisis de varianza, y pruebe la...
# ...significancia de la regresión.

# Tabla ANOVA

# Calculamos la suma de cuadrados de la regresión:

# Datos
n <- dim(atletas)[1] # Número de observaciones
k <- dim(atletas)[2] - 1 # Variables predictoras (menos lbm)
p <- dim(atletas)[2] # Número de parámetros: k + 1 (intercepto)
  
# Suma de cuadrados total; df = n - 1
SCT <- sum((lbm - mean(lbm)) ^ 2)
df_SCT <- n - 1

# Suma de cuadrados de la regresión; df = k (predictores)
SCReg <- sum((fitted.values(modelo) - mean(lbm)) ^ 2)
df_SCReg <- k

# Suma de cuadrados residual; df = n - p
SCRes <- sum((lbm - fitted.values(modelo)) ^ 2)
df_SCRes <- n - p

# Visualización de las 3 sumas de cuadrados
cbind(SCT, SCReg, SCRes)

# Fuentes de variabilidad
Fuentes <- c("Regresion", "Residual", "Total")
Sumas_cuadrados <- c(SCReg, SCRes, SCT)
Grados_libertad <- c(df_SCReg, df_SCRes, df_SCT)
Cuadrados_medios <- Sumas_cuadrados / Grados_libertad
Estadistico_F <- c(Cuadrados_medios[1] / Cuadrados_medios[2], NA, NA)
Tabla_ANOVA <- tibble(Fuentes, Sumas_cuadrados, Grados_libertad,
                      Cuadrados_medios, Estadistico_F)

# Valor p para el estadístico F
pf(Estadistico_F[1], df_SCReg, df_SCRes, lower.tail = F)

# Valor crítico para F
fcrit <- qf(0.05, df_SCReg, df_SCRes, lower.tail = F)

# Prueba de hipótesis
H0 <- "B1 = B2 = B3 = 0"
H1 <- "Bj != 0 para al menos una j"
if (abs(Estadistico_F[1]) > fcrit){
  print(paste("Se rechaza H0, o sea, ", H1))
} else {
  print(paste("No se rechaza H0, o sea, ", H0, " es verdadero"))
}


####===========================================================####
# Calcular R2 y R2adj para este modelo.

# Datos
n <- dim(atletas)[1]
p <- dim(atletas)[2]

# R2 y R2adj  
R2 <- SCReg / SCT
R2adj <- 1 - (SCRes / (n - p)) / (SCT / (n - 1))


####===========================================================####
# Determinar un intervalo de confianza de 95% para los...
# ...parámetros del modelo. CI = bi +- t(a/2,n-p) * SEbi

# Valores constantes para los tres parámetros:

# Para "t(a/2,n-p)" utilizamos el 'tcrit' obtenido anteriormente...
# ...o sea, qt(0.05/2, gl, lower.tail = F)
tcrit

# Cuadrado medio residual calculado antes: CMRes <- SCRes / gl
CMRes

# Crear un data frame para guardar los resultados
ci <- data.frame(parametro = colnames(atletas)[2:4],
                 lim_inf = NA, lim_sup = NA)

# Guardar los parametros del modelo en una lista
parametros <- colnames(atletas)[2:4]

# Obtener la información en un loop a lo largo de esa lista
for (parametro in parametros){
  Cjj <- diag(invmat)[[parametro]]
  ErrEst <- sqrt(CMRes * Cjj)
  limsup <- modelo$coefficients[[parametro]] +
    (tcrit * ErrEst)
  liminf <- modelo$coefficients[[parametro]] -
    (tcrit * ErrEst)
  ci[which(ci$parametro==parametro), c(2, 3)] <-
    cbind(lim_inf = round(liminf, 2), lim_sup = round(limsup, 2))
}
ci # Mostrar la tabla

  
####===========================================================####  
# Determinar un intervalo de confianza de 95% para la masa...
# ...corporal magra promedio de una atleta cuya estatura es 180...
# ...cms, con un peso de 78 kilogramos y un conteo de glóbulos...
# ...rojos de 4.50 (millones de glóbulos rojos por microlitro...
# ...de sangre).

# Y0 = -1.30797 + (0.06848 * 180) + (0.56637 * 78) + (1.42480 * 4.50)

# Vector de los nuevos valores de X
x0 <- as.matrix(c(1, 180, 78, 4.50))

# Y estimado según valores
Yhat <- t(x0) %*% modelo$coefficients

# Varianza de ese Yhat (usamos el CMRes calculado anteriormente)
Yhat_var <- CMRes %*% t(x0) %*% invmat %*% x0

# CI (usando el 'tcrit' calculado en puntos anteriores):
Lim_inf <- Yhat - (tcrit * sqrt(Yhat_var))
Lim_sup <- Yhat + (tcrit * sqrt(Yhat_var))
cbind(Lim_inf = round(Lim_inf[1], 2), Lim_sup = round(Lim_sup[1], 2))

# Con una función de R se puede verificar los resultados:
x0_predict <- data.frame(ht = 180, wt = 78, rcc = 4.50)
predict(modelo, newdata = x0_predict, interval = "confidence")
