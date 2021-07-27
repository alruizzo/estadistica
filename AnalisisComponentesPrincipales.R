####===========================================================A.L.R.R.2021
# Análisis Multivariado: Validación de Supuestos y Análisis de
# Componentes Principales (PCA)


####===========================================================####
# Install necessary packages
library(ggplot2)
library(ggcorrplot)
library(mvnormtest)
library(normtest)
library(nortest)
library(heplots)
library(FactoMineR)
library(factoextra)
library(corrplot)


####===========================================================####
# Set working directory
setwd(paste("/Users/adriana/Documents/default/",
            "default", sep = ""))


####===========================================================####
# Validación de los supuestos del análisis...
# ...multivariado: normalidad multivariada,...
# ...determinante positivo de la matriz de varianzas-covarianzas
# ...e igualdad de matrices de varianzas covarianzas.

# Cargar los datos y añadir columnas al environment
quejas_FC <- read.csv("quejas_FC.csv")
attach(quejas_FC)

# Mostrar primeras observaciones de la base de datos y...
# ...su estructura
head(quejas_FC)

# Restringiremos la base de datos a la...
# ...medición inicial para hacer más sencillo el trabajo
quejas_FC <- quejas_FC[which(quejas_FC$timepoint==1),]
rownames(quejas_FC) <- NULL # acomodamos la numeración de filas
quejas_FC$timepoint <- NULL # eliminamos la columna de timepoint

# Volver factores las variables de character
quejas_FC$subjects <- factor(quejas_FC$subjects)
quejas_FC$SCD <- factor(quejas_FC$SCD, levels = c("SCD", "CON"))
quejas_FC$Gender <- factor(quejas_FC$Gender)

# Mostramos un resumen de la base de datos y su estructura
summary(quejas_FC)
str(quejas_FC)

# Trasponemos la base de datos de las variables numéricas:
# Primero copiamos la base de datos original en una base de datos
# numérica
quejas_FC_num <- quejas_FC[, unlist(lapply(quejas_FC, is.numeric))]

# Luego hacemos imputación para las variables con valores NA
# (ya que el test de Shapiro-Wilk arrojó error cuando habían NAs)
quejas_FC_num[is.na(quejas_FC_num$MFQFOFInvAver), "MFQFOFInvAver"] <-
  mean(quejas_FC_num$MFQFOFInvAver, na.rm = T)
quejas_FC_num[is.na(quejas_FC_num$fminor_ICVF), "fminor_ICVF"] <-
  mean(quejas_FC_num$fminor_ICVF, na.rm = T)

# Trasponemos la base de datos numérica en una matriz
datos <- t(quejas_FC_num)
head(datos)

# Normalidad multivariada con el test de Shapiro-Wilk:
mshapiro.test(datos)

# Normalidad multivariada con el test de Jarque-Bera:
jb.norm.test(quejas_FC_num)

# Normalidad univariada: función para generar todos los...
# ...tests al tiempo
uninorm <- function(varname = "variable", mydata = "data"){
  # la variable debe estar en el environment (con "attach")
  ad <- ad.test(varname)[1:2]
  cvm <- cvm.test(varname)[1:2]
  lillie <- lillie.test(varname)[1:2]
  chi2 <- pearson.test(varname)[1:2]
  shapiro <- sf.test(varname)[1:2]
  result <- list("Anderson Darling" = ad,
                 "Cramer von Mises" = cvm,
                 "Kolmogorov-Smirnov" = lillie,
                 "Pearson chi-cuadrado" = chi2,
                 "Shapiro-Francia" = shapiro)
  return(result)
}

# Correr función para todas las variables de la base de datos
df <- quejas_FC_num # seleccionar df de interés
resultado <- vector("list", length(df)) # crear lista de resultados
resultado <- setNames(resultado, paste0(colnames(df))) # asignar nombres
for (variable in colnames(df)){
  resultado[which(names(resultado) == variable)] <-
    list(uninorm(df[[variable]], df)) # poblar cada elemento de la lista
}
rm(df)

# Revisar los resultados
View(resultado)
resultado[(colnames(quejas_FC_num)[7])] # Para imprimir en la pantalla

# Determinante de una matriz
# Creamos la matriz de varianza-covarianza
matriz <- cov(quejas_FC_num)
det(matriz)

# Matriz de correlaciones
cormatriz <- round(cov2cor(matriz), 2)

# Igualdad de matrices de varianzas covarianzas
# Utilizamos la información de la base de datos inicial, que contiene 
# ...una división en grupos (variable 'SCD') para chequear
# ...que los determinantes de ambas matrices en ambos grupos
# ...sean positivos y probar la homogeneidad con la prueba de Box M
covCON <- cov(quejas_FC_num[1:43,])
det(covCON)

covSCD <- cov(quejas_FC_num[44:69,])
det(covSCD)

# Prueba de Box M de homogeneidad de matrices de covarianza
# H0: las matrices de covarianza observadas para las variables
# dependientes son iguales entre los grupos
boxM(quejas_FC_num[,-14], quejas_FC_num$SCD)


####===========================================================####
# Análisis de componentes principales (ACP) sobre...
# ...las variables cuantitativas y porcentaje de varianza...
# ...explicado por cada dimensión

# Realizaremos el PCA de las variables de FC, que son las que...
# ...representan un mismo constructo, el de conectividad...
# ...funcional (de la columna 4 hasta la 13)
pca_fc <- PCA(quejas_FC_num[,-c(1:3)])

# Obtenemos los autovalores o varianzas
get_eigenvalue(pca_fc)

# Gráfico de Scree de los autovalores
jpeg("Eigenvalue.jpeg", width = 1900, height= 1400, res = 300)
fviz_eig(pca_fc, addlabels = T,
         barfill = "darkolivegreen4",
         barcolor = "darkolivegreen4",
         linecolor = "black",
         main = "Scree plot PCA de conectividad funcional",
         xlab = "Dimensiones",
         ylab = "Porcentaje de varianza explicado")
dev.off()


####===========================================================####
# Calidad de representación de las variables...
# ...respecto a las componentes seleccionadas y contribución...
# ...que realiza cada variable a la construcción de cada componente

# Listar los resultados de PCA para las variables
var <- get_pca_var(pca_fc)

# Calidad de la representación de las variables (cos2)
# tabla
var$cos2

# Gráfico de círculos en rejilla
jpeg("cos2_corrplot.jpeg", width = 1900, height= 1400, res = 300)
corrplot(var$cos2, is.corr = F, cl.lim = c(0, 1))
dev.off()

# Gráfico de barras con valores de representación decrecientes
jpeg("cos2_fviz1.jpeg", width = 1900, height= 1400, res = 300)
fviz_cos2(pca_fc, choice = "var", fill = "lightgoldenrod3", 
          color = "lightgoldenrod3",
          title = "Calidad de la representación de las variables (Dim1)",
          ylim = c(0, 0.8))
dev.off()
jpeg("cos2_fviz2.jpeg", width = 1900, height= 1400, res = 300)
fviz_cos2(pca_fc, choice = "var", fill = "lightgoldenrod3", 
          color = "lightgoldenrod3",
          title = "Calidad de la representación de las variables (Dim2)",
          axes = 2, ylim = c(0, 0.8))
dev.off()

# Calidad de la representación en el círculo de correlaciones
jpeg("cos2_cir_corr.jpeg", width = 1900, height= 1400, res = 300)
fviz_pca_var(pca_fc, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = T,
             title = "Calidad de la representación de las variables")
dev.off()

# Contribución de cada variable a la construcción del componente
# tabla
var$contrib

# Gráfico de círculos en rejilla
jpeg("contrib_corrplot.jpeg", width = 1900, height= 1400, res = 300)
corrplot(var$contrib, is.corr = F)
dev.off()

# Gráfico de barras con valores de representación decrecientes
# Contribuciones de variables a PC1 y PC2
jpeg("contribuc_fviz1.jpeg", width = 1900, height= 1400, res = 300)
fviz_contrib(pca_fc, choice = "var", axes = 1:2,
             title = "Contribución de cada variable (Dim1 y Dim2)",
             color = "turquoise4",
             fill = "turquoise4")
dev.off()
# Contribuciones de variables a PC3, PC4 y PC5
jpeg("contribuc_fviz2.jpeg", width = 1900, height= 1400, res = 300)
fviz_contrib(pca_fc, choice = "var", axes = 3:5,
             fill = "turquoise4",
             color = "turquoise4",
             title = "Contribución de cada variable (Dim3 a Dim5)")
dev.off()

# Contribuciones de variables en el círculo de correlaciones
jpeg("contrib_cir_corr1.jpeg", width = 1900, height= 1400, res = 300)
fviz_pca_var(pca_fc, col.var = "contrib",
             gradient.cols = c("blueviolet", "dodgerblue3",
                               "forestgreen", "chartreuse3",
                               "gold", "firebrick"),
             alpha.var = 0.8,
             axes = c(1, 2),
             repel = T,
             title = "Contribución relativa de las variables a PC1 y PC2")
dev.off()

jpeg("contrib_cir_corr2.jpeg", width = 1900, height= 1400, res = 300)
fviz_pca_var(pca_fc, col.var = "contrib",
             gradient.cols = c("blueviolet", "dodgerblue3",
                               "forestgreen", "chartreuse3",
                               "gold", "firebrick"),
             alpha.var = 0.8,
             axes = c(3, 4),
             repel = T,
             title = "Contribución relativa de las variables a PC3 y PC4")
dev.off()

jpeg("contrib_cir_corr3.jpeg", width = 1900, height= 1400, res = 300)
fviz_pca_var(pca_fc, col.var = "contrib",
             gradient.cols = c("blueviolet", "dodgerblue3",
                               "forestgreen", "chartreuse3",
                               "gold", "firebrick"),
             alpha.var = 0.8,
             axes = c(4, 5),
             repel = T,
             title = "Contribución relativa de las variables a PC4 y PC5")
dev.off()


####===========================================================####                
# Componentes principales obtenidos

# Correlaciones entre variables y components
var$cor

# Descripción de las 2 dimensiones relevantes (muestra la correlación
# como en el comando anterior, pero adiciona los valores p)
# Aunque ésto da más la idea de Hypothesis Testing, que no es el
# foco todavía, ya que éste análisis es descriptivo
desc <- dimdesc(pca_fc, axes = c(1, 2))
desc$Dim.1$quanti
desc$Dim.2$quanti

# Variables cuantitativas suplementarias
pca_interpret <- PCA(quejas_FC_num, scale.unit = T, ncp = 5,
                     quanti.sup = c(1:3))

jpeg("contrib_cir_corr_suppl.jpeg", width = 1900, height= 1400, res = 300)
