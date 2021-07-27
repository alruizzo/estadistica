####===========================================================A.L.R.R.2021
# Análisis Multivariado: Análisis de Correspondencia Múltiple,
# Análisis Factorial de Datos Mixtos, Análisis de Correlación
# Canónica


# Install necessary packages ==============================================
pacman::p_load(ggplot2, GGally, fields, CCA, vegan, readxl, tidyverse,
               caret, ggpubr, MASS, dplyr, mda, klaR, FactoMineR,
               factoextra, corrplot, RColorBrewer)


# Set working directory ====================================================
setwd(paste("/Users/adriana/Documents/default/",
            "default",
            sep = ""))


# Prepare data frame ========================================================
# Prepare data frame (to have categorical variables)

# Read data frame of missing data
datos <- read.csv("data_wide_missing.csv")
colnames(datos)

# Eliminar variables que no necesito
datos <- datos[,
               which(colnames(datos) %in% c("subjects", "AgeatBaseline_1",
                                            "Gender_1", "MFQFOFInvAver_1",
                                            "fminor_ICVF_2", "fminor_ICVF_3",
                                            "ACC_LMFG_2", "ACC_LMFG_3"))]

# Volver variables categóricas
str(datos)
datos$AgeatBaseline_1 <- factor(ifelse(datos$AgeatBaseline_1 >= mean(
  datos$AgeatBaseline_1, na.rm = T),
  "edad >= media",
  "edad < media"))

datos$MFQFOFInvAver_1 <- factor(ifelse(datos$MFQFOFInvAver_1 >= mean(
  datos$MFQFOFInvAver_1, na.rm = T),
  "quejas >= media",
  "quejas < media"))

datos$Gender_1 <- factor(ifelse(datos$Gender == 0, "Fem", "Masc"))

datos$subjects <- factor(ifelse(grepl("sci", datos$subjects) == T,
                                "quejas", "control"))

colnames(datos) <- sub("ACC_LMFG", "conec_func", colnames(datos))
colnames(datos) <- sub("fminor_ICVF", "conec_struc", colnames(datos))
colnames(datos) <- sub("MFQFOFInvAver_1", "quejas", colnames(datos))
colnames(datos) <- sub("AgeatBaseline_1", "edad_inicial", colnames(datos))
colnames(datos) <- sub("Gender_1", "sexo", colnames(datos))
colnames(datos) <- sub("subjects", "grupo", colnames(datos))

# Marcar datos faltantes como tal
for (i in colnames(datos)){
  if (is.numeric(datos[, which(colnames(datos) == i)]) == T){
    datos[, which(colnames(datos) == i)] <-
      factor(ifelse(datos[, which(colnames(datos) == i)] == 1,
                    "faltante", "no faltante"))
  }
}

# Chequear 'datos'
str(datos)

# Guardar datos
write.csv(datos, "datos_cat_faltantes.csv", quote = F, row.names = F)


# 1. ========================================================================
# Análisis de correspondencias múltiple (MCA)

# Cargar base de datos
datos <- read.csv("datos_cat_faltantes.csv")
attach(datos)
str(datos)

# Convertir las variables de tipo character a factor
datos[, unlist(lapply(datos, is.character))] <-
  lapply(datos[, unlist(lapply(datos, is.character))], factor)
str(datos)
head(datos)

# Mostrar información descriptiva
display.brewer.all()
colores <- brewer.pal(12, "Set3")
for (variable in colnames(datos)){
  jpeg(paste(variable, ".jpg", sep = ""), width = 1800, height = 1400,
       res = 200)
  plot(datos[, which(colnames(datos) == variable)],
       main = variable, ylab = "Conteo",
       col = colores[which(colnames(datos) == variable)],
       ylim = c(0, 55))
  dev.off()
}

# Análisis de correspondencia múltiple
resultado <- MCA(datos)
resultado

# Proporción de varianza retenida
get_eigenvalue(resultado)

# Información de la varianza en un Scree Plot
jpeg("1_screeplot.jpg", width = 1800, height = 1400, res = 200)
fviz_screeplot(resultado, addlabels = T)
dev.off()

# Biplot de individuos y categorías de variables:
jpeg("1_biplot.jpg", width = 1800, height = 1400, res = 200)
fviz_mca_biplot(resultado, axes = c(1, 2), repel = T)
dev.off()

# Variables: coordenadas, representación y contribuciones
var <- get_mca_var(resultado)
var$coord
var$cos2
var$contrib

# Gráfico de coordenadas teniendo en cuenta la calidad de la representación
# Dimensiones 1 y 2
jpeg("1_coord12.jpg", width = 1800, height = 1400, res = 200)
fviz_mca_var(resultado, col.var = "cos2",
             axes = c(1, 2),
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = T)
dev.off()
# Dimensiones 3 y 4
jpeg("1_coord13.jpg", width = 1800, height = 1400, res = 200)
fviz_mca_var(resultado, col.var = "cos2",
             axes = c(3, 4),
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = T)
dev.off()

# Gráfico de contribuciones de las variables en todas las dimensiones
# a la vez
jpeg("1_corrplot_contrib.jpg", width = 1800, height = 1400, res = 200)
corrplot(var$contrib, is.corr = F, tl.col = "black",
        tl.srt = 45, cl.align.text = "l",
        title = "Contribuciones de las variables a las dimensiones",
        mar = c(0, 0, 2, 0))
dev.off()

# Individuos
ind <- get_mca_ind(res.mca)

# Gráficos
# Calidad de la representación
# Dimensiones 1 y 2
jpeg("1_cos2_12_ind.jpg", width = 1800, height = 1400, res = 200)
fviz_mca_ind(resultado, col.ind = "cos2", axes = c(1, 2),
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = T)
dev.off()
# Dimensiones 1 y 3
jpeg("1_cos2_13_ind.jpg", width = 1800, height = 1400, res = 200)
fviz_mca_ind(resultado, col.ind = "cos2", axes = c(1, 3),
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = T)
dev.off()

# Contribución de los individuos a las 3 dimensiones
jpeg("1_contrib_ind.jpg", width = 1800, height = 1400, res = 200)
fviz_contrib(resultado, choice = "ind", axes = 1:3)
dev.off()

# Agrupación por niveles de algunas variables
# Dimensiones 1 y 2
jpeg("1_ellipses12.jpg", width = 1800, height = 1400, res = 200)
fviz_ellipses(resultado, c("grupo", "sexo", "edad_inicial"),
              geom = "point", axes = c(1, 2),
              palette = brewer.pal(8, "Set2"))
dev.off()
# Dimensiones 1 y 3
jpeg("1_ellipses13.jpg", width = 1800, height = 1400, res = 200)
fviz_ellipses(resultado, c("grupo", "sexo", "edad_inicial"),
              geom = "point", axes = c(1, 3),
              palette = brewer.pal(8, "Set2"))
dev.off()


# 2. ========================================================================
# Análisis factorial para datos mixtos (FADM)

# Cargamos otra base de datos que contiene datos cuantitativos de la
# muestra con la que estamos trabajando
cuanti <- read.csv("quejas_cuantitativas.csv")
str(cuanti)

# Ajustar tipos de variables
cuanti[, -1] <- lapply(cuanti[, -1], as.numeric)
str(cuanti)

# Mostrar información descriptiva
for (variable in colnames(cuanti)){
  if (is.numeric(cuanti[[variable]])){
  jpeg(paste(variable, ".jpg", sep = ""), width = 1800, height = 1400,
       res = 200)
  hist(cuanti[, which(colnames(cuanti) == variable)],
       main = variable, ylab = "Conteo",
       xlab = "Valores",
       col = colores[which(colnames(cuanti) == variable)])
  dev.off()
  }
}

# Imputar valores faltantes con la media de la variable
for (variable in colnames(cuanti)){
  if (is.numeric(cuanti[[variable]])){
    cuanti[which(is.na(cuanti[[variable]]) == T) , variable] <-
      round(mean(cuanti[[variable]], na.rm = T), 0)
  }
}

# Revisar si quedaron celdas con NAs en la base de datos
which(is.na(cuanti) == T)

# Unir esta base de datos a la original
datos <- cbind(datos, cuanti[, -1])
head(datos)
summary(datos)

# Diferenciar valores de algunas celdas
datos$conec_struc_2 <- paste("sc2", datos$conec_struc_2)
datos$conec_struc_3 <- paste("sc3", datos$conec_struc_3)
datos$conec_func_2 <- paste("fc2", datos$conec_func_2)
datos$conec_func_3 <- paste("fc3", datos$conec_func_3)

# Análisis factorial para datos mixtos
resultado <- FAMD(datos)
resultado

# Valores propios
get_eigenvalue(resultado)
jpeg("2_screeplot.jpg", width = 1800, height = 1400, res = 200)
fviz_screeplot(resultado, addlabels = T)
dev.off()

# Gráficar variables en el mapa factorial
# Dimensiones 1 y 2
jpeg("2_factorial12.jpg", width = 1800, height = 1400, res = 200)
fviz_famd_var(resultado, repel = T)
dev.off()
# Dimensiones 3 y 4
jpeg("2_factorial34.jpg", width = 1800, height = 1400, res = 200)
fviz_famd_var(resultado, repel = T, axes = c(3, 4))
dev.off()
# Dimensiones 1 y 5
jpeg("2_factorial15.jpg", width = 1800, height = 1400, res = 200)
fviz_famd_var(resultado, repel = T, axes = c(1, 5))
dev.off()

# Gráfico de contribuciones a todas las dimensiones
jpeg("2_contrib_var.jpg", width = 1800, height = 1400, res = 200)
fviz_contrib(resultado, "var", axes = 1:5)
dev.off()

# Variables
# Extraemos información para las variables
# Cuantitativas
quanti.var <- get_famd_var(resultado, "quanti.var")
# Cualitativas
quali.var <- get_famd_var(resultado, "quali.var")

# Gráfico en términos de la contribución de las variables
# Cuantitativas
display.brewer.all()
colores <- brewer.pal(10, "PuOr")
jpeg("2_corrplot_quanti.jpg", width = 1800, height = 1400, res = 200)
corrplot(quanti.var$contrib, is.corr = F, tl.col = "black",
         tl.srt = 45, cl.align.text = "l", method = "pie",
         title = "Contribuciones de las variables cuantitativas a las dimensiones",
         mar = c(0, 0, 2, 0), col = colorRampPalette(colores)(100))
dev.off()
# Cualitativas
jpeg("2_corrplot_quali.jpg", width = 1800, height = 1400, res = 200)
corrplot(quali.var$contrib, is.corr = F, tl.col = "black",
         tl.srt = 45, cl.align.text = "l", method = "pie",
         title = "Contribuciones de las variables cualitativas a las dimensiones",
         mar = c(0, 0, 2, 0), col = colorRampPalette(colores)(100))
dev.off()

# Gráfico en términos de la calidad de la representación de las variables
# Cuantitativas
display.brewer.all()
colores <- brewer.pal(8, "YlOrBr")
jpeg("2_cos2_quanti.jpg", width = 1800, height = 1400, res = 200)
corrplot(quanti.var$cos2, is.corr = F, tl.col = "black",
         tl.srt = 45, cl.align.text = "l",
         title = "Representación de las variables cuantitativas en las dimensiones",
         mar = c(0, 0, 2, 0), col = colorRampPalette(colores)(50))
dev.off()
# Cualitativas
jpeg("2_cos2_quali.jpg", width = 1800, height = 1400, res = 200)
corrplot(quali.var$cos2, is.corr = F, tl.col = "black",
         tl.srt = 45, cl.align.text = "l",
         title = "Representación de las variables cualitativas en las dimensiones",
         mar = c(0, 0, 2, 0), col = colorRampPalette(colores)(50))
dev.off()

# Individuos
# Mapa de coordenadas
# Dimensiones 1 y 2
jpeg("2_coord12_ind.jpg", width = 1800, height = 1400, res = 200)
fviz_famd_ind(resultado, repel = T,
              col.ind = "cos2",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
dev.off()
# Dimensiones 3 y 4
jpeg("2_coord34_ind.jpg", width = 1800, height = 1400, res = 200)
fviz_famd_ind(resultado, repel = T,
              col.ind = "cos2",
              axes = c(3, 4),
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
dev.off()

# Mapa de factores
colores <- brewer.pal(12, "Paired")
jpeg("2_ellipses.jpg", width = 1800, height = 1400, res = 200)
fviz_ellipses(resultado, c("grupo", "quejas"), repel = T,
              palette = colores)
dev.off()


# 3. ========================================================================
# Análisis de correlación canónica (CCA)

# Cargamos los datos
conect <- read.csv("conectividad.csv", row.names = 1)
psico <- read.csv("psicologicas.csv", row.names = 1)

# Verificamos los tipos de variables
str(conect)
str(psico)

# Ajustamos la clase de algunas variables
conect$Gender <- factor(conect$Gender)
conect$SCD <- factor(conect$SCD, levels = c("SCD", "CON"))

# Crear una copia de 'conect' con variables numéricas solamente
conectq <- conect[, unlist(lapply(conect, is.numeric))]

# Graficar relaciones para cada conjunto de variables
jpeg("3_ggpairsconect.jpg", width = 1800, height = 1400, res = 200)
ggpairs(conectq, title = "Quejas y conectividad")
dev.off()
jpeg("3_ggpairspsico.jpg", width = 1800, height = 1400, res = 200)
ggpairs(psico, title = "Variables psicológicas")
dev.off()

# Graficar relaciones entre ambos conjuntos de variables
conjunto <- cbind(psico, conect)
jpeg("3_ggduo.jpg", width = 1800, height = 1400, res = 200)
ggduo(conjunto, columnsX = 1:5, columnsY = 8:12,
      types = list(continuous = "smooth_lm"),
      title = "Correlación entre conjuntos de variables",
      mapping = ggplot2::aes(color = SCD),
      legend = 3,
      xlab = "Variables psicológicas",
      ylab = "Quejas y conectividad") 
dev.off()

# Matrices de correlación entre matrices y dentro de cada matriz
mat_cor <- matcor(psico, conectq)
round(mat_cor$XYcor, 2)

# Análisis de correlación canónica
analisiscc <- cc(psico, conectq)

# Gráfico de la matriz de correlaciones
display.brewer.all()
colores <- brewer.pal(11, "RdBu")
jpeg("3_corrplot_matcor.jpg", width = 1800, height = 1400, res = 200)
corrplot(mat_cor$XYcor, tl.col = "black",
         method = "color",
         tl.srt = 45, col = colorRampPalette(rev(colores))(100),
         cl.align.text = "l",
         mar = c(0, 0, 1, 0),
         tl.cex = 0.8,
         title = "Matriz de correlaciones XY")
dev.off()

# Gráfico del análisis de correlación canónica
jpeg("3_acc.jpg", width = 1800, height = 1400, res = 200)
plt.cc(analisiscc,var.label = T)
dev.off()

