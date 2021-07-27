####===========================================================A.L.R.R.2021
# Análisis Multivariado: Análisis de correspondencias, Biplot, Chi-cuadrado


####===========================================================####
# Install necessary packages
library(ggplot2)
library(ggcorrplot)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(gplots)
library(graphics)
library(vcd)
library(RColorBrewer)


####===========================================================####
# Set working directory
setwd(paste("/Users/adriana/Documents/default/",
            "default",
            sep = ""))


####===========================================================####
# Prepare data frame (to have categorical variables)

# Read data frame of missing data
datos <- read.csv("data_wide_missing.csv")
colnames(datos)

# Eliminar variables que no necesito
datos <- datos[,
               -which(colnames(datos) %in% c("MFQFOFInvAver_1",
                  "fminor_FA_1", "fminor_FA_2",
                  "fminor_FA_3", "fminor_MD_1",
                  "fminor_MD_2", "fminor_MD_3", "is_SCD_1",
                  "is_SCD_2", "is_SCD_3"))]

# Crear otra base de datos dividida por grupos
oldest_female <- datos[which(
  datos$AgeatBaseline_1 >= mean(datos$AgeatBaseline_1) &
    datos$Gender_1 == 0), ]
oldest_male <- datos[which(
  datos$AgeatBaseline_1 >= mean(datos$AgeatBaseline_1) &
    datos$Gender_1 == 1), ]
youngest_female <- datos[which(
  datos$AgeatBaseline_1 < mean(datos$AgeatBaseline_1) &
    datos$Gender_1 == 0), ]
youngest_male <- datos[which(
  datos$AgeatBaseline_1 < mean(datos$AgeatBaseline_1) &
    datos$Gender_1 == 1), ]

# Eliminar Age and Gender de "datos"
datos <- datos[, -which(colnames(datos) %in% c("AgeatBaseline_1",
                                             "Gender_1"))]

# Crear data frame con summaries
df <- datos[c(1:4), ]
df[c(1:4),] <- ""
df$subjects[1] <- "oldest_female"
df$subjects[2] <- "oldest_male"
df$subjects[3] <- "youngest_female"
df$subjects[4] <- "youngest_male"

# Rellenar la base de datos de faltantes basado en
# las dos bases de datos anteriores (excluyendo las 3 primeras
# variables: subjects, Age, and Gender)
for (i in df$subjects){
  if (i == "oldest_female") {
    df[which(df$subjects == i), -1] <-colSums(oldest_female[,-c(1:3)])
  } else if (i == "oldest_male") {
    df[which(df$subjects == i), -1] <- colSums(oldest_male[,-c(1:3)])
  } else if (i == "youngest_female") {
    df[which(df$subjects == i), -1] <- colSums(youngest_female[,-c(1:3)])
  } else {
    df[which(df$subjects == i), -1] <- colSums(youngest_male[,-c(1:3)])
  }
}

rm(list = ls(pattern="*male"))

# Trasponer la base datos
df <- t(df)
colnames(df) <- df[1,]
df <- df[-1,]

# Eliminar filas redundantes (todas vbles funcionales)
df <- df[c(1:6),]

# Renombrar variables funcionales
row.names(df) <- sub("ACC_LMFG", "SN_FC", row.names(df))

# Guardar la base de datos nueva para el trabajo
write.csv(df, "datos_faltantes.csv", quote = F)


####===========================================================####
# Análisis de correspondencias (CA) con gráfica de la tabla...
# ...de contingencias

# Cargar la base de datos (frecuencia de datos faltantes)
faltantes <- read.csv("datos_faltantes.csv", row.names = 1)
attach(faltantes)
str(faltantes)
faltantes

# Volver tabla para el siguiente gráfico
faltantes_tbl <- as.table(as.matrix(faltantes))

# Gráfica de balloon
jpeg("balloonplot.jpeg", width = 2800, height= 1800, res = 200)
balloonplot(faltantes_tbl, main = "Variables con datos faltantes",
            xlab ="", ylab = "",
            label = F, show.margins = T,
            scale.range = "relative")
dev.off()

# Gráfica de mosaico
jpeg("mosaicplot.jpeg", width = 2000, height= 1600, res = 210)
mosaicplot(faltantes, color = T, shade = c(1, 2), las = 1,
           main = "Datos faltantes")
dev.off()


####===========================================================####
# Prueba de independencia Chi - Squared

# Chi-squared
chisq.test(faltantes)
chisq.test(faltantes, simulate.p.value = T)

# Se ajusta la base de datos, ya que algunas celdas tienen... 
# ...valores muy pequeños. Sabemos que es más probable que hayan...
# ...datos faltantes en el segundo y tercer punto del tiempo,...
# ...así que podemos intentar eliminar el primer punto de tiempo:
faltantes_tiempo <- faltantes[-grep("_1", row.names(faltantes)), ]
faltantes_tiempo

# Intentamos el chi-cuadrado nuevamente
chisq <- chisq.test(faltantes_tiempo)
chisq.test(faltantes_tiempo, simulate.p.value = T)

# Resultados
# Residuales de Pearson (observados - esperados) / sqrt(esperados)
round(chisq$residuals, 3)

# Valores esperados
round(chisq$expected, 0)

# Valores observados
chisq$observed

# Grado de correlación (o dependencia) entre filas y
# ...columnas a partir de los residuales de la prueba X2
jpeg("corrplot_res.jpeg", width = 2000, height= 1600, res = 250)
corrplot(chisq$residuals, is.cor = F, tl.col = "black",
         cl.lim = c(-0.5, 0.5), tl.srt = 45,
         col = colorRampPalette(c(
           "dodgerblue4","white","firebrick4"))(200))
dev.off()

# Contribución en porcentajes (%)
contrib <- 100 * chisq$residuals ^ 2 / chisq$statistic
round(contrib, 2)

# Visualizar la contribución
jpeg("contrib.jpeg", width = 2000, height= 1600, res = 250)
corrplot(contrib, is.cor = F, tl.col = "black", tl.srt = 45)
dev.off()


####===========================================================####
# Autovalores y proporción de varianza retenida.

# Realizar el análisis de correspondencia
resultado <- CA(faltantes)
resultado

# Autovalores y proporción de varianza retenida
get_eigenvalue(resultado)


####===========================================================####                
# Screeplot
jpeg("screeplot.jpeg", width = 2000, height= 1600, res = 250)
fviz_screeplot(resultado, addlabels = T, ylim = c(0, 100),
               ylab = "Porcentaje de varianza explicada",
               xlab = "Dimensiones",
               barfill = "orange3",
               barcolor = "orange3",
               linecolor = "deepskyblue4") +
  geom_hline(yintercept = 1/(nrow(faltantes)-1) * 100,
             linetype = 2, color = "darkgray")
dev.off()


####===========================================================####                
# Biplot
jpeg("biplot.jpeg", width = 2000, height= 1600, res = 250)
fviz_ca_biplot(resultado, repel = T, arrow = c(F, F))
dev.off()


####===========================================================#### 
# Análisis de filas y columnas

# Filas
filas <- get_ca_row(resultado)

# Mostrar coordenadas de las filas
round(filas$coord, 3)

# Mostrar calidad de la representación de las filas en
# el espacio de coordenadas
round(filas$cos2, 3)
jpeg("ca_row_cos2.jpeg", width = 2000, height= 1600, res = 250)
fviz_ca_row(resultado, col.row = "cos2",
            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
            repel = T)
dev.off()

# Mostrar contribución de las filas (en %) a las dimensiones
round(filas$contrib, 2)
# A la dimensión 1
jpeg("row_cont_dim1.jpeg", width = 2000, height= 1600, res = 250)
fviz_contrib(resultado, choice = "row", axes = 1, fill = "lightblue3",
             color = "lightblue3")
dev.off()
# A la dimensión 2
jpeg("row_cont_dim2.jpeg", width = 2000, height= 1600, res = 250)
fviz_contrib(resultado, choice = "row", axes = 2, fill = "lightblue3",
             color = "lightblue3")
dev.off()
# A la dimensión 3
jpeg("row_cont_dim3.jpeg", width = 2000, height= 1600, res = 250)
fviz_contrib(resultado, choice = "row", axes = 3, fill = "lightblue3",
             color = "lightblue3")
dev.off()

# Columnas
cols <- get_ca_col(resultado)

# Mostrar coordenadas de las columnas
round(cols$coord, 3)

# Mostrar calidad de la representación (cos2) de las columnas en
# el espacio de coordenadas
round(cols$cos2, 3)
jpeg("col_cos2.jpeg", width = 2000, height= 1600, res = 250)
fviz_ca_col(resultado, col.col = "cos2",
            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
            repel = T)
dev.off()

# Mostrar contribución de las filas (en %) a las dimensiones
round(cols$contrib, 2)
# Utilizaremos, en esta ocasión, un gráfico de puntos para
# obtener esta información de manera simultánea
display.brewer.all() # paquete RColorBrewer
colores <- brewer.pal(9, "BuPu") # seleccionamos paleta
jpeg("corrplot_contrib_col.jpeg", width = 2400, height= 1600, res = 250)
corrplot(cols$contrib, is.corr = F, tl.col = "black",
         tl.cex = 1.2, cl.align.text = "l",
         tl.srt = 15, col = colorRampPalette(colores)(100),
         mar = c(0, 0, 0, 8))
dev.off()

# Ambos: filas y columnas
# Biplots en términos de la contribución de las filas (variables)
jpeg("biplot_flechas_filas.jpeg", width = 2000, height= 1600, res = 250)
fviz_ca_biplot(resultado, map = "colgreen",
               arrow = c(T, F),
               repel = T)
dev.off()

# Biplots en términos de la contribución de las columnas (actores)
jpeg("biplot_flechas_cols.jpeg", width = 2000, height= 1600, res = 250)
fviz_ca_biplot(resultado, map = "rowgreen",
               arrow = c(F, T),
               repel = T)
dev.off()

