####===========================================================A.L.R.R.2021
# SCRIPT Análisis Multivariado


####===========================================================####
# Install necessary packages
library(ggplot2)
library(aplpack)
library(andrews)
library(GGally)
library(ggcorrplot)
library(ggpubr)
library(gridExtra)


####===========================================================####
# Set working directory
setwd(paste("/Users/adriana/Documents/Default/",
            "Default",
            sep = ""))


####===========================================================####
# Vector de medias x, matriz de varianzas-covarianzas S y
# matriz de correlaciones R de las variables numéricas.

# Cargar los datos y añadir columnas al environment
quejas <- read.csv("quejas.csv")
attach(quejas)

# Mostrar primeras observaciones de la base de datos y...
# ...su estructura
head(quejas)
str(quejas)
summary(quejas)

# Volver factores las variables de character
quejas$subjects <- factor(quejas$subjects)
quejas$SCD <- factor(quejas$SCD, levels = c("SCD", "CON"))
quejas$Gender <- factor(quejas$Gender)
quejas$timepoint <- factor(quejas$timepoint)

# Calcular la matriz de medias para las variables numéricas
vector_medias <-
  round(apply(quejas[, unlist(lapply(quejas, is.numeric))],
              2, mean, na.rm = T), 3)

# Calcular matriz de varianza-covarianza S
covar_matrix <- round(cov(quejas[, unlist(lapply(quejas, is.numeric))],
                          use = "pairwise.complete.obs"), 3)

# Matriz de correlaciones R
corr_matrix <- round(cor(quejas[, unlist(lapply(quejas, is.numeric))],
                         use = "pairwise.complete.obs"), 3)

# Ordenamos los valores de la matriz de correlaciones de una...
# ...de las partes triangulares de mayor a menor
ind <- order(abs(corr_matrix[lower.tri(corr_matrix, diag = F)]),
      decreasing = T)
val <- as.vector(corr_matrix[lower.tri(
  corr_matrix, diag = F)])[order(abs(
    corr_matrix[lower.tri(corr_matrix,
                          diag = F)]),
    decreasing = T)]
rbind("índice" = ind, "valores" = val)


####===========================================================####
# Varianza generalizada |S| y varianza muestral total

# Varianza generalizada. Determinante de la matriz de VarCov
det(covar_matrix)

# Varianza muestral total
n <- nrow(quejas)
sum(diag(covar_matrix))


####===========================================================####
# Distancia de Mahalanobis

# En este caso, usaremos imputación para no perder información...
# ...y poder obtener un valor de la distancia de Mahalanobis. ...
# ...Para esto, haremos imputación por la media:

# Confirmamos qué columnas tienen valores faltantes
colnames(quejas)[colSums(is.na(quejas)) > 0]

# Hacemos imputación para cada una
quejas[is.na(quejas$MFQFOFInvAver), "MFQFOFInvAver"] <-
  vector_medias["MFQFOFInvAver"]
quejas[is.na(quejas$fminor_ICVF), "fminor_ICVF"] <-
  vector_medias["fminor_ICVF"]
head(quejas)

# Cálculo de la distancia de Mahalanobis
dmahalanobis <- sqrt(mahalanobis(quejas[, unlist(lapply(
  quejas, is.numeric))],
  vector_medias,
  covar_matrix))

# Mostrar resultados de mayor a menor
dmahalanobis[order(dmahalanobis, decreasing = T)]

# Adicionamos la probabilidad de ocurrencia de los valores...
# ...de DM, considerando que la distancia de Mahalanobis...
# ...se distribuye como una Chi Cuadrado con grados de...
# ...libertad igual al número de variables en la matriz de datos

# Combinamos la matriz de datos, la distancia de Mahalanobis
# ...y la probablidad Chi-cuadrado (P[X ≤ x]), con GL = k variables
# ...valor crítico chi cuadrado: qchisq(0.0001, 5) = 0.08217738
dmahalanobisp <-
  cbind(quejas[, unlist(lapply(quejas, is.numeric))],
        "DM" = round(dmahalanobis, 3),
        "Prob" = round(pchisq(
          dmahalanobis, length(quejas[, unlist(
            lapply(quejas, is.numeric))])), 4))

# Mostramos sólo las 6 primeras filas
head(dmahalanobisp)

# Gráfico de cajas para detectar si existen valores atípicos
jpeg("Boxplot.jpeg", width = 1700, height= 1400, res = 300)
dm_boxplot <- boxplot(dmahalanobis, ylab = "Distancia de Mahalanobis")
dev.off()

# Valores atípicos
dmahalanobisp[which(dmahalanobisp$DM %in% round(dm_boxplot$out, 3)),]
quejas$subjects[which(dmahalanobisp$DM %in% round(dm_boxplot$out, 3))]

# Para detectar los datos atípicos, filtramos el dataframe...
# ...dmahalanobisp por las filas cuya probabilidad sea menor...
# ...a 0.001.

# Creamos un subgrupo de vectores atípicos p < 0.001
atipicos <- subset(dmahalanobisp, dmahalanobisp["Prob"] < 0.001)


####===========================================================####                
# Gráficos Multivariados

# i) Matriz de diagramas de dispersión:
# De acuerdo con el estatus de "deterioro cognitivo subjetivo":
# azul: SCD; amarillo: CON

# Medición inicial
jpeg("pairs_t0.jpeg", width = 3200, height = 1800, res = 300)
pairs(quejas[which(quejas$timepoint == 1),
             unlist(lapply(quejas, is.numeric))],
      pch = 20,  cex = 1.5,
      col = c("cornflowerblue",
              "darkgoldenrod")[quejas$SCD[which(
                quejas$timepoint == 1)]],
      cex.labels = 2)
dev.off()

# Segunda medición
jpeg("pairs_t1.jpeg", width = 3200, height = 1800, res = 300)
pairs(quejas[which(quejas$timepoint == 2),
             unlist(lapply(quejas, is.numeric))],
      pch = 20,  cex = 1.5,
      col = c("cornflowerblue",
              "darkgoldenrod")[quejas$SCD[which(
                quejas$timepoint == 2)]],
      cex.labels = 2)
dev.off()

# Tercera medición
jpeg("pairs_t2.jpeg", width = 3200, height = 1800, res = 300)
pairs(quejas[which(quejas$timepoint == 3),
             unlist(lapply(quejas, is.numeric))],
      pch = 20,  cex = 1.5,
      col = c("cornflowerblue",
              "darkgoldenrod")[quejas$SCD[which(
                quejas$timepoint == 3)]],
      cex.labels = 2)
dev.off()

# Por tiempos de medida
# Todos
jpeg("pairs_timepoints_black.jpeg", width = 3200, height = 1800, res = 300)
pairs(quejas[, unlist(lapply(quejas, is.numeric))],
      pch = 20,  cex = 1.5,
      cex.labels = 2)
dev.off()

# De manera separada
jpeg("pairs_timepoints.jpeg", width = 3200, height = 1800, res = 300)
pairs(quejas[, unlist(lapply(quejas, is.numeric))],
      pch = 20,  cex = 1.5,
      col = c("cornflowerblue",
              "darkgoldenrod3", "chartreuse4")[quejas$timepoint],
      cex.labels = 2)
dev.off()

# Diagrama de estrellas.

# Convertir "subjects" a "character" para los labels:
quejas$subjects <- as.character(quejas$subjects)
class(quejas$subjects)

# Tiempo 1
jpeg("stars1.jpeg", width = 3200, height = 2000, res = 300)
stars(quejas[which(quejas$timepoint==1),
             unlist(lapply(quejas, is.numeric))],
      labels = quejas$subjects[which(quejas$timepoint==1)],
      nrow = 6, flip.labels = F, xpd = F, lwd = 2,
      main = "Variación en T0",
      key.labels = colnames(quejas[,
                                   unlist(lapply(quejas, is.numeric))]),
      key.loc = c(26, 1), draw.segments = T)
dev.off()

# Tiempo 2
jpeg("stars2.jpeg", width = 3200, height = 2000, res = 300)
stars(quejas[which(quejas$timepoint==2),
             unlist(lapply(quejas, is.numeric))],
      labels = quejas$subjects[which(quejas$timepoint==2)],
      nrow = 6, flip.labels = F, xpd = T, lwd = 2,
      main = "Variación en T1",
      key.labels = colnames(quejas[,
                                   unlist(lapply(quejas, is.numeric))]),
      key.loc = c(17, 2), draw.segments = T)
dev.off()

# Verificando un valor:
min(quejas$MFQFOFInvAver[which(quejas$timepoint==2)])

# Tiempo 3
jpeg("stars3.jpeg", width = 3200, height = 2000, res = 300)
stars(quejas[which(quejas$timepoint==3),
             unlist(lapply(quejas, is.numeric))],
      labels = quejas$subjects[which(quejas$timepoint==3)],
      nrow = 6, flip.labels = F, xpd = T, lwd = 2,
      main = "Variación en T2",
      key.labels = colnames(quejas[,
                                   unlist(lapply(quejas, is.numeric))]),
      key.loc = c(15, 2), draw.segments = T)
dev.off()

# Caras de Chernoff.
jpeg("Chernoff_faces.jpeg", width = 3200, height = 2400, res = 300)
faces(quejas[, unlist(lapply(quejas, is.numeric))],
      main = "Descripción de la muestra",
      labels = quejas$subjects,
      ncol.plot = 13,
      cex = 1.5)
dev.off()

# Curvas de Andrews.
# Variación por punto de tiempo
jpeg("andrews_time.jpeg", width = 3200, height = 2400, res = 300)
andrews(quejas, clr = 4)
dev.off()

# Variación por sexo de los participantes
jpeg("andrews_sex.jpeg", width = 3200, height = 2400, res = 300)
andrews(quejas, clr = 3)
dev.off()

# Variación por estatus de SCD
jpeg("andrews_SCD.jpeg", width = 3200, height = 2400, res = 300)
andrews(quejas, type = 1, clr = 2)
dev.off()

# Otros gráficos apropiados:
# En forma de matriz
ggpairs(quejas[, c(4, 5:9)],
        mapping = ggplot2::aes(colour = timepoint))
ggsave("ggpairs.jpeg", width = 30, height = 20, units = "cm")

# Correlograma
ggcorrplot(corr_matrix,
           p.mat = cor_pmat(quejas[, unlist(lapply(quejas, is.numeric))]),
           type = "lower",
           color = c("#00AFBB", "white", "#FC4E07"),
           outline.col = "white",
           lab = T)
ggsave("ggcorrplot.jpeg", width = 20, height = 18, units = "cm")

# Lollipops modificado
# Quejas de memoria (MFQ)
ggplot(quejas, aes(x = subjects, y = MFQFOFInvAver)) +
  geom_segment(aes(x = subjects,
                   xend = subjects, y = mean(MFQFOFInvAver),
                   yend = MFQFOFInvAver),
               size = 0.5, color = "black") + coord_flip(
               ) +
  geom_hline(yintercept = mean(quejas$MFQFOFInvAver)) +
  geom_point(aes(color = timepoint),
             size = 3, alpha = 0.9) +
  theme_light() + theme(
    panel.border = element_blank())
ggsave("lollipop_mfq.png", width = 20, height = 24, units = "cm")

# ICVF
ggplot(quejas, aes(x = subjects, y = fminor_ICVF)) +
  geom_segment(aes(x = subjects,
                   xend = subjects, y = mean(fminor_ICVF),
                   yend = fminor_ICVF),
                  size = 0.5, color = "black") + coord_flip(
                  ) + geom_hline(yintercept = mean(quejas$fminor_ICVF)) +
  geom_point(aes(color = timepoint),
                                 size = 3, alpha = 0.9) +
  theme_light() + theme(
                    panel.border = element_blank())

# ACC_MFG
ggplot(quejas, aes(x = subjects, y = ACC_MFG)) +
  geom_segment(aes(x = subjects,
                   xend = subjects, y = mean(ACC_MFG),
                   yend = ACC_MFG),
               size = 0.5, color = "black") + coord_flip(
               ) + geom_hline(yintercept = mean(quejas$ACC_MFG)) +
  geom_point(aes(color = timepoint),
             size = 3, alpha = 0.9) +
  theme_light() + theme(
    panel.border = element_blank())

# ACC_INS
ggplot(quejas, aes(x = subjects, y = ACC_INS)) +
  geom_segment(aes(x = subjects,
                   xend = subjects, y = mean(ACC_INS),
                   yend = ACC_INS),
               size = 0.5, color = "black") + coord_flip(
               ) + geom_hline(yintercept = mean(quejas$ACC_INS)) +
  geom_point(aes(color = timepoint),
             size = 3, alpha = 0.9) +
  theme_light() + theme(
    panel.border = element_blank())

#ggarrange(plotlist = c(mfq, icvf, mfg, ins))
