####===========================================================A.L.R.R.2021
# SCRIPT básico sobre modelo de regresión lineal simple


####===========================================================####
# Install necessary packages

install.packages("Hmisc")
library(Hmisc)


####===========================================================####
# Set working directory
setwd(paste('/Users/adriana/Documents/default/',
            'default/', sep = ""))


####===========================================================####
# 1. Realice un análisis exploratorio de la base de datos.

# Read file
geiser <- read.csv("./geiser.csv", row.names = "X")
View(geiser)
attach(geiser)

# Get a sense of the data
head(geiser)

# Size of variables
dim(geiser)

# Descriptives based on the Hmisc package
describe(eruptions) # Duración erupciones
sd(eruptions) # Desviación estándar
describe(waiting) # Tiempo de espera entre erupciones
sd(waiting) # Desviación estándar

# Histogram of eruptions
jpeg("eruptions.jpeg", width = 1440, height = 1440, res = 300)
hist(eruptions, main = "Histograma duración erupciones",
     xlab = "Duración de la erupción Geiser Old Faithful",
     ylab = "Frecuencia", col = "lightblue"); dev.off()
# Histogram of waiting
jpeg("waiting.jpeg", width = 1440, height = 1440, res = 300)
hist(waiting, main = "Histograma tiempo de espera",
     xlab = "Tiempo de espera entre erupciones Geiser Old Faithful",
     ylab = "Frecuencia", col = "lightblue"); dev.off()

# Outliers
jpeg("eruptions_boxplot.jpeg", width = 1440, height = 1440, res = 300)
boxplot(eruptions, ylab = "Duración de la erupción"); dev.off()
jpeg("waiting_boxplot.jpeg", width = 1440, height = 1440, res = 300)
boxplot(waiting, ylab = "Tiempo de espera erupción"); dev.off()


####===========================================================####
# 2. Construya un gráfico de dispersion que relacione la...
# ...variable predictora de la duracion de la erupcion...
# ...(eruptions) y la variable respuesta tiempo de espera...
# ...(waiting). ¿Es razonable suponer que existe una relacion de...
# ...dependencia lineal entre estas variables?

jpeg("scatter_plot.jpeg", width = 1440, height = 1070, res = 300)
plot(eruptions, waiting, xlab = "Duración erupción",
     ylab = "Tiempo espera para erupción",
     main = "Gráfico de dispersión", col = "black")
dev.off()


####===========================================================####
# 3. Ajuste un modelo de regresión lineal que relacione el tiempo...
# ...de espera con la duración de la erupción. Interprete los...
# ...parámetros estimados del modelo.

# Especificación del modelo: 'waiting': variable respuesta (Y)...
# ...'eruptions', variable predictora (X):
modelo_geiser <- lm(waiting ~ eruptions)

# Model values
summary(modelo_geiser)
