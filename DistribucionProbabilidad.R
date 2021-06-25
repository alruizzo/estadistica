####===========================================================A.L.R.R.2021
# SCRIPT Random Variables and Distribution Probability


####===========================================================####
# Set working directory
setwd(paste('/Users/adriana/Documents/default/',
            '/default/', sep = ""))


####===========================================================####
# 1. Determine the value c so that each of the following functions...
# ...can serve as a probability distribution of the discrete...
# ...random variable X: f (x) = c (x2 + 4), para x = 0; 1; 2; 3:

# El conjunto de pares ordenados (x, f (x)) es una distribución...
# ...de probabilidad de la variable aleatoria discreta X...
# ...si, para cada resultado x posible, Sum𝑥 𝑓(𝑥) = 1

x <- c(0, 1, 2, 3)
c <- 1 / sum((x^2 + 4))
fx <- c * (x^2 + 4)
probdist <- data.frame(x, fx)
    
# Express the results graphically as a probability histogram
jpeg("probhist1.jpeg", width = 1440, height = 1440, res = 300)
barplot(probdist$fx, main = "Distribución de Probabilidad de X",
        names.arg = c("0", "1","2","3"), border = "darkgreen",
        col = "lightblue", xlab = "Valores posibles de x",
        ylab = "Probabilidad de X")
dev.off()


####===========================================================####
# 2. A shipment of 7 television sets contains 2 defective sets...
# A hotel makes a random purchase of 3 of the sets. If x is the...
# ...number of defective sets purchased by the hotel, and the...
# probability distribution of X.

# Distribución de probabilidad
totaltv <- 7 # television sets; total N
defect <- 2 # defective sets; éxitos k
buenos <- totaltv - defect # m
compra <- 3 # random purchase of sets n

# Probability distribution
x <- c(0, 1, 2) # Nr possible defective sets
Px <- dhyper(x, defect, exitos, compra) # prob distribution
tabla <- data.frame(x, "Px" = round(Px, 2))

# Express the results graphically as a probability histogram.
jpeg("probhist2a.jpeg", width = 1440, height = 1440, res = 300)
plot(x, Px, type = "h", lty  = 3, lwd = 3, pch = 2,
     xlab = "Número posible de TVs defectuosos",
     ylab="Prob de comprar TVs defectuosos",
     main="Función de Probabilidad Hyper(7, 3, 2)",
     col="blue", ylim = c(0, 0.6))
dev.off()

# Express the results graphically as a probability histogram
jpeg("probhist2.jpeg", width = 1440, height = 1440, res = 300)
barplot(tabla$Px, main = "Histograma de Probabilidad de X=x",
       names.arg = c("0", "1", "2"),
       border = "darkgreen",
       col = "lightblue",
       xlab = "Número posible TVs defectuosos",
       ylab = "Probabilidad de comprar TVs defectuosos",
       ylim = c(0, 0.6))
dev.off()


####===========================================================####
# 3.  An attendant at a car wash is paid according to the number...
# ...of cars that pass through. Suppose the probabilities are...
# ...1/12, 1/12, 1/4, 1/4, 1/6, and 1/6, respectively, that the...
# ...attendant receives $7, $9, $11, $13, $15, or $17 between...
# ...4:00 P.M. and 5:00 P.M. on any sunny Friday. Find the...
# ...attendant's expected earnings for this particular period.

# Solución: Valor esperado de la variable aleatoria x:

ganancias <- c(7, 9, 11, 13, 15, 17) # x
probs <- c(1/12, 1/12, 1/4, 1/4, 1/6, 1/6) # f(x)
tabla <- data.frame(ganancias, probs=round(probs, 2))
valoresp <- sum(tabla$ganancias * tabla$probs) # sum(x * f(x))

print(paste("La ganacia esperada entre las 4 y 5 p.m. de ",
            "cualquier viernes para este trabajador son de $",
            valoresp, sep = ""))

# Express the results graphically as a probability histogram. 
jpeg("probhist3.jpeg", width = 1440, height = 1440, res = 300)
barplot(tabla$probs, main = "Distribución de Probabilidad de X",
        names.arg = c("7", "9", "11", "13", "15", "17"),
        border = "darkgreen",
        col = "lightblue", xlab = "Ganancias posibles en US$",
        ylab = "Probabilidad de Ganancia")
abline(v = 3.8, col = "darkgreen", lwd = 4, lty = 2)
dev.off()


####===========================================================####
# 4. The stature of 1000 students is normally distributed with...
# ...a mean of 174.5 centimeters and a standard deviation of 6.9...
# ...centimeters. If heights are assumed to be rounded to the...
# ...nearest half centimeter, how many of these students would...
# ...expect that they had a stature:

n <- 1000 # número de estudiantes; como n > 30 ~ Z
mean <- 174.5 # media de estatura en cm
sd <- 6.9 # desv estándar de estatura en cm

set.seed(100) # establecer comienzo común
students <- rnorm(n, mean, sd) # crear una distribución aleatoria
students <- round(students / 0.5) * 0.5 # redondear al .5cm más cercano
studDens <- dnorm(students, mean, sd) # densidad de la distribución

# a) less than 160.0 centimeters?
q <- 160.0 # valor cuantil para buscar probabilidad
p <- pnorm(q, mean, sd) # probabilidad de encontrar valores < 160.0
round((p * n)) # casos favorables / casos totales para esta "p"
print(paste("De 1000 estudiantes, se espera que ",
            round((p * n)),
            " tengan estatura por debajo de 160.0 cm",
            sep = ""))

# b) between 171.5 and 182.0 centimeters inclusive?
p <- pnorm((182.0 - 0.5), mean, sd) -
  pnorm((171.5 - 0.5), mean, sd)
round((p * n)) # casos favorables / casos totales para esta "p"
print(paste("De 1000 estudiantes, se espera que ",
            round((p * n)),
            " tengan estaturas entre 171.0 y 182.5 cm",
            sep = ""))

# c) equal to 175.0 centimeters?
print(paste("De 1000 estudiantes, no se espera que ",
            "*ninguno* tenga una estatura exactamente igual a 175.0 ",
            "cm, ya que el cálculo de la probabilidad de una ",
            "variable aleatoria continua se basa en límites para ",
            "el cálculo del área bajo la curva.",
            sep = ""))
  
# d) greater than or equal to 188.0 centimeters?
q <- 188.0 - 0.5 # valor cuantil para buscar la prob (-0.5 para incluirlo)
p <- pnorm(q, mean, sd, lower.tail = F) # prob de encontrar valores >= 160.0  
print(paste("De 1000 estudiantes, se espera que ",
            round((p * n)), " estudiantes",
            " tengan estaturas mayores o iguales a 188.0 cm",
            sep = ""))

