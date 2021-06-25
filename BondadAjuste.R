####===========================================================A.L.R.R.2021
# SCRIPT Probabilidad e Intervalos de confianza


####===========================================================####
# Set working directory
setwd(paste('/Users/adriana/Documents/default/',
            '/deafult', sep = ""))


####===========================================================####
# 1. We are interested in the heights of the players of the two...
# ... basketball teams Brose Baskets Bamberg and Bayer Giants...
# ... Leverkusen as well as the football team SV Werder Bremen.
# Calculate a 95% confidence interval for all three teams and..
# ...interpret the results

# Size of the 2 basketball teams and the football team
bamberg_n <- 16
leverkusen_n <- 14
bremen_n <- 23
all_n <- bamberg_n + leverkusen_n + bremen_n # n > 30

# Mean height: mean of the mean of all teams
bamberg_mean <- 199.06
leverkusen_mean <- 196.00
bremen_mean <- 187.52
all_mean <- mean(c(bamberg_mean, leverkusen_mean, bremen_mean))

# Variance in height: mean of the variance of all teams
bamberg_sd <- 7.047
leverkusen_sd <- 9.782
bremen_sd <- 5.239
all_sd <- mean(c(bamberg_sd, leverkusen_sd, bremen_sd))

# Confidence quotient: Z alpha/2
alpha2 <- (1 - 0.95) / 2
z025 <- qnorm(alpha2, lower.tail = F)

# Standard Error
errorest <- all_sd / sqrt(all_n)

# Margin of error
margerr <- z025 * errorest

# Lower limit
liminf <- all_mean - margerr

# Upper limit
limsup <- all_mean + margerr

# Output
cbind("Lim_inf(cm)" = round(liminf, 2),
      "Lim_sup(cm)" = round(limsup, 2))


####===========================================================####
# 2. A married couple tosses a coin after each dinner to...
# ...determine who has to wash the dishes. If the coin shows...
# ...head, then the husband has to wash the dishes, and if the...
# ...coin shows tails, then the wife has to wash the dishes...
# ...After 98 dinners, the wife notes that the coin has shown...
# ...heads 59 times.

# Estimate the probability that the wife has to wash the dishes.
print(paste("Dado que la decisión está basada en el",
            "lanzamiento de una moneda, y los intentos de cada",
            "lanzamiento son independientes el uno del otro,",
            "la probabilidad de que la esposa tenga que lavar",
            "los platos esta noche es de 0.5 o del 50%",
            sep = " "))

# Calculate and interpret the 95% confidence interval for p.

# Binomial experiment: distribución muestral es aprox. normal
# con media p y error estándar SE = sqrt(pq/n)
n <- 98 # Total number of trials
q <- 59 # Total number of failures (when the husband gets it)
x <- n - q # Total number of successes (the wife gets it)

# Proporción muestral: mejor estimador puntual para proporción...
# ... * poblacional *
pgorro <- x / n # Mean probability of success
qgorro <- 1 - pgorro

# Confidence quotient
z025 <- qnorm(0.05 / 2, lower.tail = F)

# Lower Limit
liminf <- pgorro - (z025 * sqrt((pgorro * qgorro) / n))

# Upper Limit
limsup <- pgorro + (z025 * sqrt((pgorro * qgorro) / n))

# Output
cbind("Lim_inf(p)" = round(liminf, 2),
      "Lim_sup(p)" = round(limsup, 2))


####===========================================================####
# 3. A random sample of 100 deaths recorded in the United States...
# ...last year revealed an average lifespan of 71.8 years....
# ...Assuming a population standard deviation of 8.9 years, does...
# ...this seem to indicate that the current half-life is greater...
# ...than 70 years? Use a significance level of 0.05.

# Datos
n <- 100
xbarra <- 71.8
sigma <- 8.9
signif <- 0.05
mu <- 70
H0 <- "mu = 70 years"
HA <- "mu > 70 years"

# Estadístico de prueba
Zest <- (xbarra - mu) / (sigma / sqrt(n))

# Valor crítico
Zcrit <- qnorm(signif, lower.tail = F)

# Prueba
if (Zest >= Zcrit) {
  print(paste("Se rechaza la H0 de que",
              H0),
        sep = " ")
} else {
  print(paste("No se rechaza la H0 de que",
                  H0),
            sep = " ")
}

# Valor p específico
p <- pnorm(Zest, lower.tail = F)

# Plot
library(ggplot2)
randNorm <- rnorm(100)
randDensity <- dnorm(randNorm)
ggplot(data.frame(x = randNorm, y = randDensity)) + 
  aes(x = x, y = y) +
  geom_point(
    ) + 
  labs(x = "Variable Normal Aleatoria", y = "Densidad"
       ) + geom_segment(aes(x = Zest, xend = Zest,
                                  y = 0, yend = 0.06),
                        lty = 3, color = "blue") + geom_segment(
                                             aes(x = Zcrit, xend = Zcrit,
                                                 y = 0, yend = 0.1),
                                             lty = 3, color = "red")
ggsave("normal.jpeg")

  
####===========================================================####
# 4. What is Wilcoxon-Mann-Whitney (WMW) U-Test?
# ...Give an example using R

# Select an R data set
data(iris)

# Count number of data
nrow(iris)

# Check that the structure of the data
str(iris)

# Subselect less than 30 rows
iris_np <- iris[sample(nrow(iris), 27) ,]
row.names(iris_np) <- NULL

# Check number of data points in each factor...
# ...each factor will be assumed to be an independent
# ...sample
nrow(iris_np[which(iris_np$Species=="virginica"),])
nrow(iris_np[which(iris_np$Species=="setosa"),])
nrow(iris_np[which(iris_np$Species=="versicolor"),])

# Create two data frames to compare
virginica <- iris_np[which(iris_np$Species=="virginica"), ]
versicolor <- iris_np[which(iris_np$Species=="versicolor"),]

# Check normality in Petal Length
shapiro.test(virginica$Sepal.Length)
shapiro.test(versicolor$Sepal.Length)

# U-Mann-Whitney
wilcox.test(virginica$Sepal.Length, versicolor$Sepal.Length)


####===========================================================####
# 5. What is the goodness-of-fit test 𝟀2?
# ...Give an example using R

# Following the previous example, we can test whether the sub-
# ...sample we followed
virginica <- iris_np[which(iris_np$Species=="virginica"), ]
versicolor <- iris_np[which(iris_np$Species=="versicolor"),]
setosa <- iris_np[which(iris_np$Species == "setosa"),]

iris_expected <- c(50/150, 50/150, 50/150)
iris_observed <- c(nrow(virginica), nrow(versicolor), nrow(setosa))

resultado <- chisq.test(iris_observed, p = iris_expected)
resultado

# Con un ∝=0.05 y dos grados de libertad, se tiene:

chiqalpha <- qchisq(0.05, 2, lower.tail = F)


# Prueba
H0 <- "hay un buen ajuste entre las frecuencias esperadas y las observadas."
HA <- "hay un ajuste deficiente entre las frecuencias esperadas y las observadas."

if (resultado$statistic > chiqalpha) {
  print(paste("Se rechaza la H0 de que",
              H0),
        sep = "")
} else {
  print(paste("No se rechaza H0. Por lo tanto, se asume que ",
              H0, sep = ""))
}
