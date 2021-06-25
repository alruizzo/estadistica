####===========================================================A.L.R.R.2021
# SCRIPT Probabilidades


####===========================================================####
# Install required packages
install.packages("prob")
require(prob)


####===========================================================####
# Set working directory
setwd(paste('/Users/adriana/Documents/default/',
            '/Actividades/Unidad_2', sep = ""))


####===========================================================####
# 1. Finding probabilities

# Two dice are rolled
A <- rolldie(2, makespace = T)

# The sum of the numbers is a 7
Prob(A, X1 + X2 == 7)

# The sum is at least 11
Prob(A, X1 + X2 >= 11)

# The sum is at most 2
Prob(A, X1 + X2 <= 2)

# You get a double
Prob(A, X1 == X2)

# Do not get a double
Prob(A, X1 != X2)


####===========================================================####
# 2. Determining a probability

# 8 red, 3 white, and 9 blue [=20] chips;
# 3 tiles are drawn w/o replacement and without order.
# (n r) = n! / r!(n -r)!
# R = 8; W = 3; B = 9

N <- 20; R <- 8; W <- 3; B <- 9; drawn <- 3

# a. All 3 tiles are white (combinatorial rule)
allw <- round((choose(W, 3) / choose(N, drawn)), 5); allw

# b. 2 are red and 1 are white (multiplication rule)
redw <- round (((choose(W, 1) * choose(R, 2)) /
                  choose(N, drawn)), 4); redw

# c. At least 1 is white (1, 2 or 3)
wleast <- 1-((choose(W, 0) * (choose((N-W), drawn))) /
                    choose(N, drawn)); round(wleast, 4)

# d. One of each color is drawn
each <- round(((choose(W, 1) * choose(R, 1) * choose(B, 1)) /
                 choose(N, drawn)), 4); round(each, 2)


####===========================================================####
# Load data for Point 3

# Read file
datasethomework <- read.csv("./datasethomework2.csv",
                            row.names = "X")


####===========================================================####
# Point 3: Consider only observations 150 through 193...
# ... Suppose four students are selected at random  

# Define as "dataWORK2" and check its size:
dataWORK2 <- data.frame(datasethomework[150:193,]) # Create df
row.names(dataWORK2) <- NULL # Adjust row numbering
size_dataWORK2 <- dim(dataWORK2); size_dataWORK2 # Check size

# Define as "Financing" the object that represents the type of...
# financing used by students to pay for their studies...
# ...Make it a factor

# Check dataframe structure to identify that object:
str(dataWORK2)

# Change name of that object:
colnames(dataWORK2)[which(colnames(
  dataWORK2)=="Financiacion")] <-"Financing"

# Make it a factor:
dataWORK2$Financing <- factor(dataWORK2$Financing)
# Check whether it's a factor and its levels:
is.factor(dataWORK2$Financing); levels(dataWORK2$Financing)

# Construct a frequency table for the variable Financing
# ...and save it as a data frame to make things easier
freqtable <- data.frame(table(dataWORK2$Financing))
freqtable <- data.frame(t(freqtable)); freqtable
colnames(freqtable) <- freqtable[1,]
freqtable <- freqtable[2,]; row.names(freqtable) <- NULL
freqtable[] <- as.numeric(freqtable[]); str(freqtable)

# What is the probability of selecting two scholarship holders...
# ...and two non-scholarship holders? (4 students selected)
twotwo <- (choose(freqtable$Beca, 2) *
             choose((freqtable$Credito + freqtable$Otro), 2)) /
  choose(nrow(dataWORK2), 4); twotwo

# If they select one at a time, what is the probability that...
# ...the first two students selected are on scholarships and the...
# ...last two are not on scholarships.
P <- (freqtable$Beca/nrow(dataWORK2)) *
  ((freqtable$Beca-1)/(nrow(dataWORK2)-1)) *
  ((freqtable$Credito + freqtable$Otro)/(nrow(dataWORK2)-2)) *
  (((freqtable$Credito + freqtable$Otro)-1)/(nrow(dataWORK2)-3))
round(P, 4)

# What is the probability of selecting four scholarship students?
four <- choose(freqtable$Beca, 4) /
  choose(nrow(dataWORK2), 4); four

# What is the probability of selecting three non-scholarship students?
three <- choose((freqtable$Credito + freqtable$Otro), 3) *
  choose(freqtable$Beca, 1) /
  choose(nrow(dataWORK2), 4); round(three, 2)
