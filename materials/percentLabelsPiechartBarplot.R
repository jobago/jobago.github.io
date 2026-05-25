# Adding percentage to pie charts and bar plots
# Jose Barrera. 29-03-2009
# jbarrera@mat.uab.cat
#
# The two following functions perform pie charts and barplots with percentage labels for a variable "x". This variable must be defined as a factor and "levels" and "labels" for this factor also must be defined.
#
# Input:
#  factor: a class factor variable with defined "levels" and "labels".
#  dec: number of decimal places in the percentage output.
#  color: fill color in the barplot. 

require(Hmisc)

# Adding percentage to pie charts:
mypie <- function(factor, dec = 0, color = 0)
 {
  x <- summary(factor)
  l <- length(x)
  col <- ifelse(color == 0, rainbow(l), color)
  perc <- round(100 * x / sum(x), dec)
  info <- paste(levels(factor), perc, sep = ": ")
  info <- paste(info, "%", sep = "")
  pie(x, labels = info, col = col)
  title(main = label(factor))
 }

# Adding percentage to bar plots:
mybars <- function(factor, dec = 0, color = 7)
 {
  x <- summary(factor)
  l <- length(x)
  perc <- round(100 * x / sum(x), dec)
  info <- paste(perc, "%", sep = "")
  barplot(x, main = label(factor), ylab="Frequency", col = color)
  text(1.2 * (0:(l - 1)) + 0.7, x / 2, info)
 }


# Example:


# Function for generating a multinomial sample (thanks to Klaus Langohr):
multinomialSample <- function(n, pr)
 rep(1:length(pr), rmultinom(1, size = n, pr))

# Factor "pop.sample" containing a place of residence:
# multinomial sample:
population <- c(5345477,  769818, 709246,  417917) 
popSample <- multinomialSample(n = 1200, pr = population) 
popSample <- as.factor(popSample)
levels(popSample) <- c("Barcelona", "Tarragona", "Girona", "Lleida")
label(popSample) <- "Location (simple random sample)" 
head(popSample)
table(popSample)

# Factor "ageSample" containing an age multinomial sample:
agepop <- c(1083951, 5084172, 1195955)
ageSample <- multinomialSample(n = 1200, pr = agepop) 
ageSample <- as.factor(ageSample)
levels(ageSample) <- c("0-14", "15-64", "65+")
label(ageSample) <- "Age (simple random sample)" 
head(ageSample)


# Plots:
mypie(factor = popSample, color = c(3, 4, 7, 2))
mybars(factor = ageSample, color = "pink")
