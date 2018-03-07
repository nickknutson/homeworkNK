library(tidyverse)
library(ggplot2)

# B1 is my m#
# B0 is my b#
#x is my predictor#


##question 1 - make a function where value of B1, n and st err are all different and ...#
#that I can use to create a column with the t value ...#
#(the estimate scaled by standard error of the estimate#

gen.df <- function(n = 100, b0 = 2, b1 = 3, sd.err = 1) {
  predictor <- runif(n) - 0.5 
  response <- b0 + b1 * predictor + rnorm(n, mean = 0, sd = sd.err) 
  return(data.frame(response = response, predictor = predictor)) 
}

n.times <- 1:10 #running 10 times with each n#
n <- c(25, 55, 85) #range of sample size#
b1 <- c(0.3, 1.2, 2.5, 3.6) #range of B1#
sd.err <- c(0.4, 1.7, 3.3, 5.2) #range of error#


nick.df <- expand.grid(n.times = n.times, n = n, b1 = b1, sd.err = sd.err)

View(nick.df)

# i have 480 rows, 4 columns. first column is n.times and goes 1 to 10 48 times / #
#next column is n which has 10 rows of 25, 55, 85 for each value of n.times / # 
#b1 goes from 0.2 to 1 to 2.7 to 4 every 30 rows / #
#st err goes from 0.2 to 1.5 to 3 to 5 every 40 rows#
#so for each value from one to 10, i have a row with 

# Q2 - use that dataframe to add in a t value column#

for (i in 1:length(nick.df$n.times)) {
  tmp.df <- gen.df(n = nick.df$n[i], b0 = 2, b1 = nick.df$b1[i], sd.err = nick.df$sd.err[i])
  tmp.lm <- lm(response ~ predictor, data = tmp.df)
  nick.df$tval[i] <- summary(tmp.lm)$coefficients["predictor", "t value"]
}

# Q3 show my results with a graph#

nickgraph.df <- ggplot(data = nick.df, aes(tval, group = b1, color = b1))
nickgraph.df + geom_density() + xlab("t value") + ylab("Density") + facet_grid(~sd.err)

#Q4 do the same thing but use a single parameter estimate of B1=0, so make B0=0. use 100 as sample size, instead of t value, make p value#

question4.df <- function(n = 100, b0 = 2, b1 = 3, sd.err = 1) {
  predictor <- runif(n) - 0.5 
  response <- b0 + b1 * predictor + rnorm(n, mean = 0, sd = sd.err) 
  return(data.frame(response = response, predictor = predictor)) 
}

n.times <- 1:10
n <- c(25, 55, 85)
b1 <- c(0)
sd.err <- c(0.4, 1.7, 3.3, 5.2)

test2.df <- expand.grid(n.times = n.times, n = n, b1 = b1, sd.err = sd.err)

for (i in 1:length(test2.df$n.times)) {
  tmp.df2 <- question4.df(n = test2.df$n[i], b0 = 0, b1 = test2.df$b1[i], sd.err = test2.df$sd.err[i])
  tmp.lm2 <- lm(response ~ predictor, data = tmp.df2)
  test2.df$pval[i] <- summary(tmp.lm2)$coefficients["predictor", "Pr(>|t|)"]
}

View(test2.df)

# question 4a#

question4a <- ggplot(data = test2.df, aes(pval, group = n, col = n))
question4a + geom_density() + facet_grid(~sd.err) + xlab("p value") + ylab("Density")

#question 4b#


pvalue <- which(test2.df$p.val <= 0.05)
tabulate(pvalues)

prop <- (sum(tabulate(pvalue))/120) * 100
print(prop)

#question 4#

question4c <- ggplot(data = test2.df, aes(pval, group = n, col = n))
question4c + geom_density() + facet_grid(~sd.err)

#question 5#

give him an html of results
