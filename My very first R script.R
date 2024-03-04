# ------
# MY VERY FIRST INTRODUCTION TO r
# AUTHOR: SK
# DATE: March 4 2024
# Description: Frist lecture notes from the CP Bootcamp

# Introduction -----
3+4
3 ^42
3+ 
  4
3+ 5
3 + 5 ; 12 - 8
3
3:10
3:100
6 / 2^2 -1
(6 / 2)^2 -1

# This is my first vector; 
1:6
# Here is a longer vector
23:59

# R Objects -----
a <- 3

die <- 1:6

# Naming objects
nameAreCaseSensitivie <- 3
nameAreCaseSENSITIVE <- 5

# They cannot start with a number or a special character
1object < - 100
@noSpecialChracters < - 200

underscores_or.dots.are.okay <- 10

nameAreCaseSensitivie / 2

ls()

NA <- 3
NULL <- 5
TRUE <- 10

# elementwise operations
die
die - 1
die * 2

double_die <- die * 2

die * die
die * double_die

die %*% die

die %o% die
rep(1/6, 6)
probability = rep(1/6, 6)

die * probability

## multiply probability and sum --> %*%
sum(die * probability)
die %*% probability

## multiply two vector
die * 1:2
die * c(1,2,1,2,1,2)
die * 1:3
## it gives warning but it works 
die * 1:4
die * 1:5
## this doesn't give you warning because die has 1 to 6 
die * 1:6

# gives *1 and *2 (according to your second vector)
die %o% c(1,2)
die %o% c(1,2,3)

# Functions -----
round(3.5)
rnorm(1) ## generate random number from a normal distribution 
factorial(5)
log2(16)
log10(120)

rnorm(n = 10, mean = 0, sd = 3)
mean(1:10)
mean

round(mean(1:10))
round(exp(2))

a <- exp(2) ## will be updated (if that is specified before)
rounded_a <- round(a)

sample(x = die, size = 2, replace = TRUE) ## random die, sampling with replacement (i.e. sampling, and replace the number)
sample(die, 2, TRUE)
sample(x = die, size = 2, replace = FALSE) ## sampling without replacement

sample(x = die, size = 7) ## gives error because size assigned is bigger than die

?sample()
??sample

roll2 <- function(){
  dice <- sample(1:6, 2, TRUE)
  return(sum(dice))
}

roll2()
outcome <- roll2()

roll2()
roll2()
roll2()

roll2.v2 <- function(x){
  dice <- sample(x, 2, TRUE)
  return(sum(dice))
}

roll2.v2(die)


# Packages -----
?install.packages()

install.packages('useful')

## once in a lifetime
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install('ComplexHeatmap', force=TRUE)


library(ggplot2)
qplot()

library(askgpt)

library(tidyverse)

dplyr::filter()
library(ComplexHeatmap)

ComplexHeatmap::Heatmap()

Heatmap()

x <- c(-1, 0.8, 0.5, 3, -2, 10, 1.1, 5)
x <- sort(x)
y<-x^3
y
plot(x,y, type="b")

?base::plot

qplot(x, y)

qplot(y)


