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

# Function -----
library(ggplot2)

roll_fair <- function(){
  dice <- sample(1:6, 2, TRUE)
  #results <- paste0("Roll: ", dice[1], " and ", dice[2], " gives ", sum(dice))
  results <- sum(dice)
  return(results)
}

roll_loaded <- function(){
  dice <- sample(1:6, 2, TRUE, prob=c(1,1,1,1,10,10))
  #results <- paste0("Roll: ", dice[1], " and ", dice[2], " gives ", sum(dice))
  results <- sum(dice)
  return(results)
}

fair_sum <- replicate(1000, roll_fair())
loaded_sum <- replicate(1000, roll_loaded())

hist(fair_sum, 10)
hist(loaded_sum, 10)

fig1 <- ggplot2::qplot(fair_sum)
fig2 <- ggplot2::qplot(loaded_sum)

fig1

install.packages("cowplot")
library(cowplot)

cowplot::plot_grid(fig1, fig2, nrow = 1)

roll_many <- function(n){
  dice1 <- sample(1:6, n, TRUE)
  dice2 <- sample(1:6, n, TRUE)
  return(dice1 + dice2)
}

roll_many(2)

ggplot2::qplot(roll_many(1e4))


library(magrittr)

# manual
x <- 3
y <- exp(x)
z <- sqrt(y)
t <- log10(z)
s <- abs(t)

# Or in one line
s <- abs(log10(sqrt(exp(3))))

# Or pipe
x %>%
  exp() %>%
  sqrt() %>%
  log10() %>%
  abs()

x <- 3
x %>%
  substraction(1)

# R OBJECTS -----

# atomic vectors
die

is.vector(die)
length(die) # will use very common

five <-5
is.vector(five)
length(five)

typeof(die)
typeof(five)

sqrt(2)^2 -2 


logicals <- c(TRUE, FALSE, T, F, F, F)

logicals
typeof(logicals)















