
# ----

# a royal flush

hand <- c('ace', 'king', 'queen', 'jack', 'ten')

typeof(hand)
length(hand)

# matrices are two dimensional vectors
?matrix()
die = 1:6
matrix(data = die)
m <- matrix(data = die, nrow = 2)
matrix(data = die, ncol = 2)
matrix(die, ncol = 3, byrow = TRUE)

matrix(c(die,1), nrow = 2)

ar <- array(c(11:14, 21:24, 31:34),
      dim = c(2,2,3)) ##row, column, number of matrix


attributes(die)
attributes(m)

names(die) <- c('one', 'two', 'three', 'four', 'five', 'six')
attributes(die)

die

dim(die) <- c(2,3) ## lost names
die

rownames(die) <- c('r1', 'r2')
colnames(die) <- c('c1', 'c2', 'c3')
die 
