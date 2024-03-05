
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

attributes(die)

colnames(die) <- NULL
die

rownames(die) <- NULL
die

class(die)

die<-1:6
class(die)

# we use lists to group arbitrary objects
list1 <- list(1:100, 3, 'five', mean, matrix(1:12, 3) )
names(list1) <- c('vector', 'numeric', 'character', 'function', 'matrix')
list1

list2 <- list('vector' = 1:10,
              'numeric' = 3,
              'character' = 'five',
              'func' = mean,
              'matrix' = matrix(1:12, 3))

list2

# a data frame is a list of vectors of the same length
df <- data.frame(face = c('ace', 'two', 'six'),
                 suit = c('clubs', 'clubs', 'clubs'),
                 value = c(1,2,3))

df

# if any of the entries is a constant, it will be recycled
data.frame(face = c('ace', 'two', 'six'),
           suit = c('clubs', 'clubs', 'clubs'),
           value = 1)
typeof(df)
class(df)
str(df)

faces <- c('king', 'queen', 'jack', 'ten', 'nine',
           'eight', 'seven', 'six', 'five', 'four',
           'three', 'two', 'one')
suits = c('spades', 'clubs', 'hearts', 'diamonds')
values <- seq()

rep(c('a', 'b', 'c'), 2)

deck <- data.frame(suit = rep(suits, each = 13),
                   face = rep(faces, 4),
                   value = rep(values, times = 4))


deck

# how to save and load data
write.csv(x=deck, file = 'Data/deck.csv')

deck.read <- read.csv('Data/deck.csv')
deck.read

head(deck, n=10)
tail(deck)
summary(deck)
glimpse(deck)
str(deck)

## Rdata save
list1
saveRDS(list1, file='Data/list.RDS')
list1.read <- readRDS('Data/list.RDS')
list1.read

deck.read <- data.table::fread('Data/deck.csv')



random_matrix <- matrix(rnorm(3000), 50)
dim(random_matrix)

library(useful)
corner(random_matrix) ## 5 x 5 upper left

write.csv(random_matrix, 
          'Data/random_matrix.csv')

random_matrix.read <- read.csv('Data/random_matrix.csv')

random_matrix.read %>% corner


random_matrix.read <- data.table::fread('Data/random_matrix.csv')
random_matrix.read %>% as.matrix()


# Dicing and selecting datasets -----

dim(deck)
head(deck)
nrow(deck)

deck[1,1]
deck[1,1,3]
deck[1:5, 2:3]

deck[,0]

deck[1:5, -3]
deck[1:4, c(-1,3)] # doesn't work

deck[1:4, c('suit', 'value')]

names(die) <- c('one', 'two', 'three', 'four', 'five', 'six')
die
die['three']

deck[1:4, c(TRUE, FALSE, TRUE)]

deck[1:5, 2, drop=FALSE]

deck$suit
deck$face
deck[deck$suit == "spades", ]

list1$vector
list1$character

list1[1]
list1[1:2]
list1[-1]

list1[1] %>% class()

deck$value %>% median()

deck$suit %>% unique()

suit.table <- deck$suit %>% table()

suit.table['clubs']

sum(deck$suit == 'clubs')

mean(deck$suit == 'clubs')

deck2 <- deck

# Modifying values -----

# a fresy copy of the deck
deck2 <- deck

vec <- rep(0,6)
vec

vec[1]
vec[1] <- 1000

vec[c(1,3,5)] <- c(1,2,1)
vec

vec[4:6] <- vec[4:6] +1
vec

vec[1:3] <- vec[1:3] + vec[4:6]
vec

vec[7] <- 0
vec
vec[7] <- NA
vec
vec <- vec[-7] ## remove
vec


deck2 %>% head()

deck2$new <- 1:52

deck2$new <- NULL

deck2 %>% head()

# let's bump the values of aces to 14
deck2$value[deck2$face == 'one'] <- c(14)
deck2[deck2$face == 'one', 3] <- 14 

sample(10)

sample(1:6, replace=TRUE)

# shuffling the deck
deck3 <- deck[sample(52), ]
deck3

deck$face == 'one'

deck3[deck3$face == 'one', 3] <- 14

deck3$face[deck3$face == 'one'] <- 'ace'
deck3

# some simple examples with booleans
1 > 2
2>1
1 > c(0,1,2)
c(1,2) > c(0,1,2)
all(c(1,2,3) == c(3,2,1))

1 %in% c(3,4,5)
c(1,2) %in% c(3,4,5) #no recycling!
c(1,2,3) %in% c(3,4,5)
c(1,2,3,4) %in% c(3,4,5)

any(1 == c(3,4,5)) # equal to 1 %in% c(3,4,5)

deck4 <- deck3[sample(52), ] ## shuffle

deck4$value[deck4$suit != 'hearts'] <- 0
deck4

deck4$value[deck4$face == 'queen' & deck4$suit == 'spades'] <- 13

# NA's are contiguous
1 + NA
NA == 1
c(NA, 1:50)
mean(c(NA, 1:50))

deck4$value[1] <- NA
deck4

x <- c(NA,1:50)
x[!is.na(x)]

x <- c(NA, 1:50, 1/0)
x
is.finite(x)

deal <- function(deck, n){
    shuffle <- deck[sample(nrow(deck), n, replace=TRUE), ]
    return(shuffle)
}

deal <- function(deck, n){
  n.deck <- nrow(deck)
  shuffled_deck <- deck[sample(n.deck), ]
  hand <- head(shuffled_deck, n)
  rest <- head(shuffled_deck, n.deck-n)
  return(list(hand = hand, rest = rest))
}


# Taiga -----
install.packages('devtools')
library(devtools)
devtools::install_github("https://github.com/broadinstitute/taigr")

dir.create(path.expand("~/.taiga"))
write("d9899913-c88e-4c6f-962e-ebd0a2071ffe", file=path.expand("~/.taiga/token"))
options(taigaclient.path=path.expand("~/miniconda3/envs/taigapy/bin/taigaclient"))
library(taigr)
tmp <- taigr::load.from.taiga("taigr-data-40f2.7/tiny_matrix")
tmp


# Conditionals -----

num <- -2
if(num < 0){
  num <- num * -1
}
num

absolute_value <- function(num){
  if(num<0){
    num <- num * -1
  }
  return(num)
}

absolute_value(-2)

x <- 1
if(3 == 3){
  x <- 2
}
x


x <- 1
if(x==1){
  x <- 2
  if(x==1){
    x <-3
  }
}
x


a <- 3.14
dec <- a - trunc(0) # truncated part of integer a
dec

if(dec >= 0.5){
  a <- trunc(a) + 1
} else
  {a <- trunc(a)
}
a

a <- 20
b <- 20

if(a>b){
  print("A wins")

}

# write a function that takes one integer as input and 
# print 'Fizz' if it is a multiple of 3, 
# prints 'Buzz' if it is a multiple of 5 ,
# and 'FizzBuzz' if it is a multiple of 15
# if input is none of those, print the input itself

f <- function(input){
  if(input%%3 == 0){
    if(input%%15 == 0){
      print('FizzBuzz')
    } else {
    print('Fizz')
    }
  } else if(input%%5 == 0){
    if(input%%15 == 0){
      print('FizzBuzz')
    } else {
    print('Buzz')
    }
  } else {
    print(input)
  }
}

f(45)

# sometimes it is easier to use look-up table

print_die <- function(x){
  die = c('one', 'two', 'three', 'four', 'five', 'six')
  if(x %in% 1:6){
    print(die[x])
  }else{
    print("Input is not between 1 and 6!")
  }
}

print_die(10)


# create some cell line data
cell_line_data <- data.frame(cell_line = paste0('c', 1:100), # cell line 
                             growth_rate = runif(100, 0.5, 5), # doubling 
                             log_period = rexp(100, 1), # in days
                             initial_cell_count = sample(c(25, 50, 100), 100, replace = TRUE, prob = c(0.25, 0.5, 0.25))
)

cell_line_data$log_period %>%
  sort() %>%
  plot(type = "s")





