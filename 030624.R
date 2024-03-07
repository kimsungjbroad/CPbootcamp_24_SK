
# create some cell line data
cell_line_data <- data.frame(cell_line = paste0('c', 1:100), # cell line 
                             growth_rate = runif(100, 0.5, 5), # uniform distribution from 0.5 to 5 
                             lag_period = rexp(100, 1), # exponential dist (in days)
                             initial_cell_count = sample(c(25, 50, 100), 100, replace = TRUE, prob = c(0.25, 0.5, 0.25))
)

cell_line_data$lag_period %>%
  sort() %>% 
  plot(type = "s")

assay_end_point <- 5
pcr_bottleneck <- 2e4
seq_depth <- 1e6
threshold <- 40 * seq_depth / pcr_bottleneck

# First attempt (planning)
simulate_experiment <- function(cell_line_data,
                                assay_end_point = 5,
                                pcr_bottleneck = 2e4,
                                seq_depth = 1e6,
                                threshold = NULL){
  
  browser()
  
  if(is.null(threshold)){
    threshold <- 40 * seq_depth / pcr_bottleneck
  }
  
  # 1. calculate cell counts at the end-point
  simulate_growth <- function(cell_line_data, assay_end_point){
    # how long each cell doubled
    exp_period <- pmax(assay_end_point - cell_line_data$lag_period, 0) ## avoid negative values
    
    # how many doublings
    number_of_doublings <- exp_period * cell_line_data$growth_rate
    
    # final cell counts
    lysate_cell_counts <- cell_line_data$initial_cell_count * 2^number_of_doublings
    
    return(lysate_cell_counts)
  }
  lysate_cell_counts <- simulate_growth(cell_line_data,
                                        assay_end_point)
  
  # 2. simulate the pcr bottleneck
  simulate_pcr <- function(lysate_cell_counts, pcr_bottleneck){
    n <- length(lysate_cell_counts)
    sample_cells <- sample(n, pcr_bottleneck, replace = TRUE, prob = lysate_cell_counts)
    
    pcr_counts <- rep(0, 100)
    for(x in 1:100){
      pcr_counts[x] <- sum(sample_cells == x)
    }
    return(pcr_counts)
    ## without a loop
    # sample_table <- table(sample_cells)
    # pcr_counts <- rep(0, 100)
    # names(pcr_counts) <- 1:n
    # pcr_counts[names(sample_table)] <- sample_table
  }
  pcr_counts <- simulate_pcr(lysate_cell_counts,
                             pcr_bottleneck)
  
  # 3. simulate sequencing
  simulate_sequencing <- function(pcr_counts, seq_depth){
    n <- length(pcr_counts)
    sampled_counts = sample(n, seq_depth, replace = TRUE, prob = pcr_counts)
    sequencing_counts <- rep(0, n)
    for(x in 1:n){
      sequencing_counts[x] <- sum(sampled_counts == x)
    }
    return(sequencing_counts)
  }
  sequencing_counts <- simulate_sequencing(pcr_counts,
                                           seq_depth)
  
  # 4. count the detected cell lines
  count_detected <- function(sequencing_counts, threshold){
    detected_lines <- cell_line_data$cell_line[sequencing_counts >= threshold] 
    return(detected_lines)
  }
  detected_lines <- count_detected(sequencing_counts,
                                   threshold)
  
  return(detected_lines)
}

# a quick test
lysate_cell_counts = simulate_growth(cell_line_data, 5)
plot(simulate_growth(cell_line_data, 3), simulate_growth(cell_line_data, 5))

pcr_counts <- simulate_pcr(lysate_cell_counts, pcr_bottleneck)
hist(lysate_cell_counts)
hist(pcr_counts)

plot(log2(1 + lysate_cell_counts),
     log2(1 + pcr_counts))

sequencing_counts <- simulate_sequencing(pcr_counts, seq_depth)

install.packages('psych')
library(psych)

data.frame(initial = cell_line_data$initial_cell_count,
           lysate = lysate_cell_counts,
           pcr_input = pcr_counts,
           seq_output = sequencing_counts) %>%
  psych::pairs.panels()

data.frame(initial = log2(1 + cell_line_data$initial_cell_count),
           lysate = log2(1+ lysate_cell_counts),
           pcr_input = log2(1 + pcr_counts),
           seq_output = log2(1 + sequencing_counts)) %>%
  psych::pairs.panels()


# Some simple examples of loops!

# for loops (iterative)
#for(dummy in some_vector){
#  do this for each value of dummy in the vector
#}

for(value in c('my', 'first', 'for', 'loop')){
  print(value)
}

chars <- rep('', 4)
words <- c('my', 'second', 'for', 'loop')

for(i in 1:4){
  chars[i] <- words[i]
}

chars <- c()
for(i in 1:4){
  chars <- c(chars, words[i])
}


# while loops:
#while(condition){
#  run this as long as condition is TRUE
#}

roll <- function(){
  sample(6, 2, replace = TRUE)
}

roll()

n = 4
while(n>0){
  print(factorial(n))
  n = n-1
}


# let's write a loop that calls roll() till it gets a 6, 6 (how many times)
roll <- function(){
  sample(6, 2, replace = TRUE)
}
roll()

dice<-roll()
count <- 0
while(sum(dice) != 12){ # all(dice == c(6,6)) or dice[1] == 6 & dice[2] === 6
  count <- count + 1
  dice <- roll()
}
print(count)

# let's simulate this experiment 1000 times plot histogram of the counts
results = c()
for(i in 1:1000){
  dice<-roll()
  count <- 0
  while(sum(dice) != 12){ # all(dice == c(6,6)) or dice[1] == 6 & dice[2] === 6
    count <- count + 1
    dice <- roll()
  }
  results[i] <- count 
}
results
hist(results, 100)

fizzbuzz <- function(input){
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


# Write a vector version of the fizzbuzz
# test
# c(45, 46, 48, 50) gives c('FizzBuzz', '46', 'Fizz', 'Buzz')

fizzbuzz <- function(vector){
  results = c() ## this would be computationally expensive 
  results = rep('', length(vector)) ## if you know the length, it will be efficient, creating vector of '' and length = length(vector)
  for(i in 1:length(vector)){
    input = vector[i]
    if(input%%3 == 0){
      if(input%%15 == 0){
        results[i] <- 'FizzBuzz'
      } else {
        results[i] <- 'Fizz'
      }
    } else if(input%%5 == 0){
      if(input%%15 == 0){
        results[i] <- 'FizzBuzz'
      } else {
        results[i] <- 'Buzz'
      }
    } else {
      results[i] <- input
    }
  }
  return(results)
}


vector <- c(45, 46, 48, 50)


fizzbuzz(vector)



system.time((fizzbuzz(vector)))


# Tidyverse -----
install.packages('tidyverse')
library(tidyverse)
library(useful)
install.packages('nycflights13')
library(nycflights13)

# Tibbles vs Data Frames -----
vignette('tibble')

iris
?iris

iris %>% head

as_tibble(iris)

tibble(x = 1:5,
       y = -1,
       z = x^2 + 1)

# you cannot refer to the prvious column while creating a data frame
data.frame(x = 1:5,
           y = 1,
           z = x^2 + 1)

tibble(':(' = 'sad', ' ' = 'space', '200'  = 'number')

iris[, 'Sepal.Length', drop=FALSE] %>% head
iris_t[, 'Sepal.Length']

# transposed tibble
tribble(~x, ~y, ~z,
        'a', 1, 2,
        'b', 3, 4)


nycflights13::flights
flights

flights %>% 
  print(n=10, width = Inf)

flights %>%
  View

df <- tibble(x = runif(5),
             y = rnorm(5))
df

df$x

df[['x']]
df[[1]]

df %>%
  .$x

df %>%
  .[['x']]

as.data.frame(df)

df[1,] # first row
df[,1] # first column
df[1] # first column

as.data.frame(df)[1,]
as.data.frame(df)[,1]

# Data Visualization -----
?mpg

install.packages('Hmisc')
mpg %>% Hmisc::describe()

ggplot(data = mpg) +
  geom_point(aes(x = hwy,
                 y = cyl)) # point -> each point will have aesthetic of 

dim(mpg)
mpg

# scatter plot
ggplot(data = mpg) +
  geom_point(aes(x = class,
            y = drv))
# scatter plot 'jitter'
ggplot(data = mpg) +
  geom_jitter(aes(x = class,
                 y = drv))

ggplot(mpg) +
  geom_point(aes(x = displ,
                 y = hwy,
                 color = class))
  
install.packages('ggthemes')
library(ggthemes)
install.packages('ggpubr')
library(ggpubr)
install.packages('ggrepel')
library(ggrepel)
library(scales)
library(tidyverse)
library(useful)

ggplot(mpg) +
  geom_point(aes(x = displ,
                 y = hwy,
                 color = class),
             size = 4, alpha = .75) + #shape 
  theme_bw(base_size = 16,
           base_family = 'GillSans') +
  scale_color_wsj() +
  labs(x = 'Engine Displacement (liters)',
       y = 'Highway Efficiency (galons/mile)',
       color = '',
       title = 'Excpet 2 seaters, larger engine less efficient',
       subtitle = 'subtitle')

ggplot(mpg) + 
  geom_point(mapping = aes (x = displ, y = hwy, alpha = class))
  
ggplot(mpg) + 
  geom_point(mapping = aes (x = displ, y = hwy, shape = class))



ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = class)) +
  geom_point(size = 4, alpha = .75) +
  geom_text(data = mpg[mpg$class == '2seater', ], aes(label=class)) + 
  theme_bw(base_size = 16,
           base_family = 'GillSans') +
  scale_color_wsj() +
  labs(x = 'Engine Displacement (liters)',
       y = 'Highway Efficiency (galons/mile)',
       color = '',
       title = 'Excpet 2 seaters, larger engine less efficient',
       subtitle = 'subtitle')


ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = class)) +
  geom_point(size = 4, alpha = .75) +
  geom_text_repel(aes(label = ifelse(class == '2seater',  class, NA)),
                  color = 'black') + 
  theme_bw(base_size = 16,
           base_family = 'GillSans') +
  scale_color_wsj() +
  labs(x = 'Engine Displacement (liters)',
       y = 'Highway Efficiency (galons/mile)',
       color = '',
       title = 'Excpet 2 seaters, larger engine less efficient',
       subtitle = 'subtitle')

x <- ifelse(condition, x1, x2)

ggplot(mpg) +
  geom_point(aes(x = displ, y = hwy, colour = "firebricks"))
  
ggplot(mpg) +
  geom_point(aes(x = displ, y = hwy, color = cty))

ggplot(mpg) +
  geom_point(aes(x = displ, y = hwy, color = cty)) +
  scale_color_viridis_c()

# Facets -----

ggplot(mpg) +
  geom_point(aes(x = displ,
                  y = hwy)) +
  facet_wrap(. ~ class + drv) 

ggplot(mpg) +
  geom_point(aes(x = displ,
                 y = hwy)) +
  facet_wrap(. ~class, nrow = 2)



# Geometric Objects -----







