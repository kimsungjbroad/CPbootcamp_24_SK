# Geometric (continued) -----

install.packages('ggbeeswarm')
library(ggbeeswarm)
library(tidyverse)

ggplot(mpg, aes(class, hwy, color = drv)) +
  geom_quasirandom()

ggplot(mpg, aes(class, hwy, color = drv)) +
  geom_beeswarm()

mpg %>%
  ggplot() +
  geom_bar(aes(x=class))

?geom_bar

mpg %>%
  ggplot() +
  geom_bar(aes(x=class, y = after_stat(count)))

mpg %>%
  ggplot() +
  geom_bar(aes(x=class, y = after_stat(prop), group = 1))

mpg %>% 
  dplyr::count(class) %>% 
  ggplot() + 
  geom_bar((aes(x=class, y= n)), stat = 'identity')


mpg %>%
  ggplot() +
  stat_summary(aes(x = class, y = hwy), 
               fun = median,
               fun.min = min, fun.max = max)


mpg %>%
  ggplot() +
  geom_bar(aes(x=class, y = after_stat(prop))) # out of 2 seater, all 2 seater (of course)

mpg %>%
  ggplot() +
  geom_bar(aes(x=class, y = after_stat(prop), group = 1)) #group =1 -> everything 

mpg %>%
  ggplot() +
  geom_bar(aes(x=class, fill = drv, y = after_stat(prop)))

mpg %>%
  ggplot() +
  geom_bar(aes(x=class, fill = drv, y = after_stat(prop), group = drv))


mpg %>% head


# Position obeject (fill)
diamonds

diamonds %>% 
  ggplot() +
  geom_bar(aes(x = cut, fill = clarity)) # default gives count? 

diamonds %>% 
  ggplot() +
  geom_bar(aes(x = cut, fill = color))

diamonds %>% 
  ggplot() +
  geom_bar(aes(x = cut, fill = clarity),
              position = 'stack')

diamonds %>% 
  ggplot() +
  geom_bar(aes(x = cut, fill = clarity),
           position = 'dodge') ## separate by fill group (clarity in this case) 

diamonds %>% 
  ggplot() +
  geom_bar(aes(x = cut, fill = clarity),
           position = 'dodge2') ## clearer than dodge?
diamonds %>% 
  ggplot() +
  geom_bar(aes(x = cut, fill = clarity),
           position = 'jitter')

diamonds %>% 
  ggplot() +
  geom_bar(aes(x = cut, fill = clarity),
           position = 'fill') ## 


diamonds %>% 
  ggplot() +
  geom_bar(aes(x = cut, fill = clarity),
           position = 'identity') ## stacked bar


diamonds %>% 
  ggplot() +
  geom_bar(aes(x = cut, fill = clarity),
           position = 'identity',
           alpha = 0.5) 


diamonds %>%
  ggplot() +
  geom_histogram(aes(price, fill = clarity), position ='identity')


diamonds %>%
  ggplot() +
  geom_boxplot(aes(x = clarity, y = price, fill = color), position = 'dodge2')

?geom_histogram
?geom_bar
?geom_boxplot


mpg %>% 
  ggplot() +
  geom_point(aes(x = displ, y = hwy),
             position = 'jitter') # scatter?


mpg %>%
  ggplot() +
  geom_boxplot(aes(x = class, y = hwy, fill = class), alpha = 0.5)

?geom_boxplot

mpg %>%
  ggplot() +
  geom_boxplot(aes(x = as.character(cyl), y=hwy, fill = as.character(cyl)), alpha = 0.7) +
  coord_flip() # flip x and y


mpg %>%
  ggplot() +
  geom_boxplot(aes(x = as.factor(cyl), y=hwy)) ## factor = categorical

mpg %>%
  ggplot() +
  geom_boxplot(aes(x = as.numeric(cyl), y=hwy))

?fct_infreq

mpg %>%
  ggplot() +
  geom_boxplot(aes(x = reorder(trans, displ, median),
                   ))

mpg %>%
  ggplot() +
  geom_boxplot(aes(x = as.character(cyl), y=hwy, fill = class), alpha = 0.7) # or you can use color instead of fill

mpg %>%
  ggplot() +
  geom_boxplot(aes(x = as.character(cyl), y=hwy, fill = class), alpha = 0.7) # or you can use color instead of fill


reorder()


# Coordinate Systems -----

box <- mpg %>% 
  ggplot() +
  geom_boxplot(aes(x = class, y =hwy, fill = class)) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=0.5),
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=1)) +
  theme(aspect.ratio = 1) + ## tight
  labs(x = 'Class', y = 'Highway')
box

bar <- diamonds %>% 
  ggplot() +
  geom_bar(aes(x = cut, fill =cut), 
           show.legend = FALSE, width = 1) + # width is your bar width
  theme(aspect.ratio = 1)

bar2 <- bar + coord_polar() 

f1 <- cowplot::plot_grid(box, bar, bar2, nrow=1) ## merge different plots
f1

mpg %>%
  ggplot() +
  geom_point(aes(x=cty, y=hwy)) +
  coord_trans(x = 'log', y = 'sqrt')

mpg %>%
  ggplot() +
  geom_point(aes(x=cty, y=hwy)) +
  coord_cartesian(xlim = c(0,30), ylim = c(0,50))

mpg %>%
  ggplot(aes(x=cty, y=hwy)) +
  geom_point() +
  geom_smooth()  

mpg %>%
  ggplot(aes(x=cty, y=hwy)) +
  geom_point() +
  geom_smooth() +
  coord_cartesian(xlim = c(10, 20), ylim = c(10,30)) ## zoom in and out
  
mpg %>%
  ggplot(aes(x=cty, y=hwy)) +
  geom_point() +
  geom_smooth() +
  geom_abline(color = 'grey') +
  geom_vline(aes(xintercept = 15, color = 'red')) +
  geom_hline(aes(yintercept = 20, color = 'red')) +
  geom_abline(slope = 2, intercept = 1, color='grey') +
  coord_cartesian(xlim = c(10, 20), ylim = c(10,30)) 


# Libraries ----
library(tidyverse)
library(taigr)
library(nycflights13)
options(taigaclient.path=path.expand("~/miniconda3/envs/taigapy/bin/taigaclient"))
tiny <- taigr::load.from.taiga("taigr-data-40f2.7/tiny_matrix")

flights
glimpse(flights)
summary(flights)

# Filtering rows -----
jan1 <- flights %>%
  dplyr::filter(month == 1, day == 1)

(march24 <- flights %>% dplyr::filter(month == 3, day ==24)) ## both print and assign when in ()

filter(flights, month ==1)

# filter flights depart either nov or dec
(nov_dec <- filter(flights, month == 11 | month == 12))

(nov_dec <- flights |> dplyr::filter(month == 11 | month == 12))

flights %>% dplyr::filter(!(arr_delay > 120 | dep_delay > 120)) # = arr_delay <= 120 & arr_delay <= 120

flights %>% dplyr::filter(arr_delay > 120, dep_delay > 120) # , works as & in default

# NA's are contigous
NA > 5
10 == NA
NA == NA

is.na(NA)

df <- tibble(x = c(1,NA,3)) #FALSE NA TRUE
df %>% dplyr::filter(x>1)
df %>% dplyr::filter(!(x>1))

df %>% dplyr::filter(is.na(x))

df %>% dplyr::filter((x>1) | is.na(x))

flights %>% dplyr::filter(arr_delay >= 2)

# Houston: IAH, HOU
colnames(flights)
flights %>% dplyr::filter(dest %in% c('IAH', 'HOU'))

flights %>% dplyr::filter(dest == 'IAH' | dest == 'HOU')

flights %>% dplyr::filter(is.na(dep_time))


# Arrange to sort the table
flights %>%
  dplyr::arrange(year, month, day) %>% head()

flights %>%
  dplyr::arrange(desc(dep_delay)) %>% head() ## descending order = largest value first

flights %>%
  dplyr::arrange(-dep_delay) %>% head()

## creating new group (column) & order categorical variables? & apply dictionary
flights$carrier




  