#Chapter 4
1 / 200 * 30

(59 + 73 + 2) / 3

sin(pi / 2)

x <- 3 * 4

x

this_is_a_really_long_name <- 2.5

r_rocks <- 2 ^ 3

r_rock

R_rocks

seq(1, 10)

x <- "hello world"

y <- seq(1, 10, length.out = 5)
y

(y <- seq(1, 10, length.out = 5))

my_variable <- 10
my_varıable

library(tidyverse)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

filter(mpg, cyl == 8)
filter(diamonds, carat > 3)

#Chapter 1
library(ggplot2)

install.packages("nycflights13")
library(nycflights13)
library(dplyr)
library(knitr)

flights

glimpse(flights)

airlines
kable(airlines)

airlines$name

glimpse(airports)

?flights

#Comments about R
#I learned that glimpse() shows all of the variables for a data set. 
#I also learned that the function kable() tells you what the abbreviations within
# a data set mean and its more reader friendly then glimpse()

# IRIS Data
head(iris)
glimpse(iris)

ggplot(data = iris, aes(x= Petal.Length, y = Petal.Width, color = Species)) +
  geom_point(size = 3, shape = "diamond") 

#As the petal length gets larger does the petal width get larger as well?
#The answer to this question is yes. It is obvious that as the petal length grows,
#so does the petal width.











