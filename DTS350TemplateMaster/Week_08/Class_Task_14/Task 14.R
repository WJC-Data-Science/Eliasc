## Task 14

## Load in libraries

library(ggplot2)
library(tidyverse)
library(readr)
library(stringr)

## Load in the datasets

RandomLetters <- read_lines("https://github.com/WJC-Data-Science/DTS350/raw/master/randomletters.txt")

RandomLettters_numbers <- read_lines("https://github.com/WJC-Data-Science/DTS350/raw/master/randomletters_wnumbers.txt")


## Problem 1
every1700 <- c()
for (i in seq(1, str_length(RandomLetters)/1700)) {every1700 <- str_c(every1700, str_sub(RandomLetters, start = i*1700, end = i*1700))}

every1700

str_length(RandomLetters)

## "he plural of anecdote is not data"

## Problem 2
SingleandDoubleNum <- c()
SingleandDoubleNum <- str_extract_all(RandomLettters_numbers,("\\d\\d|\\d"))  
SingleandDoubleNum

convert <- str_replace_all(SingleandDoubleNum, c("10" = "j","11" = "k","12" = "l","13" = "m","14" = "n","15" = "o","16" = "p","17" = "q","18" = "r","19" = "s","20" = "t","21" = "u","22" = "v","23" = "w","24" = "x","25" = "y","26" = "z","1" = "a","2" = "b","3" = "c","4" = "d","5" = "e","6" = "f","7" = "g","8" = "h","9" = "i"
                                                 ))
convert
### Experts Often Posses More Data Than Judgement 

## Problem 3
RandomLetters_NoSpace <- RandomLetters %>% 
  str_remove_all("[ ]")

RandomLetters_NoSpace_NoPeriods <- RandomLetters_NoSpace %>% 
  str_remove_all("[.]")

RandomLetters_NoSpace_NoPeriods

str_detect(RandomLetters_NoSpace_NoPeriods, ("[aeiou]{7}"))

str_locate(RandomLetters_NoSpace_NoPeriods, ("[aeiou]{7}"))

str_extract(RandomLetters_NoSpace_NoPeriods, ("[aeiou]{7}"))



