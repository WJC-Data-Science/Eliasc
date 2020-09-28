---
title: "Case Study 5"
output: 
  html_document:
    keep_md: TRUE
---
## FiveThirtyEight Article
In this article, they show the 33,000 gun deaths that happen each year and in which categories they fit in to. The categories are suicide, homocide, accidental/undetermined, mass shootings, terrorism, and shootings of police officers


```r
library(tidyverse)
```

```
## ── Attaching packages ────────────────────────────────────────────────── tidyverse 1.3.0 ──
```

```
## ✓ ggplot2 3.3.0     ✓ purrr   0.3.3
## ✓ tibble  2.1.3     ✓ dplyr   0.8.5
## ✓ tidyr   1.0.2     ✓ stringr 1.4.0
## ✓ readr   1.3.1     ✓ forcats 0.5.0
```

```
## ── Conflicts ───────────────────────────────────────────────────── tidyverse_conflicts() ──
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
library(dplyr)
library(ggplot2)

download.file("https://raw.githubusercontent.com/fivethirtyeight/guns-data/master/full_data.csv", "full_data.csv")

full_data <- read_csv("full_data.csv")
```

```
## Warning: Missing column names filled in: 'X1' [1]
```

```
## Parsed with column specification:
## cols(
##   X1 = col_double(),
##   year = col_double(),
##   month = col_character(),
##   intent = col_character(),
##   police = col_double(),
##   sex = col_character(),
##   age = col_double(),
##   race = col_character(),
##   hispanic = col_double(),
##   place = col_character(),
##   education = col_character()
## )
```

```r
head(full_data)
```

```
## # A tibble: 6 x 11
##      X1  year month intent  police sex     age race    hispanic place  education
##   <dbl> <dbl> <chr> <chr>    <dbl> <chr> <dbl> <chr>      <dbl> <chr>  <chr>    
## 1     1  2012 01    Suicide      0 M        34 Asian/…      100 Home   BA+      
## 2     2  2012 01    Suicide      0 F        21 White        100 Street Some col…
## 3     3  2012 01    Suicide      0 M        60 White        100 Other… BA+      
## 4     4  2012 02    Suicide      0 M        64 White        100 Home   BA+      
## 5     5  2012 02    Suicide      0 M        31 White        100 Other… HS/GED   
## 6     6  2012 02    Suicide      0 M        17 Native…      100 Home   Less tha…
```

```r
view(full_data)
```
## Graphic to mimic article's findings

```r
male_suicide = full_data %>% 
  filter(intent == "Suicide" & age >= 45)

ggplot(data = male_suicide) +
  geom_bar(mapping = aes(x = month, fill = sex), position = 'dodge') +
  facet_wrap(~month)
```

![](Case-Study-5_files/figure-html/unnamed-chunk-2-1.png)<!-- -->
This is a grpahic that reflects how the article stated that the male population far out weighs the females population in regards to total amount of suicides.



### Customer's Concerns

```r
homicide <- full_data %>% 
  filter(intent == "Homicide", age <= 30, race == "Black")

ggplot(data = homicide) +
  geom_bar(mapping = aes(x = month, fill = sex), position = 'dodge')+
  facet_wrap(~month) +
  ggtitle("Homicide Rates") +
  theme_bw()
```

![](Case-Study-5_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
suicide <- full_data %>% 
  filter(intent == "Suicide", age <= 20, race == "White")

ggplot(data = suicide) +
  geom_bar(mapping = aes(x = month, fill = sex), position = 'dodge') +
  facet_wrap(~month) +
  ggtitle("Suicide Rates") +
  theme_bw()
```

![](Case-Study-5_files/figure-html/unnamed-chunk-3-2.png)<!-- -->

```r
undetermined <- full_data %>% 
  filter(intent == "Undetermined")


ggplot(data = undetermined) +
  geom_bar(mapping = aes(x = month, fill = sex), position = 'dodge') +
  facet_wrap(~month) +
  ggtitle("Undetermined Rates") +
  theme_bw()
```

![](Case-Study-5_files/figure-html/unnamed-chunk-3-3.png)<!-- -->
## Homicide Image
Looking at the data for homicides througout the months, I would recommmend the company to focus on the younger black populations during the summer and winter months

## Suicide Image
Looking at the data for suicides rates for the white population below 20 years old, I would recommend the company focuses on the spring months, especially April

## Undetermined Image
If the company was interested in focusing on undetermined cases, then they should devote their time in November for males and September for females.

