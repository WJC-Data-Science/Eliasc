---
title: "Case Study 2"
output: 
  html_document: 
    keep_md: true
---
#Background
I learned several things while making these plots. I learned house to use the geom_path() function in ggplot. I also leared how to use the group_by() funciton several different ways. Be able to change the legend titles was new to me as well.I believe this task gave me an overall better unerstanding of how tidyverse and ggplot2 are connected

#Images

```r
library(ggplot2)
library(gapminder)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
head(gapminder)
```

```
## # A tibble: 6 x 6
##   country     continent  year lifeExp      pop gdpPercap
##   <fct>       <fct>     <int>   <dbl>    <int>     <dbl>
## 1 Afghanistan Asia       1952    28.8  8425333      779.
## 2 Afghanistan Asia       1957    30.3  9240934      821.
## 3 Afghanistan Asia       1962    32.0 10267083      853.
## 4 Afghanistan Asia       1967    34.0 11537966      836.
## 5 Afghanistan Asia       1972    36.1 13079460      740.
## 6 Afghanistan Asia       1977    38.4 14880372      786.
```

```r
mydata <- filter(gapminder, country != "Kuwait")

mydata
```

```
## # A tibble: 1,692 x 6
##    country     continent  year lifeExp      pop gdpPercap
##    <fct>       <fct>     <int>   <dbl>    <int>     <dbl>
##  1 Afghanistan Asia       1952    28.8  8425333      779.
##  2 Afghanistan Asia       1957    30.3  9240934      821.
##  3 Afghanistan Asia       1962    32.0 10267083      853.
##  4 Afghanistan Asia       1967    34.0 11537966      836.
##  5 Afghanistan Asia       1972    36.1 13079460      740.
##  6 Afghanistan Asia       1977    38.4 14880372      786.
##  7 Afghanistan Asia       1982    39.9 12881816      978.
##  8 Afghanistan Asia       1987    40.8 13867957      852.
##  9 Afghanistan Asia       1992    41.7 16317921      649.
## 10 Afghanistan Asia       1997    41.8 22227415      635.
## # … with 1,682 more rows
```

```r
#Graph #1
ggplot(data = mydata) +
  geom_point(mapping = aes(x = lifeExp, y = gdpPercap,
                           size = pop/100000, 
                           color = continent)) +
  xlab("Life Expectancy") +
  ylab("GPD per Capita") +
  scale_y_continuous(trans = "sqrt") +
  scale_size_continuous(name = "Population 100k") +
  facet_wrap(~year, ncol = 12) +
  theme_bw() 
```

![](Case-Study-2_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

```r
ggsave(
  filename = "graph1.png",
  plot = last_plot(),
  width = 15,
  units = c("in"),
  dpi = 300
)
```

```
## Saving 15 x 5 in image
```

```r
#Graph #2
graph2_data <- mydata %>%
  select(continent,year, gdpPercap, pop) %>%
  group_by(continent, year) %>%
  summarize( gdp =weighted.mean(gdpPercap, pop),sum = sum(pop))
```

```
## Warning in summarise_impl(.data, dots, environment(), caller_env()): integer
## overflow - use sum(as.numeric(.))

## Warning in summarise_impl(.data, dots, environment(), caller_env()): integer
## overflow - use sum(as.numeric(.))

## Warning in summarise_impl(.data, dots, environment(), caller_env()): integer
## overflow - use sum(as.numeric(.))

## Warning in summarise_impl(.data, dots, environment(), caller_env()): integer
## overflow - use sum(as.numeric(.))

## Warning in summarise_impl(.data, dots, environment(), caller_env()): integer
## overflow - use sum(as.numeric(.))

## Warning in summarise_impl(.data, dots, environment(), caller_env()): integer
## overflow - use sum(as.numeric(.))

## Warning in summarise_impl(.data, dots, environment(), caller_env()): integer
## overflow - use sum(as.numeric(.))

## Warning in summarise_impl(.data, dots, environment(), caller_env()): integer
## overflow - use sum(as.numeric(.))
```

```r
graph2_data
```

```
## # A tibble: 60 x 4
## # Groups:   continent [5]
##    continent  year   gdp       sum
##    <fct>     <int> <dbl>     <int>
##  1 Africa     1952 1311. 237640501
##  2 Africa     1957 1445. 264837738
##  3 Africa     1962 1541. 296516865
##  4 Africa     1967 1775. 335289489
##  5 Africa     1972 2063. 379879541
##  6 Africa     1977 2245. 433061021
##  7 Africa     1982 2295. 499348587
##  8 Africa     1987 2181. 574834110
##  9 Africa     1992 2072. 659081517
## 10 Africa     1997 2099. 743832984
## # … with 50 more rows
```

```r
ggplot(data = mydata) +
  geom_point(mapping = aes(x = year, y = gdpPercap, 
                            size = pop/100000, 
                            color = continent)) +
  geom_path(mapping = aes(x = year, y = gdpPercap, color = continent, group = country)) +
  geom_point(data = graph2_data, mapping = aes(x = year, y = gdp, size = sum/100000)) +
  geom_line(data = graph2_data, mapping = aes(x = year, y = gdp)) +
  xlab("Year") +
  ylab("GPD per Captia") +
  scale_size_continuous(name = "Population 100k") +
  facet_wrap(~continent, ncol = 5) +
  theme_bw()
```

```
## Warning: Removed 8 rows containing missing values (geom_point).
```

![](Case-Study-2_files/figure-html/unnamed-chunk-1-2.png)<!-- -->

```r
ggsave(
  filename = "graph2.png",
  plot = last_plot(),
  width = 15,
  units = c("in"),
  dpi = 300
)
```

```
## Saving 15 x 5 in image
```

```
## Warning: Removed 8 rows containing missing values (geom_point).
```

```r
ggplot(data = graph2_data) +
  geom_point(data = graph2_data, mapping = aes(x = year, y = gdp, size = sum)) +
  geom_line(mapping = aes(x = year, y = gdp)) +
  facet_wrap(~continent, ncol = 5)
```

```
## Warning: Removed 8 rows containing missing values (geom_point).
```

![](Case-Study-2_files/figure-html/unnamed-chunk-1-3.png)<!-- -->

