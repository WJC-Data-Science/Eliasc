---
title: "Case Study 4"
output:
  html_document:
    keep_md: True
code_folding: hide
---


```r
library(nycflights13)
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
library(ggplot2)

df_flights <- nycflights13::flights
df_flights
```

```
## # A tibble: 336,776 x 19
##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
##  1  2013     1     1      517            515         2      830            819
##  2  2013     1     1      533            529         4      850            830
##  3  2013     1     1      542            540         2      923            850
##  4  2013     1     1      544            545        -1     1004           1022
##  5  2013     1     1      554            600        -6      812            837
##  6  2013     1     1      554            558        -4      740            728
##  7  2013     1     1      555            600        -5      913            854
##  8  2013     1     1      557            600        -3      709            723
##  9  2013     1     1      557            600        -3      838            846
## 10  2013     1     1      558            600        -2      753            745
## # … with 336,766 more rows, and 11 more variables: arr_delay <dbl>,
## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>
```

```r
q2 <- df_flights %>% 
  filter(carrier == "DL") %>% 
  group_by(origin) %>% 
  summarise(avg_delay_time = mean(arr_delay, na.rm = TRUE))

q2
```

```
## # A tibble: 3 x 2
##   origin avg_delay_time
##   <chr>           <dbl>
## 1 EWR              8.78
## 2 JFK             -2.38
## 3 LGA              3.93
```

```r
ggplot(data = q2, mapping = aes(x = origin, y = avg_delay_time)) +
  geom_col(fill = "blue") +
  ggtitle("Average arrival delay for Delta Airlines") +
  theme_light()
```

![](Case-Study-4_files/figure-html/unnamed-chunk-1-1.png)<!-- -->
Based on the data and graphic above, JFK obviously has the best chance to not expect and arrival delays while flying with Delta Airlines.



```r
q3 <- df_flights %>% 
  filter(dest != "LGA") %>% 
  group_by(dest) %>% 
  summarise(avg_arrival_del = mean(arr_delay, na.rm = TRUE)) %>% 
  arrange(avg_arrival_del)

q3
```

```
## # A tibble: 104 x 2
##    dest  avg_arrival_del
##    <chr>           <dbl>
##  1 LEX          -22     
##  2 PSP          -12.7   
##  3 SNA           -7.87  
##  4 STT           -3.84  
##  5 ANC           -2.5   
##  6 HNL           -1.37  
##  7 SEA           -1.10  
##  8 MVY           -0.286 
##  9 LGB           -0.0620
## 10 SLC            0.176 
## # … with 94 more rows
```

```r
worst10 <- tail(q3, 10)

worst10
```

```
## # A tibble: 10 x 2
##    dest  avg_arrival_del
##    <chr>           <dbl>
##  1 GRR              18.2
##  2 DSM              19.0
##  3 CAK              19.7
##  4 RIC              20.1
##  5 MSN              20.2
##  6 TYS              24.1
##  7 JAC              28.1
##  8 OKC              30.6
##  9 TUL              33.7
## 10 CAE              41.8
```

```r
ggplot(data = worst10) +
  geom_col(mapping = aes(fct_reorder(dest, avg_arrival_del), 
                         y = avg_arrival_del, fill = dest))
```

![](Case-Study-4_files/figure-html/unnamed-chunk-2-1.png)<!-- -->
For this question, I used the arrival delay time in order to judge which destination is the worst. I grouped the data by dest and took the average arrival delay. The results above show that destination CAE is the worst when judging by which airport has the highest chance of delaying your arrival.
