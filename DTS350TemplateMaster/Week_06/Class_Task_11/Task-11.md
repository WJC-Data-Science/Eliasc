---
title: "Task 11"
output: 
  html_document:
    keep_md: TRUE
    code_folding: hide
---

# Findings

```r
library(tidyverse)
```

```
## ── Attaching packages ────────────────────────────────────────────────────────────────────────────────────────── tidyverse 1.3.0 ──
```

```
## ✓ ggplot2 3.3.0     ✓ purrr   0.3.3
## ✓ tibble  2.1.3     ✓ dplyr   0.8.5
## ✓ tidyr   1.0.2     ✓ stringr 1.4.0
## ✓ readr   1.3.1     ✓ forcats 0.5.0
```

```
## ── Conflicts ───────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
library(ggplot2)

devtools::install_github("drsimonj/ourworldindata")
```

```
## Skipping install of 'ourworldindata' from a github remote, the SHA1 (ed2fc17b) has not changed since last install.
##   Use `force = TRUE` to force installation
```

```r
finance_df <- ourworldindata::financing_healthcare

head(finance_df)
```

```
## # A tibble: 6 x 17
##    year country continent health_exp_total health_exp_publ… health_insurance
##   <int> <chr>   <chr>                <dbl>            <dbl>            <int>
## 1  2015 Abkhaz… <NA>                    NA               NA               NA
## 2  1800 Afghan… Asia                    NA               NA               NA
## 3  1801 Afghan… Asia                    NA               NA               NA
## 4  1802 Afghan… Asia                    NA               NA               NA
## 5  1803 Afghan… Asia                    NA               NA               NA
## 6  1804 Afghan… Asia                    NA               NA               NA
## # … with 11 more variables: nhs_exp <dbl>, health_exp_private <dbl>,
## #   health_insurance_govt <dbl>, health_insurance_private <dbl>,
## #   health_insurance_any <dbl>, health_exp_public_percent <dbl>,
## #   health_exp_oop_percent <dbl>, no_health_insurance <dbl>, gdp <dbl>,
## #   life_expectancy <dbl>, child_mort <dbl>
```

```r
new_df <- finance_df %>% 
  filter(year >= 1925 & country != 'NA') %>% 
  group_by(year, continent) %>% 
  summarise(avg_cont = mean(child_mort, na.rm = TRUE))

new_df
```

```
## # A tibble: 546 x 3
## # Groups:   year [91]
##     year continent avg_cont
##    <int> <chr>        <dbl>
##  1  1925 Africa        410.
##  2  1925 Americas      341.
##  3  1925 Asia          392.
##  4  1925 Europe        188.
##  5  1925 Oceania       296.
##  6  1925 <NA>          369.
##  7  1926 Africa        409.
##  8  1926 Americas      340.
##  9  1926 Asia          389.
## 10  1926 Europe        186.
## # … with 536 more rows
```

```r
by_continent <- ggplot(data = new_df) +
  geom_point(mapping = aes(x = year, y = avg_cont, color = continent), size = 0.5) +
  geom_path(mapping = aes(x = year, y = avg_cont, color = continent), size = 0.4) +
  labs(x = 'Year', y = 'Average Continent Child Mortality', color = "Continent", title = 'Average Child Mortality by Continent') +
  theme_bw()

by_continent
```

```
## Warning: Removed 21 rows containing missing values (geom_point).
```

```
## Warning: Removed 12 row(s) containing missing values (geom_path).
```

![](Task-11_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

```r
ggsave("by_continent.png", by_continent)
```

```
## Saving 7 x 5 in image
```

```
## Warning: Removed 21 rows containing missing values (geom_point).

## Warning: Removed 12 row(s) containing missing values (geom_path).
```
## Report
I took the average child mortality data by continent and graphed in by year since 1925. It is clear to see that in just under a hundred years the child mortality rate has been steadily decreasing in all continents. Every continent has been able to cut their mortality rates by over half from 1925 which is really good. It is also interesting how NA started very high in 1925, and then finished near the lowest by present day. 
I noticed that while the data says its by continent, the data is really by region. This is becuase there is just a group called 'Americas' and not 'North America' and 'South America.' I would like to see the data by actual continents and not just regions, becuase I believe South America would weigh heavely on the Americas region.
