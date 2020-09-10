---
title: "Data Import and ggplot2"
output: 
  html_document: 
    keep_md: true
---

I learned how to import data from .csv and .xslv as well as how to label data inside a visualization. The thing I found most difficult was needing to know how to change the data type when necessary.


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
download.file("https://github.com/fivethirtyeight/data/tree/master/mlb-elo", "https://projects.fivethirtyeight.com/mlb-api/mlb_elo.csv")
```

```
## Warning in download.file("https://github.com/fivethirtyeight/data/tree/master/
## mlb-elo", : URL https://github.com/fivethirtyeight/data/tree/master/mlb-elo:
## cannot open destfile 'https://projects.fivethirtyeight.com/mlb-api/mlb_elo.csv',
## reason 'No such file or directory'
```

```
## Warning in download.file("https://github.com/fivethirtyeight/data/tree/master/
## mlb-elo", : download had nonzero exit status
```

```r
mlb_elo <- read_csv("https://projects.fivethirtyeight.com/mlb-api/mlb_elo.csv")
```

```
## Parsed with column specification:
## cols(
##   .default = col_double(),
##   date = col_date(format = ""),
##   playoff = col_character(),
##   team1 = col_character(),
##   team2 = col_character(),
##   pitcher1 = col_character(),
##   pitcher2 = col_character()
## )
```

```
## See spec(...) for full column specifications.
```

```r
mlb_elo
```

```
## # A tibble: 220,908 x 26
##    date       season neutral playoff team1 team2 elo1_pre elo2_pre elo_prob1
##    <date>      <dbl>   <dbl> <chr>   <chr> <chr>    <dbl>    <dbl>     <dbl>
##  1 2020-09-27   2020       0 <NA>    STL   MIL      1530.    1503.     0.573
##  2 2020-09-27   2020       0 <NA>    TBD   PHI      1553.    1504.     0.604
##  3 2020-09-27   2020       0 <NA>    OAK   SEA      1556.    1476.     0.645
##  4 2020-09-27   2020       0 <NA>    MIN   CIN      1533.    1482.     0.607
##  5 2020-09-27   2020       0 <NA>    LAD   ANA      1588.    1474.     0.689
##  6 2020-09-27   2020       0 <NA>    CLE   PIT      1535.    1455.     0.645
##  7 2020-09-27   2020       0 <NA>    ATL   BOS      1541.    1487.     0.610
##  8 2020-09-27   2020       0 <NA>    ARI   COL      1488.    1473.     0.556
##  9 2020-09-27   2020       0 <NA>    TOR   BAL      1507.    1471.     0.586
## 10 2020-09-27   2020       0 <NA>    WSN   NYM      1530.    1512.     0.561
## # … with 220,898 more rows, and 17 more variables: elo_prob2 <dbl>,
## #   elo1_post <dbl>, elo2_post <dbl>, rating1_pre <dbl>, rating2_pre <dbl>,
## #   pitcher1 <chr>, pitcher2 <chr>, pitcher1_rgs <dbl>, pitcher2_rgs <dbl>,
## #   pitcher1_adj <dbl>, pitcher2_adj <dbl>, rating_prob1 <dbl>,
## #   rating_prob2 <dbl>, rating1_post <dbl>, rating2_post <dbl>, score1 <dbl>,
## #   score2 <dbl>
```

```r
mlb_elo_kcr <- filter(mlb_elo, team1 == "KCR")

write_csv(mlb_elo_kcr, "KCRoyals_Home_Games.csv")
```

