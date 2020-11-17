## Case Study 12

library(tidyverse)
library(USAboundaries)
library(USAboundariesData)
library(dplyr)
library(ggplot2)
library(sf)



permits <- read_csv(url('https://github.com/WJC-Data-Science/DTS350/raw/master/permits.csv'))
head(permits)

permits <- permits %>% 
  select(state, StateAbbr,county, countyname, variable, year, variable, value)
head(permits)

state_totals <- permits %>% 
  group_by(state, county, year) %>% 
  summarise(total = mean(value, na.rm = TRUE))
head(state_totals)



state_totals <- state_totals %>% 
  rename(
    countyfp = county,
    statefp = state
  )

final1 <- state_totals %>% 
  filter(year == 1986)
final1

final1g <- merge(final1, us_counties, all = TRUE, by = c("statefp","countyfp"))



us_states <- us_states()
head(us_states)

us_counties <- us_counties()
us_counties$statefp <- as.numeric(us_counties$statefp)
us_counties$countyfp <- as.numeric(us_counties$countyfp)

final <- merge(state_totals, us_counties, all = TRUE, by = c("countyfp","statefp"))
view(final)




## 1986
ggplot() +
  geom_sf(data = final1g, aes(geometry = geometry,  fill = total)) +
  coord_sf(xlim = c(-125, -65), ylim = c(25, 50)) +
  labs(title = '1986') +
  theme_bw()


## 1991
final2 <- state_totals %>% 
  filter(year == 1991)

final2g <- merge(final2, us_counties, all = TRUE, by = c("statefp","countyfp"))

ggplot() +
  geom_sf(data = final2g, aes(geometry = geometry,  fill = total)) +
  coord_sf(xlim = c(-125, -65), ylim = c(25, 50)) +
  labs(title = '1991') +
  theme_bw()

## 2005 
final3 <- state_totals %>% 
  filter(year == 2005)

final3g <- merge(final3, us_counties, all = TRUE, by = c("statefp","countyfp"))


ggplot() +
  geom_sf(data = final3g, aes(geometry = geometry,  fill = total)) +
  coord_sf(xlim = c(-125, -65), ylim = c(25, 50)) +
  labs(title = '2005') +
  theme_bw()
## 2009
final4 <- state_totals %>% 
  filter(year == 2009)

final4g <- merge(final4, us_counties, all = TRUE, by = c("statefp","countyfp"))


ggplot() +
  geom_sf(data = final4g, aes(geometry = geometry,  fill = total)) +
  coord_sf(xlim = c(-125, -65), ylim = c(25, 50)) +
  labs(title = '2009') +
  theme_bw()
