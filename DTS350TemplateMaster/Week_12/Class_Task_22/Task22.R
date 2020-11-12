## Task 22

install.packages('USAboundaries')
library(USAboundaries)
library(ggrepel)
install.packages('sf')
library(sf)
library(ggplot2)
install.packages("maps")
library(maps)
library(dplyr)
library(tidyverse)
library(tidyr)

install.packages("devtools")
devtools::install_github("tidyverse/ggplot2")

install.packages("USAboundariesData", repos = "http://packages.ropensci.org", type = "source")

?geom_sf

usa <- us_states()

devtools::install_github("ropensci/USAboundariesData")
library(USAboundariesData)
idao_counties <- us_counties(states = 'ID')
cities <- us_cities()
head(cities)

top3 <- cities %>% 
  group_by(state_name) %>% 
  slice_max(., order_by = population, n = 3)
top3

top3 <- top3 %>% 
  mutate(rank = rank(-population))
top3$rank

top_city <- top3 %>% 
  filter(rank == 1)


devtools::install_github("yutannihilation/ggsflabel")
library(ggsflabel)




ggplot() +
  geom_sf(data = usa, fill = NA) +
  geom_sf(data = idao_counties, fill = NA) +
  geom_sf(data = top3, mapping = aes(size = population/1000, color = rank)) +
  geom_sf_label_repel(data = top_city, mapping = aes(label = city, position = 'dodge'), color = 'navy blue') +
  coord_sf(xlim = c(-125, -65), ylim = c(25, 50)) +
  labs(x = NULL, y = NULL, size = "Population \n (1,000)") +
  theme_bw() +
  guides(color = FALSE)
