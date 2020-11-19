## Task 24

install.packages('leaflet')
library(leaflet)
library(tidyverse)
library(maps)

mapStates = map("state", fill = TRUE, plot = FALSE)
leaflet(data = mapStates) %>% 
  addTiles() %>% 
  addPolygons(fillColor = 'white', color = 'black', stroke = FALSE)

library(readr)
states <- read_csv("/Users/chaseelias/Documents/William Jewell/3.Jewell Junior/DTS 350/Eliasc/states.csv", 
                   col_names = FALSE)

uscities <- read_csv("/Users/chaseelias/Documents/William Jewell/3.Jewell Junior/DTS 350/Eliasc/uscities.csv")
head(uscities)

top3 <- uscities %>% 
  group_by(state_id) %>% 
  filter(row_number(desc(population)) <= 3) %>% 
  mutate(population = population/1000)
head(top3)

top3rnk <- top3 %>% 
  mutate(rank = rank(desc(population))) %>% 
  select(city, state_id, lat, lng, population, rank) %>% 
  arrange(state_id)
top3rnk

states
statesLatLng <- transmute(states, X1, Lat = X2, Long = X3)

topcity <- top3rnk %>% 
  filter(rank == 1)
head(topcity)

pall <- colorFactor(c("navy", "blue", "cyan"), domain = c(1,2,3))

all_states1 <- leaflet(data = statesLatLng) %>% 
  addTiles() %>% 
  addCircleMarkers(data = top3rnk,
                   radius = 3.5,
                   color = ~pall(rank),
                   stroke = FALSE,
                   fillOpacity = .6) %>%
  addLabelOnlyMarkers(data = topcity,
                      label = ~as.character(city),
                      labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T,
                                                  style = list('color' = 'blue')))


all_states1
