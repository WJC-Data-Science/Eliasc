library(nycflights13)
library(tidyverse)
library(ggplot2)

df_flight <- nycflights13::flights

df_flight

#JFK
JFK_dep_delay <- df_flight %>% 
  filter(origin == "JFK") %>% 
  group_by(carrier) %>% 
  summarise(avg_dep_delay = mean(dep_delay, na.rm = TRUE)) %>% 
  arrange(avg_dep_delay)

JFK_dep_delay

JFK_dep_delay_plot <- ggplot(data = JFK_dep_delay) +
  geom_col(mapping = aes(fct_reorder(carrier, avg_dep_delay),
                         y = avg_dep_delay, fill = carrier))

JFK_dep_delay_plot
#average departure delay for each carrier at JFK


#EWR
EWR_dep_delay <- df_flight %>% 
  filter(origin == "EWR") %>% 
  group_by(carrier) %>% 
  summarise(avg_dep_delay_ewr = mean(dep_delay, na.rm = TRUE)) %>% 
  arrange(avg_dep_delay_ewr)

EWR_dep_delay
#average departure delay for each carrier at EWR

EWR_dep_delay_plot <- ggplot(data = EWR_dep_delay) +
  geom_col(mapping = aes(fct_reorder(carrier, avg_dep_delay_ewr),
                         y = avg_dep_delay_ewr, fill = carrier )) +
  theme_bw()

EWR_dep_delay_plot

#LGA
LGA_dep_delay <- df_flight %>% 
  filter(origin == "LGA") %>% 
  group_by(carrier) %>% 
  summarise(avg_dep_delay_lga = mean(dep_delay, na.rm = TRUE)) %>% 
  arrange(avg_dep_delay_lga)

LGA_dep_delay
#average departure delay for each carrier at LGA


LGA_dep_delay_plot <- ggplot(data = LGA_dep_delay) +
  geom_col(mapping = aes(fct_reorder(carrier, avg_dep_delay_lga),
                         y = avg_dep_delay_lga, fill = carrier)) +
  theme_bw()

LGA_dep_delay_plot

# This data shows the carrier at each of the three airports with the lowest average 
# departure delay. JFK was HA, and LGA and EWR are US. We can conclude that the carrier
# does matter and not just the airport for departure delays becuase US is consisitly amongst 
# the lowest of carriers in avergare departure delays
