library(tidyverse)
library(ggplot2)
library(dplyr)

pgatour <- read_csv("pgaTourData.csv")

head(pgatour)


# Avgerage Distance of Drive####
avgdist <- pgatour %>% 
  group_by(Year) %>% 
  summarise(avgdist = mean(`Avg Distance`, na.rm = TRUE))

avgdist

ggplot(data = avgdist) +
  geom_point(mapping = aes(x = Year, y = avgdist), col = "blue") +
  geom_path(mapping = aes(x = Year, y = avgdist), col = "blue") +
  theme_bw()

# Standard Deviation of Driving####
sddist <- pgatour %>% 
  group_by(Year) %>% 
  summarise(sddistance = sd(`Avg Distance`, na.rm = TRUE))

ggplot() +
  geom_point(data = sddist, mapping = aes(x = Year, y = sddistance), col = "blue") +
  geom_path(data = sddist, mapping = aes(x = Year, y = sddistance), col = "blue") + 
  theme_bw()


# Fairway Percentage####
frwyp <- pgatour %>% 
  group_by(Year) %>% 
  summarise(avg_frwyp = mean(`Fairway Percentage`,na.rm = TRUE))

frwyp

ggplot(data = frwyp) +
  geom_point(mapping = aes(x = Year, y = avg_frwyp), col = "blue") +
  geom_path(mapping = aes(x = Year, y = avg_frwyp), col = "blue") +
  theme_bw()
# Summary of Data & Limitations####
summary(pgatour)

hist(pgatour.Rounds)

#The data is missing values for certain pga pros in certain years
#I wish the data went back further than 10 years

#####



#There is missing data for certain players in particualr years
#How has the game of golf changed over the past 10 years?
#Does an increase in driving distance really determine the performance of a player?
#Has a a player's short game ability gone down if their driving distance has gone up?
#Has a players score gone down if their driving distance has gone up?

