# Load in Libraries ####

library(tidyverse)
library(plyr)
library(dplyr)
library(ggplot2)



# Load in Data ####

pgatour <- read_csv("pgaTourData.csv")

head(pgatour)
view(pgatour)

# Average Distance by Year and Player ####

average_distance <- pgatour %>% 
  group_by(Year) %>% 
  summarise(dist_year = mean(`Avg Distance`, na.rm = TRUE))
average_distance

ggplot(data = average_distance) +
  geom_point(mapping = aes(x = Year, y = dist_year), color = 'dark green') +
  geom_path(mapping = aes(x = Year, y = dist_year), color = 'dark green') +
  labs(title = 'Average Distance Driven Over Years', y = 'Average Distance') +
  theme_bw()

# Standard Deviation of Distance

stdv_distance <- pgatour %>% 
  group_by(Year) %>% 
  summarise(stdv = sd(`Avg Distance`, na.rm = TRUE))
stdv_distance

ggplot(data = stdv_distance, mapping = aes(x = Year, y = stdv)) +
  geom_point(color = 'dark green') +
  geom_path(color = 'dark green') +
  labs(title = 'Standard Deviation of Distance', y = 'Standard Deviation') +
  theme_bw()

# Comparing distance to fwpc
avg_distance_fwp <- pgatour %>% 
  group_by(Year) %>% 
  summarise(avg_distance = mean())
  
pgatour$`Avg Distance`

ggplot(data = pgatour) +
  geom_smooth(mapping = aes(x = `Avg Distance`, y = `Fairway Percentage`), color = 'dark green') +
  labs(title = 'Distance Driven vs. Fairway Percentage', x = 'Distance Driven') +
  theme_bw()
  

# Fairway Pecentage and GIR Percentage ####

Fairway_perc <- pgatour %>%
  group_by(Year) %>% 
  summarise(Percent = mean(`Fairway Percentage`, na.rm = TRUE))
Fairway_perc

Fairway_perc$Percentage <- 'Fairway Percent'
Fairway_perc

GIR_Percent1 <- pgatour %>% 
  group_by(Year) %>% 
  summarise(Percent = mean(gir, na.rm = TRUE))
GIR_Percent1

GIR_Percent1$Percentage <- 'Greens in Reg.'
GIR_Percent1

Fairway_GIR_perc <- bind_rows(Fairway_perc, GIR_Percent1)

ggplot(data = Fairway_GIR_perc) +
  geom_point(mapping = aes(x = Year, y = Percent, color = Percentage)) +
  geom_path(mapping = aes(x = Year, y = Percent, color = Percentage)) +
  labs(title = 'Percent of Fairways and Greens in Reguation') +
  theme_bw()

pgatour$gir
ggplot(pgatour) +
  geom_smooth(mapping = aes(x = `Avg Distance`, y = gir), color = 'dark green') +
  labs(title = 'Distance Driven vs. Greens in Reg.', x = 'Distance Driven', y = 'GIR') +
  theme_bw()


# Short Game ####

## scrambling percentage
scram_perc <- pgatour %>% 
  group_by(Year) %>% 
  summarise(scram_year = mean(`Average Scrambling`, na.rm = TRUE))

ggplot(data = scram_perc) +
  geom_point(mapping = aes(x = Year, y = scram_year), color = 'dark green') +
  geom_path(mapping = aes(x = Year, y = scram_year), color = 'dark green') +
  labs(title = 'Average Scrambling Percentage', y = 'Percent') +
  theme_bw()


pgatour$`SG:ARG`

ggplot(data = pgatour) +
  geom_smooth(mapping = aes(x = `SG:ARG`, y = `Average Scrambling`), color = 'dark green') +
  theme_bw()

ggplot(data = pgatour) +
  geom_smooth(mapping = aes(x = `Avg Distance`, y = `Average Scrambling`), color = 'dark green') +
  theme_bw()

ggplot(data = pgatour) +
  geom_smooth(mapping = aes(x = `Avg Distance`, y = `SG:ARG`), color = 'dark green') +
  theme_bw()

## Puts per round
pgatour$`Average Putts`
putts <- pgatour %>% 
  group_by(Year) %>% 
  summarise(avg_putts = mean(`Average Putts`, na.rm = TRUE))

ggplot(data = putts, mapping = aes(x = Year, y = avg_putts)) +
  geom_point(color = 'dark green') +
  geom_path(color = 'dark green') +
  theme_bw()

distance_vs_putts <- ggplot(data = pgatour) +
  geom_smooth(mapping = aes(x = `Avg Distance`, y = `Average Putts`), color = 'dark green') +
  labs(title = 'Distance vs. Putts') +
  theme_bw()
distance_vs_putts
# GIR Percentage and Scrambling Percentage ####

GIR_Percent <- pgatour %>% 
  group_by(Year) %>% 
  summarise(Percent = mean(gir, na.rm = TRUE))
GIR_Percent

GIR_Percent$Situation <- 'Greens in Reg.'
GIR_Percent

Scrambling_perc <- pgatour %>% 
  group_by(Year) %>% 
  summarise(Percent = mean(`Average Scrambling`, na.rm = TRUE))
Scrambling_perc

Scrambling_perc$Situation <- 'Scrambling'
Scrambling_perc

GIR_Scrambling_Perc <- bind_rows(GIR_Percent, Scrambling_perc)
GIR_Scrambling_Perc

ggplot(data = GIR_Scrambling_Perc) +
  geom_point(mapping = aes(x = Year, y = Percent, color = Situation)) +
  geom_path(mapping = aes(x = Year, y = Percent, color = Situation)) +
  labs(title = 'Percent of Greens in Reguation and Scrambling') +
  theme_bw()


# SG:OTT ####

ott <- pgatour %>% 
  group_by(Year) %>% 
  summarise(avg_ott = mean(`SG:OTT`, na.rm = TRUE))
ott

ggplot(data = ott) +
  geom_point(mapping = aes(x = Year, y = avg_ott), color = 'dark green') +
  geom_path(mapping = aes(x = Year, y = avg_ott), color = 'dark green') +
  labs(title = 'Average SG:OTT', y = 'SG:OTT') +
  theme_bw()

sg_ott <- pgatour %>% 
  select(`Player Name`, `Avg Distance`, `SG:OTT`)
names(sg_ott)[3] <- 'SG'
sg_ott$sg_type <- 'Off the tee'
sg_ott

sg_total <- pgatour %>% 
  select(`Player Name`, `Avg Distance`, `Average SG Total`)
names(sg_total)[3] <- 'SG'
sg_total$sg_type <- 'Total'


sg_ott_total <- bind_rows(sg_ott, sg_total)
sg_ott_total

ggplot(data = sg_ott_total) +
  geom_smooth(mapping = aes(x = `Avg Distance`, y = SG, color = sg_type)) +
  labs(title = 'Strokes Gained vs. Distance Driven', x = 'Distance Driven', y = 'Strokes Gained', color = 'SG Type') +
  theme_bw()





# 3D Graph ####

install.packages('scatterplot3d')
library(scatterplot3d)

players_sg <- pgatour %>% 
  group_by(`Player Name`) %>% 
  summarise(avg_sg_ott = mean(`SG:OTT`, na.rm = TRUE),
                              avg_sg_apr = mean(`SG:OTT`, na.rm = TRUE),
                                                avg_sg_arg = mean(`SG:ARG`, na.rm = TRUE))
players_sg

scatterplot3d(players_sg[,2:4])



# Comparing Variables ####

fp_dist <- pgatour %>% 
  group_by(`Fairway Percentage`) %>% 
  summarise(avg_dist = mean(`Avg Distance`, na.rm = TRUE))

fp_dist



ggplot(data = fp_dist) +
  geom_smooth(mapping = aes(x = `Fairway Percentage`, y = avg_dist)) +
  theme_bw()



ggplot() +
  geom_smooth(data = pgatour, mapping = aes(x = `Fairway Percentage`, y = `gir`)) +
  geom_point(data = dj, mapping = aes(x = `Fairway Percentage`, y = `gir`), color = 'red') +
  theme_bw()

dj <- pgatour %>%
  filter(`Player Name` == 'Dustin Johnson')
dj

avg_dj <- dj %>% 
  summarise(avg_distance = mean(`Avg Distance`, na.rm = TRUE),
            avg_sgott = mean(`SG:OTT`, na.rm = TRUE))
avg_dj

ggplot() +
  geom_smooth(data = pgatour, mapping = aes(x = `Avg Distance`, y = `SG:OTT`)) +
  geom_point(data = avg_dj, mapping = aes(x = avg_distance, y = avg_sgott),
             color = 'red', size = 1.5) +
  annotate("text", x = avg_dj$avg_distance, y = avg_dj$avg_sgott + .05, label = 'DJ') +
  theme_bw()
# Winners ####

winners <- pgatour %>% 
    filter(Wins >= 1)

winners

pgatour$`Average SG Total`

ggplot() +
  geom_smooth(data = pgatour, mapping = aes(x = `Avg Distance`, y = `Average SG Total`), color = 'dark green') +
  geom_smooth(data = winners, mapping = aes(x = `Avg Distance`, y = `Average SG Total`), color = 'blue') +
  theme_bw()

ggplot() +
  geom_smooth(data = pgatour, mapping = aes(x = `Avg Distance`, y = `gir`), color = 'dark green') +
  geom_smooth(data = winners, mapping = aes(x = `Avg Distance`, y = `gir`), color = 'blue') +
  theme_bw()

ggplot() +
  geom_smooth(data = winners, mapping = aes(x = Year, y = `Avg Distance`), color = 'dark green') +
  geom_path(data = average_distance,mapping = aes(x = Year, y = dist_year), color = 'blue')

##
sapply(pgatour, typeof)

new_money <- gsub(",","", money$Money)
money$new_money <- new_money

view(money)
sapply(money, typeof)

money$new_money <- as.numeric(money$new_money)
sapply(money, typeof)

arrange(money, desc(new_money))
## Official Money
sapply(pgatour, typeof)

earnings <- pgatour %>% 
  group_by(`Player Name`) %>% 
  filter(Money > 0)
view(earnings)

no_comma_earnings <- gsub(",", "", earnings$Money)
earnings$player_earnings <- no_comma_earnings
view(earnings)

sapply(earnings, typeof)

earnings$player_earnings <- as.numeric(earnings$player_earnings)
sapply(earnings, typeof)

earnings <- earnings %>% 
  select(-Money)
earnings



nrow(earnings)

earnings

##


earnings <- earnings %>% 
  arrange(desc(player_earnings)) %>% 
  mutate(ranking = rank(-player_earnings, ties.method = 'first'))
view(earnings)

###

earnings <- earnings %>% 
  arrange(desc(player_earnings)) %>% 
  mutate(ranking = 1:nrow(earnings))

top25 <- nrow(earnings)*0.25
top25

### Top 25 
top25_perc <- earnings %>% 
  filter(rank <= top25)
view(top25_perc)


sum_top25 <- top25_perc %>% 
  summarise(total = sum(player_earnings, na.rm = TRUE))
sum_top25

sum_top25$Category <- 'Top 25%'
sum_top25


### Bottom 75
bottom25perc <- earnings %>% 
  filter(rank >= top25)
bottom25perc



sum_bottom25 <- bottom25perc %>% 
  summarise(total = sum(player_earnings, na.rm = TRUE))
sum_bottom25

sum_bottom25$Category <- 'Rest of the Field'
sum_bottom25

comparison_winners <- bind_rows(sum_top25, sum_bottom25)

ggplot(data = comparison_winners) +
  geom_bar(mapping = aes(x = total, fill = Category))


### Code Used
bottom25perc$Category <- 'Rest of the Field'
top25_perc$Category <- 'Top 25%'

together <- bind_rows(top25_perc, bottom25perc)
together
ggplot(data = together) +
  geom_col(mapping = aes(x = order(desc(Category)), y = player_earnings, fill = Category, color = Category)) +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(title = 'Top 25% Earnings', y = 'Money') +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

ggplot(data = together) +
  geom_col(mapping = aes(x = Category, y = player_earnings, fill = Category)) +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(title = 'Top 25% Earnings', y = 'Money') +
  theme_bw()

# Winners Compared to General Population ####

top25_perc
bottom25perc

top25_names <- top25_perc$`Player Name`
top25_names

bottom25_names <- bottom25perc$`Player Name`
bottom25_names

top25_all <- pgatour %>% 
  filter(`Player Name` %in% top25_names)
view(top25_all)

bottom25_all <- pgatour %>% 
  filter(`Player Name` %in% bottom25_names)
view(bottom25_all)


## Making Categories
top25_all$Category <- 'Top 25%'
bottom25_all$Category <- 'Rest of Field'

pga_df_distributed <- bind_rows(top25_all, bottom25_all)
view(pga_df_distributed)
view(pgatour)


avg_driving_compared <- pga_df_distributed %>% 
  dplyr::group_by(Year, Category) %>% 
  dplyr::summarise(avg_distance = mean(`Avg Distance`, na.rm = TRUE))
avg_driving_compared


ggplot(data = avg_driving_compared) +
  geom_point(mapping = aes(x = Year, y = avg_distance, color = Category)) +
  geom_path(mapping = aes(x = Year, y = avg_distance, color = Category)) +
  labs(title = 'Winners vs. Field - Distance', y = 'Distance') +
  theme_bw()

ggplot(data = pga_df_distributed) +
  geom_smooth(mapping = aes(x = `Avg Distance`, y = `Average Scrambling`, color = Category)) +
  labs(title = 'Winner vs. Field - Distance vs. Scrambling') +
  theme_bw()
  
pga_df_distributed$`Average Scrambling`

scram_perc_winners <- pga_df_distributed %>% 
  dplyr::group_by(Year, Category) %>% 
  dplyr::summarise(avg_scram = mean(`Average Scrambling`, na.rm = TRUE))
scram_perc_winners

ggplot(data = scram_perc_winners, mapping = aes(x = Year, y = avg_scram, color = Category)) +
  geom_point() +
  geom_path() +
  labs(title = 'Average Scrambling - Winners vs. Field', y = 'Scrambling Percentage') +
  theme_bw()

