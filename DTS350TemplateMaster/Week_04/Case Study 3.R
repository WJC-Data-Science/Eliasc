library(tidyverse)
library(ggplot2)
library(dplyr)
install.packages("measurements")
library(measurements)

census <- read_csv("CensusAtSchool.csv")
census

df_inch <- census %>% 
  transmute(Height_inch = conv_unit(Height, "cm", "inch" ),
            Foot_Length_Inch = conv_unit(Foot_Length, "cm", "inch"),
            Arm_Span_Inch = conv_unit(Arm_Span, "cm", "inch"),
            )
  
df_inch
#

df_environment <- census %>% 
  filter(Importance_reducing_pollution >= 750 || Importance_recycling_rubbish >= 750 ||
         Importance_conserving_water >= 750 || Importance_saving_enery >= 750 ||
           Importance_owning_computer >= 750 || Importance_Internet_access >= 750)

df_environment <- arrange(df_environment, desc(Ageyears))
df_environment
#

df_extra <- census %>% 
  select(Country, Region, Gender, Ageyears, Handed, Height, Foot_Length, Arm_Span,
         Languages_spoken, Travel_to_School, Travel_time_to_School, Reaction_time,
         Score_in_memory_game, Favourite_physical_activity)

df_extra


df_numbers <- census %>% 
  group_by(Country) %>% 
  summarise(num_males = sum(Gender == "M", na.rm = TRUE),
            num_females = sum(Gender == "F",na.rm = TRUE),
            mean(Importance_Internet_access,na.rm = TRUE),
            mean(Importance_reducing_pollution,na.rm = TRUE),
            mean(Importance_recycling_rubbish, na.rm = TRUE),
            mean(Importance_conserving_water, na.rm = TRUE),
            mean(Importance_saving_enery, na.rm = TRUE),
            mean(Importance_owning_computer, na.rm = TRUE))
            

df_numbers
#

df_gender <- census %>% 
  group_by(Country, Gender) %>% 
  summarise(mean(Importance_reducing_pollution, na.rm = TRUE),
            sd(Importance_reducing_pollution, na.rm = TRUE),
            mean(Importance_recycling_rubbish, na.rm = TRUE),
            sd(Importance_recycling_rubbish, na.rm = TRUE),
            mean(Importance_conserving_water, na.rm = TRUE),
            sd(Importance_conserving_water, na.rm = TRUE),
            mean(Importance_saving_enery, na.rm = TRUE),
            sd(Importance_saving_enery, na.rm = TRUE),
            mean(Importance_owning_computer, na.rm = TRUE),
            sd(Importance_owning_computer, na.rm = TRUE),
            mean(Importance_Internet_access, na.rm = TRUE),
            sd(Importance_Internet_access, na.rm = TRUE))
            

df_gender



  
Twelve <- census %>% 
  group_by(Ageyears) %>% 
  summarise(
    Reducing_Pollution = mean(Importance_reducing_pollution, na.rm = TRUE)) %>% 
  filter(Ageyears <= 12)     

Twelve
  
Less_than_12

ggplot(data = Twelve) +
  geom_path(mapping = aes(x = Ageyears, y = Reducing_Pollution, color = "red"))

Older_than_twelve <- census %>% 
  group_by(Ageyears) %>% 
  summarise(
    Reducing_Pollution = mean(Importance_reducing_pollution, na.rm = TRUE)) %>% 
  filter(Ageyears >= 12)

Older_than_twelve            

Age_comparison <- ggplot(data = Twelve) +
  geom_path(mapping = aes(x = Ageyears, y = Reducing_Pollution, color = "darkred")) +
  geom_path(data = Older_than_twelve, mapping = aes(x = Ageyears, y = Reducing_Pollution,
                                                    )) +
  theme_bw()

Age_comparison




