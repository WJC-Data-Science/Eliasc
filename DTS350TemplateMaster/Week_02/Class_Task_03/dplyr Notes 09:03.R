# Day 3:  DPLYR

library(nycflights13) # for data frame
library(tidyverse) # for ggplot2
library(dplyr) # for getting data frame the way we want it

# Our data frame will be flights
?flights
str(flights)
# Question:  How many observations are in this data frame?
# Question:  How many variables are in this data frame?


# The dplyr functions we will learn about:  filter(), arrange(), select(), mutate(), and summarise()

# These will often include the group_by() function and the pipe %>%.

## Filter(data frame, exp1, exp2, ...) 

# Example:  Find all the flights on January 1st.
filter(flights, month == 1, day == 1)



# Example:  Name the new data frame created in the previous example jan1.
jan1data <- filter(flights, month == 1, day ==1)
jan1data

# Practice:  Create a data frame called dec25 which gives all the flights on Christmas day.  Write the code to immediately display the results.
(dec25 <- filter(flights, month == 12, day == 25))
#Putting it in () automatically displays it on first command 

# Comparisons:  >, >=, <, <=, !=, ==

# Try the following:
sqrt(2)^2 == 2

1 / 49 * 49 == 1

near(sqrt(2) ^ 2, 2)

near(1 / 49 * 49, 1)
#near() tells you if the numbers are equal without connsidering their domains

# Logical Operators:  &, |, !

# Example:  Find all the flight information for any flight in October or December.
filter(flights, month == 10 | month == 12)

# What is going on with this code?
filter(flights, month == (10 | 12))

#fixed
filter(flights, month %in% c(10,12))

## Arrange(data frame, exp1, exp2, ...) This is like sort in Excel

# Example:  Create a data frame with all of the data from flights, but with the data sorted first by year, then month, then day.
arrange(flights, year, month, day)


# Example:  Sort the data in flights by descreasing order in dep_delay.
arrange(flights, desc(dep_delay))


## Select(data frame, exp1, exp2, ...) pulls off specific columns

# Example:  Create a new data frame with only the variables year, month, adn day.
select(flights, year:day)


# Helper functions with select():
# starts_with("abc")
# ends_with("xyz")
# contains("ijk")
# everything() for rearranging columns

# Example:  Re-arrange the columns in flights so the first two columns are time_hour, air_time, and the rest of the variables follow the same ordering as the original data set.
select(flights, time_hour, air_time, everything())



## Mutate(data frame, newCol1, newCol2, ...)

# Example:  Make a new data frame called flights_sml which has year, month, day, anything with "delay" at the end, distance, and air_time.  Add two columns to flights:  gain = dep_delay - arr_delay and speed = distance / air_time * 60
flights_sml <- select(flights, year:day, ends_with("delay"), distance, air_time)
flights_sml

mutate(flights_sml, 
        gain = dep_delay - arr_delay,
        speed = distance / air_time * 60)

# If you only want new variables, use transmute()
transmute(flights_sml, 
       gain = dep_delay - arr_delay,
       speed = distance / air_time * 60)

## Summarise(data frame, expression)


# Example:  Get the average departure delay for all of the data in flights.
summarise(flights, mean(dep_delay, na.rm = TRUE))


# Example:  Find the average departure delay time for each day.
by_day <- group_by(flights, year, month,day)
by_day

summarize(by_day, delay = mean(dep_delay, na.rm = TRUE))

## The pipe %>%

# Example:  For each destination which is not HNL, find the number of flights to that destination, 
#the average distance for the flights to that destination, and the average arrival delay for that destination. 
#Only display results that have at least 20 flights to the destination.

by_dest <- group_by(flights, dest)

delay <- summarize(by_dest,
          count = n(),
          avg_dist = mean(distance, na.rm =  TRUE),
          avg_arr_delay = mean(arr_delay, na.rm = TRUE))
delay

final_frame <- filter(delay, count > 20, dest != 'HNL')
final_frame

ggplot(final_frame, mapping = aes(x = avg_dist, y = avg_arr_delay)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE)


delays <- flights %>%
  group_by(dest) %>%
  summarize(
    count = n(),
    dist= mean(distance, na.rm = TRUE),
    delay = mean(arr_delay, na.rm = TRUE)) %>%
    filter(count > 20, dest != 'HNL')
