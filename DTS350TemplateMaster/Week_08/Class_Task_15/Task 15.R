## Case Study 8

install.packages("Lahman")
library(Lahman)
install.packages("blscrapeR")
library(blscrapeR)
library(ggplot2)



head(CollegePlaying)
head(Master)
head(Salaries)
head(Schools)

## Adjusting Inflation
salaries1985 <- filter(Salaries, yearID == "1985") %>%
  mutate(salary = salary * 2.42)
salaries1986 <- filter(Salaries, yearID == "1986") %>%
  mutate(salary = salary * 2.37)
salaries1987 <- filter(Salaries, yearID == "1987") %>%
  mutate(salary = salary * 2.29)
salaries1988 <- filter(Salaries, yearID == "1988") %>%
  mutate(salary = salary * 2.2)
salaries1989 <- filter(Salaries, yearID == "1989") %>%
  mutate(salary = salary * 2.1)
salaries1990 <- filter(Salaries, yearID == "1990") %>%
  mutate(salary = salary * 1.99)
salaries1991 <- filter(Salaries, yearID == "1991") %>%
  mutate(salary = salary * 1.91)
salaries1992 <- filter(Salaries, yearID == "1992") %>%
  mutate(salary = salary * 1.86)
salaries1993 <- filter(Salaries, yearID == "1993") %>%
  mutate(salary = salary * 1.8)
salaries1994 <- filter(Salaries, yearID == "1994") %>%
  mutate(salary = salary * 1.76)
salaries1995 <- filter(Salaries, yearID == "1995") %>%
  mutate(salary = salary * 1.71)
salaries1996 <- filter(Salaries, yearID == "1996") %>%
  mutate(salary = salary * 1.66)
salaries1997 <- filter(Salaries, yearID == "1997") %>%
  mutate(salary = salary * 1.62)
salaries1998 <- filter(Salaries, yearID == "1998") %>%
  mutate(salary = salary * 1.6)
salaries1999 <- filter(Salaries, yearID == "1999") %>%
  mutate(salary = salary * 1.56)
salaries2000 <- filter(Salaries, yearID == "2000") %>%
  mutate(salary = salary * 1.51)
salaries2001 <- filter(Salaries, yearID == "2001") %>%
  mutate(salary = salary * 1.47)
salaries2002 <- filter(Salaries, yearID == "2002") %>%
  mutate(salary = salary * 1.45)
salaries2003 <- filter(Salaries, yearID == "2003") %>%
  mutate(salary = salary * 1.41)
salaries2004 <- filter(Salaries, yearID == "2004") %>%
  mutate(salary = salary * 1.38)
salaries2005 <- filter(Salaries, yearID == "2005") %>%
  mutate(salary = salary * 1.33)
salaries2006 <- filter(Salaries, yearID == "2006") %>%
  mutate(salary = salary * 1.29)
salaries2007 <- filter(Salaries, yearID == "2007") %>%
  mutate(salary = salary * 1.26)
salaries2008 <- filter(Salaries, yearID == "2008") %>%
  mutate(salary = salary * 1.21)
salaries2009 <- filter(Salaries, yearID == "2009") %>%
  mutate(salary = salary * 1.21)
salaries2010 <- filter(Salaries, yearID == "2010") %>%
  mutate(salary = salary * 1.19)
salaries2011 <- filter(Salaries, yearID == "2011") %>%
  mutate(salary = salary * 1.16)
salaries2012 <- filter(Salaries, yearID == "2012") %>%
  mutate(salary = salary * 1.13)
salaries2013 <- filter(Salaries, yearID == "2013") %>%
  mutate(salary = salary * 1.12)
salaries2014 <- filter(Salaries, yearID == "2014") %>%
  mutate(salary = salary * 1.1)
salaries2015 <- filter(Salaries, yearID == "2015") %>%
  mutate(salary = salary * 1.1)
salaries2016 <- filter(Salaries, yearID == "2016") %>%
  mutate(salary = salary * 1.08)

salary_data <- bind_rows(salaries1985,
                         salaries1986,
                         salaries1987,
                         salaries1988,
                         salaries1989,
                         salaries1990,
                         salaries1991,
                         salaries1992,
                         salaries1993,
                         salaries1994,
                         salaries1995,
                         salaries1996,
                         salaries1997,
                         salaries1998,
                         salaries1999,
                         salaries2000,
                         salaries2001,
                         salaries2002,
                         salaries2003,
                         salaries2004,
                         salaries2005,
                         salaries2006,
                         salaries2007,
                         salaries2008,
                         salaries2009,
                         salaries2010,
                         salaries2011,
                         salaries2012,
                         salaries2013,
                         salaries2014,
                         salaries2015,
                         salaries2016)

head(salary_data)



college <- left_join(CollegePlaying, Master, by = "playerID")

head(college)

college1 <- left_join(college, Salaries, by = "playerID")

head(college1)

college2 <- left_join(college1, Schools, by = "schoolID")

head(college2)

college_df <- college2 %>% 
  select(playerID, nameGiven, name_full, schoolID, state, salary, yearID.y)

head(college_df)



## Missouri Colleges
missouri_colleges <- college_df %>% 
  filter(state == "MO")

head(missouri_colleges)
view(missouri_colleges)


### Do this after you correct for inflation
total_salary <- missouri_colleges %>% 
  group_by(name_full) %>% 
  summarise(total_salary = sum(salary, na.rm = TRUE))
head(total_salary)

new_total <- total_salary %>% 
  filter(total_salary > 0)



averaged <- missouri_colleges %>% 
  group_by(name_full) %>% 
  summarise(total_salary = mean(salary, na.rm = TRUE))
head(averaged)

new_averaged <- na.omit(averaged)
head(new_averaged)

## Graphs
ggplot(data = new_total) +
  geom_col(mapping = aes(x = reorder(name_full, total_salary), y = total_salary, fill = name_full)) +
  theme_bw() +
  theme(legend.position = 'none',
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank()) +
  labs(x = 'College or University',
       y = 'Total Salary in Dollars',
       title = 'Accumulated Salary by School') +
  coord_flip()


ggplot(data = new_averaged) +
  geom_col(mapping = aes(x = reorder(name_full,total_salary), y = total_salary, fill = name_full)) +
  theme_bw() +
  theme(legend.position = 'none',
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank()) +
  labs(x = 'College or University',
       y = 'Averaged Salary in Dollars',
       title = 'Averaged Salary by School') +
  coord_flip()
