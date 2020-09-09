library(dplyr)

head(iris)

sep.length <- arrange(iris, Sepal.Length)
head(sep.length)

testdat <- select(iris, Species, Petal.Width)
testdat

avg_columns <- iris %>%
  group_by(Species) %>%
  summarise(
    avg_sepal_length = mean(Sepal.Length, na.rm = TRUE),
    avg_sepal_width = mean(Sepal.Width, na.rm = TRUE),
    avg_petal_length = mean(Petal.Length, na.rm = TRUE),
    avg_petal_width = mean(Petal.Width, na.rm = TRUE))
avg_columns  

?summarize_all

avg_species <- iris %>%
  group_by(Species)  %>%
  summarize_all(c(mean,sd))

avg_species
