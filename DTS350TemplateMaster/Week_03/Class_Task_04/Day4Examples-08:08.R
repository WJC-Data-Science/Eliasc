library(tidyverse)
library(ggplot2)
library(dplyr)

#Example 1: Plot Sepal.Width vs Speal.Length

ggplot(data = iris, mapping = aes(x=Sepal.Width, y = Sepal.Length)) +
  geom_point()

ggplot(data = iris) +
  geom_point(mapping = aes(x = Sepal.Width, y = Sepal.Length))

#Example 2 - ALl data points are diamonds
ggplot(data = iris, mapping = aes(x = Sepal.Width, 
                                  y = Sepal.Length, 
                                  color = Species)) +
  geom_point(shape = 18)

# Making it a different shape
ggplot(data = iris, mapping = aes(x = Sepal.Width, 
                                  y = Sepal.Length, 
                                  color = Species)) +
  geom_point(shape = 10)

#Example 3
ggplot(data = iris, mapping = aes(x = Sepal.Width, 
                                  y = Sepal.Length, 
                                  color = Species,
                                  shape = Species)) +
  geom_point()
#Keeping something within the aes() keeps it consistent throughout the whole graph

#Example 4
ggplot(data = iris, mapping = aes(x = Sepal.Width, 
                                  y = Sepal.Length, 
                                  color = Species,
                                  shape = Species)) +
  geom_point() +
  scale_shape_manual(values = c(1,5,7))

#Example 5
ggplot(data = iris, mapping = aes(x = Sepal.Width, 
                                  y = Sepal.Length, 
                                  color = Species,
                                  shape = Species)) +
  geom_point() +
  scale_shape_manual(values = c(1,5,7)) +
  scale_x_log10() +
  scale_y_log10()

#Example 6
ggplot(data = iris, mapping = aes(x = Sepal.Width, 
                                  y = Sepal.Length, 
                                  color = Species,
                                  shape = Species)) +
  geom_point() +
  scale_shape_manual(values = c(1,5,7)) +
  scale_x_log10() +
  scale_y_log10() +
  scale_color_manual(values = c("purple", "darkorange", "blue"))

#Example 7
p <- ggplot(data = iris, mapping = aes(x=Sepal.Width, 
                                       y = Sepal.Length, 
                                       color = Species,
                                       shape = Species),
            size = 5) +
  geom_point() +
  scale_color_brewer(palette = "Set1") 

print(p)

#Example 8
ggplot(data = iris, mapping = aes(x = Sepal.Width, 
                                  y = Sepal.Length, 
                                  color = Species,
                                  shape = Species)) +
  geom_point() +
  scale_shape_manual(values =  c(1, 5, 7)) +
  scale_x_log10() +
  scale_y_log10() +
  scale_color_manual(values = c("purple", "darkorange", "blue")) + 
  labs(x = "Sepal Width (cm)",
       y = "Sepal Length (cm)",
       title = "This is where the title goes",
       shape = "Spieces of Iris",
       color = "Spieces of Iris")

#If the color and the shape have the same title name, then they combine to one legend

#Example 9
ggplot(data = iris, mapping = aes(x=Sepal.Width, 
                                  y = Sepal.Length, 
                                  color = Species,
                                  shape = Species)) +
  geom_point() +
  scale_shape_manual(values =  c(1, 5, 7)) +
  scale_x_log10() +
  scale_y_log10() +
  scale_color_manual(values = c(setosa = "purple", versicolor = "darkorange", virginica = "blue")) +
  labs(x= "Sepal Width (cm)",
       y = "Sepal Length (cm)",
       title = "This is where I would put a title") +
  theme(plot.title = element_text(hjust = .5))

#Example 10
ggplot(data = iris, mapping = aes(x = Sepal.Width, 
                                  y = Sepal.Length, color = Species, shape = Species)) +
  geom_point() +
  scale_shape_manual(values =  c(1, 5, 7)) +
  scale_x_log10() +
  scale_y_log10() +
  scale_color_manual(values = c("purple", "orange", "blue")) +
  labs(x = "Sepal Width (cm)",
       y = "Sepal Length (cm)",
       title = "This is where I would put a title",
       shape = "Species of Iris",
       color = "Species of Iris") +
  theme_bw()


ggplot(data = iris, mapping = aes(x = Sepal.Width, 
                                  y = Sepal.Length, color = Species, shape = Species)) +
  geom_point() +
  scale_shape_manual(values =  c(1, 5, 7)) +
  scale_x_log10() +
  scale_y_log10() +
  scale_color_manual(values = c("purple", "orange", "blue")) +
  labs(x = "Sepal Width (cm)",
       y = "Sepal Length (cm)",
       title = "This is where I would put a title",
       shape = "Species of Iris",
       color = "Species of Iris") +
  theme_dark()

#Example 11
p <- ggplot(data = iris, mapping = aes(x = Sepal.Width, 
                                       y = Sepal.Length, 
                                       color = Species,
                                       shape = Species)) +
  geom_point() +
  scale_shape_manual(values =  c(1, 5, 7)) +
  scale_x_log10() +
  scale_y_log10() +
  scale_color_manual(values = c("purple", "orange", "blue")) +
  labs(x = "Sepal Length (cm)",
       y = "Sepal Width (cm)",
       title = "This is where I would put a title",
       color = "Species of Iris",
       shape = "Species of Iris") + 
  theme(plot.title = element_text(hjust = .5)) +
  theme_bw() +
  facet_wrap(vars(Species)) 

print(p)

averages <- iris %>% group_by(Species) %>% summarise(avglength = mean(Sepal.Length))

p + geom_hline(data = averages, mapping = aes(yintercept = avglength))

#You could change the color in the aes() in order for it to be red and not black


