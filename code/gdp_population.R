
# Run life expectancy and CO2 emission versus population with gapminder
# Date: Jan 17th, 2023
# Author:Marian Schmidt

# load in packages necessary for analysis


library("tidyverse")
library("readr")
library("ggprism")

# Read in data for analysis
gapminder_1997 <- read_csv("gapminder_1997.csv")

# Plotting data for visualization

ggplot(data = gapminder_1997) + 
  aes(x = gdpPercap) + 
  labs(x = "GDP Per Capita") +
  aes(y = lifeExp) +
  labs(y = "life Expectancy (yrs)") + 
  geom_point() +
  labs(title = "Do people in wealthy contries live longer?")+
  aes(color = continent)+
  scale_color_brewer(palette = "Set1")+
  aes(size = pop/1000000)+
  labs(size = "Population (in millions)")+
  aes(shape = continent)

# short handed ggplot
ggplot(data = gapminder_1997, 
       aes(x = gdpPercap, y = lifeExp, color = continet, 
           shape = continent, size = pop)+
  labs(x = "GDP Per Capita", y = "Life Expectancy", title = "Do people in wealthy countries live longer?",
       size = "population (in million)")+
  geom_point())

  #read in all of the data from ggminder (more years than 1997!)
  gapminder_1997 <- read_csv("gapminder_1997.csv")
gapminder_data <- read_csv("gapminder_data.csv")
view(gapminder_data)
dim(gapminder_data)
head(gapminder_data)
tail(gapminder_data)


ggplot(data = gapminder_data)+
  aes(x = year)+
  aes(y = lifeExp)+
  aes(color = continent, group = country)+
  geom_line()

str(gapminder_data)

ggplot(data = gapminder_1997)+
aes(x = continent)+
aes(y = lifeExp, color = continent)+
  geom_violin(fill = "pink", color = "cornflowerblue")+
  geom_jitter(aes(size = pop))


# histogram
ggplot(gapminder_1997)+
  aes(x = lifeExp)+
  geom_histogram(bins = 20)+
theme_prism()


install.packages("ggprism")

hyerim <- ggplot(gapminder_1997)+
  aes(x = lifeExp)+
  geom_histogram(bins = 20)+
  theme_prism()


ggplot(gapminder_1997)+
  aes(x = gdpPercap, y = lifeExp)+
  geom_point() +
  facet_grid(rows = vars(continent))

ggsave("awesome_plot.tiff", device = "tiff", width = 6, height = 4, units = "cm")
ggsave(plot = hyerim, file = "11hyerim.tiff", device = "tiff", width = 6, height = 4, units = "cm")

