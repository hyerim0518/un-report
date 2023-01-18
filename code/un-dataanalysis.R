library(tidyverse)
getwd()
gapminder_data <- read_csv("data/gapminder_data.csv")


summarize(gapminder_data, averageLifeExp=mean(lifeExp))

gapminder_summary <- gapminder_data %>% 
  summarize(averageLifeExp=mean(lifeExp))

gapminder_summary

#filtering
gapminder_summary_2007 <- gapminder_data%>%
filter(year == 2007)%>%
  summarize(average = mean(lifeExp))

#question

gapminder_data %>%
  summarize(Firstyear =min(year))

gapminder_data %>%
  filter(year == 1952) %>%
  summarize(average_gdp=mean(gdpPercap))
  
