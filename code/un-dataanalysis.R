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

#group by
gapminder_data %>%
  group_by(year, continent) %>%
  summarize(average=mean(lifeExp),
            error = sd(lifeExp))
#mutate

gapminder_data %>%
  mutate(gdp = pop * gdpPercap)

gapminder_data %>%
  mutate(popInMillions = pop/1000000)

#select
gapminder_data %>%
  select(pop, year)

gapminder_data %>%
  select(-continent, pop)

gapminder_data %>%
  select(country, continent, year, lifeExp) %>%
  pivot_wider(names_from = year, values_from = lifeExp )%>%
  view()

# 
co2_emissions_dirty <- read_csv("co2-un-data.csv", skip=2,
                                col_names=c("region", "country", "year", "series", "value", "footnotes", "source"))

co2_emissions<-co2_emissions_dirty %>% 
  select(country, year, series, value) %>%
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions"))%>%
  pivot_wider(names_from=series, values_from=value)%>%
  filter(year == 2005) %>%
  select(-year)

# bring in 2007 population data
gapminder_data_2007 <- read_csv("data/gapminder_data.csv")%>%
filter(year == 2007)%>%
select(country, pop, lifeExp, gdpPercap)

joined_co2_pop <- inner_join(co2_emissions, gapminder_data_2007, by = "country")

anti_join(gapminder_data_2007, co2_emissions, by="country")

full_join(co2_emissions, gapminder_data_2007)

#writing csv
write_csv(joined_co2_pop, file = "data/joined_co2_pop.csv")

#read back in the CSV file we just wrote
hyerim <- read_csv("data/joined_co2_pop.csv")
hyerim%>%
ggplot(aes(x = gdpPercap))+
geom_histogram()

#co2 and gdp plots
gdp_co2_plot<-hyerim%>%
ggplot(aes(x = gdpPercap, y = per_capita_emissions))+
 geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
labs(x = "GDP Per Capita", y = "Co2 emissions per capita", title = "Comparing things")+
theme_classic()+
  ggpubr::stat_regline_equation(aes(label = after_stat(rr.label)))

install.packages("ggpubr")

ggsave(gdp_co2_plot, filename = "figures/gdp_vs_co2_plot.png",
height = 4, width = 6, unit = "in", dpi = 300)
