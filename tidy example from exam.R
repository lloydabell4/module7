library(lubridate)
library(tidyverse)

covid_data_by_week <- read_csv("data/covid_data_henrico_byweek.csv")
covid_data_sex <- read_csv("data/covid_data_greaterrichmondarea_byweek_sex.csv")

summer_months <- c("May", "June", "July", "August")

do_vaccines_work <- covid_data_by_week %>%
  mutate(year = year(week_of),
         month = month(week_of, label = TRUE, abbr = FALSE))%>%
  filter(month %in% summer_months)%>%
  group_by(year, Age_Group)%>%
  summarise(total_cases = sum(sum_cases),
            total_hospitalized = sum(sum_hospital),
            total_deaths = sum(sum_deaths))%>%
  mutate(percent_hospitalized = total_hospitalized / total_cases, 
         percent_death = total_deaths / total_cases)

ggplot(do_vaccines_work, aes(Age_Group, y = percent_death, fill = as.factor(year)))+
  geom_bar(stat = 'identity', position = "dodge")+
  coord_flip()


tidy_dvw <- do_vaccines_work%>%
  pivot_longer(total_cases:percent_death, names_to = "key", values_to = "values")

ggplot(tidy_dvw, aes(x = Age_Group, y = values, fill = as.factor(year)))+
  geom_bar(stat = 'identity', position = "dodge")+
  coord_flip()+
  facet_grid(~key, scales = "free_x")
