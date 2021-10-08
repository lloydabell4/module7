#Class_UntidyChallenge_2

library(tidyverse)


#Making_Tidy_Data
#Add_a_new_column_that_shows_just_the_founding_year

Sample_data_new <- Sample_data %>%
  pivot_longer(c('2001', '2006', '2011', '2016', '2020'), names_to = "Year", values_to = "PC_Users") %>%
  separate("Founding Date", into = c("Founding Year", "Founding Month", "Founding Day")) %>%
  replace_na(list(PC_Users = 0))


#Create_a_table_that_generates_the_mean_#_of_PCs_used_for_each_country

Average_PC_Users <- Sample_data_new %>%
  group_by(Country) %>%
  summarize(mean_PC_Users = mean(PC_Users))


#Develop_a_visualization_that_describes_the_relationship_between_#_of_PC_users_and_year

ggplot(Sample_data_new, mapping = aes(x = Year, y = PC_Users, color = Country)) +
  geom_point() +
  geom_line() +
  labs(title = "Exploring the Realtionship Between PC Users and Year", x = "Year", y = "PC Users") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))