#Class_UntidyChallenge_1

library(tidyverse)

untidy_1 <- read_csv("data/untidy_1.csv")

# Making_Tidy_Data
tidy_1 <- untidy_1%>%
  pivot_longer(Trial_1_Cortisol:Trial_3_Cortisol, names_to = "trial", values_to = "cortisol_levels")%>%
  separate(`Date of diagnosis`, into = c("year","month","day"))%>%
  mutate(trial_names = factor(trial, levels = unique(trial)))


# calculating the mean year, age, and cortisol_level across all data points
mean_tidy_1 <- tidy_1%>%
  summarise(mean_year = mean(as.numeric(year), na.rm = TRUE),
            mean_age = mean(Age),
            mean_cortisol_levels = mean(cortisol_levels, na.rm = TRUE))

# visualization for how cortisol levels change over time/trials

tidy_1%>%
  ggplot(aes(x = trial_names, y = cortisol_levels))+
  geom_point(alpha = .1)+
  geom_line(aes(group = Subject_ID), alpha = .3)+
  geom_violin(aes(fill = trial_names), alpha = .3)+
  guides(color = "none", fill = "none")+
  labs(title = "Cortisol Levels Dramatically Increase \nDuring the 3rd Trial", x = "Trial", y = "Cortisol Levels (uL)")
