rm(list=ls())

setwd("/Users/prachiarya/Desktop/TidyTuesday/2022/netflix")

country <- read_csv("data/all-weeks-countries.csv")

global  <- read_csv("data/all-weeks-global.csv")

data <- country %>% 
  filter(country_name %in% c("United States", 
                             "Australia", 
                             "South Korea", 
                             "France", 
                             "Mexico", 
                             "India")) %>%
  select(country_name, 
         week, 
         category, 
         weekly_rank, 
         show_title, 
         cumulative_weeks_in_top_10) %>%
  group_by(country_name) %>%
  arrange(desc(cumulative_weeks_in_top_10)) %>%
  distinct(country_name, category, show_title, cumulative_weeks_in_top_10)
