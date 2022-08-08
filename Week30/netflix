rm(list=ls())

setwd("/Users/prachiarya/Desktop/TidyTuesday/2022/netflix")

library(markdown)
library(showtext)
library(ggtext)
library(ggplot2)
library(glue)

country <- read_csv("data/all-weeks-countries.csv")
global  <- read_csv("data/all-weeks-global.csv")

# font

showtext_auto()
font_add_google("Bebas Neue")
font_add_google("Exo 2")

# Wrangling 

shows <- global %>%
  arrange(desc(weekly_hours_viewed)) %>%
  distinct(show_title, .keep_all = T) %>%
  top_n(17, weekly_hours_viewed)


data <- country %>% 
  filter(country_name %in% c("United States", 
                             "Australia", 
                             "Nigeria", 
                             "Mexico", 
                             "United Kingdom",
                             "Singapore",
                             "France"),
         show_title %in% shows$show_title) %>%
  select(country_name,
         country_iso2,
         show_title, 
         cumulative_weeks_in_top_10) %>%
  arrange(desc(cumulative_weeks_in_top_10)) %>%
  group_by(country_name) %>%
  distinct(show_title, .keep_all = T) %>%
  arrange(country_name) %>%
  ungroup()%>%
  mutate(number = as.numeric(factor(country_name)))


# Plot

colors <- c("#A60603", "#FECAAC", "#DE87A6", "#5E97FB", "#B0E9F6", "#6DECFD", "#5FBC79")

plot <- data %>% ggplot () +
  geom_segment(data = data.frame(y=c(5, 15)), aes(x=0.25, xend=7.75, y=y, yend=y), linetype="8f", colour= "#f2f2f2", alpha = 0.5) +
  geom_text(data = data.frame(x=0, y=c(5, 15)), aes(x=x, y=y, label=y), size=3, family = "Montserrat", colour = "#f2f2f2", alpha = 0.5) +
  geom_bar(aes(x = number, y=cumulative_weeks_in_top_10, fill=as.factor(number)), stat="identity") +
  facet_wrap(~show_title, strip.position = "bottom")+
  scale_fill_manual(values = colors) +
  ylim(-4, 18) +
  xlim(0, 8) +
  coord_polar()+
  labs(
    title = "NETFLIX",
    subtitle = sprintf(
  "<p>The length of the bars represent the number of weeks<br>for which the Netflix show trended in the Top 10 in<span style='color:%s;font-weight:bold'>Australia,</span><br><span style='color:%s;font-weight:bold;'>France,</span><span style='color:%s;font-weight:bold;'>Mexico,</span><span style='color:%s;font-weight:bold'>Nigeria,</span><span style='color:%s;font-weight:bold'>Singapore,</span><span style='color:%s;font-weight:bold;'>United Kingdom,</span>and the<br><span style='color:%s;font-weight:bold;'>United States</span>in<span style=font-weight:bold;'> 2021.</span></p>", 
  "#A60603",  
  "#FECAAC", 
  "#DE87A6", 
  "#5E97FB", 
  "#B0E9F6", 
  "#6DECFD", 
  "#5FBC79"),
  caption = "Twitter: @ar_prachi_| Github: Prachi-Ar | Data source: Kaggle/Mikit Kanakia"
       )+
  theme_void()+
  theme(legend.position = "none",
        strip.text = element_text(size=10, family = "Exo 2", face="bold", lineheight = 0.3, vjust =1, colour = "#f2f2f2"),
        plot.background = element_rect(fill = "black"),
        plot.title = element_text(color = "#E30914", family = "Bebas Neue", size = 50, hjust = 0.5, margin = margin(0, 10, 5, 10)),
        plot.caption = element_text(family = "Exo 2", color = "#f2f2f2", margin = margin(10,0,5,0), hjust = 0.5),
        plot.subtitle = element_markdown(family = "Exo 2", color = "#f2f2f2", margin = margin(10, 10, 20, 0), hjust = 0.5),
        plot.margin = margin(0,0,0,20)
        )
  
ggsave("netflix", device = "jpeg", height = 1440, width = 900, units = "px", dpi = 150)

