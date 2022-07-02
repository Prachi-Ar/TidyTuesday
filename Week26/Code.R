library(scales)
library(tidyverse)
library(showtext)
library(gridExtra)
library(cowplot)

paygap <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-28/paygap.csv')

showtext_auto()
font_add_google("Alegreya Sans")

paygap_data <- paygap %>% 
  separate(employer_size, into = c("lower_employee_size", "upper_employee_size"), sep = "to", remove =TRUE, convert = FALSE) 

paygap_data$upper_employee_size <- unlist(paygap_data$upper_employee_size)
paygap_data$upper_employee_size <- as.numeric(gsub(",", "", paygap_data$upper_employee_size))
paygap_data <- paygap_data %>% arrange(desc(male_top_quartile)) %>%
  select(employer_name, upper_employee_size, male_lower_quartile, female_lower_quartile, male_top_quartile, female_top_quartile)%>%
  mutate(employer_name_lower = tolower(employer_name)) 
  
paygap_data <- paygap_data %>% filter(grepl("university|college", employer_name_lower),
                                      grepl("oxford|cambridge", employer_name_lower),
                                      !grepl("nhs|school|brookes|regional", employer_name_lower)) %>%
  distinct(employer_name_lower, .keep_all = TRUE) %>%
  arrange(desc(male_top_quartile))

paygap_data$employer_name = ifelse(grepl("Undivided Trinity", paygap_data$employer_name), "Trinity College Oxford",
                                   ifelse(grepl("Churchill College", paygap_data$employer_name), "Churchill College Cambridge",
                                          ifelse(grepl("SOMERVILLE", paygap_data$employer_name), "Somerville College Oxford",
                                                 ifelse(grepl("HOMERTON", paygap_data$employer_name), "Homerton College Cambridge",
                                                        ifelse(grepl("Queen's", paygap_data$employer_name), "Queen's College Oxford",
                                                               ifelse(grepl("MANCHESTER", paygap_data$employer_name), "Harris Manchester College Oxford", 
                                                                      ifelse(grepl("Bailey", paygap_data$employer_name), "St Peter's College Oxford", paygap_data$employer_name)))))))

 
paygap_data$employer_name <- tolower(paygap_data$employer_name) 
paygap_data$employer_name <- gsub("college oxford", "college, oxford", paygap_data$employer_name) 
paygap_data$employer_name <- gsub("college cambridge", "college, cambridge", paygap_data$employer_name) 
paygap_data$employer_name <- str_to_title(paygap_data$employer_name)
paygap_data$employer_name <- gsub("Of", "of", paygap_data$employer_name) 
paygap_data <- paygap_data %>% arrange(female_top_quartile)


top_quartile <- paygap_data %>% 
  select(employer_name, male_top_quartile, female_top_quartile) %>%
  pivot_longer(cols =c(male_top_quartile, female_top_quartile), names_to = "gender", values_to = "percentage")

top_quartile$employer_name <- factor(top_quartile$employer_name, levels = rev(unique(top_quartile$employer_name)))


lower_quartile <- paygap_data %>% 
  select(employer_name, male_lower_quartile, female_lower_quartile) %>%
  pivot_longer(cols =c(male_lower_quartile, female_lower_quartile), names_to = "gender", values_to = "percentage")

lower_quartile$gender = ifelse(grepl("female", lower_quartile$gender), "Female", "Male")
lower_quartile$employer_name <- factor(lower_quartile$employer_name, levels = rev(unique(lower_quartile$employer_name)))



g1 <- ggplot(top_quartile, aes(x = percentage, y = employer_name, fill=gender)) +
  ggtitle("Top Hourly Pay Quarter")+
  geom_bar(stat = "identity") +
  geom_text(aes(label = ifelse(gender == "female_top_quartile",  paste0(percentage, "%"),""), family = "Alegreya Sans", fontface = "bold"), position = position_stack(vjust = 0.7), colour="#FFFFFF") +
  geom_text(aes(label = ifelse(gender == "male_top_quartile",  paste0(percentage, "%"),""), family = "Alegreya Sans", fontface = "bold"), position = position_stack(vjust = 0.6), colour="#FFFFFF") +
  scale_fill_manual(values = c("#685F74","#A6C48A")) +
  theme_void()+
  theme(plot.title = element_text(hjust = 0.9, family = "Alegreya Sans", size = 10, face = "bold", color = "#595858", margin=margin(0,0,8,0)),
        legend.position = "none")

g2 <- ggplot(lower_quartile, aes(x = percentage, y = employer_name, fill=gender)) +
  ggtitle("Lower Hourly Pay Quarter")+
  geom_bar(stat = "identity") +
  geom_text(aes(label = ifelse(gender == "Female",  paste0(percentage, "%"),""), family = "Alegreya Sans", fontface = "bold"), position = position_stack(vjust = 0.7), colour="#FFFFFF") +
  geom_text(aes(label = ifelse(gender == "Male",  paste0(percentage, "%"),""), family = "Alegreya Sans", fontface = "bold"), position = position_stack(vjust = 0.6), colour="#FFFFFF") +
  scale_fill_manual(values = c("#685F74", "#A6C48A")) +
  theme_void()+
  theme(plot.title = element_text(hjust = 0.1, family = "Alegreya Sans", size = 10, face = "bold", color = "#595858", margin=margin(0,0,8,0)))

g.mid <- ggplot(top_quartile, aes(x = employer_name, y = 1, label = employer_name))+
  coord_flip()+
  ggtitle("Colleges")+
  theme_void()+
  theme(axis.text.y = element_text(face = "bold", family = "Alegreya Sans"),
        plot.margin = unit(c(1,-1,1,-1), "mm"),
        plot.title = element_text(hjust = 0.1, family = "Alegreya Sans", size = 10, face = "bold", color = "#FFFFFF", margin=margin(0,0,8,0)),
        legend.position = "none",
        panel.background = "#FFFFFF")

gg1 <- ggplot_gtable(ggplot_build(g1) + theme(panel.background = "#FFFFFF"))
gg2 <- ggplot_gtable(ggplot_build(g2 + theme(legend.position = "none",
                                             panel.background = "#FFFFFF")))
gg.mid <- ggplot_gtable(ggplot_build(g.mid) + theme(panel.background = "#FFFFFF"))

legend <- get_legend(g2 + theme(legend.title = element_blank(),
                                legend.text = element_text(family = "Alegreya Sans", face = "bold", size = 10),
                                legend.position = "top",
                                legend.direction = "horizontal"))

grid <- grid.arrange(gg2,gg.mid,gg1,ncol=3,widths=c(4/9,1/9,4/9))
grid1 <- plot_grid(legend, grid, ncol = 1, align = "v", axis = "t", rel_heights = c(1,12))

color = c("Male" = "#685F74","Female" = "#A6C48A")

cowplot::ggdraw(grid1) +
  ggtitle("The Oxbridge Gender Pay Gap",
          subtitle = "Comparing the gender distribution of the lowest and the highest hourly pay quartiles \n at colleges in the University of Oxford and the University of Cambridge.")+
  labs(caption="Twitter: @ar_prachi_ | GitHub: Prachi-Ar")+
  theme(plot.title = element_text(hjust = 0.5, family = "Alegreya Sans", size = 20, face = "bold", color = "#595858", margin=margin(15,0,10,0)),
        plot.subtitle = element_text(hjust = 0.5, family = "Alegreya Sans", size = 14, color = "#595858", margin=margin(0,0,15,0)),
        plot.caption = element_text(hjust = 0.5, family = "Alegreya Sans", face = "bold", size = 8, color = "#595858", margin=margin(10,0,15,0)),
        panel.background = element_rect(colour = "#FFFFFF"))

ggsave("Week_26.png", width = 18, height = 11)



