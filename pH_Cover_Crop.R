library(tidyverse)
library (ggplot2)
library (dplyr)

pH_all_days <- read.csv("Data/Summer_pH_Final2.csv")

sum(is.na(pH_all_days))

all_data_pH <- pH_all_days%>% 
  separate(col=sample.name, into= c('temperature',  'day', 'Treatment'), sep='_')

all_data_pH$day_numeric<- str_replace(all_data_pH$day, "D", " ")
all_data_pH$day_numeric <- as.numeric(all_data_pH$day_numeric)

all_data_pH <- all_data_pH %>% 
  mutate(Treatment = case_when(
    Treatment == "T1" ~ "Soil Control (No CC)",
    Treatment == "T2" ~ "CN10",
    Treatment == "T3" ~ "CN14",
    Treatment == "T4" ~ "CN19",
    Treatment == "T5" ~ "CN30",
    Treatment == "T6"~ "CN30+FertN",
    TRUE ~ Treatment
  ))    

all_data_pH <- all_data_pH %>% 
  mutate(temperature = case_when(
    temperature == "A10" ~ "10C",
    temperature == "B15" ~ "15C",
    temperature == "C20" ~ "20C",
    TRUE ~ temperature
  ))

all_data_pH$day <- NULL

custom_colors <- c("CN10" ="#90b76e","CN14"= "#ef5f00","CN19"= "#3399FF","CN30"= "#D4C95F","CN30+FertN" ="#cad9f4","Soil Control (No CC)" = "#682714")
custom_shapes <- c("CN10"=8, "CN14"=17, "CN19"=18, "CN30"=19, "CN30+FertN"=10, "Soil Control (No CC)" =0)   # Custom shapes for each Treatment
colnames(all_data_pH)

# Correctly defining a labeller function using the 'as_labeller' function
temperature_labeller <- as_labeller(function(x) gsub("C", "°", x))

#figure 6
all_data_pH%>%
  ggplot(aes(x=day_numeric, y=pH, group=Treatment,color=Treatment, shape=Treatment))+
  geom_point(size = 3) +  # Increase the size of the points
  geom_line(linetype= "solid", linewidth=0.75) +
  facet_wrap(~temperature, ncol=1, labeller = temperature_labeller)+
  labs(x="Incubation Duration (d)", y= "pH")+
  #scale_x_continuous(breaks = all_data_pH$day_numeric) + 
  scale_color_manual(values=custom_colors)+
  scale_shape_manual(values=custom_shapes)+
  theme_bw() +  # Apply base theme first
  theme(
    strip.text = element_text(size = 14, face = "bold"),  # Customize facet label size and style
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )
  #+theme(legend.position = "none")

ggsave(last_plot(), 
       filename = "Figures/pH.png",
       width=5,
       height=8,
       units= "in",
       dpi=300)
