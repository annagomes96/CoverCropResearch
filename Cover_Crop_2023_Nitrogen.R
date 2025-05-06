#script for analyzing cover crop lab incubation data, still current in June 2024
library(tidyverse)
library (ggplot2)
library (dplyr)
library(writexl)
library(multcomp)
library(multcompView)
library(broom)
library(agricolae)
library(stringr)
library(forcats) # for facet_relabel

PAN_Day1 <- read.csv("Data/Gomes_Run2_711.csv", skip=1)
PAN_Day15 <- read.csv("Data/Gomes_Run3_727.csv", skip=1)
PAN_Day29 <- read.csv("Data/Copy_of_Gomes_Run_8.csv", skip=1)
PAN_Day57 <- read.csv ("Data/Gomes_Run_12_PAN_Gomes_Updated.csv", skip=1)
PAN_Day85 <- read.csv ("Data/Gomes_Run_15_Oct_3.csv", skip=1)
PAN_Day112 <- read.csv ("Data/Gomes_Run_22_Oct_31.xlsx - PAN_Soil.csv", skip=1)
Initial_Nitrogen <- read.csv ("Data/percent_min_updated.csv")

# Create DF with treatments, CN, percent N, and N application rate
Treatment <- c("CN10", "CN14", "CN19", "CN30")
CN_ratio <- c("10:1", "14:1", "19:1", "30:1")
N_content <- c(4.11, 2.99, 2.28, 1.51)
N_app_rate <- c(263.04,
                191.36,
                145.92,
                96.64) #mgN/kg soi or ugN/g soil
N_app_rate_kg <- c(322.5, 234.6, 178.9, 118.5) 
                      
# Creating the dataframe
Treatment_info <- data.frame(Treatment, CN_ratio, N_content, N_app_rate, N_app_rate_kg)

PAN_Day29$New.PAN <- NULL

all_data_PAN <- rbind(PAN_Day1, PAN_Day15, PAN_Day29, PAN_Day57, PAN_Day85, PAN_Day112)

sum(is.na(all_data_PAN))

all_data_tidy <- all_data_PAN %>% 
  separate(col=Sample, into= c('tube', 'temperature', 'Treatment', 'replicate', 'day'), sep='_')

all_data_tidy$day_numeric<- str_replace(all_data_tidy$day, "D", " ")
all_data_tidy$day_numeric <- as.numeric(all_data_tidy$day_numeric)


all_data_tidy$r_numeric<- str_replace(all_data_tidy$replicate, "R", " ")
all_data_tidy$r_numeric <- as.numeric(all_data_tidy$r_numeric)

all_data_tidy <- all_data_tidy %>% 
  mutate(Treatment = case_when(
    Treatment == "T1" ~ "Soil Control",
    Treatment == "T2" ~ "CN10",
    Treatment == "T3" ~ "CN14",
    Treatment == "T4" ~ "CN19",
    Treatment == "T5" ~ "CN30",
    Treatment == "T6"~ "CN30+FertN",
    TRUE ~ Treatment
  ))  

all_data_tidy <- all_data_tidy %>% 
  mutate(temperature = case_when(
    temperature == "A10" ~ "10C",
    temperature == "B15" ~ "15C",
    temperature == "C20" ~ "20C",
    TRUE ~ temperature
  ))

all_data_tidy$soil_type <- NULL
all_data_tidy$replicate <- NULL
all_data_tidy$day <- NULL

# Define the colors for each treatment
#color_palette <- c("Fallow" = "#0057e7", "Early Termination-Fall" = "#008744", "Late Termination-Fall" = "#ffa700", "Winter CC" = "#d62d20")
custom_colors <- c("CN10" ="#90b76e","CN14"= "#ef5f00","CN19"= "#3399FF","CN30"= "#D4C95F","CN30+FertN" ="#707070","Soil Control" = "#682714")
custom_shapes <- c("CN10"=8, "CN14"=17, "CN19"=18, "CN30"=19, "CN30+FertN"=10, "Soil Control" =0)   # Custom shapes for each Treatment
#original c("#24693D","#308344","#68B05D","#95B958","#D4C95F","#682714") 
#027662
colnames(all_data_tidy)

#naming the nitrate and ammonium-N columns
colnames(all_data_tidy)[colnames(all_data_tidy) == "final_value"] <- "nitrate_N"
colnames(all_data_tidy)[colnames(all_data_tidy) == "final_value.1"] <- "ammonium_N"

#export DF to a csv file 
write.csv(all_data_tidy, file = "PAN_csv_file.csv", row.names = FALSE)

PAN_avg <- all_data_tidy %>%
  group_by(temperature, Treatment, day_numeric) %>%
  summarise(avg=mean(PAN), sd=sd(PAN), n=n()) %>% 
  mutate(se=sd/sqrt(n))

# Calculate the confidence intervals
PAN_avg_df <- PAN_avg %>%
  mutate(
    lower_ci = avg - qt(0.975, df = n - 1) * (se / sqrt(n)),
    upper_ci = avg + qt(0.975, df = n - 1) * (se / sqrt(n))
  )

NO3_avg <- all_data_tidy %>%
  group_by(temperature, Treatment, day_numeric) %>%
  summarise(avg=mean(nitrate_N), sd=sd(nitrate_N), n=n()) %>% 
  mutate(se=sd/sqrt(n))

NH4_avg <- all_data_tidy %>%
  group_by(temperature, Treatment, day_numeric) %>%
  summarise(avg=mean(ammonium_N), sd=sd(ammonium_N), n=n()) %>% 
  mutate(se=sd/sqrt(n))

PAN_avg %>%
  #filter(day_numeric<29)%>%
  ggplot(aes(x=day_numeric, y=avg, group=Treatment, color=Treatment, shape=Treatment)) +
  geom_point() +
  geom_line(linetype= "solid") +
  facet_wrap(~temperature) +
  #scale_x_continuous(breaks = PAN_avg$day_numeric) +  # Set x-axis breaks at the days of data
  geom_errorbar(aes(ymin=avg-se, ymax=avg+se), width=.2, position=position_dodge(0.05)) +
  labs(x="Day of Incubation", y= "NH4+ & NO3- (ug Nitrogen/g of dry soil)") +
  ggtitle("Nitrogen Mineralization of Cover Crop Residue") +
  scale_color_manual(values=custom_colors) +
  scale_shape_manual(values=custom_shapes) +
  theme_bw() +  # Apply base theme first
  theme(
    strip.text = element_text(size = 14, face = "bold"),  # Customize facet label size and style
    # Add any other theme customizations here
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

ggsave(last_plot(), 
       filename = "Figures/dynamics_Nitrogen_mineralization_errorbars.png",
       width=8,
       height=6,
       units= "in",
       dpi=300)

PAN_avg_df %>%
  ggplot(aes(x=day_numeric, y=avg, group=Treatment, color=Treatment))+
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = Treatment), alpha = 0.2) +
  geom_point()+
  geom_line(linetype= "solid") +
  facet_wrap(~temperature)+
  #scale_x_continuous(breaks = PAN_avg_df$day_numeric) +  # Set x-axis breaks at the days of data
  labs(x="Day of Incubation", y= "NH4+ & NO3- (ug Nitrogen/g of dry soil)")+
  ggtitle("Nitrogen Mineralization of Cover Crop Residue")+
  scale_color_manual(values=custom_colors)+
  scale_fill_manual (values=custom_colors)+
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred", linewidth=1) +
  theme_bw() +  # Apply base theme first
  theme(
    strip.text = element_text(size = 14, face = "bold"),  # Customize facet label size and style
    # Add any other theme customizations here
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

ggsave(last_plot(), 
       filename = "Figures/dynamics_Nitrogen_mineralization_confidence_interval_updated.jpg",
       width=8,
       height=6,
       units= "in",
       dpi=300)

#facet wrap by treatment
PAN_avg_df %>%
  ggplot(aes(x=day_numeric, y=avg, group=temperature, color=temperature))+
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.2) +
  geom_point()+
  geom_line(linetype= "solid") +
  facet_wrap(~Treatment, scale="free")+
  #scale_x_continuous(breaks = PAN_avg_df$day_numeric) +  # Set x-axis breaks at the days of data
  labs(x="Day of Incubation", y= "NH4+ & NO3- (ug Nitrogen/g of dry soil)")+
  ggtitle("Nitrogen Mineralization of Cover Crop Residue by Temperature")+
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred", linewidth=1) +
  theme_bw() +  # Apply base theme first
  theme(
    strip.text = element_text(size = 14, face = "bold"),  # Customize facet label size and style
    # Add any other theme customizations here
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

ggsave(last_plot(), 
       filename = "Figures/dynamics_Nitrogen_temperature.png",
       width=8,
       height=6,
       units= "in",
       dpi=300)

# Correctly defining a labeller function using the 'as_labeller' function
temperature_labeller <- as_labeller(function(x) gsub("C", "°", x))

#only plot the highest value for each treatment/temperature combo?
highest_avg_NH4 <- NH4_avg %>%
  group_by(Treatment, temperature) %>%
  mutate(max_avg = max(avg)) %>%
  filter(avg == max_avg) %>%
  ungroup()

# SI fig 1
NH4_avg %>%
  #filter(Treatment == "CN30" | Treatment == "CN30+FertN") %>%
  ggplot(aes(x=day_numeric, y=avg, group=Treatment, color=Treatment, shape=Treatment)) +
  geom_point(size=3) +
  geom_line(linetype= "dashed", size=1) +
  facet_wrap(~temperature, labeller = temperature_labeller) + # Correctly use the labeller function
  #scale_x_continuous(breaks = NH4_avg$day_numeric) +  # Set x-axis breaks at the days of data
  geom_errorbar(aes(ymin=avg-se, ymax=avg+se), width=.2, position=position_dodge(0.05)) +
  labs(x="Incubation Duration (d)", y=expression(NH[4]*"-Nitrogen"~(mu*g/g)))+
  scale_color_manual(values=custom_colors) +
  scale_shape_manual(values = custom_shapes) +  # Apply custom shapes
  #geom_text(data = highest_avg_NH4, aes(label=round(avg)), vjust=-0.5, hjust=-0.1, color="black") + # Only highest avg values shown
  theme_bw() +  # Apply base theme first
  theme(
    strip.text = element_text(size = 14, face = "bold"),  # Customize facet label size and style
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

ggsave(last_plot(), 
       filename = "Figures/dynamics_Ammonium_SI_1.png",
       width=8,
       height=5,
       units= "in",
       dpi=300)

#only plot the highest value for each treatment/temperature combo?
highest_avg_NO3 <- NO3_avg %>%
  group_by(Treatment, temperature) %>%
  mutate(max_avg = max(avg)) %>%
  filter(avg == max_avg) %>%
  ungroup()

#only nitrate, figure 3
NO3_avg %>%
  #filter(Treatment == "CN30" | Treatment == "CN30+FertN") %>%
  ggplot(aes(x=day_numeric, y=avg, group=Treatment, color=Treatment, shape=Treatment)) +
  geom_point(size=3) +
  geom_line(linetype= "dashed", size=1) +
  facet_wrap(~temperature, labeller = temperature_labeller) + # Correctly use the labeller function
  #scale_x_continuous(breaks = NO3_avg$day_numeric) +  # Set x-axis breaks at the days of data
  geom_errorbar(aes(ymin=avg-se, ymax=avg+se), width=.2, position=position_dodge(0.05)) +
  labs(x="Incubation Duration (d)", y=expression(NO[3]*"-Nitrogen"~(mu*g/g)))+
  scale_color_manual(values=custom_colors) +
  scale_shape_manual(values = custom_shapes) +  # Apply custom shapes
  #geom_text(data = highest_avg_NO3, aes(label=round(avg, 2)), vjust=-0.5, hjust=1, color="black") + # Only highest avg values shown
  theme_bw() +  # Apply base theme first
  theme(
    strip.text = element_text(size = 14, face = "bold"),  # Customize facet label size and style
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

ggsave(last_plot(), 
       filename = "Figures/dynamics_Nitrate_N_fig3.png",
       width=8,
       height=5,
       units= "in",
       dpi=300)

#figure SI 3
PAN_avg %>%
  filter(Treatment == "Soil Control") %>%
  ggplot(aes(x = day_numeric, y = avg, group = temperature)) +
  geom_line(aes(linetype = temperature), color = "black") +
  scale_linetype_manual(values = c("10C" = "solid", "15C" = "dashed", "20C" = "dotted")) +
  geom_errorbar(aes(ymin = avg - se, ymax = avg + se), width = .2, position = position_dodge(0.05)) +
  labs(x = "Incubation Duration (d)", y = expression(NO[3] * "-Nitrogen & " * NH[4] * "-Nitrogen" ~ (mu * g/g))) +
  theme_bw() +
  theme(
    strip.text = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "none"  # Remove the legend
  ) +
  geom_text(
    data = . %>% filter(day_numeric == max(day_numeric)),  # Place labels at the end of the lines
    aes(label = paste0(temperature, "°C")),
    hjust = -0.1, vjust = 0.5  # Adjust these values to fine-tune label positions
  )


ggsave(last_plot(), 
       filename = "Figures/dynamics_soilonly_SI3.png",
       width=8,
       height=6,
       units= "in",
       dpi=300)

#percent mineralized: (average for Soil Only then at end (%min))
Soil_Only <- all_data_tidy %>%
  filter(Treatment=="Soil Control") %>%
  group_by(temperature, day_numeric) %>%
  summarise(avg=mean(PAN), sd=sd(PAN), n=n()) 

Net_Mineralized <- all_data_tidy %>%
left_join(Soil_Only, by= c("day_numeric", "temperature")) %>%
  mutate(net_min = PAN - avg)

#filter out 'Soil Control' since now have Net Mineralized
Net_Mineralized<- Net_Mineralized %>%
  filter(Treatment!="Soil Control")

#subtract out mineral N added to T6 before calc %N Mineralized 
Net_Mineralized <- Net_Mineralized %>%
  mutate(net_min = if_else(Treatment == "CN30+FertN", net_min - 9.2565, net_min))

#now calc Net N min per tube (250grams)
Net_Mineralized <- Net_Mineralized %>%
  group_by(temperature, Treatment, day_numeric, r_numeric)%>%
  mutate(Nmin_per_tube= (net_min * 250))

Avg_Net_Min <- Net_Mineralized %>%
  group_by(temperature, Treatment, day_numeric) %>%
  summarise(avg=mean(net_min), sd=sd(net_min), n=n()) %>%
  mutate(se=sd/sqrt(n))

Avg_Net_Min_T6 <- Avg_Net_Min %>%
  filter(Treatment != "CN30+FertN")

#plot Net N Mineralized without T6, Fig 4
Avg_Net_Min_T6 %>%
  ggplot(aes(x=day_numeric, y=avg, group= Treatment, color=Treatment, shape=Treatment))+
  geom_point(size=3)+
  geom_line(linetype= "dashed", size=1) +
  facet_wrap(~temperature, labeller = temperature_labeller) + 
  geom_errorbar(aes(ymin=avg-se, ymax= avg+se), width=.2, position=position_dodge(0.05))+
  labs(x="Incubation Duration (d)", y = expression(NO[3] * "-Nitrogen & " * NH[4] * "-Nitrogen" ~ (mu * g/g))) +
  #ggtitle("Net Nitrogen Mineralization of Cover Crop Residue")+
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred", linewidth=1) +
  scale_color_manual(values=custom_colors) +  # Remove the legend
  scale_shape_manual(values = custom_shapes) +  # Apply custom shapes
  #geom_text(aes(label=Treatment, y = avg), color="black", size=5, vjust = -0.8, hjust = 1, data = subset(Avg_Net_Min_T6, day_numeric == max(day_numeric))) + # Add labels at the last point
  #scale_x_continuous(breaks = Avg_Net_Min_T6$day_numeric) +  # Set x-axis breaks at the days of data
  theme_bw() +  # Apply base theme first
  theme(
    strip.text = element_text(size = 14, face = "bold"),  # Customize facet label size and style
    # Add any other theme customizations here
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

ggsave(last_plot(), 
       filename = "Figures/Net_Nitrogen_fig4.png",
       width=8,
       height=5,
       units= "in",
       dpi=300)

#calculate percent nitrification (nitrate-N/ total PAN for each sampling date THEN average)
#plot again with percent nitrate on the data points..as labels 
  
#now calc percent mineralized (Net_min_tube_g/Start_ON_per tube_g)
Initial_Nitrogen <- Initial_Nitrogen %>%
  mutate(Treatment = gsub("CC", "CN", Treatment))

Percent_Mineralized <- Net_Mineralized %>% 
  left_join(Initial_Nitrogen, by= ("Treatment")) %>%
  mutate(min = (Nmin_per_tube/start_ON_ug_tube)) %>%
  mutate(per_min = min*100) #turn into a percentage

#now average the percent min between the four replicates
Percent_Mineralized_avg <- Percent_Mineralized %>%
  group_by(temperature, Treatment, day_numeric) %>%
  summarise(avg_per_min=mean(per_min), sd=sd(per_min), n=n()) %>% 
  mutate(se=sd/sqrt(n))  

#not including T6
#plot avg percent min
Percent_Mineralized_avg %>%
  filter (Treatment != "CN30+FertN") %>% 
  ggplot(aes(x=day_numeric, y=avg_per_min, group= Treatment, color=Treatment))+
  geom_point()+
  geom_line(linetype= "solid") +
  facet_wrap(~temperature)+
  geom_errorbar(aes(ymin=avg_per_min-se, ymax= avg_per_min+se), width=.2, position=position_dodge(0.05))+
  labs(x="Day of Incubation", y= "% Nitrogen Mineralized")+
  ggtitle("Percent Nitrogen Mineralized")+
  scale_color_manual(values=custom_colors)+
  #scale_x_continuous(breaks = Percent_Mineralized_avg$day_numeric) +  # Set x-axis breaks at the days of data
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred", linewidth=1) +
  theme_bw() +  # Apply base theme first
  theme(
    strip.text = element_text(size = 14, face = "bold"),  # Customize facet label size and style
    # Add any other theme customizations here
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

ggsave(last_plot(), 
       filename = "Figures/N_min_EB.png",
       width=8,
       height=5,
       units= "in",
       dpi=300)

#plot avg percent min now with CI instead! 
#calc 95% CI 
Percent_Mineralized_CI <- Percent_Mineralized %>% 
  group_by(temperature, Treatment, day_numeric) %>%
  summarise(
    mean_per_min = mean(per_min),
    se_per_min = sd(per_min) / sqrt(n()),
    ci_lower = mean_per_min - qt(0.975, df = n() - 1) * se_per_min,
    ci_upper = mean_per_min + qt(0.975, df = n() - 1) * se_per_min
  )

#figure 6
Percent_Mineralized_CI %>%
  filter (Treatment != "CN30+FertN") %>%
  ggplot(aes(x = day_numeric, y = mean_per_min, group = Treatment, color = Treatment, shape=Treatment)) +
  geom_point(size=3) +
  geom_line(linetype="dashed", size=1) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = Treatment), alpha = 0.1, color = NA) +
  facet_wrap(~temperature, labeller = temperature_labeller) +
  labs(x = "Incubation Period (d)", y = expression("Mineralized N" ~ "[" ~ NO[4] + NO[3] ~ "]" ~ "(%)")) +
  #ggtitle("Mineralization (%) of Cover Crop Residue Nitrogen") +
  scale_color_manual(values = custom_colors) +
  scale_shape_manual(values = custom_shapes) +  # Apply custom shapes
  scale_fill_manual(values = custom_colors) +  # Ensure fill colors match line colors
  #scale_x_continuous(breaks = unique(Percent_Mineralized_CI$day_numeric)) +  # Set x-axis breaks at the days of data
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred", linewidth = 1) +
  theme_bw() +  # Apply base theme first
  theme(
    strip.text = element_text(size = 14, face = "bold"),  # Customize facet label size and style
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

ggsave(last_plot(), 
       filename = "Figures/N_min_fig6.png",
       width=8,
       height=5,
       units= "in",
       dpi=300)

#save to Excel file for writing reference 
write_xlsx(Percent_Mineralized_CI, "Percent_Min_CI.xlsx")

#plot avg percent min T5 vs.s T6
#figure 9
Percent_Mineralized_avg %>%
  filter (Treatment == "CN30" | Treatment == "CN30+FertN") %>% 
  ggplot(aes(x=day_numeric, y=avg_per_min, group= Treatment, color=Treatment, shape=Treatment)) +
  geom_point(size=3) +
  geom_line(linetype= "dashed", linewidth=1) +
  facet_wrap(~temperature, labeller = temperature_labeller) +
  #scale_x_continuous(breaks = unique(Percent_Mineralized_avg$day_numeric)) +  # Set x-axis breaks at the days of data
  geom_errorbar(aes(ymin = avg_per_min - se, ymax = avg_per_min + se), 
                width = 16,  # Adjust this value for the length of the caps
                position = position_dodge(0.05)) +
  labs(x = "Incubation Period (d)", y = expression("Mineralized N" ~ "[" ~ NO[4] + NO[3] ~ "]" ~ "(%)"))+
  #ggtitle("Percent Nitrogen Mineralized, CN30 with Doubled Residual Soil N")+
  scale_color_manual(values=custom_colors)+
  scale_shape_manual(values = custom_shapes) +  # Apply custom shapes
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred", linewidth=1) +
  theme_bw() +  # Apply base theme first
  theme(
    strip.text = element_text(size = 14, face = "bold"),  # Customize facet label size and style
    # Add any other theme customizations here
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank()  # Remove major grid lines
    #panel.grid.minor = element_blank()   # Remove minor grid lines
  )

ggsave(last_plot(), 
       filename = "Figures/N_minT6_fig9.png",
       width=8,
       height=5,
       units= "in",
       dpi=300)

#ANOVA to compare b/w treatments, all reps in three temps 
#just Day 112 Nmin% 
Percent_Mineralized_d112 <- Percent_Mineralized %>%
  filter(day_numeric == 112)

anova <- aov(per_min ~ Treatment, data = Percent_Mineralized)
summary(anova)

tukey <- TukeyHSD(anova)
print(tukey)

cld <- multcompLetters4(anova, tukey)
print(cld)

Tk <- group_by(Percent_Mineralized, Treatment) %>%
  summarise(mean=mean(per_min), quant = quantile(per_min, probs = 0.75)) %>%
  arrange(desc(mean))

# extracting the compact letter display and adding to the Tk table
cld <- as.data.frame.list(cld$Treatment)
Tk$cld <- cld$Letters

Tk <- Tk %>%
  filter (Treatment != "CN30+FertN")
print(Tk)

#figure 5
Percent_Mineralized %>%
  filter (Treatment != "CN30+FertN") %>% 
  ggplot(aes(x=Treatment, y=per_min, group= Treatment, color=Treatment))+
  geom_boxplot()+
  geom_text(data = Tk, aes(x = Treatment, y = quant, label = cld), size = 5, vjust=-1, hjust =-1) + 
  labs(x="Treatment", y = expression("Mineralized N" ~ "[" ~ NO[4] + NO[3] ~ "]" ~ "(%)")) +
  #ggtitle("Percent Nitrogen Mineralized, All Temperatures")+
  scale_color_manual(values=custom_colors)+
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred", linewidth=1) +
  theme_bw() +  # Apply base theme first
  theme(
    strip.text = element_text(size = 14, face = "bold"),  # Customize facet label size and style
    # Add any other theme customizations here
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

ggsave(last_plot(), 
       filename = "Figures/N_min_box_tukey_all.png",
       width=8,
       height=5,
       units= "in",
       dpi=300)

#now only Day 112 Nmin Data
anova <- aov(per_min ~ Treatment, data = Percent_Mineralized_d112)
summary(anova)

tukey <- TukeyHSD(anova)
print(tukey)

cld <- multcompLetters4(anova, tukey)
print(cld)

Tk <- group_by(Percent_Mineralized_d112, Treatment) %>%
  summarise(mean=mean(per_min), quant = quantile(per_min, probs = 0.75)) %>%
  arrange(desc(mean))

# extracting the compact letter display and adding to the Tk table
cld <- as.data.frame.list(cld$Treatment)
Tk$cld <- cld$Letters

Tk <- Tk %>%
  filter (Treatment != "CN30+FertN")
print(Tk)

#figure 7 (d112)
Percent_Mineralized_d112 %>%
  filter (Treatment != "CN30+FertN") %>% 
  ggplot(aes(x=Treatment, y=per_min, group= Treatment, color=Treatment))+
  geom_boxplot()+
  geom_text(data = Tk, aes(x = Treatment, y = quant, label = cld), size = 5, vjust=-1, hjust =-1) + 
  labs(x="Treatment", y= "Mineralized N [NO4+NO3] (%)")+
  #ggtitle("Percent Nitrogen Mineralized, All Temperatures")+
  scale_color_manual(values=custom_colors)+
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred", linewidth=1) +
  theme_bw() +  # Apply base theme first
  theme(
    strip.text = element_text(size = 14, face = "bold"),  # Customize facet label size and style
    # Add any other theme customizations here
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )


ggsave(last_plot(), 
       filename = "Figures/N_min_box_tukey_d112.png",
       width=8,
       height=5,
       units= "in",
       dpi=300)


# C:N Ratio or percent N vs. % Mineralized (Day 112)
Day112_df <- Percent_Mineralized_avg %>%
  filter(day_numeric == 112, Treatment != "CN30+FertN")

#add intial percent N to dataframe
Day112_df <- Day112_df %>% 
  left_join(Initial_Nitrogen, by= ("Treatment"))

#pull out C:N ratio and make into numeric collumn
# Step 1: Extract numeric values from Treatment column
Day112_df$CN <- as.numeric(gsub("CN", "", Day112_df$Treatment))
Day112_df$temperature_num <- as.numeric(gsub("C", "", Day112_df$temperature))
Day112_df$temperature_num <- as.factor(Day112_df$temperature_num)
custom_colors_CN <- c(`10` = "#90b76e", `14` = "#ef5f00", `19` = "#3399FF", `30` = "#D4C95F")


# Calculate linear model coefficients and R-squared values for each temperature group
models <- Day112_df %>%
  group_by(temperature_num) %>%
  do({
    model <- lm(avg_per_min ~ CN, data = .)
    data.frame(
      label = paste("italic(y) == ", format(coef(model)[1], digits = 2), " + ", 
                    format(coef(model)[2], digits = 2), "*italic(x) ~ ',' ~",
                    "italic(R)^2 == ", format(summary(model)$r.squared, digits = 2), sep=""),
      intercept = coef(model)[1],
      slope = coef(model)[2],
      r_squared = summary(model)$r.squared,
      CN = mean(.$CN),  # Adjust the position for the label if necessary
      avg_per_min = max(.$avg_per_min) + 0.1  # Position above the highest point
    )
  })

# Now use this updated models data frame in your plot,  Fig 5 
ggplot(Day112_df, aes(x = CN, y = avg_per_min)) +
  geom_point(color = "black") +  # Make all points black
  geom_smooth(aes(linetype = temperature_num), method = "lm", se = FALSE, formula = y ~ x, color = "black") +  # Make all lines black with different linetypes
  #geom_text(data = models, aes(label = label, x = CN, y = avg_per_min), parse = TRUE, hjust = 0.5, vjust = 1) +
  labs(x = "C:N Ratio", y = expression("Mineralized N" ~ "[" ~ NO[4] + NO[3] ~ "]" ~ "(%)")) +
  scale_linetype_manual(values = c("10" = "solid", "15" = "dashed", "20" = "dotted"), name = "Temperature (°C)") +
  theme_bw() +
  theme(
    strip.text = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = c(0.95, 0.95),  # Position legend inside the plot, top-right corner
    legend.justification = c("right", "top"),  # Adjust legend anchor point
    legend.background = element_rect(fill = alpha('white', 0.5)),  # Optional: semi-transparent legend background
    legend.key.width = unit(1.5, "cm")  # Increase the width of the legend key to show more of the linetype
  )


print(models)

ggsave(last_plot(), 
       filename = "Figures/Linear_Fig5.png",
       width=8,
       height=5,
       units= "in",
       dpi=300)

# %Nmin vs. Temp for the four treatments (C:N ratios)
# Ensure temperature_num is numeric
Day112_df <- Day112_df %>%
  mutate(temperature_num = as.numeric(as.character(temperature_num)))

# Fit the linear model for each treatment
models_Treatments <- Day112_df %>%
  group_by(Treatment) %>%
  do({
    model <- lm(avg_per_min ~ temperature_num, data = .)
    data.frame(
      Treatment = unique(.$Treatment),
      Equation = paste("y =", format(coef(model)[1], digits = 2), "+", 
                       format(coef(model)[2], digits = 2), "x"),
      R_squared = format(summary(model)$r.squared, digits = 2)
    )
  })

# Print out the equations and R-squared values
print(models_Treatments)

Day112_df <- Day112_df %>%
  mutate(temperature_num = factor(temperature_num))

#figure 7
ggplot(Day112_df, aes(x = temperature_num, y = avg_per_min, group = Treatment, shape = Treatment)) +
  geom_point(aes(color = Treatment), size=3) +  # Points with custom colors
  geom_smooth(aes(color = Treatment), method = "lm", se = FALSE, formula = y ~ x) +  # Lines with custom colors
  scale_color_manual(values = custom_colors) +  # Apply custom colors
  scale_shape_manual(values = custom_shapes) +  # Apply custom shapes
  labs(x = "Temperature (°C)", y = expression("Mineralized N" ~ "[" ~ NO[4] + NO[3] ~ "]" ~ "(%)"), color = "Treatment", linetype = "Treatment") +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

# Calculate the midpoint for each line
# Convert the temperature character to numeric
Day112_df <- Day112_df %>%
  mutate(temperature_num = as.numeric(gsub("C", "", temperature)))
mid_points <- Day112_df %>%
  group_by(Treatment) %>%
  summarize(mid_temp = mean(temperature_num), 
            mid_avg_per_min = mean(avg_per_min))

#figure 7 with legend gone
ggplot(Day112_df, aes(x = temperature_num, y = avg_per_min, group = Treatment, shape = Treatment)) +
  geom_point(aes(color = Treatment), size = 3) +  # Points with custom colors
  geom_smooth(aes(color = Treatment), method = "lm", se = FALSE, formula = y ~ x) +  # Lines with custom colors
  scale_color_manual(values = custom_colors) +  # Apply custom colors
  scale_shape_manual(values = custom_shapes) +  # Apply custom shapes
  
  # Add labels at the midpoint of the lines
  geom_text(data = mid_points, aes(x = mid_temp, y = mid_avg_per_min, label = Treatment, color = Treatment), 
            size = 4, fontface = "bold", vjust = -1) +  # Adjust the position with vjust
  
  labs(x = "Temperature (°C)", 
       y = expression("Mineralized N" ~ "[" ~ NO[4] + NO[3] ~ "]" ~ "(%)"), 
       color = "Treatment", 
       linetype = "Treatment") +
  theme_minimal() +
  theme(
    legend.position = "none",  # Remove legend as labels are on the lines
    legend.title = element_text(face = "bold"),
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )
#now try quadratic fit (n=2 degrees)
# %Nmin vs. Temp for the four treatments (C:N ratios)
# Ensure temperature_num is numeric
Day112_df <- Day112_df %>%
  mutate(temperature_num = as.numeric(as.character(temperature_num)))

# Fit the quadratic model for each treatment
models_Treatments <- Day112_df %>%
  group_by(Treatment) %>%
  do({
    model <- lm(avg_per_min ~ poly(temperature_num, 2), data = .)  # Fit quadratic model
    data.frame(
      Treatment = unique(.$Treatment),
      Equation = paste("y =", format(coef(model)[1], digits = 2), "+", 
                       format(coef(model)[2], digits = 2), "x +", 
                       format(coef(model)[3], digits = 2), "x^2"),  # Quadratic equation
      R_squared = format(summary(model)$r.squared, digits = 2)
    )
  })

# Print out the quadratic equations and R-squared values
print(models_Treatments)

# Convert the temperature character to numeric for plotting
Day112_df <- Day112_df %>%
  mutate(temperature_num = factor(temperature_num))

# Plot quadratic fits for each treatment
ggplot(Day112_df, aes(x = temperature_num, y = avg_per_min, group = Treatment, shape = Treatment)) +
  geom_point(aes(color = Treatment), size = 3) +  # Points with custom colors
  geom_smooth(aes(color = Treatment), method = "lm", se = FALSE, formula = y ~ poly(x, 2)) +  # Quadratic lines
  scale_color_manual(values = custom_colors) +  # Apply custom colors
  scale_shape_manual(values = custom_shapes) +  # Apply custom shapes
  labs(x = "Temperature (°C)", 
       y = expression("Mineralized N" ~ "[" ~ NO[4] + NO[3] ~ "]" ~ "(%)"), 
       color = "Treatment", linetype = "Treatment") +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

# Calculate the midpoint for each line
Day112_df <- Day112_df %>%
  mutate(temperature_num = as.numeric(gsub("C", "", temperature)))
mid_points <- Day112_df %>%
  group_by(Treatment) %>%
  summarize(mid_temp = mean(temperature_num), 
            mid_avg_per_min = mean(avg_per_min))

# Plot quadratic lines without legend (with labels at the midpoint)
ggplot(Day112_df, aes(x = temperature_num, y = avg_per_min, group = Treatment, shape = Treatment)) +
  geom_point(aes(color = Treatment), size = 3) +  # Points with custom colors
  geom_smooth(aes(color = Treatment), method = "lm", se = FALSE, formula = y ~ poly(x, 2)) +  # Quadratic lines
  scale_color_manual(values = custom_colors) +  # Apply custom colors
  scale_shape_manual(values = custom_shapes) +  # Apply custom shapes
  
  # Add labels at the midpoint of the lines
  geom_text(data = mid_points, aes(x = mid_temp, y = mid_avg_per_min, label = Treatment, color = Treatment), 
            size = 4, fontface = "bold", vjust = -1) +  # Adjust the position with vjust
  
  labs(x = "Temperature (°C)", 
       y = expression("Mineralized N" ~ "[" ~ NO[4] + NO[3] ~ "]" ~ "(%)"), 
       color = "Treatment", 
       linetype = "Treatment") +
  theme_minimal() +
  theme(
    legend.position = "none",  # Remove legend as labels are on the lines
    legend.title = element_text(face = "bold"),
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

ggsave(last_plot(), 
       filename = "Figures/Linear_temp_fig7.png",
       width=8,
       height=5,
       units= "in",
       dpi=300)

#now plot N application rate vs. avg_per_min (day 112 percent Nmin)
#need to merge Treatment_info with Day112_df

Napp_Nmin_df <- Day112_df %>%
  left_join(Treatment_info, by = "Treatment")

# Calculate the linear models for each temperature
models_by_temperature <- Napp_Nmin_df %>%
  group_by(temperature_num) %>%
  do({
    model <- lm(avg_per_min ~ N_app_rate_kg, data = .)
    data.frame(
      Temperature = unique(.$temperature_num),
      Equation = paste("y =", format(coef(model)[1], digits = 2), "+", 
                       format(coef(model)[2], digits = 2), "x"),
      R_squared = format(summary(model)$r.squared, digits = 2)
    )
  })

# Print out the equations and R-squared values
print(models_by_temperature)

#figure 10
ggplot(Napp_Nmin_df, aes(x = N_app_rate_kg, y = avg_per_min, color = temperature_num, linetype = temperature_num)) +
  geom_point() +  # Only apply color to points
  geom_smooth(method = "lm", se = FALSE) +  # Apply color and linetype to lines
  scale_color_manual(
    values = c("10" = "black", "15" = "black", "20" = "black"),
    labels = c("10°C - Solid", "15°C - Dashed", "20°C - Dotted"),
    name = "Temperature (°C)"
  ) +
  scale_linetype_manual(
    values = c("10" = "solid", "15" = "dashed", "20" = "dotted"),
    labels = c("10°C - Solid", "15°C - Dashed", "20°C - Dotted"),
    name = "Temperature (°C)"
  ) +
  labs(x = "Application Rate (N-kg/ha)", y = "Mineralized N [NO3+NH4] (%)") +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 10)
  ) +
  guides(color = guide_legend("Temperature (°C)"), linetype = guide_legend("Temperature (°C)"))


ggsave(last_plot(), 
       filename = "Figures/Linear_Napp_PerN_kg.png",
       width=8,
       height=5,
       units= "in",
       dpi=300)
