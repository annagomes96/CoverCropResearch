#script for analyzing cover crop lab incubation data, still current in June 2024

#install.packages("writexl")
#install.packages("multcomp")
#install.packages("agricolae")

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
Initial_Nitrogen <- read.csv ("Data/Percent_min_updated.csv")

# Create DF with treatments, CN, percent N, and N application rate
Treatment <- c("CN10", "CN14", "CN19", "CN30")
CN_ratio <- c("10:1", "14:1", "19:1", "30:1")
N_content <- c(4.11, 2.99, 2.28, 1.51)
N_app_rate <- c(8.96, 6.52, 4.97, 3.29)
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
    Treatment == "T1" ~ "Soil Control (No CC)",
    Treatment == "T2" ~ "CN10",
    Treatment == "T3" ~ "CN14",
    Treatment == "T4" ~ "CN19",
    Treatment == "T5" ~ "CN30",
    Treatment == "T6"~ "CN30+RN",
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
custom_colors <- c("CN10" ="#90b76e","CN14"= "#ef5f00","CN19"= "#3399FF","CN30"= "#D4C95F","CN30+RN" ="#cad9f4","Soil Control (No CC)" = "#682714")

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

#not including 
PAN_avg %>%
  #filter(day_numeric<29)%>%
  ggplot(aes(x=day_numeric, y=avg, group=Treatment, color=Treatment)) +
  geom_point() +
  geom_line(linetype= "solid") +
  facet_wrap(~temperature) +
  scale_x_continuous(breaks = PAN_avg$day_numeric) +  # Set x-axis breaks at the days of data
  geom_errorbar(aes(ymin=avg-se, ymax=avg+se), width=.2, position=position_dodge(0.05)) +
  labs(x="Day of Incubation", y= "NH4+ & NO3- (ug Nitrogen/g of dry soil)") +
  ggtitle("Nitrogen Mineralization of Cover Crop Residue") +
  scale_color_manual(values=custom_colors) +
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

#not inlcuding 
PAN_avg_df %>%
  ggplot(aes(x=day_numeric, y=avg, group=Treatment, color=Treatment))+
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = Treatment), alpha = 0.2) +
  geom_point()+
  geom_line(linetype= "solid") +
  facet_wrap(~temperature)+
  scale_x_continuous(breaks = PAN_avg_df$day_numeric) +  # Set x-axis breaks at the days of data
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

#not including
#facet wrap by treatment
PAN_avg_df %>%
  ggplot(aes(x=day_numeric, y=avg, group=temperature, color=temperature))+
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.2) +
  geom_point()+
  geom_line(linetype= "solid") +
  facet_wrap(~Treatment, scale="free")+
  scale_x_continuous(breaks = PAN_avg_df$day_numeric) +  # Set x-axis breaks at the days of data
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

# Figure 3
NH4_avg %>%
  ggplot(aes(x=day_numeric, y=avg, group=Treatment, color=Treatment)) +
  geom_point() +
  geom_line(linetype= "solid") +
  facet_wrap(~temperature, labeller = temperature_labeller) + # Correctly use the labeller function
  scale_x_continuous(breaks = NH4_avg$day_numeric) +  # Set x-axis breaks at the days of data
  geom_errorbar(aes(ymin=avg-se, ymax=avg+se), width=.2, position=position_dodge(0.05)) +
  labs(x="Incubation Duration (d)", y= "NH4-Nitrogen (ug/g)") +
  scale_color_manual(values=custom_colors) +
  #geom_text(data = highest_avg_NH4, aes(label=round(avg)), vjust=-0.5, hjust=-0.1, color="black") + # Only highest avg values shown
  theme_bw() +  # Apply base theme first
  theme(
    strip.text = element_text(size = 14, face = "bold"),  # Customize facet label size and style
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

ggsave(last_plot(), 
       filename = "Figures/dynamics_Ammonium_N_min_errorbars.png",
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

#only nitrate, figure 4
NO3_avg %>%
  ggplot(aes(x=day_numeric, y=avg, group=Treatment, color=Treatment)) +
  geom_point() +
  geom_line(linetype= "solid") +
  facet_wrap(~temperature, labeller = temperature_labeller) + # Correctly use the labeller function
  scale_x_continuous(breaks = NO3_avg$day_numeric) +  # Set x-axis breaks at the days of data
  geom_errorbar(aes(ymin=avg-se, ymax=avg+se), width=.2, position=position_dodge(0.05)) +
  labs(x="Incubation Duration (d)", y= "NO3-Nitrogen (ug/g)") +
  scale_color_manual(values=custom_colors) +
  #geom_text(data = highest_avg_NO3, aes(label=round(avg, 2)), vjust=-0.5, hjust=1, color="black") + # Only highest avg values shown
  theme_bw() +  # Apply base theme first
  theme(
    strip.text = element_text(size = 14, face = "bold"),  # Customize facet label size and style
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

ggsave(last_plot(), 
       filename = "Figures/dynamics_Nitrate_N_min_errorbars.png",
       width=8,
       height=5,
       units= "in",
       dpi=300)


#only soil - control, figure 13 
PAN_avg %>%
  filter(Treatment=="Soil Control (No CC)") %>% 
  ggplot(aes(x=day_numeric, y=avg, group= temperature))+
  #geom_point(aes(shape = temperature), size = 3) +  # Different shapes for each temperature
  geom_line(aes(linetype= temperature), color="black") +
  scale_x_continuous(breaks = PAN_avg$day_numeric) +  # Set x-axis breaks at the days of data
  scale_linetype_manual(values = c("10C" = "solid", "15C" = "dashed", "20C" = "dotted")) +  # Customize linetypes
  geom_errorbar(aes(ymin=avg-se, ymax= avg+se), width=.2, position=position_dodge(0.05))+
  labs(x="Incubation Duration (d)", y= "NO3-Nitrogen & NH4-Nitrogen (ug/g)") +
  #ggtitle("Nitrogen Mineralization of Unamended Soil Control")+
  labs(linetype = "Temperature") +
  #scale_color_manual(values = c("10C" = "black", "15C" = "yellow", "20C" = "orangered")) +
  theme_bw() +  # Apply base theme first
  theme(
    strip.text = element_text(size = 14, face = "bold"),  # Customize facet label size and style
    # Add any other theme customizations here
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

ggsave(last_plot(), 
       filename = "Figures/dynamics_Nitrogen_soilonly.png",
       width=8,
       height=6,
       units= "in",
       dpi=300)

#percent mineralized: (average for Soil Only then at end (%min))
Soil_Only <- all_data_tidy %>%
  filter(Treatment=="Soil Control (No CC)") %>%
  group_by(temperature, day_numeric) %>%
  summarise(avg=mean(PAN), sd=sd(PAN), n=n()) 

Net_Mineralized <- all_data_tidy %>%
left_join(Soil_Only, by= c("day_numeric", "temperature")) %>%
  mutate(net_min = PAN - avg)

#filter out 'Soil Control' since now have Net Mineralized
Net_Mineralized<- Net_Mineralized %>%
  filter(Treatment!="Soil Control (No CC)")

#subtract out mineral N added to T6 before calc %N Mineralized 
Net_Mineralized <- Net_Mineralized %>%
  mutate(net_min = if_else(Treatment == "CN30+RN", net_min - 9.256, net_min))

#now calc Net N min per tube (250grams)
Net_Mineralized <- Net_Mineralized %>%
  group_by(temperature, Treatment, day_numeric, r_numeric)%>%
  mutate(Nmin_per_tube= (net_min * 250))

Avg_Net_Min <- Net_Mineralized %>%
  group_by(temperature, Treatment, day_numeric) %>%
  summarise(avg=mean(net_min), sd=sd(net_min), n=n()) %>%
  mutate(se=sd/sqrt(n))

# Assuming you want to remove a treatment to keep it out of the plot labels 
Avg_Net_Min_T6 <- Avg_Net_Min %>%
  filter(Treatment != "CN30+RN")


#plot Net N Mineralized without T6, Fig 5
Avg_Net_Min_T6 %>%
  ggplot(aes(x=day_numeric, y=avg, group= Treatment, color=Treatment))+
  geom_point()+
  geom_line(linetype= "solid") +
  facet_wrap(~temperature, labeller = temperature_labeller) + 
  geom_errorbar(aes(ymin=avg-se, ymax= avg+se), width=.2, position=position_dodge(0.05))+
  labs(x="Incubation Duration (d)", y= "NO3-N & NH4-N (ug/g)") +
  #ggtitle("Net Nitrogen Mineralization of Cover Crop Residue")+
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred", linewidth=1) +
  scale_color_manual(values=custom_colors, guide = "none") +  # Remove the legend
  geom_text(aes(label=Treatment, y = avg), vjust = -1, hjust = 1, data = subset(Avg_Net_Min_T6, day_numeric == max(day_numeric))) + # Add labels at the last point
  scale_x_continuous(breaks = Avg_Net_Min_T6$day_numeric) +  # Set x-axis breaks at the days of data
  theme_bw() +  # Apply base theme first
  theme(
    strip.text = element_text(size = 14, face = "bold"),  # Customize facet label size and style
    # Add any other theme customizations here
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

ggsave(last_plot(), 
       filename = "Figures/Net_Nitrogen_SE.png",
       width=8,
       height=5,
       units= "in",
       dpi=300)

#calculate percent nitrification (nitrate-N/ total PAN for each sampling date THEN average)
#plot again with percent nitrate on the data points..as labels 
  
    
#now calc percent mineralized (Net_min_tube_gStart_ON_per tube_g)
Initial_Nitrogen <- Initial_Nitrogen %>%
  mutate(Treatment = gsub("CC", "CN", Treatment))

Percent_Mineralized <- Net_Mineralized %>% 
  left_join(Initial_Nitrogen, by= ("Treatment")) %>%
  mutate(min = (Nmin_per_tube/start_ON_ug_g)) %>%
  mutate(per_min = min*100) #turn into a percentage

#now average the percent min between the four replicates
Percent_Mineralized_avg <- Percent_Mineralized %>%
  group_by(temperature, Treatment, day_numeric) %>%
  summarise(avg_per_min=mean(per_min), sd=sd(per_min), n=n()) %>% 
  mutate(se=sd/sqrt(n))  

#not including
#plot avg percent min
Percent_Mineralized_avg %>%
  filter (Treatment != "CN30+RN") %>% 
  ggplot(aes(x=day_numeric, y=avg_per_min, group= Treatment, color=Treatment))+
  geom_point()+
  geom_line(linetype= "solid") +
  facet_wrap(~temperature)+
  geom_errorbar(aes(ymin=avg_per_min-se, ymax= avg_per_min+se), width=.2, position=position_dodge(0.05))+
  labs(x="Day of Incubation", y= "% Nitrogen Mineralized")+
  ggtitle("Percent Nitrogen Mineralized")+
  scale_color_manual(values=custom_colors)+
  scale_x_continuous(breaks = Percent_Mineralized_avg$day_numeric) +  # Set x-axis breaks at the days of data
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

#figure 8
Percent_Mineralized_CI %>%
  filter (Treatment != "CN30+RN") %>%
  ggplot(aes(x = day_numeric, y = mean_per_min, group = Treatment, color = Treatment)) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = Treatment), alpha = 0.1, color = NA) +
  facet_wrap(~temperature, labeller = temperature_labeller) +
  labs(x = "Incubation Period (d)", y = "Mineralized N [NO4+NO3] (%)") +
  #ggtitle("Mineralization (%) of Cover Crop Residue Nitrogen") +
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +  # Ensure fill colors match line colors
  scale_x_continuous(breaks = unique(Percent_Mineralized_CI$day_numeric)) +  # Set x-axis breaks at the days of data
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred", linewidth = 1) +
  theme_bw() +  # Apply base theme first
  theme(
    strip.text = element_text(size = 14, face = "bold"),  # Customize facet label size and style
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

ggsave(last_plot(), 
       filename = "Figures/N_min_CI.png",
       width=8,
       height=5,
       units= "in",
       dpi=300)

#plot avg percent min T5 vs.s T6
#figure 12
Percent_Mineralized_avg %>%
  filter (Treatment == "CN30" | Treatment == "CN30+RN" ) %>% 
  ggplot(aes(x=day_numeric, y=avg_per_min, group= Treatment, color=Treatment)) +
  geom_point() +
  geom_line(linetype= "solid") +
  facet_wrap(~temperature, labeller = temperature_labeller) +
  scale_x_continuous(breaks = unique(Percent_Mineralized_avg$day_numeric)) +  # Set x-axis breaks at the days of data
  geom_errorbar(aes(ymin=avg_per_min-se, ymax= avg_per_min+se), width=.2, position=position_dodge(0.05))+
  labs(x = "Incubation Period (d)", y = "Mineralized N [NO4+NO3] (%)") +
  #ggtitle("Percent Nitrogen Mineralized, CN30 with Doubled Residual Soil N")+
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
       filename = "Figures/N_min_T6_EB.png",
       width=8,
       height=5,
       units= "in",
       dpi=300)

#START HERE
#ANOVA to compare b/w treatments, all reps in three temps 
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
  filter (Treatment != "CN30+RN")
print(Tk)

#figure 7
Percent_Mineralized %>%
  filter (Treatment != "CN30+RN") %>% 
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
       filename = "Figures/N_min_box_tukey.png",
       width=8,
       height=5,
       units= "in",
       dpi=300)


# C:N Ratio or percent N vs. % Mineralized (Day 112)
Day112_df <- Percent_Mineralized_avg %>%
  filter(day_numeric == 112, Treatment != "CN30+RN")

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

# Now use this updated models data frame in your plot, Figure 9
ggplot(Day112_df, aes(x = CN, y = avg_per_min)) +
  geom_point(color = "black") +  # Make all points black
  geom_smooth(aes(linetype = temperature_num), method = "lm", se = FALSE, formula = y ~ x, color = "black") +  # Make all lines black with different linetypes
  #geom_text(data = models, aes(label = label, x = CN, y = avg_per_min), parse = TRUE, hjust = 0.5, vjust = 1) +
  labs(x = "C:N Ratio", y = "Day 112 Mineralized N [NO4+NO3] (%)") +
  scale_linetype_manual(values = c("10" = "solid", "15" = "dashed", "20" = "dotted"), name = "Temperature (°C)") +
  theme_bw() +
  theme(
    strip.text = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

print(models)

ggsave(last_plot(), 
       filename = "Figures/Linear_CN_PerN.png",
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

#figure 11
ggplot(Day112_df, aes(x = temperature_num, y = avg_per_min, group = Treatment)) +
  geom_point(aes(color = Treatment)) +  # Points with custom colors
  geom_smooth(aes(color = Treatment), method = "lm", se = FALSE, formula = y ~ x) +  # Lines with custom colors
  scale_color_manual(values = custom_colors) +  # Apply custom colors
  labs(x = "Temperature (°C)", y = "Day 112 Mineralized N [NO3+NH4] (%)", color = "Treatment", linetype = "Treatment") +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

ggsave(last_plot(), 
       filename = "Figures/Linear_temp_PerN.png",
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
  labs(x = "Application Rate (kgN/ha)", y = "Day 112 Mineralized N [NO3+NH4] (%)") +
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
