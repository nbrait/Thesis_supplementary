################
#Script for Figure 2 in Chapter 1
################

# dependencies
library(ggplot2)
library(dplyr)
library(readr)
library(ggpubr)
library(tidyr)

# Code

year_counts <- read_tsv("year_counts.tsv")  
year_counts <- year_counts %>%
  mutate(Year = as.numeric(Year))

EVE_counts <- year_counts[,-c(2,3)]

EVE_counts <- EVE_counts %>%
  mutate(Year = as.numeric(Year))

# data transformation to long format
EVE_long <- year_counts %>%
  select(Year, assemblies_Eukaryotes, assembly_total, ERV_count, nrEVE_Count, EVE_Count) %>%
  pivot_longer(cols = -Year, names_to = "Category", values_to = "Count")

all_long <- EVE_counts %>%
  select(Year, ERV_count, nrEVE_Count, EVE_Count) %>%
  pivot_longer(cols = -Year, names_to = "Category", values_to = "Count")


# annual Progression
ggplot(EVE_long, aes(x = Year, y = Count, color = Category)) +
  geom_segment(aes(xend = Year, yend = 0), size = 1) +  
  geom_point(data = subset(EVE_long, Category %in% c("ERV_count", "nrEVE_Count", "EVE_Count")),
             aes(shape = Category), size = 3) +  
  geom_point(data = subset(EVE_long, !(Category %in% c("ERV_count", "nrEVE_Count", "EVE_Count"))),
             size = 3) +  
  scale_y_log10(
    breaks = scales::log_breaks(base = 10, n = 8),  
    labels = scales::comma) +
  labs(title = "Annual Progression of Genome Assemblies & EVE Publications",x = "Year", y = "Count (log scale)") +
  scale_color_viridis_d(option = "plasma") +  # Apply viridis plasma color palette
  scale_x_continuous(breaks = seq(min(EVE_long$Year), max(EVE_long$Year), by = 1)) +
  scale_shape_manual(values = c("ERV_count" = 17, "nrEVE_Count" = 17, "EVE_Count" = 17)) +  # Shape 4 for crosses
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),)


# ggplot(EVE_long, aes(x = Year, y = Count, color = Category)) +
#   geom_segment(aes(xend = Year, yend = 0), size = 1) +  
#   geom_point(data = subset(EVE_long, Category %in% c("ERV_count", "nrEVE_Count", "EVE_Count")),
#              aes(shape = Category), size = 3) +  
#   geom_point(data = subset(EVE_long, !(Category %in% c("ERV_count", "nrEVE_Count", "EVE_Count"))),
#              size = 3) +  
#   # scale_y_log10(
#   #   breaks = scales::log_breaks(base = 10, n = 8),  
#   #   labels = scales::comma  
#   # ) +
#   labs(title = "Annual Progression of Genome Assemblies & EVE Mentions",x = "Year", y = "Count (log scale)") +
#   scale_color_viridis_d(option = "plasma") +
#   scale_x_continuous(breaks = seq(min(EVE_long$Year), max(EVE_long$Year), by = 1)) +
#   scale_shape_manual(values = c("ERV_count" = 17, "nrEVE_Count" = 17, "EVE_Count" = 17)) +  
#   theme_minimal() +  
#   theme(axis.text.x = element_text(angle = 90, hjust = 1),)
# 
# ggplot(all_long, aes(x = Year, y = Count, color = Category)) +
#   geom_segment(aes(xend = Year, yend = 0), size = 1) +  
#   geom_point(data = subset(all_long, Category %in% c("ERV_count", "nrEVE_Count", "EVE_Count")),
#              aes(shape = Category), size = 3) +  
#   geom_point(data = subset(all_long, !(Category %in% c("ERV_count", "nrEVE_Count", "EVE_Count"))),
#              size = 3) +  
#    scale_y_log10(
#      breaks = scales::log_breaks(base = 10, n = 20),  
#      labels = scales::comma  
#    ) +
#   labs(title = "Annual Progression of Genome Assemblies & EVE Mentions",
#        x = "Year", y = "Count (log scale)") +
#   scale_color_viridis_d(option = "plasma") +  
#   scale_x_continuous(breaks = seq(min(EVE_long$Year), max(EVE_long$Year), by = 1)) +
#   scale_shape_manual(values = c("ERV_count" = 17, "nrEVE_Count" = 17, "EVE_Count" = 17)) +  # Shape 4 for crosses
#   theme_minimal() +  
#   theme(axis.text.x = element_text(angle = 90, hjust = 1), )

##########################

counts_cumulative <- year_counts %>%
  arrange(Year) %>%  
  mutate(across(-Year, cumsum)) 

EVE_long_cumulative <- counts_cumulative %>%
  pivot_longer(cols = -Year, names_to = "Category", values_to = "Cumulative_Count")

# Cumulative progression
ggplot(EVE_long_cumulative, aes(x = Year, y = Cumulative_Count, color = Category)) +
  geom_line(size = 1) +  
  geom_point(data = subset(EVE_long_cumulative, Category %in% c("ERV_count", "nrEVE_Count", "EVE_Count")),
             aes(shape = Category), size = 3) +  
  geom_point(data = subset(EVE_long_cumulative, !(Category %in% c("ERV_count", "nrEVE_Count", "EVE_Count"))),
             size = 3) +    
  scale_y_log10(
    breaks = scales::log_breaks(base = 10, n = 10),  
    labels = scales::comma) +
  scale_x_continuous(breaks = seq(min(EVE_long_cumulative$Year), max(EVE_long_cumulative$Year), by = 1)) + 
  scale_color_viridis_d(option = "plasma") +  
  scale_shape_manual(values = c("ERV_count" = 17, "nrEVE_Count" = 17, "EVE_Count" = 17)) +  
  labs(title = "Cumulative Growth of Genome Assemblies & EVE Publications",
       x = "Year", y = "Cumulative Count (log scale)") +
  theme_minimal() +  
  theme(axis.text.x = element_text(angle = 90, hjust = 1),)


