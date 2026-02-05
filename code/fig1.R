# ==============================================================================
# Script Name: Figure 1 - Rank Bump Chart of Infectious Diseases
# Project: The changing landscape of major infectious diseases in China, 2004–2023
# Description: Generates a bump chart to visualize the ranking changes of Class B NIDs.
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. Environment Setup and Dependency Loading
# ------------------------------------------------------------------------------
rm(list = ls()) # Clear environment

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(ggbump)
library(ggtext)
library(tidyverse)
library(sf)
library(ggspatial)
library(RColorBrewer)
library(sjmisc)
library(readxl)
library(cowplot)
library(classInt)
library(patchwork)

# --- [IMPORTANT] Set Working Directory and File Paths ---
# Adjust 'work_dir' to match your local machine environment
work_dir <- "****"
input_file <- file.path(work_dir, "*****/all_data.csv")
output_file <- file.path(work_dir, "*****/Figure1_Rank_Change.tiff")

# Check if output directory exists, create if not
if(!dir.exists(dirname(output_file))) dir.create(dirname(output_file), recursive = TRUE)

# ------------------------------------------------------------------------------
# 2. Data Loading and Preprocessing
# ------------------------------------------------------------------------------
infection_data <- read.csv(input_file)

# Filter years, exclude specific diseases, format rates, and categorize transmission routes
rank_data <- infection_data %>% 
  select(metric2, time, incidence, death, incidence_rate, death_rate) %>% 
  filter(
    time %in% c("2004", "2019", "2023"),
    !metric2 %in% c(
      "Hepatitis (Unspecified)", "Hepatitis D", "Human Avian Influenza",
      "Poliomyelitis", "Diphtheria", "SARS", "Amebic Dysentery",
      "Bacillary Dysentery", "Typhoid Fever", "Paratyphoid Fever"
    )
  ) %>% 
  arrange(time, metric2) %>% 
  mutate(across(c(incidence_rate, death_rate), ~round(., 2))) %>% 
  mutate(
    transmission_group = case_when(
      metric2 %in% c("Pulmonary Tuberculosis", "Measles", "Pertussis", "Scarlet Fever", 
                     "Meningococcal Meningitis", "Human Avian Influenza", "Human H7N9 Avian Influenza", 
                     "SARS", "Diphtheria", "Influenza") ~ "Respiratory",
      
      metric2 %in% c("Hepatitis A", "Hepatitis E", "Typhoid Fever", "Paratyphoid Fever", 
                     "Typhoid and Paratyphoid Fever", "Dysentery", "Bacillary Dysentery", 
                     "Amebic Dysentery", "Poliomyelitis") ~ "Fecal-Oral",
      
      metric2 %in% c("AIDS", "Syphilis", "Gonorrhea", "Hepatitis B", "Hepatitis C", 
                     "Hepatitis D", "Viral Hepatitis", "Hepatitis (Unspecified)", "Hepatitis") ~ "Sexual and Blood-borne",
      
      metric2 %in% c("Dengue Fever", "Malaria", "Japanese Encephalitis") ~ "Vector-borne",
      
      metric2 %in% c("Brucellosis", "Rabies", "Hemorrhagic Fever", "Anthrax") ~ "Zoonotic",
      
      metric2 %in% c("Schistosomiasis", "Leptospirosis", "Neonatal Tetanus") ~ "Other Transmission",
      
      TRUE ~ "Uncategorized"
    )
  )

# Convert to Long Format: Calculate rank for each year
rank_data_long <- rank_data %>% 
  group_by(time) %>%
  arrange(desc(incidence_rate)) %>%
  mutate(rank = row_number()) %>%
  ungroup()

# Convert to Wide Format: For cross-year calculations
rank_data_wider <- rank_data_long %>%
  pivot_wider(
    id_cols = c(metric2, transmission_group),
    names_from = c(time),
    values_from = c(incidence, death, incidence_rate, death_rate, rank)
  ) 

# ------------------------------------------------------------------------------
# 3. Calculation of Percentage Changes and Rank Shifts
# ------------------------------------------------------------------------------
change_data_calculated <- rank_data_wider %>%
  mutate(
    # --- Calculate 2004-2019 Change (Handling zero denominators) ---
    pct_change_incidence_2019 = case_when(
      incidence_2004 == 0 & incidence_2019 == 0 ~ 0,
      incidence_2004 == 0 & incidence_2019 > 0  ~ 100,
      TRUE ~ ((incidence_2019 - incidence_2004) / incidence_2004) * 100
    ),
    
    # --- Calculate 2019-2023 Change ---
    pct_change_incidence_2023 = case_when(
      incidence_2019 == 0 & incidence_2023 > 0 ~ 100,
      incidence_2019 == 0 ~ 0,
      TRUE ~ ((incidence_2023 - incidence_2019) / incidence_2019) * 100
    ),
    
    # --- Calculate 2004-2023 Overall Change ---
    pct_change_incidence_rate_2023_2004 = case_when(
      incidence_rate_2004 == 0 & incidence_rate_2023 > 0 ~ 100,
      incidence_rate_2004 == 0 ~ 0,
      TRUE ~ ((incidence_rate_2023 - incidence_rate_2004) / incidence_rate_2004) * 100
    )
  ) %>%
  mutate(
    # Define Line Types based on Rank Shift
    rank_change_type_2019 = case_when(
      rank_2019 < rank_2004 ~ "Rank Increased (Solid)", # Higher rank (smaller number)
      rank_2019 > rank_2004 ~ "Rank Decreased (Dashed)",
      TRUE ~ "Rank Stable (Dashed)" 
    ),
    rank_change_type_2023 = case_when(
      rank_2023 < rank_2019 ~ "Rank Increased (Solid)",
      rank_2023 > rank_2019 ~ "Rank Decreased (Dashed)",
      TRUE ~ "Rank Stable (Dashed)" 
    )
  )

# ------------------------------------------------------------------------------
# 4. Plotting Preparation: Coordinates, Colors, and Helper Data
# ------------------------------------------------------------------------------

# 4.1 Define Color Palette and Line Types
my_color_palette <- c(
  "Sexual and Blood-borne" = "#F8766D", 
  "Fecal-Oral"             = "#7CAE00",    
  "Vector-borne"           = "#00BFC4",             
  "Respiratory"            = "#C77CFF",              
  "Zoonotic"               = "#FFB600",       
  "Other Transmission"     = "#1F77a1"
)

# Note: Adjust linetypes here if needed (e.g., solid vs dashed)
my_linetype_palette <- c(
  "Rank Increased (Solid)"  = "solid",
  "Rank Decreased (Dashed)" = "dashed",
  "Rank Stable (Dashed)"    = "dashed"
)

# 4.2 Define X-axis positions (Controls spacing between columns)
pos_2004  <- 0      # Start of 2004
pos_2019  <- 5      # Start of 2019
pos_pct_1 <- 14.4   # 2004-2019 Change column
pos_2023  <- 20     # Start of 2023
pos_pct_2 <- 26.3   # 2019-2023 Change column
pos_pct_3 <- 29.55  # 2004-2023 Overall Change column

# 4.3 Construct Plot Data
# Add X position to main data
ranked_data_final <- rank_data_long %>%
  mutate(
    x_pos = case_when(
      time == 2004 ~ pos_2004,
      time == 2019 ~ pos_2019,
      time == 2023 ~ pos_2023
    )
  )

# Segment Data: 2004-2019
segment_04_19 <- change_data_calculated %>%
  select(metric2, transmission_group, rank_2004, rank_2019, rank_change_type_2019) %>%
  mutate(x_start = pos_2004, x_end = pos_2019)

# Segment Data: 2019-2023
segment_19_23 <- change_data_calculated %>%
  select(metric2, transmission_group, rank_2019, rank_2023, rank_change_type_2023) %>%
  mutate(x_start = pos_pct_1, x_end = pos_2023)

# Column Headers Data
title_data <- data.frame(
  x = c(pos_2004, pos_2019, pos_pct_1, pos_2023, pos_pct_2, pos_pct_3),
  y = -1, # Negative y places title above Rank 1
  label = c(
    "Disease 2004", 
    "Disease 2019",
    "Percentage change in\nincidence per 100000\n(2004-2019)", 
    "Disease 2023",
    "Percentage change in\nincidence per 100000\n(2019-2023)",
    "Percentage change in\nincidence per 100000\n(2004-2023)"
  ),
  hjust_val = c(1, 0, 1, 0, 0, 0),
  x_nudge   = c(0, 0, 0, 0, 0, 1)
)

# ------------------------------------------------------------------------------
# 5. Generate Plot (ggplot2)
# ------------------------------------------------------------------------------
p_final <- ggplot() +
  
  # --- Connection Lines ---
  geom_segment(data = segment_04_19,
               aes(x = x_start, xend = x_end, y = rank_2004, yend = rank_2019,
                   color = transmission_group, linetype = rank_change_type_2019),
               size = 0.8, alpha = 0.7) +
  
  geom_segment(data = segment_19_23,
               aes(x = x_start, xend = x_end, y = rank_2019, yend = rank_2023,
                   color = transmission_group, linetype = rank_change_type_2023),
               size = 0.8, alpha = 0.7) +
  
  # --- 2004 Labels ---
  geom_textbox(
    data = ranked_data_final %>% filter(time == 2004),
    aes(x = x_pos, y = rank, label = paste0(rank, "\\. ", metric2, " (", round(incidence_rate, 2), ")"), fill = transmission_group),
    hjust = 1, width = unit(6, "cm"), height = unit(0.8, "cm"), size = 3.2, 
    color = "black", box.size = 0.2, box.r = unit(0, "pt"), halign = 0, valign = 0.5
  ) +
  
  # --- 2019 Labels ---
  geom_textbox(
    data = ranked_data_final %>% filter(time == 2019),
    aes(x = x_pos, y = rank, label = paste0(rank, "\\. ", metric2, " (", round(incidence_rate, 2), ")"), fill = transmission_group),
    hjust = 0, width = unit(6, "cm"), height = unit(0.8, "cm"), size = 3.2, 
    color = "black", box.size = 0.2, box.r = unit(0, "pt"), halign = 0, valign = 0.5
  ) +
  
  # --- 2023 Labels ---
  geom_textbox(
    data = ranked_data_final %>% filter(time == 2023),
    aes(x = x_pos, y = rank, label = paste0(rank, "\\. ", metric2, " (", round(incidence_rate, 2), ")"), fill = transmission_group),
    hjust = 0, width = unit(6, "cm"), height = unit(0.8, "cm"), size = 3.2, 
    color = "black", box.size = 0.2, box.r = unit(0, "pt"), halign = 0, valign = 0.5
  ) +
  
  # --- Change Rate Column 1 (2004-2019) ---
  geom_textbox(
    data = change_data_calculated,
    aes(x = pos_pct_1, y = rank_2019, label = paste0(round(pct_change_incidence_2019, 1), "%"), fill = transmission_group),
    hjust = 1, width = unit(3, "cm"), height = unit(0.8, "cm"), size = 3, 
    color = "black", box.size = 0.2, box.r = unit(0, "pt"), halign = 0.5, valign = 0.5
  ) +
  
  # --- Change Rate Column 2 (2019-2023) ---
  geom_textbox(
    data = change_data_calculated,
    aes(x = pos_pct_2, y = rank_2023, label = paste0(round(pct_change_incidence_2023, 1), "%"), fill = transmission_group),
    hjust = 0, width = unit(3, "cm"), height = unit(0.8, "cm"), size = 3, 
    color = "black", box.size = 0.2, box.r = unit(0, "pt"), halign = 0.5, valign = 0.5
  ) +
  
  # --- Change Rate Column 3 (2004-2023) ---
  geom_textbox(
    data = change_data_calculated,
    aes(x = pos_pct_3, y = rank_2023, label = paste0(round(pct_change_incidence_rate_2023_2004, 1), "%"), fill = transmission_group),
    hjust = 0, width = unit(3, "cm"), height = unit(0.8, "cm"), size = 3, 
    color = "black", box.size = 0.2, box.r = unit(0, "pt"), halign = 0.5, valign = 0.5
  ) +
  
  # --- Titles and Theme ---
  geom_text(data = title_data, aes(x = x, y = y, label = label, hjust = hjust_val, nudge_x = x_nudge), fontface = "bold", size = 4) +
  scale_y_reverse(breaks = 1:max(rank_data_long$rank)) + 
  scale_color_manual(values = my_color_palette) +
  scale_fill_manual(values = scales::alpha(my_color_palette, 0.6)) +
  scale_linetype_manual(values = my_linetype_palette, guide = "none") +
  coord_cartesian(clip = "off") + 
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    plot.margin = margin(2, 4, 1, 8, "cm"),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  ) +
  guides(
    fill = guide_legend(title = "", nrow = 1, byrow = TRUE),
    color = guide_legend(title = "", nrow = 1, byrow = TRUE)
  )

# ------------------------------------------------------------------------------
# 6. Output Saving
# ------------------------------------------------------------------------------
print("Saving figure...")
ggsave(output_file, plot = p_final, width = 17.7, height = 12, dpi = 300)
print(paste("Figure saved to:", output_file))
