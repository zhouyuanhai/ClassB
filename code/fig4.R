# ==============================================================================
# Script Name: 05_figure4_polar.R
# Description: Generation of age-specific polar bar charts (Figure 4) 
#              comparing Prevalence and DALYs between 2004 and 2019.
#              (Strictly adheres to original manuscript logic for reproducibility)
# Author: Yuan-Hai Zhou & Yu-Hong Qin
# ==============================================================================

# 1. Environment Setup ----------------------------------------------------
rm(list = ls())

# Load necessary libraries
library(tidyverse)
library(data.table)
library(patchwork)
library(cowplot) # Required for legend extraction
library(grid)

# Define project paths (Relative paths for reproducibility)
DATA_PATH <- "*****/IBD_china.csv"
OUT_DIR   <- "*****/Figure2"

# Create output directory if it does not exist
if(!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

# 2. Data Loading and Preprocessing ---------------------------------------
message("Loading GBD dataset...")

if(!file.exists(DATA_PATH)){
  stop(paste0("Error: Data file not found at '", DATA_PATH, "'. Please verify the path."))
}

china_IBD <- fread(DATA_PATH)

# Variable selection and renaming
china_IBD <- china_IBD %>% select(cause_name,
                                  measure_name,location_name,
                                  metric_name,sex_name,
                                  age_name,year,
                                  val,lower,upper) %>% 
  rename(measure=measure_name,
         location=location_name,
         metric=metric_name,
         sex=sex_name,
         age=age_name,
         cause=cause_name)

age1 <- c("0-14 years","15-49 years","50-69 years","70+ years")  


# 3. Data Filtering and Standardization -----------------------------------

# Filter dataset based on inclusion criteria

infication_GBD <- china_IBD %>% 
  filter(
    cause %in% c("Acute hepatitis A" ,"Acute hepatitis B", "Acute hepatitis C","Acute hepatitis E",
                 "HIV/AIDS","Measles" ,"Rabies","Dengue","Typhoid fever","Paratyphoid fever","Pertussis",
                 "Diphtheria","Gonococcal infection","Syphilis","Malaria") &
      metric == "Rate" &
      measure %in% c("Prevalence","DALYs (Disability-Adjusted Life Years)"),
    age %in% age1 &
      year %in% c("2004","2019")&
      sex=="Both"
  )

# Standardize disease names (Mapping GBD nomenclature to manuscript terminology)
disease_map <- tibble(
  cause=c("Acute hepatitis A" ,"Acute hepatitis B", "Acute hepatitis C","Acute hepatitis E",
          "HIV/AIDS","Measles" ,"Rabies","Dengue","Typhoid fever","Paratyphoid fever","Pertussis",
          "Diphtheria","Gonococcal infection","Syphilis","Malaria"),
  cause2=c(
    "Hepatitis A","Hepatitis B","Hepatitis C","Hepatitis E",
    "AIDS","Measles","Rabies","Dengue Fever","Typhoid Fever","Paratyphoid Fever","Pertussis",
    "Diphtheria","Gonorrhea","Syphilis","Malaria"
  )
)

infication_GBD <- infication_GBD %>% left_join(disease_map,join_by(cause))

# 4. Plot Sequence Definition (Original Logic) ----------------------------
# Note: The 'case_when' logic below is preserved exactly from the original 
# analysis to ensure identical plot ordering (p1 to p16).

data_map <- infication_GBD %>% 
  mutate(
    plot_order = case_when(
      # Row 1: Prevalence 2004
      age == "0-14 years"  & measure == "Prevalence" & year == "2004" ~ "p1",
      age == "15-49 years" & measure == "Prevalence" & year == "2004" ~ "p2",
      age == "50-69 years" & measure == "Prevalence" & year == "2004" ~ "p3",
      age == "70+ years"   & measure == "Prevalence" & year == "2004" ~ "p4",
      
      # Row 2: Prevalence 2019
      age == "0-14 years"  & measure == "Prevalence" & year == "2019" ~ "p5",
      age == "15-49 years" & measure == "Prevalence" & year == "2019" ~ "p6",
      age == "50-69 years" & measure == "Prevalence" & year == "2019" ~ "p7",
      age == "70+ years"   & measure == "Prevalence" & year == "2019" ~ "p8",
      
      # Row 3: DALYs 2004
      age == "0-14 years"  & measure == "DALYs (Disability-Adjusted Life Years)" & year == "2004" ~ "p9",
      age == "15-49 years" & measure == "DALYs (Disability-Adjusted Life Years)" & year == "2004" ~ "p10",
      age == "50-69 years" & measure == "DALYs (Disability-Adjusted Life Years)" & year == "2004" ~ "p11",
      age == "70+ years"   & measure == "DALYs (Disability-Adjusted Life Years)" & year == "2004" ~ "p12",
      
      # Row 4: DALYs 2019
      age == "0-14 years"  & measure == "DALYs (Disability-Adjusted Life Years)" & year == "2019" ~ "p13",
      age == "15-49 years" & measure == "DALYs (Disability-Adjusted Life Years)" & year == "2019" ~ "p14",
      age == "50-69 years" & measure == "DALYs (Disability-Adjusted Life Years)" & year == "2019" ~ "p15",
      age == "70+ years"   & measure == "DALYs (Disability-Adjusted Life Years)" & year == "2019" ~ "p16"
    )
  )

# Define color palette
my_colors <- c(
  "AIDS" = "#0c77b8", "Gonorrhea" = "#1ab7c1", "Hepatitis B" = "#c775ab",
  "Hepatitis A" = "#36a048", "Syphilis" = "#f58030", "Hepatitis C" = "#690997",
  "Hepatitis E" = "#8e5153", "Measles" = "#86a2ca", "Rabies" = "#97243d",
  "Dengue Fever" = "#aab33e", "Typhoid Fever" = "#6a6698", "Paratyphoid Fever" = "#f9c3bf",
  "Pertussis" = "#e92a2a", "Diphtheria" = "#86a2ca", "Malaria" = "#0aa17e"
)

# Format numerical values
stat_data <- data_map %>% 
  arrange(age, measure, desc(val)) %>% 
  select(cause, measure, age, val, lower, upper) %>%  
  mutate(across(where(is.numeric), ~round(., 2)))

# 5. Visualization Generation (Loop) --------------------------------------
message("Generating polar bar charts...")

plot_list <- list()
plot_ids <- paste0("p", 1:16)

for (id in plot_ids) {
  current_data <- data_map %>% filter(plot_order == id)
  
  current_data <- current_data %>% 
    mutate(cause2=fct_reorder(cause2,val,.fun = max,.desc = TRUE))
  
  p <- ggplot()+
    geom_col(data = current_data,
             aes(x = cause2, y=val,fill = cause2)) +
    
    coord_polar(theta = "x", start = 0) +
    scale_y_continuous(labels = scales::label_comma()) +
    scale_fill_manual(values = my_colors) + 
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),  
      axis.ticks.x = element_blank(), 
      axis.title.x = element_blank(),  
      axis.title.y = element_blank(),
      axis.text.y = element_text(size = 12, color = "black"), 
      axis.ticks.y = element_line(color ="#7F7F7F"),        
      # axis.line.y = element_line(color ="#7F7F7F"),          
      
      panel.grid.major.y = element_line(color = "grey80", linewidth = 0.65),
      
      panel.grid.major.x = element_line(color = "grey80", linewidth = 0.65),
      legend.position = "bottom",             
      legend.title = element_blank(),
      legend.direction = "horizontal", 
      legend.box.spacing = unit(0.2, "cm"),     
      legend.key.size = unit(0.4, "cm"),        
      legend.text = element_text(size = 12)      
    )+
    guides(fill = guide_legend(nrow = 3))       
  
  plot_list[[id]] <- p
}
# 6. Layout Assembly and Saving -------------------------------------------
message("Assembling final figure matrix...")

plots_matrix <- wrap_plots(plot_list, ncol = 4, byrow = TRUE) 

plots_matrix

# Save the output
output_file_pdf <- file.path(OUT_DIR, "Figure4_Polar_Matrix.pdf")

# Saving as PDF (Recommended for vector editing in AI)
ggsave(output_file_pdf, 
       plot = plots_matrix,
       width = 45,
       height =30,
       dpi = 300)
