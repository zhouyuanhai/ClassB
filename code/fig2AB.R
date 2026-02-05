# ==============================================================================
# Script Name: 05_figure2_circos.R
# Description: Generate Circular Heatmaps for Incidence and Mortality (Figure 2)
# Author: Yuan-Hai Zhou & Yu-Hong Qin
# ==============================================================================

# 1. Setup & Libraries ----------------------------------------------------
rm(list = ls())

# Core data manipulation
library(tidyverse)
library(readxl)

# Visualization
library(circlize)
library(ComplexHeatmap) # For colorRamp2
library(grid)
library(classInt)

# Set global options
circos.par(points.overflow.warning = FALSE)

# Define Output Paths
# Ensure these directories exist in your project structure
DATA_PATH <- "******/all_data.csv" # !!!!!
OUT_DIR   <- "*********/Figure2"
if(!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

# 2. Data Loading & Preprocessing -----------------------------------------
cat("Loading data...\n")

infection_data <- read.csv(DATA_PATH)

# Data Cleaning and Categorization
all_data_cleaned <- infection_data %>% 
  select(metric2, time, incidence_rate, death_rate) %>%
  mutate(metric2 = str_trim(metric2)) %>%
  # --- Disease Categorization ---
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
  ) %>%
  # --- Filtering & Renaming ---
  mutate(
    incidence_rate = as.numeric(incidence_rate),
    death_rate = as.numeric(death_rate),
    year = as.integer(time)
  ) %>%
  filter(year >= 2004 & year <= 2023) %>%
  # Filter out excluded diseases
  filter(!metric2 %in% c("Hepatitis (Unspecified)", "Hepatitis D", "Human Avian Influenza", 
                         "Poliomyelitis", "Diphtheria", "SARS", "Amebic Dysentery", 
                         "Bacillary Dysentery", "Typhoid Fever", "Paratyphoid Fever")) %>%
  filter(!is.na(metric2), !is.na(transmission_group)) %>%
  # Shorten Names
  mutate(
    disease = case_when(
      metric2 == "Typhoid and Paratyphoid Fever" ~ "TP",
      metric2 == "Hemorrhagic Fever" ~ "Hemorrhagic F.",
      metric2 == "Schistosomiasis" ~ "Schisto",
      metric2 == "Viral Hepatitis" ~ "Hepatitis",
      metric2 == "Scarlet Fever" ~ "Scarlet F.",
      metric2 == "Pulmonary Tuberculosis" ~ "PTB",
      metric2 == "Neonatal Tetanus" ~ "N. Tetanus",
      metric2 == "Meningococcal Meningitis" ~ "Men. Meningitis",
      metric2 == "Japanese Encephalitis" ~ "JE",
      TRUE ~ metric2
    )
  ) %>%
  select(disease, year, incidence_rate, death_rate, category = transmission_group) %>%
  arrange(year)

# 3. Helper Function: Prepare Data Matrix ---------------------------------
# This function converts the long dataframe to a wide matrix and prepares factors
prepare_circos_data <- function(df, value_col) {
  # Create Matrix
  data_mat <- df %>%
    select(disease, year, all_of(value_col)) %>%
    distinct(disease, year, .keep_all = TRUE) %>% 
    pivot_wider(names_from = year, values_from = all_of(value_col)) %>%
    as.data.frame() %>% 
    tibble::column_to_rownames("disease") %>%
    as.matrix()
  
  # Create Ordered Factors based on Category
  disease_classification_df <- df %>% distinct(disease, category)
  
  disease_factors <- disease_classification_df %>%
    arrange(category, disease) %>% 
    pull(category, name = disease) %>% 
    factor(., levels = unique(.)) 
  
  # Align Matrix and Factors
  data_mat_ordered <- data_mat[names(disease_factors), , drop = FALSE]
  
  # Remove all-NA rows
  all_na_rows <- which(rowSums(is.na(data_mat_ordered)) == ncol(data_mat_ordered))
  if(length(all_na_rows) > 0) {
    cat("Removing all-NA rows:", rownames(data_mat_ordered)[all_na_rows], "\n")
    data_mat_ordered <- data_mat_ordered[-all_na_rows, , drop = FALSE]
    disease_factors <- disease_factors[-all_na_rows]
    disease_factors <- factor(disease_factors, levels = unique(disease_factors))
  }
  
  return(list(matrix = data_mat_ordered, factors = disease_factors))
}

# 4. Helper Function: Main Plotting Logic ---------------------------------
# This function encapsulates all your drawing logic
draw_circos_plot <- function(data_list, col_fun, breaks, colors, output_path, center_title) {
  
  mat <- data_list$matrix
  factors <- data_list$factors
  
  # Category Colors
  categories <- unique(factors)
  category_colors <- c("#E41A1C", "#377EB8","#FF7F00", "#4DAF4A","#17BECF","#9467bd")
  names(category_colors) <- categories
  
  # Open PDF Device
  pdf(output_path, height = 12, width = 12)
  
  # --- Main Circos Plot ---
  circos.clear()
  
  n_sectors <- length(factors)
  n_years <- ncol(mat)
  
  # Setup Gap (Opening)
  gap_degree <- 18 
  gap.degree_vector <- c(rep(0, n_sectors - 1), gap_degree)
  
  circos.par(start.degree = 90, track.margin = c(0,0), gap.degree = gap.degree_vector, cell.padding = c(0, 0, 0, 0))
  
  # Initialize
  init_factor <- factor(names(factors), levels = names(factors))
  circos.initialize(factors = init_factor, xlim = c(0, 1))
  
  # Track 1: Disease Labels
  circos.track(ylim = c(0, 1), track.height = 0.1, bg.border = NA, panel.fun = function(x, y) {
    circos.text(0.5, 0.5, get.cell.meta.data("sector.index"), facing = "inside", niceFacing = TRUE, adj = c(0.5, 0.5), cex = 1.2)
  })
  
  # Track 2: Category Color Ring
  circos.track(ylim = c(0, 1), track.margin = c(0.02, 0), track.height = 0.02, bg.border = NA, panel.fun = function(x, y) {
    disease_name <- get.cell.meta.data("sector.index")
    col <- category_colors[as.character(factors[disease_name])]
    circos.rect(0, 0, 1, 1, col = col, border = NA)
  })
  
  # Track 3: Heatmap
  circos.track(ylim = c(0, n_years), track.height = 0.65, bg.border = NA, panel.fun = function(x, y) {
    disease_name <- get.cell.meta.data("sector.index")
    incidence_vector <- mat[disease_name, ]
    cell_colors <- col_fun(incidence_vector)
    for(i in 1:n_years) {
      circos.rect(0, i-1, 1, i, col = cell_colors[i], border = "black")
    }
  })
  
  # Add Axis Labels (Years) in the Gap
  # Re-using Track 3
  circos.track(track.index = 3, bg.border = NA, panel.fun = function(x, y){
    if(CELL_META$sector.numeric.index == length(levels(init_factor))) { 
      cn <- colnames(mat)
      n <- length(cn)
      cell_height <- (CELL_META$cell.ylim[2] - CELL_META$cell.ylim[1]) / n
      y_coords <- seq(CELL_META$cell.ylim[1] + cell_height / 2, CELL_META$cell.ylim[2] - cell_height / 2, length.out = n)
      
      for (i in 1:n) { 
        circos.lines(c(CELL_META$cell.xlim[2], CELL_META$cell.xlim[2] + convert_x(1.5, "mm")), c(y_coords[i], y_coords[i]), col = "black", lwd = 0.4)
      } 
      circos.text(rep(CELL_META$cell.xlim[2], n) + convert_x(1.5, "mm"), y_coords, cn, facing = "inside", cex = 0.9, adj = c(0, 0.5))
      circos.text(CELL_META$cell.xlim[2], n_years + 1, "Year", facing = "inside", cex = 0.9, font = 2, adj = c(0, 0.5))
    }
  })
  
  # Track 4: Inner Category Ring
  circos.track(ylim = c(0, 1), track.height = 0.01, bg.border = NA, panel.fun = function(x, y) {
    disease_name <- get.cell.meta.data("sector.index")
    col <- category_colors[as.character(factors[disease_name])]
    circos.rect(0, 0, 1, 1, col = col, border = NA)
  })
  
  # --- Inner Legend (Picture in Picture) ---
  par(new = TRUE)
  par(mar = c(0, 0, 0, 0))
  par(fig = c(0.4, 0.6, 0.4, 0.6), new = TRUE)
  
  legend_factors <- factor(breaks, levels = breaks)
  gap_degree_legend <- 30
  gap.degree_vector_legend <- c(rep(0, length(legend_factors) - 1), gap_degree_legend)
  
  circos.clear() # Clear parameters for the inner plot
  circos.par(start.degree = 60, gap.degree = gap.degree_vector_legend, track.margin = c(0,0), cell.padding = c(0, 0, 0, 0))
  circos.initialize(factors = legend_factors, xlim = c(0, 1))
  
  circos.track(ylim = c(0, 1), track.height = 0.2, bg.border = NA, panel.fun = function(x, y) {
    sector_name <- get.cell.meta.data("sector.index")
    color_index <- which(breaks == sector_name)
    
    circos.rect(0, 0, 1, 1, col = colors[color_index], border = NA)
    
    display_label <- sector_name
    if(as.numeric(sector_name) == 1e-10) display_label <- "0"
    
    circos.text(0, y = 0 - convert_y(3, "mm"), labels = display_label, facing = "clockwise", niceFacing = TRUE, cex = 0.8)
    
    if(sector_name == levels(legend_factors)[1]) {
      circos.text(0, 0.5, "Legend", facing = "inside", cex = 0.75, font = 2, adj = c(1, 0.5))
    }
  })
  
  grid.text(label = center_title, x = 0.5, y = 0.5, gp = gpar(cex = 0.8, fontface = "bold"))
  
  circos.clear()
  dev.off()
  cat("Saved plot to:", output_path, "\n")
}

# 5. Execution: Generate Plots --------------------------------------------

# --- A. Incidence Plot ---
cat("Generating Incidence Plot...\n")
inc_data <- prepare_circos_data(all_data_cleaned, "incidence_rate")

# Define Breaks & Colors for Incidence
inc_breaks <- c(0, 1e-10, 3.4, 10, 20, 35, 50, 70, 80, 90)
inc_colors <- c("#c4e7ed","#fee5d2","#edd3d2","#fbc3c3","#f59394","#f27d81","#f15c5c","#ee322e","#ce1f26","#a41d20")
inc_col_fun <- colorRamp2(inc_breaks, inc_colors)

draw_circos_plot(
  data_list   = inc_data,
  col_fun     = inc_col_fun,
  breaks      = inc_breaks,
  colors      = inc_colors,
  output_path = file.path(OUT_DIR, "Figure2A_Incidence_Circos.pdf"),
  center_title = "Incidence\n(per 100,000)"
)

# --- B. Mortality Plot ---
cat("Generating Mortality Plot...\n")
death_data <- prepare_circos_data(all_data_cleaned, "death_rate")

# Define Breaks & Colors for Mortality
death_breaks <- c(0, 1e-10, 0.03, 0.1, 0.2, 0.4, 0.7, 0.9, 1.1, 1.3)
death_colors <- c("#c4e7ed","#fee5d2","#edd3d2","#fbc3c3","#f59394","#f27d81","#f15c5c","#ee322e","#ce1f26","#a41d20")
death_col_fun <- colorRamp2(death_breaks, death_colors)

draw_circos_plot(
  data_list   = death_data,
  col_fun     = death_col_fun,
  breaks      = death_breaks,
  colors      = death_colors,
  output_path = file.path(OUT_DIR, "Figure2B_Mortality_Circos.pdf"),
  center_title = "Death rate\n(per 100,000)"
)

cat("Done! All plots saved to output folder.\n")
