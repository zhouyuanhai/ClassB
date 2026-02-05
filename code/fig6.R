# ==============================================================================
# Script Name: Figure 6 
# Project: Burden of Class B Infectious Diseases
# Yuan-Hai ZHOU
# ==============================================================================
rm(list = ls())
library(tidyverse)
library(cowplot)
library(scales)

# 1. Data Loading --------------------------------------------------------------
data_path <- "./ARD_RRD_results.csv" 
df <- read.csv(data_path)

# 2. Global Axis Scaling (Identical to original logic) -------------------------
abs_data <- df %>% filter(time %in% 2020:2023)
global_abs_min <- min(abs_data$absolut_rate_diff, na.rm = TRUE) * 1.3
global_abs_max <- max(abs_data$absolut_rate_diff, na.rm = TRUE) * 1.3

rel_data <- df %>% filter(time %in% 2020:2023)
global_rel_min <- min(rel_data$relat_rate_diff_percent, na.rm = TRUE) * 1.3
global_rel_max <- max(rel_data$relat_rate_diff_percent, na.rm = TRUE) * 1.3

# 3. Core Plotting Function with Fixed Color Mapping ---------------------------
generate_panel <- function(data, target_year, type = "abs") {
  
  # Configuration per metric type
  if(type == "abs") {
    val_col <- "absolut_rate_diff"; low_col <- "ard_lower_95"; up_col <- "ard_upper_95"
    y_lims  <- c(global_abs_min, global_abs_max); y_breaks <- 5
    x_label <- paste("Absolute Rate Difference in", target_year)
  } else {
    val_col <- "relat_rate_diff_percent"; low_col <- "rrd_lower_95_percent"; up_col <- "rrd_upper_95_percent"
    y_lims  <- c(global_rel_min, global_rel_max); y_breaks <- 50
    x_label <- "Relative rate differences (%)"
  }
  
  # Yearly Color Definitions (Hex codes from your original code)
  color_map <- list(
    "2020" = c("Positive (Excess)" = "#FF9896", "Negative (Averted)" = "#393B79"),
    "2021" = c("Positive (Excess)" = "#97243d", "Negative (Averted)" = "#1ab7c1"),
    "2022" = c("Positive (Excess)" = "#FF7F0E", "Negative (Averted)" = "#1F77a1"),
    "2023" = c("Positive (Excess)" = "#f7cc94", "Negative (Averted)" = "#9E9AC8")
  )
  
  # Process data
  panel_df <- data %>%
    filter(time == target_year) %>%
    filter(!is.na(.data[[val_col]])) %>%
    mutate(
      val = as.numeric(.data[[val_col]]),
      impact_type = if_else(val > 0, "Positive (Excess)", "Negative (Averted)"),
      is_sig = ( .data[[low_col]] > 0 & !is.na(.data[[low_col]]) ) | 
        ( .data[[up_col]]  < 0 & !is.na(.data[[up_col]]) ),
      plot_label = paste0(metric, if_else(is_sig, "*", ""))
    ) %>%
    arrange(val) %>%
    mutate(plot_label = factor(plot_label, levels = unique(plot_label)))
  
  # ggplot construction
  ggplot(panel_df, aes(x = plot_label, y = val, fill = impact_type)) +
    geom_col(color = "#FFF", linewidth = 0.02) +
    geom_text(
      aes(label = plot_label, hjust = if_else(val > 0, -0.1, 1.1)), 
      vjust = 0.5, size = 3.1
    ) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.2) + 
    coord_flip() + 
    # CRITICAL FIX: Mapping the specific year's colors to the impact_type levels
    scale_fill_manual(values = color_map[[as.character(target_year)]]) +
    scale_y_continuous(limits = y_lims, breaks = scales::breaks_width(y_breaks)) +
    labs(title = "", y = "", x = x_label) +
    theme_minimal() +
    theme(
      legend.position = "none",
      panel.grid = element_blank(),
      axis.line.x = element_line(color = "black", linewidth = 0.2),
      axis.title.y = element_text(size = 14, face = "bold"),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )
}

# 4. Generate Panels -----------------------------------------------------------
p1 <- generate_panel(df, 2020, "abs"); p5 <- generate_panel(df, 2020, "rel")
p2 <- generate_panel(df, 2021, "abs"); p6 <- generate_panel(df, 2021, "rel")
p3 <- generate_panel(df, 2022, "abs"); p7 <- generate_panel(df, 2022, "rel")
p4 <- generate_panel(df, 2023, "abs"); p8 <- generate_panel(df, 2023, "rel")

# 5. Assemble and Save ---------------------------------------------------------
final_plot <- plot_grid(p1, p5, p2, p6, p3, p7, p4, p8, ncol = 2)

ggsave("***/Figure_6_Color_Fixed.pdf", plot = final_plot, 
       height = 20, width = 18, units = "in", bg = "white")
