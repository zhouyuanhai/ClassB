# ==============================================================================
# Script Name: 05_figure2_sankey.R
# Description: Generate Alluvial/Sankey Diagram for AAPC Trends (Figure 2C)
# Author: Yuan-Hai Zhou & Yu-Hong Qin
# ==============================================================================

# 1. Setup & Libraries ----------------------------------------------------
rm(list = ls())

library(tidyverse)
library(ggalluvial)
library(readxl)
library(RColorBrewer)
library(grid) # For unit()

# --- Configuration ---
# Fonts & Sizes
PLOT_FONT <- "Helvetica"
TEXT_SIZE <- 4

# Colors
PATTERNS_COLORS <- c("#8DD3C7", "#FB8072", "#BEBADA", "#FB8072", "#80B1D3", "#FDB462")
METRIC_COLORS   <- c("#E64B35", "#4DBBD5", "#00A087", "#3C5488", "#F39B7F", "#8491B4")

# Paths
# Note: Ensure your Excel file is placed in this directory
DATA_PATH <- "****/apc.xlsx" #!!!!!!
OUT_DIR   <- "******/Figure2"#!!!!!
if(!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)


# 2. Helper Functions -----------------------------------------------------

#' Insert Spacer Nodes into Sankey Data
#' This function adds transparent "spacer" rows to separate flow categories visually
insert_spacer_nodes_discrete <- function(d) {
  d <- as_tibble(d)
  new_lvl <- list()
  
  for (ax in unique(d$axis)) {
    if (ax == "Pattern") {
      # Specific order for Pattern axis
      old <- c("decreasing", "stable", "increasing")
      old <- old[old %in% unique(filter(d, axis == ax) %>% pull(stratum))]
    } else {
      # Default order for Metric axis
      old <- filter(d, axis == ax) %>% pull(stratum) %>% unique()
    }
    
    nm  <- paste0("spacer_", ax, "_", seq_along(old)[-length(old)])
    ord <- character(0)
    ord[2 * (seq_along(old) - 1) + 1] <- old
    ord[2 * seq_along(nm)]            <- nm
    
    if (length(nm)) {
      spacer_rows <- map_df(nm, function(spacer_name) {
        tibble(
          Freq       = 0.1, # Small frequency for the spacer
          alluvium   = spacer_name,
          axis       = ax,
          stratum    = spacer_name,
          AAPC_label = NA_character_,
          Pattern    = NA_character_,
          Metric     = NA_character_
        )
      })
      d <- bind_rows(d, spacer_rows)
    }
    new_lvl[[ax]] <- ord
  }
  
  all_levels <- unlist(new_lvl)
  d$stratum <- factor(d$stratum, levels = all_levels)
  return(d)
}

#' Generate Color Mapping
make_colors <- function(items, colors, insert = NULL) {
  items <- unique(items)
  n    <- length(items)
  base <- if (!is.null(insert)) c(colors, insert) else colors
  if (n > length(base)) base <- colorRampPalette(base)(n) 
  setNames(base[seq_len(n)], items)
}


# 3. Data Loading & Cleaning ----------------------------------------------
cat("Loading Joinpoint data...\n")

# Please ensure the file exists at DATA_PATH
if (!file.exists(DATA_PATH)) {
  stop("Data file not found. Please place '发病率apc.xlsx' in 'data/processed/' and rename it to 'joinpoint_apc.xlsx' or update the path.")
}

joinpoint_data <- read_excel(DATA_PATH)

# Clean and Calculate Patterns
merged_data <- joinpoint_data %>% 
  mutate(
    metric = str_remove(metric, " - .*"),
    # Remove asterisks and convert to numeric
    val = as.numeric(gsub("\\*", "", val)),
    p   = as.numeric(gsub("[< ]", "", p))
  ) %>% 
  filter(segment == "Full Range") %>% 
  mutate(across(c("val", "lower", "uper", "p"), ~round(., 2))) %>% 
  # Define Increasing/Decreasing/Stable based on CI
  mutate(
    patterns = case_when(
      val > 0 & lower > 0 ~ "increasing",
      val < 0 & uper < 0  ~ "decreasing",
      TRUE ~ "stable"
    )
  ) %>% 
  # Add asterisk to metric name if significant
  mutate(
    metric = ifelse(patterns %in% c("increasing", "decreasing"),
                    paste0(metric, "*"),
                    metric)
  ) %>% 
  select(metric, val, lower, uper, patterns)

# 4. Data Preparation for Plotting ----------------------------------------

# --- A. Define Sorting Order ---
# Sort logic: Pattern (Inc > Stable > Dec) -> Value
ordered_metrics <- merged_data %>%
  select(metric, patterns, val) %>%
  distinct() %>%
  mutate(pattern_rank = case_when(
    patterns == "decreasing" ~ 3, 
    patterns == "stable"     ~ 2, 
    patterns == "increasing" ~ 1  
  )) %>%
  arrange(pattern_rank, val) %>% 
  pull(metric)

# --- B. Create Lodes (Alluvial Format) ---
df_lodes <- merged_data %>%
  count(Pattern = patterns, Metric = metric, name = "Freq") %>%
  mutate(
    Pattern = factor(Pattern, levels = c("decreasing", "stable", "increasing")),
    Metric  = factor(Metric, levels = ordered_metrics),
    unique_id = paste(Pattern, Metric, sep = "_")
  )

# --- C. Prepare AAPC Labels ---
aapc_labels <- merged_data %>%
  select(metric, val, lower, uper) %>%
  distinct() %>%
  mutate(AAPC_label = sprintf("%.1f (%.1f to %.1f)", val, lower, uper))

# --- D. Construct Sankey Data Structure ---
sankey_data <- df_lodes %>%
  mutate(axis = "Pattern",
         stratum = as.character(Pattern),
         alluvium = unique_id) %>%
  select(Freq, alluvium, axis, stratum, Pattern, Metric) %>%
  bind_rows(
    df_lodes %>%
      arrange(Metric) %>% # Ensure order before converting to character
      mutate(axis = "Metric",
             stratum = as.character(Metric),
             alluvium = unique_id) %>%
      select(Freq, alluvium, axis, stratum, Pattern, Metric)
  ) %>%
  left_join(aapc_labels, by = c("stratum" = "metric"))

# --- E. Add Spacers ---
sankey_data <- insert_spacer_nodes_discrete(sankey_data)

# 5. Color Mapping --------------------------------------------------------
set.seed(123)

pattern_colors_map <- make_colors(
  items = df_lodes$Pattern, 
  colors = PATTERNS_COLORS, 
  insert = brewer.pal(12, "Paired")
)

metric_colors_map <- make_colors(
  items = df_lodes$Metric, 
  colors = METRIC_COLORS, 
  insert = brewer.pal(8, "Set1")
)

# Handle transparent spacers
spacer_strata <- grep("spacer_", unique(sankey_data$stratum), value = TRUE)
spacer_colors <- setNames(rep("transparent", length(spacer_strata)), spacer_strata)

# Combine all colors
node_colors <- c(pattern_colors_map, metric_colors_map, spacer_colors)

# Apply colors to data
sankey_data <- sankey_data %>%
  mutate(axis = factor(axis, levels = c("Pattern", "Metric"))) %>%
  mutate(node_color = node_colors[as.character(stratum)]) %>%
  mutate(flow_color = case_when(
    !is.na(Pattern) ~ pattern_colors_map[as.character(Pattern)],
    TRUE ~ "transparent"
  ))

# 6. Plotting -------------------------------------------------------------
cat("Calculating layout coordinates...\n")

# --- A. Calculate Text Positions ---
# We use a dummy plot to get exact y-coordinates from the stratum geometry
dummy_plot <- ggplot(sankey_data, aes(x = axis, stratum = stratum, alluvium = alluvium, y = Freq)) +
  geom_stratum(width = 0.05)

stratum_coordinates <- layer_data(dummy_plot, 1)

# Extract center positions for Metric labels
metric_positions <- stratum_coordinates %>%
  filter(x == 2) %>% # x=2 corresponds to "Metric" axis
  select(stratum, ymin, ymax) %>%
  mutate(y_center = (ymin + ymax) / 2) %>%
  left_join(
    sankey_data %>% 
      filter(axis == "Metric", !is.na(AAPC_label)) %>% 
      distinct(stratum, AAPC_label),
    by = "stratum"
  ) %>%
  filter(!is.na(AAPC_label))

# --- B. Generate Final Plot ---
cat("Rendering plot...\n")

base_theme <- theme(
  text = element_text(family = PLOT_FONT, face = "bold"),
  plot.margin = unit(c(0, 0, 0, 0), "cm"),
  axis.title.x = element_text(margin = margin(t = 6), size = 16, family = PLOT_FONT, face = "bold"),
  panel.background = element_blank(),
  axis.text = element_blank(),
  axis.ticks = element_blank()
)

sankey_plot <- ggplot(sankey_data,
                      aes(x = axis, stratum = stratum, alluvium = alluvium,
                          y = Freq, label = stratum)) +
  # 1. Stratum (Nodes)
  geom_stratum(aes(fill = node_color), color = NA, width = 0.05) +
  
  # 2. Flows
  geom_flow(aes(fill = flow_color), alpha = 0.7, width = 0.05,
            knot.pos = 0.3, color = "transparent") +
  
  # 3. Labels for Pattern Axis (Left)
  geom_text(stat = "stratum",
            data = function(x) filter(x, axis == "Pattern"),
            aes(label = ifelse(grepl("spacer_", as.character(after_stat(stratum))), "",
                               as.character(after_stat(stratum)))),
            hjust = 1, nudge_x = -0.03,
            size = TEXT_SIZE, family = PLOT_FONT, fontface = "bold") +
  
  # 4. Labels for Metric Axis (Right - Disease Name)
  geom_text(stat = "stratum",
            data = function(x) filter(x, axis == "Metric"),
            aes(label = ifelse(grepl("spacer_", as.character(after_stat(stratum))), "",
                               as.character(after_stat(stratum)))),
            hjust = 0, nudge_x = 0.03,
            size = TEXT_SIZE, family = PLOT_FONT, fontface = "bold") +
  
  # 5. AAPC Value Labels (Right - Values)
  geom_text(data = metric_positions,
            aes(x = 2.5, y = y_center, label = AAPC_label),
            inherit.aes = FALSE,
            hjust = 0, 
            size = TEXT_SIZE - 0.5,
            family = PLOT_FONT, 
            fontface = "plain",
            color = "black") +
  
  # 6. Scales and Theme
  scale_x_discrete(expand = c(0.2, 0.7)) +
  scale_fill_identity() +
  guides(fill = "none") +
  base_theme +
  labs(x = "", y = NULL)

sankey_plot

# 7. Saving ---------------------------------------------------------------
output_file <- file.path(OUT_DIR, "Figure2C_AAPC_Sankey.pdf")
ggsave(output_file, plot = sankey_plot, width = 15, height = 9, units = "in")

cat("Plot saved successfully to:", output_file, "\n")
