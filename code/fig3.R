# ==============================================================================
# Script Name: 05_figure3_maps.R
# Description: Generate Spatiotemporal Distribution Maps (Figure 3)
#              Includes Mainland China + South China Sea Inset + Rank Annotations
# Author: Yuan-Hai Zhou & Yu-Hong Qin
# ==============================================================================

# 1. Setup & Libraries ----------------------------------------------------
rm(list = ls())

library(tidyverse)
library(sf)
library(ggspatial)
library(readxl)
library(cowplot)
library(patchwork)
library(classInt)

# Disable S2 geometry for simpler cropping/plotting
sf_use_s2(FALSE)

# --- Define Paths (Relative Paths for GitHub) ---
# Please ensure these files exist in your project directory
SHP_PROV_PATH  <- "*****/city.shp"       
SHP_CHINA_PATH <- "*****/china.shp"      
DATA_EXCEL_PATH <- "*****/incidence_rate_year.xlsx"
OUT_DIR        <- "*****/Figure3"

if(!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

# 2. Data Loading & Preprocessing -----------------------------------------

cat("Loading map data...\n")
# A. Load Shapefiles
prov_shp <- st_read(SHP_PROV_PATH, quiet = TRUE)
china_shp <- st_read(SHP_CHINA_PATH, quiet = TRUE)

# B. Load Incidence Data
raw_data <- read_excel(DATA_EXCEL_PATH)

# Clean Data
infection_data <- raw_data %>% 
  rename(location = "地区", time = "时间", incidence_rate = "发病率(1/10万)") %>%
  mutate(location = gsub(" ", "", location)) %>% 
  mutate(across(c(incidence_rate), ~round(., 2))) %>%
  # Map Province Names to ADCODE (Standardizing)
  mutate(adcode = case_when(
    location == "北京" ~ "110000", location == "天津" ~ "120000", location == "河北" ~ "130000",
    location == "山西" ~ "140000", location == "内蒙古" ~ "150000", location == "辽宁" ~ "210000",
    location == "吉林" ~ "220000", location == "黑龙江" ~ "230000", location == "上海" ~ "310000",
    location == "江苏" ~ "320000", location == "浙江" ~ "330000", location == "安徽" ~ "340000",
    location == "福建" ~ "350000", location == "江西" ~ "360000", location == "山东" ~ "370000",
    location == "河南" ~ "410000", location == "湖北" ~ "420000", location == "湖南" ~ "430000",
    location == "广东" ~ "440000", location == "广西" ~ "450000", location == "海南" ~ "460000",
    location == "重庆" ~ "500000", location == "四川" ~ "510000", location == "贵州" ~ "520000",
    location == "云南" ~ "530000", location == "西藏" ~ "540000", location == "陕西" ~ "610000",
    location == "甘肃" ~ "620000", location == "青海" ~ "630000", location == "宁夏" ~ "640000",
    location == "新疆" ~ "650000", TRUE ~ NA_character_ 
  ))

# Join Data with Shapefile Geometry
map_data_full <- infection_data %>% 
  left_join(prov_shp, join_by(adcode == "省级码")) %>% 
  select(location, ENG_NAME, adcode, time, incidence_rate, geometry) %>%
  st_as_sf()

# Calculate Centroids (for Annotation Lines)
province_centroids <- map_data_full %>%
  st_point_on_surface() %>%
  st_coordinates() %>%
  as.data.frame() %>%
  rename(x_cen = X, y_cen = Y) %>%
  bind_cols(map_data_full %>% st_drop_geometry() %>% select(ENG_NAME, adcode, time, location)) %>%
  distinct(time, location, .keep_all = TRUE)

# C. Calculate Top/Bottom 5 Ranks
rank_data <- map_data_full %>%
  st_drop_geometry() %>%
  group_by(time) %>%
  arrange(time, desc(incidence_rate)) %>%
  mutate(
    rank = row_number(),
    total_count = n(),
    rank_group = case_when(
      rank <= 5 ~ "Top 5",
      rank >= (total_count - 4) ~ "Bottom 5",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(rank_group)) %>%
  select(time, location, rank_group)

# 3. Configuration: Year-Specific Settings --------------------------------
# This list controls the specific breaks, colors, and label coordinates for each year.
# This replaces the hardcoded `case_when` blocks in your original code.

YEAR_CONFIGS <- list(
  "2004" = list(
    # Breaks from your code
    breaks = c(100, 200, 250, 350, 450, 500, 550),
    labels = c("100-200", "200-250", "250-350", "350-450", "450-500", "500-550"),
    # Colors: Orange gradients for map
    palette = c("#F7CC94", "#EEB080", "#E5946D", "#DC785A", "#ab4b39", "#871a16"),
    # Annotation Line Colors
    line_colors = c("Top 5" = "#45c5a5", "Bottom 5" = "#0072B2"), # color3, color2
    # Specific Label Coordinates (x_label, y_label)
    coords = list(
      "Top 5"    = c(x = 101, y = 47),
      "Bottom 5" = c(x = 126, y = 33)
    )
  ),
  
  "2019" = list(
    breaks = c(100, 130, 190, 250, 350, 400, 500),
    labels = c("100-130", "130-190", "190-250", "250-350", "350-400", "400-500"),
    # Colors: Blue gradients
    palette = c("#D8E5F4", "#BACADF", "#6b81a2", "#536c93", "#224274", "#061f46"),
    line_colors = c("Top 5" = "#E69F00", "Bottom 5" = "#0072B2"), 
    coords = list(
      "Top 5"    = c(x = 101, y = 45),
      "Bottom 5" = c(x = 130, y = 39)
    )
  ),
  
  "2023" = list(
    breaks = c(90, 130, 180, 230, 280, 330, 390),
    labels = c("90-130", "130-180", "180-230", "230-280", "280-330", "330-390"),
    # Colors: Green gradients
    palette = c("#b1cdc6", "#9ec0b8", "#8bb3a9", "#77a79b", "#649a8d", "#31675a"),
    line_colors = c("Top 5" = "#E69F00", "Bottom 5" = "#0072B2"), # color1, color2
    coords = list(
      "Top 5"    = c(x = 90, y = 25),
      "Bottom 5" = c(x = 130, y = 39)
    )
  )
)

# 4. Helper Function: Draw Single Map -------------------------------------

draw_china_map <- function(target_year, config) {
  
  cat(paste0("Generating map for ", target_year, "...\n"))
  
  # A. Prepare Data Subset
  current_data <- map_data_full %>% 
    filter(time == target_year) %>%
    mutate(value = cut(incidence_rate, breaks = config$breaks, labels = config$labels, include.lowest = T, right = T))
  
  # Prepare Annotation Data
  current_ranks <- rank_data %>% filter(time == target_year)
  
  # Join coordinates and add Label Target Coordinates
  anno_df <- current_ranks %>%
    left_join(province_centroids, by = c("time", "location")) %>%
    rowwise() %>%
    mutate(
      x_label = config$coords[[rank_group]]["x"],
      y_label = config$coords[[rank_group]]["y"]
    ) %>%
    ungroup()
  
  # B. Prepare Geometries (Crop Mainland)
  crop_box <- c(xmin = 73, ymin = 18, xmax = 136, ymax = 54)
  china_mainland <- st_crop(st_make_valid(china_shp), crop_box)
  map_mainland   <- st_crop(st_make_valid(current_data), crop_box)
  
  # C. Main Plot Construction
  p_main <- ggplot() +
    # Layer 1: Base Borders
    geom_sf(data = china_mainland, color = "grey", size = 0.5) +
    # Layer 2: Heatmap
    geom_sf(data = map_mainland, aes(fill = value), color = NA) +
    scale_fill_manual(
      values = config$palette, 
      na.value = "grey",
      guide = guide_legend(title = "", keywidth = unit(0.4, "cm"), keyheight = unit(0.3, "cm"))
    ) +
    # Layer 3: Province Borders
    geom_sf(data = china_mainland, fill = NA, color = "#4e4e4e", size = 0.5) +
    # Formatting
    coord_sf(ylim = c(19.2, 53.6), clip = "off") +
    theme_void() +
    theme(
      legend.position = c(0.08, 0),
      legend.justification = c("left", "bottom"),
      legend.text = element_text(color = "black", size = 18), # Slightly smaller for fit
      plot.title = element_text(size = 28, face = "bold", hjust = 0.5, vjust = -6),
      plot.margin = margin(t = 1, r = 4, b = 1, l = 4, unit = "cm") # Margins for annotations
    ) +
    labs(title = paste0("Incidence of Class B notifiable infectious diseases, ", target_year)) +
    # Compass & Scale
    annotation_scale(location = "bl", width_hint = 0.085, style = "ticks", pad_x = unit(10, "cm")) +
    annotation_north_arrow(location = "tl", which_north = "true", style = north_arrow_fancy_orienteering(text_size = 15), 
                           height = unit(1.5, "cm"), width = unit(1.5, "cm"))
  
  # D. Add Annotation Lines & Text
  p_annotated <- p_main +
    geom_segment(data = anno_df,
                 aes(x = x_cen, y = y_cen, xend = x_label, yend = y_label, color = rank_group),
                 linewidth = 0.5, show.legend = FALSE) +
    geom_text(data = anno_df %>% distinct(rank_group, .keep_all = TRUE),
              aes(x = x_label, y = y_label, label = rank_group, color = rank_group),
              hjust = ifelse(anno_df %>% distinct(rank_group, .keep_all = TRUE) %>% pull(x_label) < 100, 1.1, -0.1),
              vjust = 0.5, fontface = "bold", size = 6, show.legend = FALSE) +
    scale_color_manual(values = config$line_colors)
  
  # E. Inset Map (South China Sea)
  # Note: Inherits the same fill scale/colors manually
  p_inset <- ggplot() +
    geom_sf(data = china_shp, color = "grey", size = 0.5) +
    geom_sf(data = current_data, aes(fill = value), color = NA, show.legend = FALSE) +
    scale_fill_manual(values = config$palette, na.value = "grey") +
    geom_sf(data = china_shp, fill = NA, color = "#4e4e4e", size = 0.5) +
    coord_sf(
      xlim = c(117131.4, 2115095), 
      ylim = c(-4028017, -1877844), 
      crs = "+proj=laea +lat_0=40 +lon_0=104"
    ) +
    theme_void() +
    theme(panel.border = element_rect(colour = "#424242", fill = NA))
  
  # F. Combine with Patchwork
  final_plot <- p_annotated + 
    inset_element(p_inset, left = 0.8, bottom = 0.0, right = 1.0, top = 0.3)
  
  return(final_plot)
}

# 5. Execution Loop -------------------------------------------------------

years_to_plot <- c("2004", "2019", "2023")

for(yr in years_to_plot) {
  # 1. Get Config
  cfg <- YEAR_CONFIGS[[yr]]
  
  # 2. Draw Plot
  p <- draw_china_map(yr, cfg)
  
  # 3. Save
  filename <- file.path(OUT_DIR, paste0("Map_", yr, ".pdf"))
  ggsave(filename, plot = p, width = 20, height = 12, units = "in")
  cat(paste("Saved:", filename, "\n"))
}

p_2023 <- draw_china_map("2023", YEAR_CONFIGS[["2023"]])
print(p_2023)

p_2019 <- draw_china_map("2019", YEAR_CONFIGS[["2019"]])
print(p_2019)

p_2004 <- draw_china_map("2004", YEAR_CONFIGS[["2004"]])
print(p_2004)

cat("All maps generated successfully!\n")
