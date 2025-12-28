
set.seed(52445443) 

rm (list=(ls()))

library(forecast)
library(ggtext)
library(patchwork) 
library(scales)
library(tidyverse)
library(ggspatial)
library(RColorBrewer)
library(sjmisc)
library(readxl)
library(cowplot)
library(ggbreak)
library(grid)
library(ggsci)

setwd("**************")




infection_data <- read_excel("********/all_data.csv")


#ARIMA-----------------------------------------------------------------------

validation_accuracy <- data.frame()

model_info <- data.frame()

all_residuals_list <- list()

validation_forecasts_list <- list()

final_forecasts_list <- list()

fitted_data_list <- list()

unique_metrics <- unique(all_data$metric2)


for (disease in unique_metrics) {

  
  tryCatch({
    
   
    disease_data <- all_data %>% 
      filter(metric2 == disease) %>% 
      arrange(time)
    
    
    training_data <- disease_data %>% filter(time >= 2004 & time <= 2017)
    validation_data <- disease_data %>% filter(time >= 2018 & time <= 2019)
    
    if(nrow(training_data) < 10) {
      cat("  >>  ", disease, "\n")
      next
    }
    
    ts_train <- ts(training_data$incidence_rate, start = 2004, frequency = 1)
    arima_model <- auto.arima(ts_train, stepwise = FALSE, approximation = FALSE)
    
    train_residuals <- residuals(arima_model)
    
    residuals_df <- data.frame(
      metric = disease,
      time = training_data$time, 
      residual = as.numeric(train_residuals)
    )

    all_residuals_list[[disease]] <- residuals_df
    
    
  
    
    validation_forecast <- forecast(arima_model, h = 2)
    ts_validation_actual <- validation_data$incidence_rate
    acc_metrics <- accuracy(validation_forecast, ts_validation_actual)
    
    validation_accuracy <- rbind(validation_accuracy, data.frame(
      metric = disease,
      RMSE = acc_metrics[2, "RMSE"],
      MAE = acc_metrics[2, "MAE"],
      MAPE = acc_metrics[2, "MAPE"]
    ))
    
    validation_forecast_df <- data.frame(
      metric = disease,
      time = 2018:2019,
      predicted_incidence_rate = as.numeric(validation_forecast$mean)
    )
    validation_forecasts_list[[disease]] <- validation_forecast_df
    
    
    full_history_data <- disease_data %>% filter(time >= 2004 & time <= 2019)
    ts_full <- ts(full_history_data$incidence_rate, start = 2004, frequency = 1)
    final_model <- auto.arima(ts_full, stepwise = FALSE, approximation = FALSE)

    fitted_values <- fitted(final_model)
    
    fitted_df <- data.frame(
      metric = disease,
      time = 2004:2019,
      fitted_incidence_rate = as.numeric(fitted_values),
      observed_incidence_rate = as.numeric(ts_full) 
    )
    
    fitted_data_list[[disease]] <- fitted_df
    
    model_info <- rbind(model_info, data.frame(
      metric = disease,
      model = as.character(final_model),
      AICc = final_model$aicc
    ))
    
    
    final_forecast <- forecast(final_model, h = 4)
    
   
    forecast_df <- data.frame(
      metric = disease,
      time = 2020:2023,
      predicted_incidence_rate = as.numeric(final_forecast$mean),
      Lower_95 = as.numeric(final_forecast$lower[, "95%"]),
      Upper_95 = as.numeric(final_forecast$upper[, "95%"])
    )
    forecast_df$predicted_incidence_rate[forecast_df$predicted_incidence_rate < 0] <- 0
    final_forecasts_list[[disease]] <- forecast_df
    
  })
}


final_residuals_df <- bind_rows(all_residuals_list)

validation_forecasts_df <- bind_rows(validation_forecasts_list)

final_forecasts_df <- bind_rows(final_forecasts_list)

fitted_df <-  bind_rows(fitted_data_list)

write.csv("****")




# fig2 --------------------------------------------------------------------

rm (list=(ls()))

incidence_data <- read_excel("*****/data_of_each_city.xlsx")

prov <- st_read("***/T2024年初省级.shp")

china = st_read("****/中华人民共和国.shp")

#clean data
infection_data <- incidence_data %>% 
  rename(location="地区",
         time="时间",
         incidence_rate="发病率(1/10万)",
         death_rate="死亡率(1/10万)") %>%
  mutate(location=gsub(" ","",location)) %>% 
  mutate(across(c(incidence_rate,death_rate),~round(.,2)))



infection_data <- infection_data %>%
  mutate(
    adcode = case_when(
      location == "北京" ~ "110000",
      location == "天津" ~ "120000",
      location == "河北" ~ "130000",
      location == "山西" ~ "140000",
      location == "内蒙古" ~ "150000",
      location == "辽宁" ~ "210000",
      location == "吉林" ~ "220000",
      location == "黑龙江" ~ "230000",
      location == "上海" ~ "310000",
      location == "江苏" ~ "320000",
      location == "浙江" ~ "330000",
      location == "安徽" ~ "340000",
      location == "福建" ~ "350000",
      location == "江西" ~ "360000",
      location == "山东" ~ "370000",
      location == "河南" ~ "410000",
      location == "湖北" ~ "420000",
      location == "湖南" ~ "430000",
      location == "广东" ~ "440000",
      location == "广西" ~ "450000",
      location == "海南" ~ "460000",
      location == "重庆" ~ "500000",
      location == "四川" ~ "510000",
      location == "贵州" ~ "520000",
      location == "云南" ~ "530000",
      location == "西藏" ~ "540000",
      location == "陕西" ~ "610000",
      location == "甘肃" ~ "620000",
      location == "青海" ~ "630000",
      location == "宁夏" ~ "640000",
      location == "新疆" ~ "650000",
      TRUE ~ NA_character_ 
    )
  )


infection_data_map <- infection_data %>% left_join(prov,join_by(adcode=="省级码")) %>% 
  select("location","ENG_NAME","adcode","time","incidence_rate","death_rate","geometry") 

infection_data_map <- st_as_sf(infection_data_map)
sf_use_s2(FALSE) 


province_centroids <- infection_data_map %>%
  st_point_on_surface() %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  rename(x_cen = X, y_cen = Y) %>% 
  bind_cols(infection_data_map %>% st_drop_geometry() %>% select(ENG_NAME, adcode)) %>% 
  distinct(ENG_NAME, .keep_all = TRUE)


top_bottom_list <- infection_data_map %>%
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
  select(time, location, ENG_NAME,incidence_rate, rank_group) 

annotation_lines_df <- top_bottom_list %>% 
  mutate(x_label = case_when(time=="2023"&rank_group=="Top 5"~90,
                             time=="2023"&rank_group=="Bottom 5"~130,
                             time=="2019"&rank_group=="Top 5"~101,
                             time=="2019"&rank_group=="Bottom 5"~130,
                             time=="2004"&rank_group=="Top 5"~101,
                             time=="2004"&rank_group=="Bottom 5"~126
  ),
  y_label =case_when(time=="2023"&rank_group=="Top 5"~25,
                     time=="2023"&rank_group=="Bottom 5"~39,
                     time=="2019"&rank_group=="Top 5"~45,
                     time=="2019"&rank_group=="Bottom 5"~39,
                     time=="2004"&rank_group=="Top 5"~47,
                     time=="2004"&rank_group=="Bottom 5"~33
  )) %>% 
  mutate(color_group=case_when(time=="2023"&rank_group=="Top 5"~"color1",
                               time=="2023"&rank_group=="Bottom 5"~"color2",
                               time=="2019"&rank_group=="Top 5"~"color1",
                               time=="2019"&rank_group=="Bottom 5"~"color2",
                               time=="2004"&rank_group=="Top 5"~"color3",
                               time=="2004"&rank_group=="Bottom 5"~"color2"
  )) %>% left_join(province_centroids,join_by(ENG_NAME))

line_colors <- c( "Bottom 5" = "#0072B2", "Top 5" = "#E69F00")




#draw figure 


infection_data_map_2023 <- infection_data_map %>% 
  filter(time=="2023")


map_2023 <- infection_data_map_2023 %>%
  mutate(value = cut(incidence_rate, breaks = c(90,130,180,230,280,330,390),
                     labels = c("90-130","130-180","180-230","230-280","280-330","330-390"),
                     include.lowest = T,right = T))

map_2023 <- st_as_sf(map_2023)

sf_use_s2(FALSE)
crop_box <- c(xmin = 73, ymin = 18, xmax = 136, ymax = 54)

china_mainland <- china %>%
  st_make_valid() %>%    
  st_crop(crop_box)

map_2023_mainland <- map_2023 %>%
  st_make_valid() %>% 
  st_crop(crop_box)


plot <- ggplot()

plot <- plot +
  geom_sf(data = china_mainland, color = "grey", size= 0.5) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank())

plot <- plot +
  geom_sf(data = map_2023_mainland,aes(fill=value),color=NA)+
  
  scale_fill_manual(
    values = c( "#b1cdc6","#9ec0b8","#8bb3a9","#77a79b","#649a8d","#31675a"),
    na.value = "grey",
    guide = guide_legend(title = "",
                         keywidth = unit(0.4, "cm"), 
                         keyheight = unit(0.3, "cm")))+
  theme(legend.position = c(0.08,0),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.justification = c("left", "bottom"),
        legend.text = element_text(color="black",size = 23),
        legend.key.spacing.y = unit(0.01, "cm"),
        plot.title = element_text(
          size = 34,       
          face = "bold",   
          hjust = 0.5,
          vjust = -6))+
  labs(title = "Incidence of Class B notifiable infectious diseases, 2023")


plot <- plot +
  geom_sf(data = china_mainland,fill=NA,color="#4e4e4e",size=0.5)+
  coord_sf(ylim = c(19.21169,53.56327))


SouthChinaSea <- ggplot()  +
  geom_sf(data = china, color = "grey", size = 0.5) +
  geom_sf(data = map_2023, aes(fill = value), color = NA, show.legend = FALSE) +
  scale_fill_manual(values = c("#b1cdc6","#9ec0b8","#8bb3a9","#77a79b","#649a8d","#31675a"), na.value = "grey") +

  geom_sf(data = china, fill = NA, color = "#4e4e4e", size = 0.5) +
  coord_sf(
    xlim = c(117131.4, 2115095),
    ylim = c(-4028017, -1877844),
    crs = "+proj=laea +lat_0=40 +lon_0=104"
  ) +
  theme(
    aspect.ratio = 1.25,
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(colour = "#424242", fill = NA),
    plot.margin = unit(c(0, 0, 0, 0), "mm")
  ) +
  theme(legend.position = "none")+
  labs(title = NULL, subtitle = NULL, caption = NULL)

print(SouthChinaSea)


plot  <- plot +
  annotation_scale(
    location = "bl",          
    text_cex = 1.4,
    width_hint = 0.085,          
    tick_height = 0,
    style = "ticks",           
    unit_category = "metric",  
    pad_x = unit(10, "cm"),   
    pad_y = unit(0.2, "cm")    
  )+
  annotation_north_arrow(
    location = "tl",          
    which_north = "true",      
    style = north_arrow_fancy_orienteering(
      text_size = 20
    ),  
    height = unit(2, "cm"), 
    width = unit(2, "cm"),   
    pad_x = unit(0.2, "cm"),   
    pad_y = unit(0.2, "cm")   
  )

plot1 <- plot +
  geom_segment(data = annotation_lines_df %>% filter(time=="2023"),
               aes(x = x_cen, y = y_cen, xend = x_label, yend = y_label, color = color_group),
               linewidth = 0.5, show.legend = FALSE) +
  geom_text(data = annotation_lines_df %>% filter(time=="2023")%>% distinct(rank_group, .keep_all = TRUE),
            aes(x = x_label, y = y_label, label = rank_group, color = color_group),
            hjust = ifelse(annotation_lines_df %>% filter(time=="2023")%>% distinct(rank_group, .keep_all = TRUE)%>% pull(x_label) < 100, 1.1, -0.1),
            vjust = 0.5, fontface = "bold", size = 6, show.legend = FALSE) +
  
  scale_color_manual(values =c("color1"="#E69F00",
                               "color2"="#0072B2"))+
  
  coord_sf(ylim = c(19.21169,53.56327), clip = "off") +
  
  theme(
    plot.margin = margin(t = 1, r = 4, b = 1, l = 4, unit = "cm")
  ) 

plot2 <- plot1  + 
  inset_element(SouthChinaSea, left = 0.8, bottom = 0.0, right = 1.0, top = 0.3)

plot2 

ggsave("******")


# fig3 --------------------------------------------------------------------

rm (list=(ls()))

library(forecast)
library(ggtext)
library(patchwork) 
library(scales)
library(tidyverse)
library(ggspatial)
library(RColorBrewer)
library(sjmisc)
library(readxl)
library(cowplot)
library(ggbreak)
library(grid)
library(ggsci)
library(broom)
library(circlize)
library(ComplexHeatmap) 
library(classInt)

infection_data <- read.csv("*******/all_data.csv")


unique(infection_data$metric2)
colnames(infection_data)

hart_data <- infection_data %>% 
  select("metric2","time","incidence_rate","death_rate")

trans_data <- hart_data  %>% 
  mutate(metric2 = str_trim(metric2)) %>%

  mutate(
    transmission_group = case_when(
      metric2 %in% c(
        "Pulmonary Tuberculosis",       
        "Measles",                      
        "Pertussis",                    
        "Scarlet Fever",               
        "Meningococcal Meningitis",     
        "Human Avian Influenza",       
        "Human H7N9 Avian Influenza",   
        "SARS",                        
        "Diphtheria",                  
        "Influenza"                     
      ) ~ "Respiratory",
      
      
      metric2 %in% c(
        "Hepatitis A",                  
        "Hepatitis E",                 
        "Typhoid Fever",                
        "Paratyphoid Fever",            
        "Typhoid and Paratyphoid Fever",
        "Dysentery",                   
        "Bacillary Dysentery",          
        "Amebic Dysentery",             
        "Poliomyelitis"              
      ) ~ "Fecal-Oral",
      
      metric2 %in% c(
        "AIDS",                         
        "Syphilis",                     
        "Gonorrhea",                    
        "Hepatitis B",                  
        "Hepatitis C",                  
        "Hepatitis D",                  
        "Viral Hepatitis",             
        "Hepatitis (Unspecified)",      
        "Hepatitis"                    
      ) ~ "Sexual and Blood-borne",
      
      
      metric2 %in% c(
        "Dengue Fever",                 
        "Malaria",                      
        "Japanese Encephalitis"         
      ) ~ "Vector-borne",
      
      
      metric2 %in% c(
        "Brucellosis",                 
        "Rabies",                      
        "Hemorrhagic Fever",            
        "Anthrax"                       
      ) ~ "Zoonotic",
      
      
      metric2 %in% c(
        "Schistosomiasis",              
        "Leptospirosis",                
        "Neonatal Tetanus"             
      ) ~ "Other Transmission",
      
      
      TRUE ~ "Uncategorized"
    )
  )

all_data_cleaned <- trans_data %>%
  select(
    disease = metric2,              
    year = time,                    
    incidence_rate, 
    death_rate,
    category = transmission_group   
  ) %>%
  mutate(
    incidence_rate = as.numeric(incidence_rate),
    death_rate = as.numeric(death_rate),
    year = as.integer(year) 
  ) %>%
  filter(year >= 2004 & year <= 2023,
         !disease %in% c(
           "Hepatitis (Unspecified)",
           "Hepatitis D",
           "Human Avian Influenza",
           "Poliomyelitis",
           "Diphtheria",
           "SARS",
           "Amebic Dysentery","Bacillary Dysentery",
           "Typhoid Fever","Paratyphoid Fever" 
         )) %>%
  filter(!is.na(disease)) %>%
  filter(!is.na(category)) %>% arrange(year)

unique(all_data_cleaned$disease)




all_data_cleaned <- all_data_cleaned %>%
  mutate(
    disease = case_when(
      
      disease == "Typhoid and Paratyphoid Fever" ~ "TP",
      disease == "Hemorrhagic Fever" ~ "Hemorrhagic F.",
      disease == "Schistosomiasis" ~ "Schisto",
      
      disease == "Viral Hepatitis" ~ "Hepatitis",
      
      disease == "Scarlet Fever" ~ "Scarlet F.",
      
      disease == "Pulmonary Tuberculosis" ~ "PTB",
      
      disease == "Neonatal Tetanus" ~ "N. Tetanus",
      
      disease == "Meningococcal Meningitis" ~ "Men. Meningitis",
      
      disease == "Japanese Encephalitis" ~ "JE",
      
      TRUE ~ disease
    )
  )

data_mat <- all_data_cleaned %>%
  select(disease, year, incidence_rate) %>%
  distinct(disease, year, .keep_all = TRUE) %>% 
  pivot_wider(
    names_from = year, 
    values_from = incidence_rate
  ) %>%
  as.data.frame() %>% 
  tibble::column_to_rownames("disease") %>%
  as.matrix()

disease_classification_df <- all_data_cleaned %>%
  distinct(disease, category)

disease_factors_raw <- disease_classification_df %>%
  arrange(category, disease) %>% 
  pull(category, name = disease) %>% 
  factor(., levels = unique(.)) 


data_mat_ordered_raw <- data_mat[names(disease_factors_raw), , drop = FALSE]


all_na_rows <- which(rowSums(is.na(data_mat_ordered_raw)) == ncol(data_mat_ordered_raw))


manual_breaks <- c(0,1e-10,3.4, 10,20,35,50,70,80,90)
manual_colors <- c("#c4e7ed","#fee5d2","#edd3d2","#fbc3c3",
                   "#f59394","#f27d81","#f15c5c","#ee322e",
                   "#ce1f26","#a41d20")

col_fun_heatmap <- colorRamp2(
  breaks = manual_breaks,
  colors = manual_colors
)

categories <- unique(disease_factors)

category_colors <- c("#E41A1C", "#377EB8","#FF7F00",
                     "#4DAF4A","#17BECF","#9467bd")

names(category_colors) <- categories



pdf("*****",
    height = 12,
    width=12
)

circos.clear()


n_sectors <- length(disease_factors)
n_years <- ncol(data_mat_ordered)
year_labels <- colnames(data_mat_ordered)

gap_degree <- 18 
gap.degree_vector <- c(rep(0, n_sectors - 1), gap_degree) 
circos.par(
  start.degree = 90,
  track.margin = c(0,0),
  gap.degree = gap.degree_vector,
  cell.padding = c(0, 0, 0, 0)
)

init_factor <- factor(
  names(disease_factors), 
  levels = names(disease_factors)
)
circos.initialize(
  factors = init_factor, 
  xlim = c(0, 1)
)

circos.track(
  ylim = c(0, 1), 
  track.height = 0.1,
  bg.border = NA, 
  panel.fun = function(x, y) {
    disease_name <- get.cell.meta.data("sector.index")
    circos.text(
      x = 0.5, y = 0.5, 
      labels = disease_name,
      facing = "inside",  
      niceFacing = TRUE,     
      adj = c(0.5, 0.5), 
      cex = 1.2           
    )
  }
)


circos.track(
  ylim = c(0, 1), 
  track.margin = c(0.02, 0),
  track.height = 0.02, 
  bg.border = NA,
  panel.fun = function(x, y) {
    disease_name <- get.cell.meta.data("sector.index")
    classification_name <- as.character(disease_factors[disease_name])
    col <- category_colors[classification_name]
    circos.rect(
      xleft = 0, ybottom = 0, xright = 1, ytop = 1,
      col = col, border = NA 
    )
  }
)

circos.track(
  ylim = c(0, n_years), 
  track.height = 0.65,
  bg.border = NA,
  panel.fun = function(x, y) {
    disease_name <- get.cell.meta.data("sector.index")
    incidence_vector <- data_mat_ordered[disease_name, ]
    colors <- col_fun_heatmap(incidence_vector)
    
    for(i in 1:n_years) {
      circos.rect(
        xleft = 0, xright = 1, ybottom = i - 1, ytop = i,
        col = colors[i], border =  "black"
      )
    }
    
  }
)


circos.track(
  track.index = get.current.track.index(), 
  
  bg.border = NA, 
  
  panel.fun = function(x, y){
    
    if(CELL_META$sector.numeric.index == length(levels(init_factor))) { 
      
      cn <- colnames(data_mat_ordered) 
      
      n <- length(cn) 
    
      cell_height <- (CELL_META$cell.ylim[2] - CELL_META$cell.ylim[1]) / n 
      
      y_coords <- seq(CELL_META$cell.ylim[1] + cell_height / 2, 
                      CELL_META$cell.ylim[2] - cell_height / 2, 
                      length.out = n)
      
      for (i in 1:n) { 
   
        circos.lines( 
          c(CELL_META$cell.xlim[2], CELL_META$cell.xlim[2] + convert_x(1.5, "mm")), 
          
          c(y_coords[i], y_coords[i]), 
          

          col = "black", 
          lwd = 0.4
        ) 
      } 
      
      circos.text( 
  
        rep(CELL_META$cell.xlim[2], n) + convert_x(1.5, "mm"), 
        
      
        y_coords, 
        
       
        cex = 0.9, 
        adj = c(0, 0.5), 
        
        
        cn, 
        facing = "inside" 
      ) 
      
      
      circos.text(
        x = CELL_META$cell.xlim[2], y = n_years + 1,
        labels = "Year",
        facing = "inside", cex = 0.9, font = 2, adj = c(0, 0.5)
      )
      
    } 
  })


circos.track(
  ylim = c(0, 1), 
  track.height = 0.01, 
  bg.border = NA,
  panel.fun = function(x, y) {
    disease_name <- get.cell.meta.data("sector.index")
    classification_name <- as.character(disease_factors[disease_name])
    col <- category_colors[classification_name]
    circos.rect(
      xleft = 0, ybottom = 0, xright = 1, ytop = 1,
      col = col, border = NA 
    )
  }
)

par(new = TRUE)
par(mar = c(0, 0, 0, 0)) 
par(fig = c(0.4, 0.6, 0.4, 0.6), new = TRUE)

legend_factors <- factor(manual_breaks, levels = manual_breaks)

gap_degree <- 30
gap.degree_vector2 <- c(rep(0, length(legend_factors) - 1), gap_degree) 


circos.clear() 
circos.par(
  start.degree = 60, 
  gap.degree = gap.degree_vector2,   
  track.margin = c(0,0),
  cell.padding = c(0, 0, 0, 0)  
)

circos.initialize(factors = legend_factors, xlim = c(0, 1))


circos.track(
  ylim = c(0, 1), 
  track.height = 0.2, 
  bg.border = NA,
  panel.fun = function(x, y) {
    sector_name <- get.cell.meta.data("sector.index")
    color_index <- which(manual_breaks == sector_name)
    
    circos.rect(0, 0, 1, 1, 
                col = manual_colors[color_index], 
                border = NA)
    display_label <- sector_name

    if(as.numeric(sector_name) == 1e-10) {
      display_label <- "0" 
    }

    circos.text(0, y = 0 - convert_y(3, "mm"), 
                labels = display_label,  
                facing = "clockwise", niceFacing = TRUE, cex = 0.8)
    
    first_sector_name <- levels(legend_factors)[1]
    
    if(sector_name == first_sector_name) {
      circos.text(
        x = 0, 
        y = 0.5, 
        labels = "Legend",
        facing = "inside", 
        cex = 0.75, 
        font = 2, 
        adj = c(1, 0.5) 
      )
    }
  }
)

grid.text(
  label = "incidence\n(per 100000)",
  x = 0.5, 
  y = 0.5,
  gp = gpar(cex = 0.8, fontface = "bold")
)

circos.clear() 
dev.off()



# fig4 --------------------------------------------------------------------

rm (list=(ls()))


infection_data <- read.csv("*****/all_data.csv")


rank_data <- infection_data %>% select(metric2,time,incidence,death,incidence_rate,death_rate) %>% 
  filter(time %in% c("2004","2019","2023"),
         !metric2 %in% c(
           "Hepatitis (Unspecified)",
           "Hepatitis D",
           "Human Avian Influenza",
           "Poliomyelitis",
           "Diphtheria",
           "SARS",
           "Amebic Dysentery","Bacillary Dysentery",
           "Typhoid Fever","Paratyphoid Fever" 
         )) %>% arrange(time,metric2) %>% 
  mutate(across(c(incidence_rate, death_rate), ~round(., 2))) %>% 
  mutate(
    transmission_group = case_when(
      
      metric2 %in% c(
        "Pulmonary Tuberculosis",     
        "Measles",                     
        "Pertussis",                  
        "Scarlet Fever",               
        "Meningococcal Meningitis",    
        "Human Avian Influenza",       
        "Human H7N9 Avian Influenza",   
        "SARS",                        
        "Diphtheria",                   
        "Influenza"                   
      ) ~ "Respiratory",
      
      
      metric2 %in% c(
        "Hepatitis A",                 
        "Hepatitis E",                  
        "Typhoid Fever",                
        "Paratyphoid Fever",            
        "Typhoid and Paratyphoid Fever",
        "Dysentery",                    
        "Bacillary Dysentery",          
        "Amebic Dysentery",             
        "Poliomyelitis"              
      ) ~ "Fecal-Oral",
      
      metric2 %in% c(
        "AIDS",                         
        "Syphilis",                    
        "Gonorrhea",                    
        "Hepatitis B",                 
        "Hepatitis C",                 
        "Hepatitis D",                  
        "Viral Hepatitis",              
        "Hepatitis (Unspecified)",      
        "Hepatitis"                     
      ) ~ "Sexual and Blood-borne",
      
      metric2 %in% c(
        "Dengue Fever",                
        "Malaria",                     
        "Japanese Encephalitis"         
      ) ~ "Vector-borne",
      
      metric2 %in% c(
        "Brucellosis",                 
        "Rabies",                       
        "Hemorrhagic Fever",            
        "Anthrax"                       
      ) ~ "Zoonotic",
      
      metric2 %in% c(
        "Schistosomiasis",              
        "Leptospirosis",                
        "Neonatal Tetanus"              
      ) ~ "Other Transmission",
      
      TRUE ~ "Uncategorized"
    )
  )

rank_data_long <- rank_data %>% 
  group_by(time) %>%
  arrange(desc(incidence_rate)) %>%
  mutate(rank = row_number()) %>%
  ungroup()

rank_data_wider <- rank_data_long %>%
  pivot_wider(id_cols = c(metric2,transmission_group),
              names_from = c(time),
              values_from = c(incidence,death,incidence_rate,death_rate,rank)) 


change_data_calculated <- rank_data_wider %>%
  mutate(
    pct_change_incidence_2019 = case_when(
      incidence_2004 == 0 & incidence_2019 == 0 ~ 0,      
      incidence_2004 == 0 & incidence_2019 > 0  ~ 100,    
      TRUE ~ ((incidence_2019 - incidence_2004) / incidence_2004) * 100
    ),
    
    pct_change_incidence_rate_2019 = case_when(
      incidence_rate_2004 == 0 & incidence_rate_2019 > 0 ~ 100,
      incidence_rate_2004 == 0 ~ 0,
      TRUE ~ ((incidence_rate_2019 - incidence_rate_2004) / incidence_rate_2004) * 100
    ),
    
    pct_change_incidence_2023 = case_when(
      incidence_2019 == 0 & incidence_2023 > 0 ~ 100,
      incidence_2019 == 0 ~ 0,
      TRUE ~ ((incidence_2023 - incidence_2019) / incidence_2019) * 100
    ),
    pct_change_incidence_rate_2023 = case_when(
      incidence_rate_2019 == 0 & incidence_rate_2023 > 0 ~ 100,
      incidence_rate_2019 == 0 ~ 0,
      TRUE ~ ((incidence_rate_2023 - incidence_rate_2019) / incidence_rate_2019) * 100
    ),
    
    pct_change_incidence_2023_2004 = case_when(
      incidence_2004 == 0 & incidence_2023 > 0 ~ 100,
      incidence_2004 == 0 ~ 0,
      TRUE ~ ((incidence_2023 - incidence_2004) / incidence_2004) * 100
    ),
    pct_change_incidence_rate_2023_2004 = case_when(
      incidence_rate_2004 == 0 & incidence_rate_2023 > 0 ~ 100,
      incidence_rate_2004 == 0 ~ 0,
      TRUE ~ ((incidence_rate_2023 - incidence_rate_2004) / incidence_rate_2004) * 100
    )
  ) %>%
  mutate(
    rank_change_type_2019 = case_when(
      rank_2019 < rank_2004 ~ "up",
      rank_2019 > rank_2004 ~ "down",
      TRUE~ "stable" ),
    rank_change_type_2023 = case_when(
      rank_2023 < rank_2019 ~ "up",
      rank_2023 > rank_2019 ~ "down",
      TRUE~ "stable"  
    )
  )

my_color_palette <- c(
  "Sexual and Blood-borne" = "#F8766D", 
  "Fecal-Oral" = "#7CAE00",   
  "Vector-borne" = "#00BFC4",            
  "Respiratory" = "#C77CFF",             
  "Zoonotic" = "#FFB600",     
  "Other Transmission"="#1F77a1"
)


my_linetype_palette <- c(
  "up" = "solid", 
  "down" = "dashed",
  "stable" = "dashed"
)

pos_2004  <- 0    
pos_2019  <- 5    
pos_pct_1 <- 14.4  
pos_2023  <- 20  
pos_pct_2 <- 26.3    
pos_pct_3 <- 29.55

ranked_data_final <- rank_data_long %>%
  mutate(
    x_pos = case_when(
      time == 2004 ~ pos_2004,
      time == 2019 ~ pos_2019,
      time == 2023 ~ pos_2023
    )
  )

segment_04_19 <- change_data_calculated %>%
  select(metric2, transmission_group, rank_2004, rank_2019, rank_change_type_2019) %>%
  mutate(
    x_start = pos_2004,
    x_end = pos_2019
  )


segment_19_23 <- change_data_calculated %>%
  select(metric2, transmission_group, rank_2019, rank_2023, rank_change_type_2023) %>%
  mutate(
    x_start = pos_pct_1,
    x_end = pos_2023
  )


title_data <- data.frame(
  x = c(pos_2004, pos_2019, pos_pct_1,pos_2023, pos_pct_2,pos_pct_3),
  y = -1,
  label = c("Disease 2004", "Disease 2019",
            "Percentage change in\nincidence per 100000\n(2004-2019)", 
            "Disease 2023",
            "Percentage change in\nincidence per 100000\n(2019-2023)",
            "Percentage change in\nincidence per 100000\n(2004-2023)"),
  hjust_val = c(1, 0, 1, 0, 0, 0),
  x_nudge = c(0, 0, 0, 0, 0, 1)
)


p_final <- ggplot() +
  
  
  geom_segment(data = segment_04_19,
               aes(x = x_start, xend = x_end, 
                   y = rank_2004, yend = rank_2019,
                   color = transmission_group, 
                   linetype = rank_change_type_2019),
               size = 0.8, alpha = 0.7) +
  
 
  geom_segment(data = segment_19_23,
               aes(x = x_start, xend = x_end, 
                   y = rank_2019, yend = rank_2023,
                   color = transmission_group, 
                   linetype = rank_change_type_2023),
               size = 0.8, alpha = 0.7) +
  
 
  geom_textbox(
    data = ranked_data_final %>% filter(time == 2004),
    aes(x = x_pos, y = rank, 
        label = paste0(rank, "\\. ", metric2, " (", round(incidence_rate, 2), ")"),
        fill = transmission_group),
    hjust = 1, 
    width = unit(6, "cm"),
    height = unit(0.8, "cm"),
    size = 3.2, 
    color = "black",
    box.size = 0.2, 
    box.r = unit(0, "pt"),
    halign = 0, 
    valign = 0.5,
    key_glyph = "polygon"
  ) +
  
 
  geom_textbox(
    data = ranked_data_final %>% filter(time == 2019),
    aes(x = x_pos, y = rank, 
        label = paste0(rank, "\\. ", metric2, " (", round(incidence_rate, 2), ")"),
        fill = transmission_group),
    hjust = 0,
    width = unit(6, "cm"),
    height = unit(0.8, "cm"),
    size = 3.2, 
    color = "black", 
    box.size = 0.2, 
    box.r = unit(0, "pt"),
    halign = 0,
    valign = 0.5,
    key_glyph = "polygon"
  ) +
  
 
  geom_textbox(
    data = change_data_calculated,
    aes(x = pos_pct_1, y = rank_2019, 
        label = paste0(round(pct_change_incidence_2019, 1), "%"),
        fill = transmission_group),
    hjust = 1,
    width = unit(3, "cm"), 
    height = unit(0.8, "cm"),
    size = 3, 
    color = "black", 
    box.size = 0.2, 
    box.r = unit(0, "pt"),
    halign = 0.5,
    valign = 0.5,
    key_glyph = "polygon"
  )+
  

  geom_textbox(
    data = ranked_data_final %>% filter(time == 2023),
    aes(x = x_pos, y = rank, 
        label = paste0(rank, "\\. ", metric2, " (", round(incidence_rate, 2), ")"),
        fill = transmission_group),
    hjust = 0,
    width = unit(6, "cm"),
    height = unit(0.8, "cm"),
    size = 3.2, 
    color = "black",
    box.size = 0.2, 
    box.r = unit(0, "pt"),
    halign = 0,
    valign = 0.5,
    key_glyph = "polygon"
  ) +
  
  
  geom_textbox(
    data = change_data_calculated,
    aes(x = pos_pct_2, y = rank_2023, 
        label = paste0(round(pct_change_incidence_2023, 1), "%"),
        fill = transmission_group),
    hjust = 0,
    width = unit(3, "cm"), 
    height = unit(0.8, "cm"),
    size = 3, 
    color = "black",
    box.size = 0.2,
    box.r = unit(0, "pt"),
    halign = 0.5, 
    valign = 0.5,
    key_glyph = "polygon"
  ) +
  
 
  geom_textbox(
    data = change_data_calculated,
    aes(x = pos_pct_3, y = rank_2023, 
        label = paste0(round(pct_change_incidence_rate_2023_2004, 1), "%"),
        fill = transmission_group),
    hjust = 0,
    width = unit(3, "cm"), 
    height = unit(0.8, "cm"),
    size = 3, 
    color = "black",
    box.size = 0.2,
    box.r = unit(0, "pt"),
    halign = 0.5, 
    valign = 0.5,
    key_glyph = "polygon"
  ) +
  
  geom_text(data = title_data, 
            aes(x = x, y = y, label = label, hjust = hjust_val,nudge_x = x_nudge),
            
            fontface = "bold", size = 4) +
  
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


ggsave("****")



# fig5 --------------------------------------------------------------------


rm (list=(ls()))


china_IBD <- fread("***/GBD_China.csv")


age1 <- c("0-14 years","15-49 years","50-69 years","70+ years") 


infication_GBD <- china_IBD %>% 
  filter(
    cause %in% c("Acute hepatitis A" ,"Acute hepatitis B", "Acute hepatitis C","Acute hepatitis E",
                 "HIV/AIDS","Measles" ,"Rabies","Dengue","Typhoid fever","Paratyphoid fever","Pertussis",
                 "Diphtheria","Gonococcal infection","Syphilis","Malaria") &
      metric == "Rate" &
      measure %in% c("Prevalence","DALYs (Disability-Adjusted Life Years)"),
    age %in% age1 &
      year=="2019"&
      sex=="Both"
  )

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


data_map <- infication_GBD %>% 
  mutate(
    plot_order=case_when(
    
      age== "0-14 years" & measure=="Prevalence"  ~ "p1",
      age== "15-49 years" & measure=="Prevalence"  ~ "p2",
      age== "50-69 years" & measure=="Prevalence"  ~ "p3",
      age== "70+ years" & measure=="Prevalence"  ~ "p4",
   
      age== "0-14 years" & measure== "DALYs (Disability-Adjusted Life Years)"  ~ "p5",
      age== "15-49 years" & measure== "DALYs (Disability-Adjusted Life Years)"  ~ "p6",
      age== "50-69 years" & measure== "DALYs (Disability-Adjusted Life Years)"  ~ "p7",
      age== "70+ years" & measure== "DALYs (Disability-Adjusted Life Years)"  ~ "p8",
      
    )
  )

my_colors <- c(
  "AIDS" = "#0c77b8",
  "Gonorrhea" ="#1ab7c1",
  "Hepatitis B"="#c775ab",
  "Hepatitis A"="#36a048",
  "Syphilis"="#f58030",
  "Hepatitis C"="#690997",
  "Hepatitis E"="#8e5153",
  "Measles"="#86a2ca",
  "Rabies" = "#97243d",
  "Dengue Fever" ="#aab33e" ,
  "Typhoid Fever"="#6a6698",
  "Paratyphoid Fever"="#f9c3bf",
  "Pertussis"="#e92a2a" ,
  "Diphtheria"="#86a2ca",
  "Malaria" ="#0aa17e"
  
)

stat_data <- data_map %>% arrange(age, measure,desc( val)) %>% 
  select(cause, measure,age,val,lower ,upper ) %>%  
  mutate(across(where(is.numeric), ~round(., 2)))

plot_list <- list()
plot_ids <- paste0("p", 1:8)

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
      axis.text.y = element_text(size = 8, color = "black"),  
      axis.ticks.y = element_line(color ="#7F7F7F"),         
     
      
      panel.grid.major.y = element_line(color = "grey80", linewidth = 0.65),
      
      panel.grid.major.x = element_line(color = "grey80", linewidth = 0.65),
      legend.position = "bottom",              
      legend.title = element_blank(),
      legend.direction = "horizontal", 
      legend.box.spacing = unit(0.2, "cm"),     
      legend.key.size = unit(0.4, "cm"),        
      legend.text = element_text(size = 8)     
    )+
    guides(fill = guide_legend(nrow = 3))      
  
  plot_list[[id]] <- p
}

col_title_texts <- c(
  "0-14 years old", "15-49 years old", "50-69 years old", "70+ years old"
)

row_title_texts <- c(
  "Rate for pervalence(per 100,000)","Rate for DALYs(per 100,000)"
)

col_title_plots <- list()
for (i in 1:4) {
  col_title_plots[[i]] <- ggplot() +
    annotate(
      "text",
      x = 0.5, y = 0.5, 
      label = col_title_texts[i],
      hjust = 0.5, vjust = 0.5,
      color = "#0C0B0B",    
      size = 6,              
      fontface = "bold"
    ) +
    theme_void() 
}

row_title_plots <- list()
for (i in 1:2) {
  row_title_plots[[i]] <- ggplot() +
    annotate(
      "text",
      x = 0.5, y = 0.5,
      label = row_title_texts[i],
      hjust = 0.5, vjust = 0.5,
      color = "#0C0B0B",
      size = 6,
      fontface = "bold",
      angle = 90
    ) +
    theme_void()  
}
ts_matrix <- wrap_plots(plot_list, ncol = 4, byrow = TRUE) 
pty_plot <- ggplot() + theme_void()
design <- "
  DB
  CA
"
final_plot_layout <- wrap_plots(
  D = empty_plot,
  B = wrap_plots(col_title_plots, nrow = 1),
  C = wrap_plots(row_title_plots, ncol = 1) + 
    plot_layout(heights = c(1, 1)),  
  A = plots_matrix,
  design = design,
  widths = c(0.1, 1),
  heights = c(0.3, 2)
)

ggsave("*****")






