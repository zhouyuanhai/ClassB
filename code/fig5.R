# ==============================================================================
# Script Name: 05_figure4_polar.R
# Description: Generation of age-specific polar bar charts (Figure 5) 
#              comparing Prevalence and DALYs between 2004 and 2019.
# Author: Yuan-Hai Zhou & Yu-Hong Qin
# ==============================================================================


rm (list=(ls()))
# 1. Environment Setup ---------------------------------------------------------
rm(list = ls())

# Load required libraries
library(tidyverse)
library(readxl)
library(patchwork)
library(scales)
library(ggtext)

observed_data <- read.csv("****/original_data.csv")
pridictd_data <- read_excel("****/pridictd_data.xlsx")


# Clean predicted data
pridictd_data <- pridictd_data %>% 
  rename(incidence_rate=predicted_incidence_rate) %>% 
  select("metric", "time","incidence_rate")


# Clean observed data
same_data <- observed_data  %>%
  rename(metric=metric2) %>% 
  mutate(incidence_rate=ifelse(is.na(incidence_rate),0,incidence_rate)) %>% 
  mutate(group="observed")%>% select(-1)

# Bridge the data: Extract 2019 data to connect the line in the plot
start_point <- same_data %>% 
  filter(time == 2019)

pridictd_data2019 <- bind_rows(pridictd_data,start_point) %>% 
  mutate(group="pridict") %>% arrange(metric,time)

# Combine all datasets
merged_data <- bind_rows(pridictd_data2019,same_data) %>% 
  arrange(metric,time)

# Define biological/thematic order for diseases
metric_order <- c(
  "Hepatitis","Hepatitis A", "Hepatitis B","Hepatitis C","Hepatitis E" ,

  "Syphilis", "Gonorrhea" , "AIDS" , 

  "Pulmonary Tuberculosis","Pertussis", "Meningococcal Meningitis","Measles",               
  "Scarlet Fever" ,    

  "Malaria","Typhoid and Paratyphoid Fever", "Dysentery",

  "Brucellosis", "Rabies" ,"Hemorrhagic Fever"   , "Anthrax" , 
  "Dengue Fever" , "Japanese Encephalitis",
  "Leptospirosis" , "Neonatal Tetanus" , "Schistosomiasis" 
)

merged_data$metric <- factor(merged_data$metric,levels = metric_order)

# 4. Visualization -------------------------------------------------------------
col_one <- c("Hepatitis","Syphilis","Meningococcal Meningitis","Dysentery","Dengue Fever")

plot_list <- list()


for (diease in metric_order) {
  
  plot_data <- merged_data %>% filter(metric==diease)

  p <- ggplot()+
    geom_line(data=subset(plot_data,group=="observed"),aes(x=time,y=incidence_rate,linetype = group,color = group ))+
    
    geom_line(data=subset(plot_data,group=="pridict"),aes(x=time,y=incidence_rate,linetype = group,color = group ))+
    
    geom_vline(xintercept = 2019, linetype=2) +
    
    geom_point(data=subset(plot_data,group=="observed"),
               aes(x=time,y=incidence_rate,color = group ),
               size = 1)+
    
    geom_point(data=subset(plot_data,group=="pridict"),
               aes(x=time,y=incidence_rate,color = group ),
               size = 1)+
    
    
    scale_linetype_manual(
      values=c('observed'=1,
               "pridict"=2 
      )) +
    scale_color_manual(
      
      values = c('observed'="#4474b3",
                 "pridict"="#fc8d58")
    )+
    scale_x_continuous(breaks = seq(2004, 2023, by = 3))+
    labs(
      title = diease,
      x="Year",
      y="Rate of Incidence (per 100,000)"
    )+  
    theme(axis.line = element_line(color = "black"),  
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.title = element_blank() 
    )
  
  if (!diease %in% col_one ) {
    p <- p + theme(
      axis.title.y = element_blank()
    )
  }
  
  plot_list[[diease]] <- p
}

plots_matrix <- wrap_plots(plot_list, ncol = 5, byrow = TRUE)


final_plot_layout <- plots_matrix+plot_layout(
  guides = 'collect'
) &
  theme(legend.position = 'bottom')




