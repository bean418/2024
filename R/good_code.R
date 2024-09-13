library(readxl)
library(ggplot2)
library(dplyr)
library(purrr)
library(tidyr)

setwd("/Users/bean418/PCRL")

# 데이터 처리 함수
process_data <- function(data, threshold = 30000) {
  data %>%
    select(-2) %>%
    mutate(across(c(1, 3, 5, 7), ~round(. / 100) * 100)) %>%
    pivot_longer(cols = everything(), names_to = c(".value", "set"), names_pattern = "(V\\d)(\\d)") %>%
    group_by(V1, set) %>%
    summarise(mean_value = mean(V2), .groups = "drop") %>%
    filter(V1 >= threshold)
}

# 그래프 생성 함수
create_plot <- function(ac_data, nh3_data, temp, h) {
  ggplot() +
    geom_line(data = ac_data, aes(x = V1, y = mean_value, color = "Acetone")) +
    geom_line(data = nh3_data, aes(x = V1, y = mean_value, color = "NH3")) +
    labs(title = 'Acetone vs NH3',
         x = 'TimeElapsed (sec)',
         y = 'Resistance (Ohm)',
         color = 'Substance') +
    scale_color_manual(values = c("Acetone" = "blue", "NH3" = "red"))
}

# 메인 처리 루프
for(h in 1:9) {
  sheet <- paste0("M", h, " chip")
  
  at <- read_excel("Resistance_Acetone.xlsx", sheet = sheet)
  nh3 <- read_excel("Resistance_NH3.xlsx", sheet = sheet)
  
  df_ac <- process_data(at)
  df_nh3 <- process_data(nh3)
  
  temperatures <- seq(300, 700, by = 50)
  
  map2(split(df_ac, df_ac$set), split(df_nh3, df_nh3$set), 
       ~{
         temp <- temperatures[as.numeric(.x$set[1])]
         plot <- create_plot(.x, .y, temp, h)
         filename <- paste0("./plts/M", h, "_chip_", temp, "oC.png")
         ggsave(filename = filename, plot = plot)
       })
}