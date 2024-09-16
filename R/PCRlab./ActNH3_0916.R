library(readxl)
library(ggplot2)
setwd("/Users/bean418/PCRL")
dir()
# 함수
{
# indexing
index_data <- function(x, start, interval, range_size) {
  max_value <- max(x)
  indices <- logical(length(x))
  
  current_start <- start
  while(current_start <= max_value) {
    indices <- indices | (x >= current_start & x < (current_start + range_size))
    current_start <- current_start + interval
  }
  
  x[!indices] <- NA
  return(x)
}

# numeric
convert_to_numeric <- function(df) {
  if (ncol(df) == 9) {
    df <- df[, -2]  # 2번째 열 제거
  }
  return(as.data.frame(lapply(df[2:nrow(df), ],
                              as.numeric)))
}

# rounding
round_columns <- function(df, columns) {
  df[, columns] <- lapply(df[, columns], function(x) (round(x / 100) * 100))
  return(df)
}

# below30000_toNA
below30000_toNA <- function(x) {
  x[x < 30000] <- NA
  return(x)
}

# end of func. part
}

# initialization
col = c(1,3,5,7)

# Mh chip
# i = 1 -> 300o, i = 3 -> 350o, ...
for(h in 1:9){
  sheet = paste0("M",h," chip")
  
  at = read_excel("Resistance_Acetone.xlsx", sheet=sheet)
  nh3 = read_excel("Resistance_NH3.xlsx", sheet=sheet)
  
  # preprocessing
  df_at <- convert_to_numeric(at)
  df_nh3 <- convert_to_numeric(nh3)
  
  df_at <- round_columns(df_at, col)
  df_nh3 <- round_columns(df_nh3, col)
  
  df_at[, col] <- lapply(df_at[, col],
                         replace_30000toNA)
  df_nh3[, col] <- lapply(df_nh3[, col],
                         replace_30000toNA)
  
  # ploting
  temp = 300
  for(i in seq(from=1, to=ncol(df_at), by=2)){
    # cat("현재 i는", i, "현재 temp는", temp,'\n')
    x_at=unique(df_at[,i])
    x_nh3=unique(df_nh3[,i])
    
    x_at=x_at[!is.na(x_at)]
    x_nh3=x_nh3[!is.na(x_nh3)]
    
    x_at=x_at[x_at >= 30000]
    x_nh3=x_nh3[x_nh3 >= 30000]
    
    y_at <- tapply(df_at[,i+1], df_at[,i],
                   function(x) mean(x,na.rm=T))
    y_nh3 <- tapply(df_nh3[,i+1], df_nh3[,i],
                    function(x) mean(x,na.rm=T))
    
    gg_at = data.frame(x=x_at,y=y_at)
    gg_nh3 = data.frame(x=x_nh3,y=y_nh3)
    
    ggplot() +
      geom_line(data = gg_at, aes(x = x, y = y, color = "Acetone")) +
      geom_line(data = gg_nh3, aes(x = x, y = y, color = "NH3")) +
      labs(title = 'Acetone vs NH3',
           x = 'TimeElapsed (sec)',
           y = 'Resistance (Ohm)',
           color = 'Substance') +
      scale_color_manual(values = c("Acetone" = "blue", "NH3" = "red"))
    
    
    filename <- paste0("./plts/M", h, "_chip_",
                       temp, "oC.png")
    ggsave(filename = filename)
    
    temp = temp+50
  }
}

