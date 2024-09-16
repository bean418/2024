library(readxl)
library(ggplot2)
setwd("/Users/bean418/PCRL")
dir()
#### 함수 ####
x <- seq(30000, 60600, by = 100);x
index_data(x, min(x), 4500, 1200)

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

# Mh chip
# i = 1 -> 300o, i = 3 -> 350o, ...
for(h in 1:9){
  sheet = paste0("M",h," chip")
  
  at = read_excel("Resistance_Acetone.xlsx", sheet=sheet)
  nh3 = read_excel("Resistance_NH3.xlsx", sheet=sheet)
  
  # numeric
  # Acetone
  nr_at = nrow(at)-1
  ncol(at)
  # NH3
  nr_nh3 = nrow(nh3)-1
  ncol(nh3)
  
  
  # Acetone
  if(ncol(at)==9){
    df_at <- as.data.frame(lapply(at[2:nrow(at), -2], as.numeric))
  } else{
    df_at <- as.data.frame(lapply(at[2:nrow(at), ], as.numeric))
  }
  
  # NH3
  if(ncol(nh3)==9){
    df_nh3 <- as.data.frame(lapply(nh3[2:nrow(nh3), -2], as.numeric))
  } else{
    df_nh3 <- as.data.frame(lapply(nh3[2:nrow(nh3), ], as.numeric))
  }
  
  
  # rounding
  df_at[, c(1, 3, 5, 7)] <- lapply(df_at[, c(1, 3, 5, 7)],
                                   function(x) (round(x / 100) * 100))
  df_nh3[, c(1, 3, 5, 7)] <- lapply(df_nh3[, c(1, 3, 5, 7)],
                                    function(x) (round(x / 100) * 100))
  
  
  
  
  
  # ploting
  temp = 300
  for(i in seq(from=1, to=ncol(df_at), by=2)){
    # cat("현재 i는", i, "현재 temp는", temp,'\n')
    x_at=unique(df_at[,i])
    x_nh3=unique(df_nh3[,i])
    
    y_at=c()
    y_nh3=c()
    cnt = 1
    for(sec in x_at){
      y_at[cnt] = mean(df_at[,i+1][df_at[,i] == sec])
      
      cnt = cnt + 1
    }
    cnt = 1
    for(sec in x_nh3){
      y_nh3[cnt] = mean(df_nh3[,i+1][df_nh3[,i] == sec])
      cnt = cnt + 1
    }
    
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
    
    
    filename <- paste0("./plts2/M", h, "_chip_",
                       temp, "oC.png")
    ggsave(filename = filename)
    
    i = i+1
    temp = temp+50
  }
}

