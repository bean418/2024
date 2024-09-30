library(readxl)
library(ggplot2)
setwd("/Users/bean418/PCRL")
dir()

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
    numeric_data <- as.numeric(unlist(at[2:nrow(at), -2]));numeric_data
    df_ac = as.data.frame(matrix(numeric_data,
                                 byrow = F,
                                 nrow=nr_at))
  } else{
    numeric_data <- as.numeric(unlist(at[2:nrow(at), ]));numeric_data
    df_ac = as.data.frame(matrix(numeric_data,
                                 byrow = F,
                                 nrow=nr_at))
  }
  
  # NH3
  if(ncol(nh3)==9){
    numeric_data <- as.numeric(unlist(nh3[2:nrow(nh3), -2]));numeric_data
    df_nh3 = as.data.frame(matrix(numeric_data,
                                  byrow = F,
                                  nrow=nr_nh3))
  } else{
    numeric_data <- as.numeric(unlist(nh3[2:nrow(nh3), ]));numeric_data
    df_nh3 = as.data.frame(matrix(numeric_data,
                                  byrow = F,
                                  nrow=nr_nh3))
  }
  
  
  # rounding
  df_ac[, c(1, 3, 5, 7)] <- lapply(df_ac[, c(1, 3, 5, 7)], function(x) (round(x / 100) * 100))
  df_nh3[, c(1, 3, 5, 7)] <- lapply(df_nh3[, c(1, 3, 5, 7)], function(x) (round(x / 100) * 100))
  
  
  
  # ploting
  temp = 300
  for(i in seq(from=1, to=ncol(df_ac), by=2)){
    # cat("현재 i는", i, "현재 temp는", temp,'\n')
    x_ac=c()
    x_nh3=c()
    x_ac=unique(df_ac[,i])
    
    
    x_nh3=unique(df_nh3[,i])
    
    # 3만 이하의 값 제거
    x_ac=x_ac[x_ac >= 30000]
    x_nh3=x_nh3[x_nh3 >= 30000]
    
    y_ac=c()
    y_nh3=c()
    cnt = 1
    for(sec in x_ac){
      y_ac[cnt] = mean(df_ac[,i+1][df_ac[,i] == sec])
      
      cnt = cnt + 1
    }
    cnt = 1
    for(sec in x_nh3){
      y_nh3[cnt] = mean(df_nh3[,i+1][df_nh3[,i] == sec])
      cnt = cnt + 1
    }
    
    gg_ac = data.frame(x=x_ac,y=y_ac)
    gg_nh3 = data.frame(x=x_nh3,y=y_nh3)
    
    ggplot() +
      geom_line(data = gg_ac, aes(x = x, y = y, color = "Acetone")) +
      geom_line(data = gg_nh3, aes(x = x, y = y, color = "NH3")) +
      labs(title = 'Acetone vs NH3',
           x = 'TimeElapsed (sec)',
           y = 'Resistance (Ohm)',
           color = 'Substance') +
      scale_color_manual(values = c("Acetone" = "blue", "NH3" = "red")) +
      geom_vline(xintercept = seq(from=30000,
                                  to=max(x_ac,x_nh3),
                                  by=4500), 
                 color = "plum", 
                 size = 0.7) +
      geom_vline(xintercept = seq(from=31200,
                                  to=max(x_ac,x_nh3),
                                  by=4500), 
                 color = "skyblue", 
                 linetype = "dashed", 
                 size = 0.7)
    
    filename <- paste0("./plts/M", h, "_chip_",
                       temp, "oC.png")
    ggsave(filename = filename)
    
    i = i+1
    temp = temp+50
  }
}
