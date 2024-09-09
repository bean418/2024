library(readxl)
setwd("C:/Users/Admin/Downloads")
dir()

for(h in 1:9){
  sheet = sprintf("M%d chip", h)
  
  at = read_excel("Resistance_Acetone.xlsx", sheet=sheet)
  nh3 = read_excel("Resistance_NH3.xlsx", sheet=sheet)
  head(at)
  
  # numeric
  # Acetone
  nr_at = nrow(at)-1
  rn_at = rownames(at);rn_at
  cn_at = colnames(at);cn_at
  ncol(at)
  # NH3
  nr_nh3 = nrow(nh3)-1
  rn_nh3 = rownames(nh3)
  cn_nh3 = colnames(nh3)
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
    cat("현재 i는", i, "현재 temp는", temp)
    x_ac=c()
    x_nh3=c()
    x_ac=unique(df_ac[,i])
    x_nh3=unique(df_nh3[,i])
    
    y_ac=c()
    y_nh3=c()
    cnt = 1
    for(j in x_ac){
      y_ac[cnt] = mean(df_ac[,i+1][df_ac[,i] == j])
      
      cnt = cnt + 1
    }
    cnt = 1
    for(j in x_nh3){
      y_nh3[cnt] = mean(df_nh3[,i+1][df_nh3[,i] == j])
      cnt = cnt + 1
    }
    filename <- sprintf("M%d chip_%doC.png", h, temp)
    png(filename=filename,width=6000,height=4000,res=500)
    plot(x_ac, y_ac,
         xlim=c(min(x_ac,x_nh3), max(x_ac,x_nh3)),
         ylim=c(min(y_ac,y_nh3), max(y_ac,y_nh3)),
         type='l',
         col='black', lwd=2,
         main='Acetone vs NH3',
         xlab='TimeElapsed (sec)',
         ylab='Resistance (Ohm)'
    )
    points(x_nh3, y_nh3, type='l',col='red', lwd=2)
    legend("topright", legend=c("Acetone", "NH3"),
           col=c("black", "red"), lty=1, lwd=2)
    
    i = i+1
    temp = temp+50
    dev.off()
  }
}