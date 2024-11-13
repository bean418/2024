library(ggplot2)

# home pc dir: 'C:\\Users\\Admin\\Desktop\\gpsig\\data'
# lab. pc dir: 'C:\\Users\\imb2a\\Desktop\\study\\GPSig\\dat\\co2_gas'

rm(list=ls())
setwd('C:\\Users\\imb2a\\Desktop\\study\\GPSig\\dat\\co2_gas')
getwd()
dir()

df_co2 <- read.csv("CO2.csv")
df_gas <- read.csv("GAS.csv")

head(df_co2)

# str(df_co2) -> 데이터가 문자열이다.
# numeric으로 바꿈, 첫 번째 열(시간) 제외, 첫 번째 행(SENSOR) 제외외
df_co2 <- as.data.frame(lapply(df_co2[2:137, 2:81],
                               as.numeric))
df_gas <- as.data.frame(lapply(df_gas[2:137, 2:81],
                               as.numeric))

# 열 이름 재설정
idx <- 1
for(i in 1:5){
  for(j in 1:16){
    colnames(df_co2)[idx] <- paste0("BD", i, "_SEN", j)
    idx <- idx+1
  }
}
idx <- 1
for(i in 1:5){
  for(j in 1:16){
    colnames(df_gas)[idx] <- paste0("BD", i, "_SEN", j)
    idx <- idx+1
  }
}

#GAS 센서마다 차이가 있는가
# 데이터 준비
board = 5
start = 1
end = 16
# plot
{
  sensors <- paste0("BD",board,"_SEN", start:end)
  colors <- c("gray13", "blue", "green", "skyblue", "red",
              "aquamarine", "skyblue4", "darkolivegreen",
              "darkolivegreen1", "orange4", "navy", "darkgreen",
              "turquoise3", "yellow3", "gray", "black")
  labels <- paste0("BD",board,"_S", start:end)
  
  # 기본 플롯 생성
  p <- ggplot(df_gas, aes(x = 1:136))
  
  # 반복문으로 geom_line 추가
  for (i in seq_along(sensors)) {
    p <- p + geom_line(aes_string(y = sensors[i], color = shQuote(labels[i])))
  }
  
  # 나머지 설정 추가
  title <- paste0("Gas, diff. between each sensors in board", board)
  p <- p + 
    labs(title = title,
         x = 'Time(2min)', 
         y = 'value',
         color = 'Substance') +
    scale_color_manual(values = setNames(colors, unique(labels)))
  
  # 플롯 출력
  print(p)
}


#GAS 보드마다 차이가 있는가
{
  ggplot() +
    geom_line(data = df_gas, aes(x = 1:136, y = BD1_SEN5, color = "BD1-5")) +
    geom_line(data = df_gas, aes(x = 1:136, y = BD2_SEN5, color = "BD2-5")) +
    geom_line(data = df_gas, aes(x = 1:136, y = BD3_SEN5, color = "BD3-5")) +
    geom_line(data = df_gas, aes(x = 1:136, y = BD4_SEN5, color = "BD4-5")) +
    geom_line(data = df_gas, aes(x = 1:136, y = BD5_SEN5, color = "BD5-5")) +
    labs(title = 'Gas, diff. between each board in sensor5',
         x = 'Time(2min)',
         y = 'value',
         color = 'Substance') +
    scale_color_manual(values = c("BD1-5" = "red",
                                  "BD2-5" = "blue",
                                  "BD3-5" = "green",
                                  "BD4-5" = "skyblue",
                                  "BD5-5" = "purple"))
}

#CO2 센서마다 차이가 있는가
# 데이터 준비
board = 5
start = 1
end = 16
# plot
{
  sensors <- paste0("BD",board,"_SEN", start:end)
  colors <- c("gray13", "blue", "green", "skyblue", "red",
              "aquamarine", "skyblue4", "darkolivegreen",
              "darkolivegreen1", "orange4", "navy", "darkgreen",
              "turquoise3", "yellow3", "gray", "black")
  labels <- paste0("BD",board,"_S", start:end)
  
  # 기본 플롯 생성
  p <- ggplot(df_co2, aes(x = 1:136))
  
  # 반복문으로 geom_line 추가
  for (i in seq_along(sensors)) {
    p <- p + geom_line(aes_string(y = sensors[i], color = shQuote(labels[i])))
  }
  
  # 나머지 설정 추가
  title <- paste0("CO2, diff. between each sensors in board", board)
  p <- p + 
    labs(title = title,
         x = 'Time(2min)', 
         y = 'value',
         color = 'Substance') +
    scale_color_manual(values = setNames(colors, unique(labels)))
  
  # 플롯 출력
  print(p)
}

#CO2 보드마다 차이가 있는가
{
  board = 1:5
  sensors <- paste0("BD",board,"_SEN5")
  colors <- c("red", "blue", "green", "skyblue", "purple")
  labels <- paste0("BD",board,"_S", 5)
  
  # 기본 플롯 생성
  p <- ggplot(df_co2, aes(x = 1:136))
  
  # 반복문으로 geom_line 추가
  for (i in seq_along(sensors)) {
    p <- p + geom_line(aes_string(y = sensors[i], color = shQuote(labels[i])))
  }
  
  # 나머지 설정 추가
  title <- paste0("CO2, diff. between each sensors in board", board)
  p <- p + 
    labs(title = title,
         x = 'Time(2min)', 
         y = 'value',
         color = 'Substance') +
    scale_color_manual(values = setNames(colors, unique(labels)))
  
  # 플롯 출력
  print(p)
}