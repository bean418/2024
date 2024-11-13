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

#plot
plot(1:136, df_co2$BD1_SEN5)
plot(1:136, df_gas$BD1_SEN5)

#GAS 보드마다 차이가 있는가
ggplot() +
  geom_line(data = df_gas, aes(x = 1:136, y = BD1_SEN5, color = "BD1-5")) +
  geom_line(data = df_gas, aes(x = 1:136, y = BD2_SEN5, color = "BD2-5")) +
  geom_line(data = df_gas, aes(x = 1:136, y = BD3_SEN5, color = "BD3-5")) +
  geom_line(data = df_gas, aes(x = 1:136, y = BD4_SEN5, color = "BD4-5")) +
  geom_line(data = df_gas, aes(x = 1:136, y = BD5_SEN5, color = "BD5-5")) +
  labs(title = 'gas bd 1~5',
       x = 'Time(2min)',
       y = 'value',
       color = 'Substance') +
  scale_color_manual(values = c("BD1-5" = "red",
                                "BD2-5" = "blue",
                                "BD3-5" = "green",
                                "BD4-5" = "skyblue",
                                "BD5-5" = "purple"))

#preprocessing2
'''
사용할 인스턴스
Board 2, 3, 4에서 sensor 4~13까지
(중앙에 있는 보드와 센서가 안정적인 것으로 추측)
train과 test에 각각 15개의 인스턴스가 들어갈 예정.
'''

#co2
df_co2_2 <- c()
for(i in 2:4){
  for(j in 4:13){
    bd_sen <- paste0("BD", i, "_SEN", j)
    df_co2_2 <- c(df_co2_2,
                      df_co2[, bd_sen])
  }
}

df_co2_train <- df_co2_2[1:(length(df_co2_2)/2)]
df_co2_test <- df_co2_2[(length(df_co2_2)/2 + 1):length(df_co2_2)]

#gas
df_gas_2 <- c()
for(i in 2:4){
  for(j in 4:13){
    bd_sen <- paste0("BD", i, "_SEN", j)
    df_gas_2 <- c(df_gas_2,
                  df_gas[, bd_sen])
  }
}

df_gas_train <- df_gas_2[1:(length(df_gas_2)/2)]
df_gas_test <- df_gas_2[(length(df_gas_2)/2 + 1):length(df_gas_2)]

#plot
ggplot() +
  geom_line(aes(x = 1:2040, y = df_co2_train, color = "co2_train")) +
  geom_line(aes(x = 1:2040, y = df_co2_test, color = "co2_test")) +
  geom_line(aes(x = 1:2040, y = df_gas_train, color = "gas_train")) +
  geom_line(aes(x = 1:2040, y = df_gas_test, color = "gas_test")) +
  labs(title = 'co2 vs. gas',
       x = 'Time(2min)',
       y = 'value',
       color = 'Substance') +
  scale_color_manual(values = c("co2_train" = "red",
                                "co2_test" = "blue",
                                "gas_train" = "green",
                                "gas_test" = "skyblue"))

df_train <- data.frame(dim_0 = c(df_co2_train, df_gas_train))
df_test <- data.frame(dim_0 = c(df_co2_test, df_gas_test))

write.csv(df_train, "DF_train.csv")
write.csv(df_test, "DF_test.csv")
