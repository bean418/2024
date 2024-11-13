library(dplyr)
library(ggplot2)

rm(list=ls())

files <- paste0("C:/Users/imb2a/Desktop/study/GPSig/dat/sensor", 1:4, ".csv")

df_list <- list()

for (i in 1:4) {
  df_list[[i]] <- read.csv(files[i])
  df_list[[i]]$Unnamed..0 <- NULL
}

names(df_list) <- paste0("df", 1:4)

dim(df1)

df1 <- df_list$df1
df2 <- df_list$df2
df3 <- df_list$df3
df4 <- df_list$df4


# X는 시간, 열은 전압?
ggplot() +
  geom_line(data = df1, aes(x = 1:2254, y = X35, color = "sensor1")) +
  geom_line(data = df2, aes(x = 1:2254, y = X35, color = "sensor2")) +
  geom_line(data = df3, aes(x = 1:2254, y = X35, color = "sensor3")) +
  geom_line(data = df4, aes(x = 1:2254, y = X35, color = "sensor4")) +
  labs(title = 'X35에서의 시계열 분포',
       x = 'Time(1~2254)',
       y = 'X35',
       color = 'Substance') +
  scale_color_manual(values = c("sensor1" = "blue",
                                "sensor2" = "red",
                                "sensor3" = "green4",
                                "sensor4" = "purple"))

# Preprocessing
# 일단, ppm이 10일 때 분류가 되는지 확인해보려고 함.

df1_10ppm <- df1 %>% filter(label.ppm.==10)
df2_10ppm <- df2 %>% filter(label.ppm.==10)
df3_10ppm <- df3 %>% filter(label.ppm.==10)
df4_10ppm <- df4 %>% filter(label.ppm.==10)

df1_10ppm$label.ppm. <- NULL
df2_10ppm$label.ppm. <- NULL
df3_10ppm$label.ppm. <- NULL
df4_10ppm$label.ppm. <- NULL


ggplot() +
  geom_line(data = df1_10ppm, aes(x = 1:240, y = X45, color = "sensor1")) +
  geom_line(data = df2_10ppm, aes(x = 1:240, y = X45, color = "sensor2")) +
  geom_line(data = df3_10ppm, aes(x = 1:240, y = X45, color = "sensor3")) +
  geom_line(data = df4_10ppm, aes(x = 1:240, y = X45, color = "sensor4")) +
  labs(title = 'sensors',
       x = 'TimeElapsed (sec)',
       y = '45th DC',
       color = 'Substance') +
  scale_color_manual(values = c("sensor1" = "blue",
                                "sensor2" = "red",
                                "sensor3" = "green4",
                                "sensor4" = "purple"))

setwd("C:/Users/imb2a/Desktop/study/GPSig/dat")
getwd()

df1_10ppm$sensor <- 1
df2_10ppm$sensor <- 2
df3_10ppm$sensor <- 3
df4_10ppm$sensor <- 4

df_train_10ppm <- rbind(df1_10ppm, df2_10ppm, df3_10ppm, df4_10ppm)

write.csv(df_train_10ppm, "df_train_10ppm.csv")
