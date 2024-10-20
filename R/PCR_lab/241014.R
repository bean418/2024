library(dplyr)
library(ggplot2)

rm(list=ls())

files <- paste0("C:/Users/Admin/Desktop/sensor", 1:4, ".csv")

df_list <- list()

for (i in 1:4) {
  df_list[[i]] <- read.csv(files[i])
  df_list[[i]]$Unnamed..0 <- NULL
}

names(df_list) <- paste0("df", 1:4)

df1 <- df_list$df1
df2 <- df_list$df2
df3 <- df_list$df3
df4 <- df_list$df4


# X는 시간, 열은 전압?
ggplot() +
  geom_line(data = df1, aes(x = 1:2254, y = X45, color = "sensor1")) +
  geom_line(data = df2, aes(x = 1:2254, y = X45, color = "sensor2")) +
  geom_line(data = df3, aes(x = 1:2254, y = X45, color = "sensor3")) +
  geom_line(data = df4, aes(x = 1:2254, y = X45, color = "sensor4")) +
  labs(title = 'sensors',
       x = 'TimeElapsed (sec)',
       y = 'responses',
       color = 'Substance') +
  scale_color_manual(values = c("sensor1" = "blue",
                                "sensor2" = "red",
                                "sensor3" = "green4",
                                "sensor4" = "purple"))

Preprocessing
