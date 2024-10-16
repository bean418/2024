library(dplyr)
library(ggplot2)

rm(list=ls())

setwd("C:/Users/imb2a/Desktop/study/GPSig/dat")
getwd()

files <- paste0("C:/Users/imb2a/Desktop/study/GPSig/dat/sensor", 1:4, ".csv")

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

# Preprocessing(in ppm10)
# 일단, ppm이 10일 때 분류가 되는지 확인해보려고 함.

df1_10ppm <- df1 %>% filter(label.ppm.==10)
df2_10ppm <- df2 %>% filter(label.ppm.==10)
df3_10ppm <- df3 %>% filter(label.ppm.==10)
df4_10ppm <- df4 %>% filter(label.ppm.==10)

df1_10ppm$label.ppm. <- NULL
df2_10ppm$label.ppm. <- NULL
df3_10ppm$label.ppm. <- NULL
df4_10ppm$label.ppm. <- NULL

df1_10ppm$sensor <- 1
df2_10ppm$sensor <- 2
df3_10ppm$sensor <- 3
df4_10ppm$sensor <- 4

n <- nrow(df1_10ppm)
k <- n/2

k

set.seed(1)
selected_indices <- sample(1:n, k, replace = FALSE)
selected_indices <- sort(selected_indices)

df1_10ppm_train <- df1_10ppm[selected_indices,]
df1_10ppm_test <- df1_10ppm[-selected_indices,]

set.seed(2)
selected_indices <- sample(1:n, k, replace = FALSE)
selected_indices <- sort(selected_indices)

df2_10ppm_train <- df2_10ppm[selected_indices,]
df2_10ppm_test <- df2_10ppm[-selected_indices,]

set.seed(3)
selected_indices <- sample(1:n, k, replace = FALSE)
selected_indices <- sort(selected_indices)

df3_10ppm_train <- df3_10ppm[selected_indices,]
df3_10ppm_test <- df3_10ppm[-selected_indices,]

set.seed(4)
selected_indices <- sample(1:n, k, replace = FALSE)
selected_indices <- sort(selected_indices)

df4_10ppm_train <- df4_10ppm[selected_indices,]
df4_10ppm_test <- df4_10ppm[-selected_indices,]

DF_10ppm_train <- rbind(df1_10ppm_train, df2_10ppm_train,
                        df3_10ppm_train, df4_10ppm_train)
DF_10ppm_test <- rbind(df1_10ppm_test, df2_10ppm_test,
                       df3_10ppm_test, df4_10ppm_test)

write.csv(DF_10ppm_train, "DF_10ppm_train.csv")
write.csv(DF_10ppm_test, "DF_10ppm_test.csv")

ggplot() +
  geom_line(data = df1_10ppm_train, aes(x = 1:120, y = X45, color = "sensor1")) +
  geom_line(data = df2_10ppm_train, aes(x = 1:120, y = X45, color = "sensor2")) +
  geom_line(data = df3_10ppm_train, aes(x = 1:120, y = X45, color = "sensor3")) +
  geom_line(data = df4_10ppm_train, aes(x = 1:120, y = X45, color = "sensor4")) +
  labs(title = 'sensors',
       x = 'TimeElapsed (sec)',
       y = '45th DC',
       color = 'Substance') +
  scale_color_manual(values = c("sensor1" = "blue",
                                "sensor2" = "red",
                                "sensor3" = "green4",
                                "sensor4" = "purple"))

ggplot() +
  geom_line(data = df1_10ppm_test, aes(x = 1:120, y = X45, color = "sensor1")) +
  geom_line(data = df2_10ppm_test, aes(x = 1:120, y = X45, color = "sensor2")) +
  geom_line(data = df3_10ppm_test, aes(x = 1:120, y = X45, color = "sensor3")) +
  geom_line(data = df4_10ppm_test, aes(x = 1:120, y = X45, color = "sensor4")) +
  labs(title = 'sensors',
       x = 'TimeElapsed (sec)',
       y = '45th DC',
       color = 'Substance') +
  scale_color_manual(values = c("sensor1" = "blue",
                                "sensor2" = "red",
                                "sensor3" = "green4",
                                "sensor4" = "purple"))
