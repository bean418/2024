library(dplyr)
library(ggplot2)

rm(list=ls())

setwd("C:/Users/imb2a/Desktop/study/GPSig/dat")
getwd()

# train
files <- paste0("C:/Users/imb2a/Desktop/study/GPSig/dat/sensor", 1:4, ".csv")

df_list <- list()

for (i in 1:4) {
  df_list[[i]] <- read.csv(files[i])
  df_list[[i]]$Unnamed..0 <- NULL
}

names(df_list) <- paste0("df", 1:4)

s1 <- df_list$df1
s2 <- df_list$df2
s3 <- df_list$df3
s4 <- df_list$df4

# Preprocessing

s1$label.ppm. <- NULL
s2$label.ppm. <- NULL
s3$label.ppm. <- NULL
s4$label.ppm. <- NULL

s1$sensor <- 1
s2$sensor <- 2
s3$sensor <- 3
s4$sensor <- 4

s1 <- s1[1:2250,]
s2 <- s2[1:2250,]
s3 <- s3[1:2250,]
s4 <- s4[1:2250,]

n <- nrow(s1)
k <- n/2

set.seed(1)
selected_indices <- sample(1:n, k, replace = FALSE)
selected_indices <- sort(selected_indices)

s1_train <- s1[selected_indices,]
s1_test <- s1[-selected_indices,]

set.seed(2)
selected_indices <- sample(1:n, k, replace = FALSE)
selected_indices <- sort(selected_indices)

s2_train <- s2[selected_indices,]
s2_test <- s2[-selected_indices,]

set.seed(3)
selected_indices <- sample(1:n, k, replace = FALSE)
selected_indices <- sort(selected_indices)

s3_train <- s3[selected_indices,]
s3_test <- s3[-selected_indices,]

set.seed(4)
selected_indices <- sample(1:n, k, replace = FALSE)
selected_indices <- sort(selected_indices)

s4_train <- s4[selected_indices,]
s4_test <- s4[-selected_indices,]

DF_train <- rbind(s1_train, s2_train,
                  s3_train, s4_train)
DF_test <- rbind(s1_test, s2_test,
                 s3_test, s4_test)

write.csv(DF_train, "DF_train.csv")
write.csv(DF_test, "DF_test.csv")

# test
dim(s1_train)
ggplot() +
  geom_line(data = s1_train, aes(x = 1:1125, y = X69, color = "sensor1")) +
  geom_line(data = s1_train, aes(x = 1:1125, y = X49, color = "sensor2")) +
  geom_line(data = s1_train, aes(x = 1:1125, y = X49, color = "sensor3")) +
  geom_line(data = s1_train, aes(x = 1:1125, y = X19, color = "sensor4")) +
  labs(title = 'X35에서의 시계열 분포',
       x = 'Time(1~2254)',
       y = 'X35',
       color = 'Substance') +
  scale_color_manual(values = c("sensor1" = "blue",
                                "sensor2" = "red",
                                "sensor3" = "green4",
                                "sensor4" = "purple"))
# 19, 49, 69
