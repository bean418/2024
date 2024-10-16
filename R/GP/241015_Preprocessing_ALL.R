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

df1 <- df_list$df1
df2 <- df_list$df2
df3 <- df_list$df3
df4 <- df_list$df4

# test
files <- paste0("C:/Users/imb2a/Desktop/study/GPSig/dat/s", 1:4, "test.csv")
for (i in 1:4) {
  df_list[[i]] <- read.csv(files[i])
}



# Preprocessing

df1$label.ppm. <- NULL
df2$label.ppm. <- NULL
df3$label.ppm. <- NULL
df4$label.ppm. <- NULL

df1$sensor <- 1
df2$sensor <- 2
df3$sensor <- 3
df4$sensor <- 4

df1 <- df1[1:2250,]
df2 <- df2[1:2250,]
df3 <- df3[1:2250,]
df4 <- df4[1:2250,]

n <- nrow(df1)
k <- n/2

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
