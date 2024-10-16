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

s1 <- s1[1:2200, c(20,50)]
s2 <- s2[1:2200, c(20,50)]
s3 <- s3[1:2200, c(20,50)]
s4 <- s4[1:2200, c(20,50)]

#plot
ggplot() +
  geom_line(data = s1, aes(x = 1:2200, y = X49, color = "sensor1")) +
  geom_line(data = s2, aes(x = 1:2200, y = X49, color = "sensor2")) +
  geom_line(data = s3, aes(x = 1:2200, y = X49, color = "sensor3")) +
  geom_line(data = s4, aes(x = 1:2200, y = X49, color = "sensor4")) +
  labs(title = 'X19에서의 시계열 분포',
       x = 'Time(1~2254)',
       y = 'X35',
       color = 'Substance') +
  scale_color_manual(values = c("sensor1" = "blue",
                                "sensor2" = "red",
                                "sensor3" = "green4",
                                "sensor4" = "purple"))

# 증강
# index 생성: 100개의 무작위 인덱스 벡터 22개
f_idx <- function(){
  idx <- sample(1:2200, size=2200, replace=FALSE)
  idx <- split(idx, rep(1:22, each = 100))
  idx <- lapply(idx, sort)
  
  return(idx)
}

# plot e.g.
lst <- f_idx()
lst[[1]]
df <- s1[lst[[1]],]
ggplot() +
  geom_line(data = df, aes(x = 1:100, y = X49, color = "sensor1"))

#s1
s1_list <- lapply(f_idx(), function(indices) s1[indices, ])
s1_train <- do.call(rbind, s1_list[1:11])
s1_test <- do.call(rbind, s1_list[12:22])
s1_train$sensor <- 1
s1_test$sensor <- 1

#s2
s2_list <- lapply(f_idx(), function(indices) s2[indices, ])
s2_train <- do.call(rbind, s2_list[1:11])
s2_test <- do.call(rbind, s2_list[12:22])
s2_train$sensor <- 2
s2_test$sensor <- 2

#s3
s3_list <- lapply(f_idx(), function(indices) s3[indices, ])
s3_train <- do.call(rbind, s3_list[1:11])
s3_test <- do.call(rbind, s3_list[12:22])
s3_train$sensor <- 3
s3_test$sensor <- 3

#s4
s4_list <- lapply(f_idx(), function(indices) s4[indices, ])
s4_train <- do.call(rbind, s4_list[1:11])
s4_test <- do.call(rbind, s4_list[12:22])
s4_train$sensor <- 4
s4_test$sensor <- 4

#final train, test
DF_train <- rbind(s1_train, s2_train, s3_train, s4_train)
DF_test <- rbind(s1_test, s2_test, s3_test, s4_test)

# remove sensor
DF_train$sensor <- NULL
DF_train$sensor <- NULL
row.names(DF_train) <- NULL
row.names(DF_test) <- NULL

write.csv(DF_train, "DF_train.csv")
write.csv(DF_test, "DF_test.csv")

dim(DF_train)
