library(ggplot2)

# home pc dir: 'C:\\Users\\Admin\\Desktop\\gpsig\\data'
# lab. pc dir: 'C:\\Users\\imb2a\\Desktop\\study\\GPSig\\dat\\co2_gas'

rm(list=ls())
setwd('C:\\Users\\imb2a\\Desktop\\study\\GPSig\\dat\\co2_gas')
getwd()
dir()

add_noise <- function(x, noise_level = 0.1) {
  noise <- rnorm(length(x), mean = 0, sd = sd(x) * noise_level)
  return(x + noise)
}

df_co2 <- read.csv("CO2.csv")
df_gas <- read.csv("GAS.csv")

head(df_co2)

# str(df_co2) -> 데이터가 문자열이다.
# numeric으로 바꿈, 초반부 데이터 제외.
df_co2 <- as.data.frame(lapply(df_co2[108:137, 2:81],
                               as.numeric))
df_gas <- as.data.frame(lapply(df_gas[108:137, 2:81],
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

df_co2 <- data.frame(co2=df_co2$BD3_SEN5)
df_gas <- data.frame(gas=df_gas$BD3_SEN5)

df_co2_org <- df_co2

df_list <- list(df_co2)

for (i in 1:39) {
  noisy_data <- data.frame(co2 = add_noise(df_co2$co2))
  df_list[[i+1]] <- noisy_data
}
df_list[[1]] <- data.frame(co2 = add_noise(df_co2$co2))

df_co2 <- do.call(rbind, df_list)

rownames(df_co2) <- NULL

##
df_list <- list(df_gas)

for (i in 1:39) {
  noisy_data <- data.frame(gas = add_noise(df_gas$gas))
  df_list[[i+1]] <- noisy_data
}
df_list[[1]] <- data.frame(gas = add_noise(df_gas$gas))

df_gas <- do.call(rbind, df_list)

rownames(df_gas) <- NULL

# 세번째 클래스
##
df_list <- list(df_co2_org)

for (i in 1:39) {
  noisy_data <- data.frame(co2 = add_noise( (df_co2_org$co2+5000) ))
  df_list[[i+1]] <- noisy_data
}
df_list[[1]] <- data.frame(co2 = add_noise(df_co2_org$co2+5000))

df_co2_5000 <- do.call(rbind, df_list)

rownames(df_co2_5000) <- NULL


df_co2$co2[1:5]
DF_train <- data.frame(dim_0 = c(df_co2$co2[1:(nrow(df_co2)/2)],
                                 df_gas$gas[1:(nrow(df_gas)/2)],
                                 df_co2_5000$co2[1:(nrow(df_co2)/2)]))
DF_test <- data.frame(dim_0 = c(df_co2$co2[((nrow(df_co2)/2)+1):nrow(df_co2)],
                                df_gas$gas[((nrow(df_gas)/2)+1):nrow(df_gas)],
                                df_co2_5000$co2[((nrow(df_co2)/2)+1):nrow(df_co2)]))



write.csv(DF_train, "DF_train.csv")
write.csv(DF_test, "DF_test.csv")

