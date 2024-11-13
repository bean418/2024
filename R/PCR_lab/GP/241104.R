library(ggplot2)

rm(list=ls())
setwd('C:\\Users\\Admin\\Desktop\\gpsig\\data')
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
# numeric으로 바꿈, 첫 번째 열(시간) 제외, 첫 번째 행(SENSOR) 제외
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
plot(1:136, df_co2$BD3_SEN5)
plot(1:136, df_gas$BD3_SEN5)

df_co2 <- data.frame(co2=df_co2$BD3_SEN5)
df_gas <- data.frame(gas=df_gas$BD3_SEN5)

df_list <- list(df_co2)

for (i in 1:39) {
  noisy_data <- data.frame(co2 = add_noise(df_co2$co2))
  df_list[[i+1]] <- noisy_data
}

df_co2 <- do.call(rbind, df_list)

rownames(df_co2) <- NULL

##
df_list <- list(df_gas)

for (i in 1:39) {
  noisy_data <- data.frame(gas = add_noise(df_gas$gas))
  df_list[[i+1]] <- noisy_data
}

df_gas <- do.call(rbind, df_list)

rownames(df_gas) <- NULL

df_co2$co2[1:5]
DF_train <- data.frame(dim_0 = c(df_co2$co2[1:(nrow(df_co2)/2)],
                                 df_gas$gas[1:(nrow(df_gas)/2)]))
DF_test <- data.frame(dim_0 = c(df_co2$co2[((nrow(df_co2)/2)+1):nrow(df_co2)],
                                 df_gas$gas[((nrow(df_gas)/2)+1):nrow(df_gas)]))

write.csv(DF_train, "DF_train.csv")
write.csv(DF_test, "DF_test.csv")
