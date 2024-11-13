setwd('C:\\Users\\Admin\\Downloads\\co2_gas')
rm(list=ls())
# 데이터 불러오기
df_test <- read.csv("DF_test.csv")
df_train <- read.csv("DF_train.csv")

# 함수 정의: 노이즈 추가
add_noise <- function(x, noise_level = 0.004) {
  noise <- rnorm(length(x), mean = 0, sd = sd(x) * noise_level)
  return(x + noise)
}

# 테스트 데이터에 노이즈 추가
df_test$dim_0_noisy <- add_noise(df_test$dim_0)

# 훈련 데이터에 노이즈 추가
df_train$dim_0_noisy <- add_noise(df_train$dim_0)

# 결과 확인
head(df_test)
head(df_train)

# 원본 데이터와 노이즈가 추가된 데이터 비교 시각화
library(ggplot2)

# 테스트 데이터 시각화
ggplot(df_test, aes(x = 1:nrow(df_test))) +
  geom_line(aes(y = dim_0, color = "Original")) +
  geom_line(aes(y = dim_0_noisy, color = "Noisy")) +
  labs(title = "Test Data: Original vs Noisy",
       x = "Index", y = "Value") +
  scale_color_manual(values = c("Original" = "blue", "Noisy" = "red"))

# 훈련 데이터 시각화
ggplot(df_train, aes(x = 1:nrow(df_train))) +
  geom_line(aes(y = dim_0, color = "Original")) +
  geom_line(aes(y = dim_0_noisy, color = "Noisy")) +
  labs(title = "Train Data: Original vs Noisy",
       x = "Index", y = "Value") +
  scale_color_manual(values = c("Original" = "blue", "Noisy" = "red"))

# 처리완료
df_test$dim_0 <- add_noise(df_test$dim_0)

# 훈련 데이터에 노이즈 추가
df_train$dim_0 <- add_noise(df_train$dim_0)

df_train$X <- NULL
df_test$X <- NULL
df_train$dim_0_noisy <- NULL
df_test$dim_0_noisy <- NULL

write.csv(df_train, "DF_train_noise.csv")
write.csv(df_test, "DF_test_noise.csv")

# plot
plot(1:136, df_train$dim_0[1:136])
# noise가 제대로 들어갔는지 확인.
