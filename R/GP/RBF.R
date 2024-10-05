# RBF 커널 함수 정의
rbf_kernel <- function(x1, x2, sigma = 1.0) {
  exp(-((x1 - x2)^2) / (2 * sigma^2))
}

# x 값 생성
x <- seq(-5, 5, by = 0.1)

# RBF 커널 함수 계산
y <- sapply(x, function(x1) rbf_kernel(x1, 1, sigma = 1.0))

# 그래프 생성
plot(x, y, type = "l", col = "blue", lwd = 2, 
     xlab = "x", ylab = "RBF Kernel Value", 
     main = "RBF(x' = 1)")
# x'이 1인 상황에서 x가 1이면 커널함수의 값은 1이다.
# (한 데이터와 다른 데이터가 같은 값을 가지므로
# 상관관계가 매우 높다.)