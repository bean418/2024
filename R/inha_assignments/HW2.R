rm(list=ls())

#1
ideology <- c(1, 2, 3, 4, 5, 6, 7)
democrat <- c(5, 18, 19, 25, 7, 7, 2)
republican <- c(1, 3, 1, 11, 10, 11, 1)

data <- data.frame(ideology, democrat, republican)
data

total <- democrat + republican

prob_democrat <- democrat / total

data <- data.frame(ideology = ideology, prob_democrat = prob_democrat)
data

model <- glm(prob_democrat ~ ideology, family = binomial(link = "logit"), data = data)

summary(model)

#2
load(file="C:/Users/imb2a/Desktop/study/lecture_note/CDA/R/Crabs.RData")
head(Crabs)

#a
colnames(Crabs)
model <- glm(sat~weight, family=poisson(link='log'), data=Crabs)
summary(model)

model$coefficients[2]
log_mu <- 2.44 * model$coefficients[2] + model$coefficients[1]
mu <- exp(log_mu)
cat(mu)

#b(CI)
z_0.025 <- qnorm(0.975)
summary(model)
B1 <- 0.58930
se_B1 <- 0.06502
upper <- B1+z_0.025*se_B1
lower <- B1-z_0.025*se_B1
c(lower, upper)
c(exp(lower), exp(upper))

exp(B1)
