rm(list=ls())

#1
ideology <- c(1, 2, 3, 4, 5, 6, 7)
democrat <- c(5, 18, 19, 25, 7, 7, 2)
republican <- c(1, 3, 1, 11, 10, 11, 1)

df <- data.frame(ideology, democrat, republican)
df

n <- democrat + republican

model <- glm(democrat/n~ideology, family=binomial(link='logit'),
             weights = n,data=df)

summary(model)

B1 <- -0.5901
se_B1 <- 0.1564
z_0.025 <- qnorm(0.975)

lower <- B1 - z_0.025*se_B1
upper <- B1 + z_0.025*se_B1

CI <- c(lower, upper);CI
confint(model)

test_stat <- B1/se_B1
chi <- (test_stat)**2;chi
pchisq(chi, df=1, lower.tail = F)

summary(model)

test_stat <- 24.7983 - 7.7894;test_stat
pchisq(test_stat, df=1, lower.tail = F)

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

#c
z <- B1 / se_B1
chi <- z^2
pchisq(chi,df=1,lower.tail = F)

chi <- 632.79-560.87
pchisq(chi,df=1,lower.tail = F)

#4
rm(list=ls())
df <- read.csv('C:/Users/Admin/git/R/inha_assignments/Shuttle.txt')
head(df)

model <- glm(TD~Temp, family=binomial(link='logit'),
             data=df)
summary(model)

exp(-0.232)

predict(model, newdata = data.frame(Temp = 31),
        type = "response")

B1 <- -0.2322
se_B1 <- 0.1082

chi <- (B1/se_B1)**2
chi
pchisq(chi, df=1, lower.tail = F)

likeli <- 28.267-20.315;likeli
pchisq(likeli, df=1, lower.tail = F)

# temp = 66
pred <- predict(model, newdata = data.frame(Temp = 66), type = "link", se.fit = TRUE)
fit <- pred$fit;fit
se <- pred$se.fit;se

ci <- fit + c(-1, 1) * qnorm(0.995) * se
ci
prob_ci <- exp(ci) / (1 + exp(ci))
prob_ci

#5
load('C:/Users/Admin/git/R/inha_assignments/Crabs.RData')
df <- Crabs

model <- glm(y~factor(color),
               data=df,
               family=binomial)
summary(model)

chi <- 225.76-212.06;chi
pchisq(chi, df=3, lower.tail = F)

model <- glm(y~color,
             data=df,
             family=binomial)
summary(model)

exp(-0.7147)
1-exp(-0.7147)

B1 <- -0.7147
se_B1 <- 0.2095
z <- B1/se_B1
z2 <- z**2;z2

pchisq(z2, df=1, lower.tail = F)

summary(model)
chi <- 225.76-213.30;chi
pchisq(chi, df=1, lower.tail = F)
