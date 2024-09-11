library(ggplot2)
# ?ggplot
# ?rm
rm(list=ls())

pop = rexp(10000, 2)
xbar_vec = c()
for(i in 1:10000){
  xbar = mean(sample(x=pop, size=30));xbar
  xbar_vec = c(xbar_vec, xbar)
}

df = data.frame(pop = pop, xbar = xbar_vec)
ggplot(data=df) +
  geom_density(mapping=aes(x=pop)) +
  geom_density(mapping=aes(x=xbar_vec)) +
  coord_cartesian(xlim = c(0, 1))