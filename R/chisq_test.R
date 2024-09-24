rm(list=ls())

mat <- matrix(c(871, 821, 336, 347, 42, 83), nrow=2, byrow=T)

rownames(mat) <- c('white', 'black')
colnames(mat) <- c('democrat', 'republican', 'independent')

names(dimnames(mat)) <- c("race", "political party identification")

tab <- as.table(mat)

tab
loglin(tab, list(1,2), param=TRUE, fit=TRUE)