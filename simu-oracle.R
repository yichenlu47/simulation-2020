rm(list = ls())
# Install packages into specific library folder

# Load a library
library(dplyr, lib.loc=c(.libPaths(),'/home/students/yl002013/RRR/R_lib'))
library(data.table , lib.loc=c(.libPaths(),'/home/students/yl002013/RRR/R_lib'))
library(glmnet , lib.loc=c(.libPaths(),'/home/students/yl002013/RRR/R_lib'))
library(parallel , lib.loc=c(.libPaths(),'/home/students/yl002013/RRR/R_lib'))


args <- commandArgs(TRUE)
print(args)
cor <- as.numeric(args[1])
beta <- as.numeric(args[2])
#-----------------------------------------
# oracle r for main part
#-----------------------------------------

n = 10000000
n.causal = 1 #1 causal feature a.0
n.noise = 15
noise.name <- paste0("n.",1:n.noise)

# 
# beta = 1
# cor = 0.9

x <- matrix(rnorm(n * (n.causal + n.noise)), nrow = n)
colnames(x) <- c("a.0", noise.name)
y <- x[,1] * beta + rnorm(n, 0, 1)

r2 <- function(obs.causal, n.cor.1){
  x.cor <- data.frame(sapply(1:n.cor.1, function(i) x[,1] * sqrt(cor) + rnorm(n) * sqrt(1-cor)))
  colnames(x.cor) <- paste0("a.", 1:n.cor.1)
  x <- cbind(x, data.frame(x.cor))
  
  if(obs.causal == FALSE) x = x[,-1]
  
  lm.fit <- lm(y~as.matrix(x))
  pred = predict(lm.fit, newdata = x)
  cbind(r2 = 1 - mean((pred - y)^2)/mean((y - mean(y))^2), "obs.causal" = as.character(obs.causal), beta = beta, cor = cor, n.cor.1 = n.cor.1)
}

res1 <- r2(TRUE, 1)
res2 <- r2(FALSE, 1)
res3 <- r2(FALSE, 3)
res4 <- r2(FALSE, 6)
res5 <- r2(FALSE, 9)
res6 <- r2(FALSE, 12)
res7 <- r2(FALSE, 15)

oracle.r2.result <- rbind(res1, res2, res3, res4, res5, res6, res7)

filename <- paste0("/home/students/yl002013/RRR/simu_oracle_r2_", cor, "_", beta,".csv")
write.csv(oracle.r2.result, filename, row.names = F)



#-----------------------------------------
# oracle r for testing
#-----------------------------------------
# n = 10000
# beta = 10
# cor = 0.9
# 
# x <- matrix(rnorm(n * 1), nrow = n)
# y <- x * beta
# 
# n.cor.1 = 1
# x.cor <- data.frame(sapply(1:n.cor.1, function(i) x * cor + rnorm(n) * sqrt(1-cor^2)))
# (lm.fit <- lm(y~as.matrix(x.cor)))
# (beta_new = cor * beta)
# pred = predict(lm.fit, newdata = x.cor)
# 1 - mean((pred - y)^2)/mean((y - mean(y))^2)
# 
# for (i in 2: 5){
#   n.cor.1 = i
#   x.cor <- data.frame(sapply(1:n.cor.1, function(i) x * cor + rnorm(n) * sqrt(1-cor^2)))
#   (lm.fit <- lm(y~as.matrix(x.cor)))
#   print(summary(lm.fit))
#   print(paste("new beta", beta_new = beta * cor / (1 + cor^2 * (n.cor.1 - 1))))
#   pred = predict(lm.fit, newdata = x.cor)
#   print(paste("empirical R-squared",  1 - mean((pred - y)^2)/mean((y - mean(y))^2)))
#   print(paste("theoratical R-squared",  1 - (1 + beta ^ 2 - beta_new^2)/(1 + beta^2)))
# }
