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
# oracle r for the toy example
#-----------------------------------------

#generate data for one trial
n.causal = 1 #1 causal feature a.0
n.noise = 15
noise.name <- paste0("n.",1:n.noise)
n.cor = 40
n = 1000000

# n = 100
# beta = 1
# cor = 0.9
#!!!


gen.1.trial <- function(n.cor.1, error.in.y, obs.causal){
  # x <- matrix(c(rnorm(n * n.causal, mean = x.mean, sd = x.sd), rnorm(n * n.noise)), nrow = n)
  x <- matrix(rnorm(n * (n.causal + n.noise)), nrow = n)
  colnames(x) <- c("a.0", noise.name)

  cor.list = rep(cor, n.cor); cor.list[-n.cor.1] <- 0

  # print("cor.list"); print(cor.list)

  x.cor <- data.frame(sapply(1:n.cor, function(i) x[,1] * sqrt(cor.list[i]) + rnorm(n) * sqrt(1-cor.list[i])))
  colnames(x.cor) <- paste0("a.", 1:n.cor)
  x <- cbind(x, data.frame(x.cor))



  if(error.in.y == FALSE) y <- x[,1] * beta
  else y <- x[,1] * beta + rnorm(n, 0, 1)

  if(obs.causal == FALSE) x = x[,-1]
  # print(paste("dim of x", dim(x)))
  list(x=as.matrix(x), y=y, mse.y = mean((y-mean(y))^2)) #x is an array
}

gen.3.trial <- function(error.in.y, obs.causal){
  dat.list = lapply(1:3, function(i) {
    gen.1.trial(n.cor.1 = (i*10-9):(i*10), error.in.y = error.in.y, obs.causal = obs.causal)
  })
  xx <- do.call(rbind, lapply(1:3, function(i) dat.list[[i]]$x))
  yy <- unlist(lapply(1:3, function(i) dat.list[[i]]$y))
  mse.yy <- unlist(lapply(1:3, function(i) dat.list[[i]]$mse.y))
  list(x = xx, y = yy, mse = mse.yy)
}



r2 <- function(error.in.y, obs.causal){
  legacy = gen.3.trial(error.in.y = error.in.y, obs.causal = obs.causal)
  legacy.mse <- mean(legacy$mse)
  lm.fit = lm(y~x, legacy)
  pred = predict(lm.fit, newdata = legacy)
  legacy.mspe = mean((pred - legacy$y)^2)
  cbind(r2 = 1 - legacy.mspe/legacy.mse, beta = beta, cor = cor, "error.in.y" = as.character(error.in.y), "obs.causal" = obs.causal)
}
res1 <- r2(error.in.y = TRUE, obs.causal= TRUE)
res2 <- r2(error.in.y = TRUE, obs.causal= FALSE)
res3 <- r2(error.in.y = FALSE, obs.causal= TRUE)
res4 <- r2(error.in.y = FALSE, obs.causal= FALSE)
# legacy.var.y = mean(legacy$var.y)
# future.var.y = mean(future$var.y)

oracle.r2.result <- rbind(res1, res2, res3, res4)

filename <- paste0("/home/students/yl002013/RRR/simu-toy0_oracle_r2_", cor, "_", beta,".csv")
write.csv(oracle.r2.result, filename, row.names = F)


