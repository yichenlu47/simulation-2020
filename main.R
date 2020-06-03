rm(list = ls())
# Updated the program to reflect variance in prediction
# Install packages into specific library folder

# Load a library
library(dplyr, lib.loc=c(.libPaths(),'/home/students/yl002013/RRR/R_lib'))
library(data.table , lib.loc=c(.libPaths(),'/home/students/yl002013/RRR/R_lib'))
library(glmnet, lib.loc=c(.libPaths(),'/home/students/yl002013/RRR/R_lib'))
library(parallel, lib.loc=c(.libPaths(),'/home/students/yl002013/RRR/R_lib'))

# # beta
# for in1 in 1
# 
# #cor.random
# for in2 in TRUE FALSE
# 
# #n.cor
# for in3 in 20
# 
# #n.cor.1
# for in4 in 15
# 
# #cor
# for in5 in 0.9


args <- commandArgs(TRUE)
print(args)
beta <- as.numeric(args[1])
cor.random <- as.numeric(args[2])
n.cor <- as.numeric(args[3])
n.cor.1 <- as.numeric(args[4])
cor <- as.numeric(args[5])

# n.cor.1 <- 15
# n.cor <- 20
# cor <- 0.9
# beta <- 1
# cor.random <- 0

#global parameters
n.legacy.list = c(600, 600, 600)
n.future.list = c(600)
n.legacy.trial = length(n.legacy.list)
n.future.trial = length(n.future.list)

n.causal = 1 #1 causal feature a.0
causal.name <- paste0(letters[n.causal], ".0")
n.noise = 15
noise.name <- paste0("n.",1:n.noise)

n.sim = 500
# if (cor.random == 0) n.cor = 100

#-----------------------------------------
# set up functions
#-----------------------------------------

#generate data for one trial
gen.1.trial <- function(n, n.cor, n.cor.1, cor, beta, t){
  # print(t)
  # print(paste("generate 1 trial, trial", t))
  x <- matrix(rnorm(n * (n.causal + n.noise)), nrow = n)  
  colnames(x) <- c(causal.name, noise.name)
  
  cor.list = rep(cor, n.cor)
  
  if (cor.random == 0) {
    cor.start <- (t-1) * n.cor.1 + 1
    cor.end <- t * n.cor.1
    cor.list[-(cor.start:cor.end)] <- 0
  }
  
  else cor.list[sample(1:length(cor.list), n.cor - n.cor.1, replace = FALSE)] <- 0
  # print("cor.list"); print(cor.list)
  
  x.cor <- data.frame(sapply(1:n.cor, function(i) x[,1] * cor.list[i] + rnorm(n) * sqrt(1-cor.list[i]^2)))
  
  colnames(x.cor) <- paste0(letters[n.causal], ".", 1:n.cor)
  
  x <- cbind(x, data.frame(x.cor))
  
  y <- x[,1] * beta + rnorm(n, 0, 1)
  x = x[,-1]
  
  list(x=as.matrix(x), y=y, trial = rep(t, n)) #x is a data.frame
}


#generate data for multiple trials
gen.multi.trial <- function(n.trial.list, n.cor, n.cor.1, cor, beta, t.start){
  dat.list = lapply(1:length(n.trial.list), function(i) {
    gen.1.trial(n = n.trial.list[i], n.cor, n.cor.1, cor, beta, t = t.start + i)
  })
  
  xx <- do.call(rbind, lapply(1:length(n.trial.list), function(i) dat.list[[i]]$x))
  yy <- unlist(lapply(1:length(n.trial.list), function(i) dat.list[[i]]$y))
  
  #trial number
  tt <- unlist(lapply(1:length(n.trial.list), function(i) dat.list[[i]]$t))
  
  list(x = xx, y = yy, t = tt)
}

fit <- function(dat1, dat2){
  
  dat1.mse <- mean((dat1$y-mean(dat1$y))^2) #train mse
  dat2.mse <- mean((dat2$y-mean(dat2$y))^2) #test mse
  
  #fit simple linear regression 
  lm.fit = lm(y~x, dat1)
  
  lm.pred1 = lm.fit$fitted.values #train prediction error
  lm.pred2 = predict(lm.fit, newdata = dat2) #test prediction error
  
  lm1.r2 <- 1 - mean((lm.pred1 - dat1$y)^2)/dat1.mse #train r2
  lm2.r2 <- 1 - mean((lm.pred2 - dat2$y)^2)/dat2.mse #test r2
  
  coef.start = n.noise + n.causal + 1
  coef.end = n.noise + n.causal + n.cor
  if (cor.random == 0) coef.end = n.noise + n.causal + n.cor.1 * n.legacy.trial
  beta.mean <- mean(lm.fit$coefficients[coef.start:coef.end])
  
  #fit lasso regression model
  lasso.cv <- cv.glmnet(x=dat1$x, y=dat1$y, alpha=1, family="gaussian")
  lasso.pen <- lasso.cv$lambda.min #optimal lambda
  # print(summary(lasso.pen))
  lasso.fit <-glmnet(x=dat1$x,  dat1$y, alpha=1, family="gaussian", lambda = lasso.pen) #estimate the 
  # print(summary(lasso.fit))
  
  lasso.pred1 = predict(lasso.fit, newx = dat1$x)
  lasso.pred2 = predict(lasso.fit, newx  = dat2$x)
  
  lasso1.r2 <- 1 - mean((lasso.pred1 - dat1$y)^2)/dat1.mse
  lasso2.r2 <- 1 - mean((lasso.pred2 - dat2$y)^2)/dat2.mse
  
  c("lm1.r2" = lm1.r2, "lm2.r2" = lm2.r2, 
    "lasso1.r2" = lasso1.r2, "lasso2.r2" = lasso2.r2, 
    "beta.mean" = beta.mean) #training mse, test mse
}


####################
# ordinal cv
####################
k.cv = function(dat1){ #legacy data
  folds = sample(cut(seq(1,nrow(dat1$x)),breaks = 10,labels=FALSE))
  train <- test <- c()
  
  cv.r <- lapply(1:10, function(j){   #split the legacy data into training vs. test
    train$x <- dat1$x[folds != j,]
    train$y <- dat1$y[folds != j]
    test$x <- dat1$x[folds == j,]
    test$y <- dat1$y[folds == j]
    
    # print(paste("10-fold-cv dim(test$x)",dim(test$x)))
    
    fit(train, test)
  }) #returns training mse, test mse averaged across 10-folds
  apply(do.call(rbind, cv.r), 2, mean)
}

####################
# leave one study out cv
####################
l.cv = function(dat1){ #legacy data
  trial.name = dat1[["t"]]
  train <- test <- c()
  cv.r <- lapply(1:length(n.legacy.list), function(j){   #split the legacy data into training vs. test
    
    train$x <- dat1$x[trial.name != j,]
    train$y <- dat1$y[trial.name != j]
    test$x <- dat1$x[trial.name == j,]
    test$y <- dat1$y[trial.name == j]
    
    # print(paste("loo-cv dim(test$x)",dim(test$x)))
    fit(train, test)
  })
  apply(do.call(rbind, cv.r), 2, mean)
}

simu.1 <- function(n.cor, n.cor.1, cor, beta){
  legacy = gen.multi.trial(n.trial.list = n.legacy.list, n.cor, n.cor.1, cor, beta, t.start = 0)
  future = gen.multi.trial(n.trial.list = n.future.list, n.cor, n.cor.1, cor, beta, t.start = n.legacy.trial)
  
  f <- fit(legacy, future) 
  #output: lm1.r2, lm2.r2, lasso1.r2, lasso2.r2, beta.mean
  k <- k.cv(legacy) #k-fold cv
  l <- l.cv(legacy) #leave-one-study-out cv
  
  # Legacy trials R-squared, Generalized R-squared", Estimated generalized R-squared w/ 10-fold cv, 
  # Estimated generalized R-squared w/ loo cv
  c(f[5], f[1], f[2], k[2], l[2], f[3], f[4], k[4], l[4])
  
}

#repeat the simulation and calcualte the average legacy mse and future mse
simu.multi <- function(n.cor, n.cor.1, cor, beta){
  mm = replicate(n.sim, simu.1(n.cor, n.cor.1, cor, beta))
  return(apply(mm, 1, mean))
}


#-----------------------------------------
# run the entire simulations
#-----------------------------------------
# start_time = Sys.time()
r2 <- simu.multi(n.cor = n.cor, n.cor.1 = n.cor.1, cor = cor, beta = beta)
res1 <- as.data.frame(r2)
res1$r2.type <- c("Estimated beta",
                  rep(c("Legacy trials R-squared", "Generalized R-squared", 
                        "Estimated generalized R-squared w/ 10-fold cv", 
                        "Estimated generalized R-squared w/ loo cv"), 2))
res1$use.method <- c(rep("Simple linear regression", 5),rep("Lasso regression", 4))


#check analytically - randomly selected correlated features
v12 = matrix(rep(cor * beta, n.cor.1), nrow = 1)
v22 = matrix(rep(cor^2, n.cor.1^2), nrow = n.cor.1)
diag(v22) <- 1
beta.analytical <- mean(v12 %*% solve(v22))
r2.analytical = 1- (beta^2 + 1 - v12 %*% solve(v22) %*% t(v12))/(beta^2 + 1)
res2a = c("r2" = r2.analytical, r2.type = "Oracle R-squared (trial known)", "use.method" = "Simple linear regression")
res3a = c("r2" = r2.analytical, r2.type = "Oracle R-squared (trial known)", "use.method" = "Lasso regression")
res4a = c("r2" = beta.analytical, r2.type = "Analytical beta (trial known)", "use.method" = "Simple linear regression")
res5a = c("r2" = beta.analytical, r2.type = "Analytical beta (trial known)", "use.method" = "Lasso regression")


#check analytically - nonoverlapping correlated features
v12 = matrix(rep(cor * beta /n.legacy.trial, n.cor.1 * n.legacy.trial), nrow = 1)
v22 = matrix(rep(0, (n.cor.1 * n.legacy.trial)^2), nrow = n.cor.1 * n.legacy.trial)
for (i in 0: (n.legacy.trial-1)){
  v22[(i*n.cor.1 + 1):((i+1)*n.cor.1), (i*n.cor.1 + 1):((i+1)*n.cor.1)] <- cor^2 / n.legacy.trial
}
diag(v22) <- 1
beta.analytical <- mean(v12 %*% solve(v22))
r2.analytical <- 1 - (beta^2 + 1 - v12 %*% solve(v22) %*% t(v12))/(beta^2 + 1)
res2b = c("r2" = r2.analytical, r2.type = "Oracle R-squared (trial unknown)", "use.method" = "Simple linear regression")
res3b = c("r2" = r2.analytical, r2.type = "Oracle R-squared (trial unknown)", "use.method" = "Lasso regression")
res4b = c("r2" = beta.analytical, r2.type = "Analytical beta (trial unknown)", "use.method" = "Simple linear regression")
res5b = c("r2" = beta.analytical, r2.type = "Analytical beta (trial unknown)", "use.method" = "Lasso regression")

res6 <- rbind(res1, res2a, res3a, res4a, res5a, res2b, res3b, res4b, res5b)
res_final = cbind("n.cor" = n.cor, "n.cor.1" = n.cor.1, "cor" = cor, "beta" = beta, "cor.random" = cor.random, res6)

filename <- paste0("/home/students/yl002013/RRR/res_", n.cor.1, "_", n.cor, "_",cor,"_", beta,".csv")
write.csv(res_final, filename, row.names = F)
