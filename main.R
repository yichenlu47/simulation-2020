rm(list = ls())
# Updated the program to reflect variance in prediction
# Install packages into specific library folder

# Load a library
library(dplyr, lib.loc=c(.libPaths(),'/home/students/yl002013/RRR/R_lib'))
library(data.table , lib.loc=c(.libPaths(),'/home/students/yl002013/RRR/R_lib'))
library(glmnet, lib.loc=c(.libPaths(),'/home/students/yl002013/RRR/R_lib'))
library(parallel, lib.loc=c(.libPaths(),'/home/students/yl002013/RRR/R_lib'))


args <- commandArgs(TRUE)
# print(args)
n.cor.1 <- as.numeric(args[1])
n.cor <- as.numeric(args[2])
cor <- as.numeric(args[3])
beta <- as.numeric(args[4])

# n.cor.1 <- 15
# n.cor <- 20
# cor <- 0.9
# beta <- 1

#global parameters
n.legacy.list = c(1000, 1000, 1000)
n.future.list = c(1000, 1000, 1000)

n.causal = 1 #1 causal feature a.0
causal.name <- paste0(letters[n.causal], ".0")
n.noise = 15
noise.name <- paste0("n.",1:n.noise)

n.sim = 1000
# r2.oracle <- 1-1/(beta^2 + 1) #true r-squared

#-----------------------------------------
# set up functions
#-----------------------------------------

#generate data for one trial
gen.1.trial <- function(n, n.cor, n.cor.1, cor, beta, t){
  # print(paste("generate 1 trial, trial", t))
  x <- matrix(rnorm(n * (n.causal + n.noise)), nrow = n)  
  colnames(x) <- c(causal.name, noise.name)
  
  cor.list = rep(cor, n.cor)
  cor.list[sample(1:length(cor.list), n.cor - n.cor.1, replace = FALSE)] <- 0
  
  print("cor.list"); print(cor.list)
  
  x.cor <- data.frame(sapply(1:n.cor, function(i) x[,1] * cor.list[i] + rnorm(n) * sqrt(1-cor.list[i]^2)))
  
  colnames(x.cor) <- paste0(letters[n.causal], ".", 1:n.cor)
  
  x <- cbind(x, data.frame(x.cor))
  
  y <- x[,1] * beta + rnorm(n, 0, 1)
  
  x = x[,-1]
  print(paste("dim(x)", dim(x)))
  
  # mse.y = mean((y-mean(y))^2)
  # print(paste("mse.y", mse.y))
  list(x=as.matrix(x), y=y, trial = rep(t, n)) #x is a data.frame
}


#generate data for multiple trials
gen.multi.trial <- function(n.trial.list, n.cor, n.cor.1, cor, beta, trial){
  dat.list = lapply(1:length(n.trial.list), function(i) 
    gen.1.trial(n = n.trial.list[i], n.cor, n.cor.1, cor, beta, t = paste0(trial,i)))
  
  xx <- do.call(rbind, mclapply(1:length(n.trial.list), function(i) dat.list[[i]]$x))
  yy <- unlist(lapply(1:length(n.trial.list), function(i) dat.list[[i]]$y))
  
  #trial number
  tt <- unlist(lapply(1:length(n.trial.list), function(i) dat.list[[i]]$t))
  
  # mse.yy <- unlist(lapply(1:length(n.trial.list), function(i) dat.list[[i]]$mse.y))
  
  list(x = xx, y = yy, t = tt)
}

fit <- function(dat1, dat2){
  dat1.mse <- mean((dat1$y-mean(dat1$y))^2) #train mse
  dat2.mse <- mean((dat2$y-mean(dat2$y))^2) #test mse
  
  #fit simple linear regression 
  lm.fit = lm(y~x, dat1)
  # print(summary(lm.fit))
  
  lm.pred1 = predict(lm.fit, newdata = dat1) #train prediction error
  lm.pred2 = predict(lm.fit, newdata = dat2) #test prediction error
  
  lm1.r2 <- 1 - mean((lm.pred1 - dat1$y)^2)/dat1.mse #train r2
  lm2.r2 <- 1 - mean((lm.pred2 - dat2$y)^2)/dat2.mse #test r2

  print(dat1.mse); print(dat2.mse)
  
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
  
  c(lm1.r2, lm2.r2, lasso1.r2, lasso2.r2) #training mse, test mse
  # print(fit.res)
  # fit.res
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
    
    print(paste("10-fold-cv dim(test$x)",dim(test$x)))
    
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
    
    test.trial <- paste0("legacy",j)
    train$x <- dat1$x[trial.name != test.trial,]
    train$y <- dat1$y[trial.name != test.trial]
    test$x <- dat1$x[trial.name == test.trial,]
    test$y <- dat1$y[trial.name == test.trial]
    
    print(paste("loo-cv dim(test$x)",dim(test$x)))
    
    fit(train, test)
  })
  apply(do.call(rbind, cv.r), 2, mean)
}


simu.1 <- function(n.cor, n.cor.1, cor, beta){
  legacy = gen.multi.trial(n.trial.list = n.legacy.list, n.cor, n.cor.1, cor, beta, trial = "legacy")
  future = gen.multi.trial(n.trial.list = n.future.list, n.cor, n.cor.1, cor, beta, trial = "future")
  
  f <- fit(legacy, future) 
  k <- k.cv(legacy) #k-fold cv
  l <- l.cv(legacy) #leave-one-study-out cv
  
  # Legacy trials R-squared, Generalized R-squared", Estimated generalized R-squared w/ 10-fold cv, Estimated generalized R-squared w/ loo cv
  c(f[1], f[2], k[2], l[2], f[3], f[4], k[4], l[4])
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
r2 <- as.data.frame(r2)
r2$r2.type <- rep(c("Legacy trials R-squared", "Generalized R-squared", "Estimated generalized R-squared w/ 10-fold cv", "Estimated generalized R-squared w/ loo cv"), 2)
r2$use.method <- c(rep("Simple linear regression", 4),rep("Lasso regression", 4))

result = cbind("n.cor" = n.cor, "n.cor.1" = n.cor.1, "cor" = cor, "beta" = beta, r2)


filename <- paste0("/home/students/yl002013/RRR/main_", n.cor.1, "_", n.cor, "_",cor,"_", beta,".csv")
write.csv(result, filename, row.names = F)
