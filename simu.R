rm(list = ls())
# Updated the program to reflect variance in prediction
# Install packages into specific library folder

# Load a library
library(dplyr, lib.loc=c(.libPaths(),'/home/students/yl002013/RRR/R_lib'))
library(data.table , lib.loc=c(.libPaths(),'/home/students/yl002013/RRR/R_lib'))
library(glmnet , lib.loc=c(.libPaths(),'/home/students/yl002013/RRR/R_lib'))
library(parallel , lib.loc=c(.libPaths(),'/home/students/yl002013/RRR/R_lib'))


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
n.legacy.list = c(400, 400, 400)
n.future.list = c(400, 400, 400)
n.future.list2 = 400

n.causal = 1 #1 causal feature a.0
causal.name <- paste0(letters[n.causal], ".0")
n.noise = 15
noise.name <- paste0("n.",1:n.noise)
obs.causal = FALSE
use.method = "linear"

k = 10
n.sim = 1000
r2.oracle <- 1-1/(beta^2 + 1) #true r-squared

#-----------------------------------------
# set up functions
#-----------------------------------------

#generate data for one trial
gen.1.trial <- function(n, n.cor, n.cor.1, cor, beta, t){
  print(paste("generate 1 trial, trial", t))
  x <- matrix(rnorm(n * (n.causal + n.noise)), nrow = n)  
  colnames(x) <- c(causal.name, noise.name)
  
  cor.list = rep(cor, n.cor)
  cor.list[sample(1:length(cor.list), n.cor - n.cor.1, replace = FALSE)] <- 0
  print(cor.list)
  
  if (n.cor != 0){ #generate values for correlated features if needed
    x.cor <- data.frame(sapply(1:n.cor, function(i) x[,1] * sqrt(cor.list[i]) + rnorm(n) * sqrt(1-cor.list[i])))
    colnames(x.cor) <- paste0(letters[n.causal], ".", 1:n.cor)
    x <- cbind(x, data.frame(x.cor))
  }
  
  y <- x[,1] * beta + rnorm(n, 0, 1)
  if(obs.causal == FALSE){
    x = x[,-1]
  }
  mse.y = mean((y-mean(y))^2)
  print(paste("var(y)", var(y))); print(paste("mse(y)", mse.y))
  
  list(x=as.matrix(x), y=y, "mse.y" = mse.y, trial = rep(t, n)) #x is a data.frame
}


#generate data for multiple trials
gen.multi.trial <- function(n.trial.list, n.cor, n.cor.1, cor, beta, trial){
  dat.list = lapply(1:length(n.trial.list), function(i) 
    gen.1.trial(n = n.trial.list[i], n.cor, n.cor.1, cor, beta, t = paste0(trial,i)))
  
  xx <- do.call(rbind, mclapply(1:length(n.trial.list), function(i) dat.list[[i]]$x))
  yy <- unlist(lapply(1:length(n.trial.list), function(i) dat.list[[i]]$y))
  
  #trial number
  tt <- unlist(lapply(1:length(n.trial.list), function(i) dat.list[[i]]$t))
  
  mse.yy <- unlist(lapply(1:length(n.trial.list), function(i) dat.list[[i]]$mse.y))
  
  list(x = xx, y = yy, t = tt, mse = mse.yy)
}

fit <- function(dat1, dat2){
  lm.fit = lm(y~x, dat1)
  pred1 = predict(lm.fit, newdata = dat1) #legacy r
  pred2 = predict(lm.fit, newdata = dat2)
  c(mean((pred1 - dat1$y)^2), mean((pred2 - dat2$y)^2)) #training mse, test mse
}


####################
# ordinal cv
####################
k.cv = function(dat){ #legacy data
  folds = sample(cut(seq(1,nrow(dat$x)),breaks=k,labels=FALSE))
  train <- test <- c()
  cv.r <- mclapply(1:k, function(j){   #split the legacy data into training vs. test
    train$x <- dat$x[folds != j,]
    train$y <- dat$y[folds != j]
    test$x <- dat$x[folds == j,]
    test$y <- dat$y[folds == j]
    # print(paste("length", length(test$y)))
    fit(train, test)
  }) #returns training mse, test mse averaged across 10-folds
  apply(do.call(rbind, cv.r), 2, mean)
}

####################
# leave one study out cv
####################
l.cv = function(dat){ #legacy data
  trial.name = dat[["t"]]
  train <- test <- c()
  cv.r <- lapply(1:length(n.legacy.list), function(j){   #split the legacy data into training vs. test
    jj <- paste0("legacy",j)
    train$x <- dat$x[trial.name != jj,]
    train$y <- dat$y[trial.name != jj]
    test$x <- dat$x[trial.name == jj,]
    test$y <- dat$y[trial.name == jj]
    # print(paste("length", length(test$y)))
    fit(train, test)
  })
  apply(do.call(rbind, cv.r), 2, mean)
}

simu.1 <- function(n.cor, n.cor.1, cor, beta){
  legacy = gen.multi.trial(n.trial.list = n.legacy.list, n.cor, n.cor.1, cor, beta, trial = "legacy")
  legacy.mse <- mean(legacy$mse) #mse of y among legacy trials
  
  future = gen.multi.trial(n.trial.list = n.future.list, n.cor, n.cor.1, cor, beta, trial = "future")
  future.mse <- mean(future$mse) #mse of y among future trials 
  
  f <- fit(legacy, future) #train mse, pred mse
  
  #predict on 1 future trial and see how different those predictions are 
  # lm.fit = lm(y~x, legacy)
  # 
  # f2 <- sapply(1:n.sim, function(i) {
  #   print(paste("predict on one future trial, trial #", i))
  #   future2 = gen.multi.trial(n.trial.list = n.future.list2, n.cor, n.cor.1, cor, beta, trial = "onefuture")
  #   future2.mse <- future2$mse #mse of this one future trial
  #   
  #   pred2 = predict(lm.fit, newdata = future2)
  #   future2.mspe = mean((pred2 - future2$y)^2) #mspe of the future trial
  #   
  #   1 - future2.mspe/future2.mse #generalized r-squared
  # })
  
  c("Legacy trials R-squared" = 1 - f[1]/legacy.mse,
    "Generalized R-squared" = 1 - f[2]/future.mse, 
    # 
    # "Generalized R-squared (mean)" = mean(f2), 
    # "Generalized R-squared (variance)" = var(f2), 
    
    "Estimated generalized R-squared w/ 10-fold cv" = 1 - k.cv(legacy)[2]/legacy.mse,
    "Estimated generalized R-squared w/ loo cv" = 1 - l.cv(legacy)[2]/legacy.mse)
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
t <- simu.multi(n.cor = n.cor, n.cor.1 = n.cor.1, cor = cor, beta = beta)
tt <- as.data.frame(t)
tt$r2.type <- rownames(tt)
tt$r2 = tt$t
ttt <- tt %>% select(c(r2.type, r2))

result = cbind("n.cor" = n.cor, "n.cor.1" = n.cor.1, "cor" = cor, "beta" = beta, 
               "r2.oracle" = r2.oracle, ttt)


filename <- paste0("/home/students/yl002013/RRR/simu_", n.cor.1, "_", n.cor, "_",cor,"_", beta,".csv")
write.csv(result, filename, row.names = F)
