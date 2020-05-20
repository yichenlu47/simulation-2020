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
# toy example
#-----------------------------------------

#generate data for one trial
n.causal = 1 #1 causal feature a.0
n.noise = 15
noise.name <- paste0("n.",1:n.noise)
n.cor = 40
n = 600
#!!!
# beta = 60
# cor = 0.9
# obs.causal = TRUE
#!!!



gen.1.trial <- function(n.cor.1, obs.causal){
  # x <- matrix(c(rnorm(n * n.causal, mean = x.mean, sd = x.sd), rnorm(n * n.noise)), nrow = n)  
  x <- matrix(rnorm(n * (n.causal + n.noise)), nrow = n)
  colnames(x) <- c("a.0", noise.name)
  
  cor.list = rep(cor, n.cor); cor.list[-n.cor.1] <- 0
  
  print("cor.list"); print(cor.list)
  
  x.cor <- data.frame(sapply(1:n.cor, function(i) x[,1] * sqrt(cor.list[i]) + rnorm(n) * sqrt(1-cor.list[i])))
  colnames(x.cor) <- paste0("a.", 1:n.cor)
  x <- cbind(x, data.frame(x.cor))
  
  y <- x[,1] * beta + rnorm(n, 0, 1)
  if(obs.causal == FALSE){
    x = x[,-1]
  }
  print(paste("dim of x", dim(x)))
  list(x=as.matrix(x), y=y, var.y = var(y)) #x is an array
}


gen.3.trial <- function(obs.causal){
  dat.list = lapply(1:3, function(i) {
    gen.1.trial(n.cor.1 = (i*10-9):(i*10), obs.causal = obs.causal)
  })
  xx <- do.call(rbind, lapply(1:3, function(i) dat.list[[i]]$x))
  yy <- unlist(lapply(1:3, function(i) dat.list[[i]]$y))
  var.yy <- unlist(lapply(1:3, function(i) dat.list[[i]]$var.y))
  list(x = xx, y = yy, var.y = var.yy)
}

fit <- function(dat1, dat2){
  lm.fit = lm(y~x, dat1)
  summary(lm.fit)
  # pred1 = predict(lm.fit, newdata = dat1) #legacy r
  pred2 = predict(lm.fit, newdata = dat2)
  list(mod = summary(lm.fit), 
       mse = mean((pred2 - dat2$y)^2)) #ttest mse
}

k.cv = function(dat, k = 10){ #legacy data
  folds = sample(cut(seq(1,1800),breaks = k,labels=FALSE))
  train <- test <- c()
  cv.r <- lapply(1:k, function(j){   #split the legacy data into training vs. test
    train$x <- dat$x[folds != j,]
    train$y <- dat$y[folds != j]
    test$x <- dat$x[folds == j,]
    test$y <- dat$y[folds == j]
    fit(train, test)
  }) #returns training mse, test mse averaged across 10-folds
  cv.mod <-lapply(cv.r, "[[", 1)
  cv.mse <- lapply(cv.r, "[[", 2)
  list(mod = cv.mod, mse = cv.mse)
}

simu.1 <- function(obs.causal){
  legacy = gen.3.trial(obs.causal = obs.causal) #1,800
  future = gen.1.trial(n.cor.1 = 31:40, obs.causal= obs.causal)
  
  legacy.var.y = mean(legacy$var.y)
  future.var.y = mean(future$var.y)
  
  # k-fold cv
  m1 = k.cv(legacy)
  k.cv.mse = apply(as.data.frame(m1$mse),1,mean)
  cv.r2 = c("r2.type" = "Estimated generalized R-squared w/ 10-fold cv",
            "r2" =  1 -  k.cv.mse/legacy.var.y)
  
  #coefficient
  m2 <- lapply(1:length(m1$mod), function(i) 
    cbind("coef" = m1$mod[[i]]$coefficients[,1], 
          "mod" = paste0("cv.", i), 
          "var" = rownames(m1$mod[[i]]$coefficients)))
  m3 <- do.call(rbind, m2)
  
  #cross-validation
  M1 <- fit(legacy, future)
  M2 <- cbind("coef" = M1$mod$coefficients[,1],
              "mod" = "main", 
              "var" = rownames(M1$mod$coefficients))
  main.r2 = c( 
    "r2.type" = "Generalized R-squared",
    "r2" =  1 - M1$mse/future.var.y)
  
  list(mod = rbind(M2, m3), 
       r2 = rbind(main.r2, cv.r2))
}


#repeat the simulation and calcualte the average legacy mse and future mse
simu.multi <- function(obs.causal){
  s <- lapply(1:1000, function(i) simu.1(obs.causal = obs.causal))
  
  coef.res <- as.data.frame(do.call(rbind, lapply(s, "[[", 1)))
  coef.res2 <- coef.res %>% mutate("coef" = as.numeric(as.character(coef)), 
                                     "var" = sub("x", "", var)) %>%
    filter(var != "(Intercept)") %>% group_by(mod, var) %>% 
    summarize(coef = mean(coef))
  coef.res2$beta = beta
  coef.res2$cor = cor
  coef.res2$"obs.causal" = obs.causal
  
  
  r2.res <- as.data.frame(do.call(rbind, lapply(s, "[[", 2)))
  r2.res2 <- r2.res %>% mutate(r2 = as.numeric(as.character(r2))) %>% 
    group_by(r2.type) %>% summarize(r2 = mean(r2))
  r2.res2$beta = beta
  r2.res2$cor = cor
  r2.res2$"obs.causal" = obs.causal
  
  list(coef.res2, r2.res2)}

res.true <- simu.multi(obs.causal = TRUE)
res.false <- simu.multi(obs.causal = FALSE)
r2.result <- rbind(res.true[[2]], res.false[[2]])
coef.result <- rbind(res.true[[1]], res.false[[1]])

filename <- paste0("/home/students/yl002013/RRR/simu-toy0_coef_", cor, "_", beta,".csv")
write.csv(coef.result, filename, row.names = F)

filename <- paste0("/home/students/yl002013/RRR/simu-toy0_r2_", cor, "_", beta,".csv")
write.csv(r2.result, filename, row.names = F)

