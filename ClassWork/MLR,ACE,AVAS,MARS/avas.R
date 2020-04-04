setwd("C:/Users/yz9186ci/Desktop/DSCI425/ClassWork/ClassWork/MLR,ACE,AVAS,MARS")

library(acepack)
library(car)
library(caret)
library(e1071)
library(earth)
load("mult.Rdata")
load("Regression.Rdata")

PredAcc = function(y,ypred){
  RMSEP = sqrt(mean((y-ypred)^2))
  MAE = mean(abs(y-ypred))
  MAPE = mean(abs(y-ypred)/y)*100
  cat("RMSEP\n")
  cat("===============\n")
  cat(RMSEP,"\n\n")
  cat("MAE\n")
  cat("===============\n")
  cat(MAE,"\n\n")
  cat("MAPE\n")
  cat("===============\n")
  cat(MAPE,"\n\n")
  return(data.frame(RMSEP=RMSEP,MAE=MAE,MAPE=MAPE))
}

concrete <- read.csv("Concrete.csv")
str(concrete)

set.seed(1)
sam <- sample(1:1030, size = floor(.6666*1030),replace = F)

train <- concrete[sam,]
val <- concrete[-sam,]
names(concrete)

#a 
  pairs.plus(train)
  lm.con <- lm(Strength~., data = train)
  summary(lm.con)  
  plot(lm.con)
  
#B
  con.pred <- predict(lm.con, newdata = val)
  results <- PredAcc(val$Strength, con.pred)
  trst <- lm(Strength~., data = val)  
summary(trst)  

#C\
  concrete.lm <- lm(Strength~., data = train)
  x <- train[,-1]
  y <- train[,1]
  ace.concrete <- ace(x,y)
  maceplot(x,y,ace.concrete)
  avas.concrete <- avas(x,y)
  maceplot(x,y,avas.concrete)  
  test.lm <- lm(avas.concrete$ty~., data = train)
  summary(test.lm)
  con.pred1 <- predict(test.lm, newdata = val)
  summary(concrete)
  test <- concrete

  test$BlastFurn <- test$BlastFurn +.001  
  test$FlyAsh <- test$FlyAsh +.001  
  test$Superplast <- test$Superplast +.001  
  summary(test)  
  
  test.pp <- preProcess(test, method = "BoxCox")  
names(test.pp)  
test.pp$bc
  test$Strength <- test$Strength^(3/5)
  test$Cement <- test$Cement^(1/5)  
  test$BlastFurn <- log((test$BlastFurn-.001+1))  
  test$FlyAsh <- (test$FlyAsh)^(-1/10)  
  test$Water <- test$Water^(4/5)  
  test$Superplast <- (test$Superplast)^(1/5)  
  test$CourseAgg <- test$CourseAgg^(11/10)  
  test$FineAge <- test$FineAge^(9/5)
  test$Age <- log(test$Age)

  
  test.lm <- lm(Strength~., data = test[sam,])
  summary(test.lm)
  test.pred <- predict(test.lm, newdata = test[-sam,])
  test.results <- PredAcc(test[sam,]$Strength, test.pred)
  pairs.plus(test)
#c
  con.mars1 <- earth(Strength~., degree = 1, data = concrete[sam,])
  summary(con.mars1)    
  
  mar1.pred <- predict(con.mars1, newdata = val)  
  mars1.re <- PredAcc(val$Strength, mar1.pred)  
  
  val <- concrete[-sam,] 
  train <- concrete[sam,]
  names(val)  
  
#D 
  con.mar2 <- earth(Strength~., degree = 2, data = concrete[-sam,])
  summary(con.mar2)  
  
  
  mar2.pred <- predict(con.mar2, newdata = val)  
  mars2.re <- PredAcc(val$Strength, mar2.pred)  
  
  