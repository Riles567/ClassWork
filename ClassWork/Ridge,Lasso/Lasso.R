setwd("C:/Users/yz9186ci/Desktop/DSCI425/ClassWork/ClassWork/Ridge,Lasso")

library(ISLR)
library(genridge)
library(glmnet)

lu <- read.csv("Lu2004.csv")

MLR.ssmc = function(fit,p=.667,M=100) {
  RMSEP = rep(0,M)
  MAEP = rep(0,M)
  MAPEP = rep(0,M)
  y = fit$model[,1]
  x = fit$model[,-1]
  data = fit$model
  n = nrow(data)
  for (i in 1:M) {
    ss = floor(n*p)
    sam = sample(1:n,ss,replace=F)
    fit2 = lm(formula(fit),data=data[sam,])
    ypred = predict(fit2,newdata=x[-sam,])
    RMSEP[i] = sqrt(mean((y[-sam]-ypred)^2))
    MAEP[i] = mean(abs(y[-sam]-ypred))
    yp = ypred[y[-sam]!=0]
    ya = y[-sam][y[-sam]!=0]
    MAPEP[i]=mean(abs(yp-ya)/ya)
  }
  cat("RMSEP =",mean(RMSEP),"  MAEP=",mean(MAEP),"  MAPEP=",mean(MAPEP))
  cv = return(data.frame(RMSEP=RMSEP,MAEP=MAEP,MAPEP=MAPEP))
}

glmnet.ssmc = function(X,y,p=.667,M=100,alpha=1,lambda=1) {
  RMSEP = rep(0,M)
  MAEP = rep(0,M)
  MAPEP = rep(0,M)
  n = nrow(X)
  for (i in 1:M) {
    ss = floor(n*p)
    sam = sample(1:n,ss,replace=F)
    fit = glmnet(X[sam,],y[sam],lambda=lambda,alpha=alpha)
    ypred = predict(fit,newx=X[-sam,])
    RMSEP[i] = sqrt(mean((y[-sam]-ypred)^2))
    MAEP[i] = mean(abs(y[-sam]-ypred))
    yp = ypred[y[-sam]!=0]
    ya = y[-sam][y[-sam]!=0]
    MAPEP[i]=mean(abs(yp-ya)/ya)
  }
  cat("RMSEP =",mean(RMSEP),"  MAEP=",mean(MAEP),"  MAPEP=",mean(MAPEP))
  cv = return(data.frame(RMSEP=RMSEP,MAEP=MAEP,MAPEP=MAPEP)) 
}


data(College)

College2 <- data.frame(PctAccept = 100*(College$Accept/College$Apps), College[,-c(2:3)])

attach(College)

College4 = data.frame(logApps=log(Apps),Private,logAcc=log(Accept),logEnr=log(Enroll),Top10perc,
                      Top25perc,logFull=log(F.Undergrad),logPart=log(P.Undergrad),Outstate,Room.Board,Books,Personal,PhD,Terminal,S.F.Ratio,perc.alumni,logExp=log(Expend),Grad.Rate)

detach(College)
nrow(College2)
set.seed(1)
sam <- sample(1:777, size = floor(.667*777), replace = F)
test <- (-sam)
college.train <- College2[sam,]
college.test <- College2[-sam,]

names(College2)

X = model.matrix(logApps~.,data=College2)[,-1]
y = College2$PctAccept
Xs = scale(X)
College2.temp = data.frame(y,Xs)
sam = sample(1:length(y),floor(.6667*length(y)),replace=F)
PA.ols = lm(y~Xs,data=college.train)
PA.step <- step(PA.ols)
ypred = predict(PA.ols,newdata=college.test)
RMSEP.ols = sqrt(mean((y[-sam]-ypred)^2))
RMSEP.ols


grid = 10^seq(10,-2,length=200)
ridge.mod = glmnet(Xs,y,alpha=0,lambda=grid)
lasso.mod = glmnet(Xs,y,alpha=1,lambda=grid)

plot(ridge.mod) 

plot(ridge.mod,xvar="lambda") 
plot(lasso.mod)
plot(lasso.mod,xvar = "lambda")

cv.out = cv.glmnet(X,y,alpha=1)
plot(cv.out)
bestlam = cv.out$lambda.min
bestlam

ridge.mod = glmnet(Xs,y,alpha=0,lambda=bestlam)
lasso.mod = glmnet(Xs,y,alpha=1,lambda=bestlam)
plot(ridge.mod) 

cbind(coef(ridge.mod), coef(lasso.mod), coef(PA.ols))

plot(y[-sam],predict(lasso.mod,newx=X[-sam,]),xlab="Test y-values",ylab="Predicted Test y-values")

ridg.best <- glmnet(X[sam,],y[sam],alpha = 0, lambda = bestlam)
ridge.pred <- predict(ridg.best, newx = X[test,])
RMSEP.rid <- sqrt(mean((y[-sam]-ridge.pred)^2))
RMSEP.rid

lasso.best <- glmnet(X[sam,],y[sam],alpha = 1, lambda = bestlam)
lasso.pred <- predict(lasso.best, newx = X[test,])
RMSEP.rid <- sqrt(mean((y[-sam]-lasso.pred)^2))
RMSEP.rid

OLS.MC <- MLR.ssmc(PA.ols, M =1000)
ridge.MC <- glmnet.ssmc(X,y,alpha = 0,M = 1000, lambda = bestlam)
lasso.MC <- glmnet.ssmc(X,y, alpha = 1, M = 1000, lambda = bestlam)
#I
set.seed(1)
train <- sample(1:777, size = floor(.667*777), replace = F)
test <- (-train)
college.train <- College4[train,]
college.test <- College4[test,]


X = model.matrix(logApps~.,data=College4)[,-1]
y = College4$logApps
Xs = scale(X)
College4.temp = data.frame(y,Xs)
sam = sample(1:length(y),floor(.6667*length(y)),replace=F)
c4.ols = lm(y~Xs,data=college.train)
c4.step <- step(c4.ols)
ypred = predict(c4.ols,newdata=college.test)
RMSEP.ols = sqrt(mean((y[-sam]-ypred)^2))
RMSEP.ols


grid = 10^seq(10,-2,length=200)
ridge.mod = glmnet(Xs,y,alpha=0,lambda=grid)
lasso.mod = glmnet(Xs,y,alpha=1,lambda=grid)

plot(ridge.mod) 

plot(ridge.mod,xvar="lambda") 
plot(lasso.mod)
plot(lasso.mod,xvar = "lambda")

cv.out = cv.glmnet(X,y,alpha=1)
plot(cv.out)
bestlam = cv.out$lambda.min
bestlam

ridge.mod = glmnet(Xs,y,alpha=0,lambda=bestlam)
lasso.mod = glmnet(Xs,y,alpha=1,lambda=bestlam)
plot(ridge.mod) 

cbind(coef(ridge.mod), coef(lasso.mod), coef(PA.ols))

plot(y[-sam],predict(ridge.mod,newx=X[-sam,]),xlab="Test y-values",ylab="Predicted Test y-values")

ridg.best <- glmnet(X[sam,],y[sam],alpha = 0, lambda = bestlam)
ridge.pred <- predict(ridg.best, newx = X[test,])
RMSEP.rid <- sqrt(mean((y[-sam]-ridge.pred)^2))
RMSEP.rid

lasso.best <- glmnet(X[sam,],y[sam],alpha = 1, lambda = bestlam)
lasso.pred <- predict(lasso.best, newx = X[test,])
RMSEP.rid <- sqrt(mean((y[-sam]-lasso.pred)^2))
RMSEP.rid

OLS.MC <- MLR.ssmc(c4.ols, M =1000)
ridge.MC <- glmnet.ssmc(X,y,alpha = 0,M = 1000, lambda = bestlam)
lasso.MC <- glmnet.ssmc(X,y, alpha = 1, M = 1000, lambda = bestlam)


# problem 2
names(lu)
nrow(lu)

X = model.matrix(Age~.,data=lu)[,-1]
y = lu$Age
Xs = scale(X)

train <- sample(1:30, size = floor(.667*30), replace = F)
test <- (-train)

grid = 10^seq(10,-2,length=200)
ridge.mod = glmnet(Xs,y,alpha=0,lambda=grid)
lasso.mod = glmnet(Xs,y,alpha=1,lambda=grid)

par(mfrow = c(2,2))
plot(ridge.mod) 

plot(ridge.mod,xvar="lambda") 
plot(lasso.mod)
plot(lasso.mod,xvar = "lambda")

par(mfrow = c(1,1))
cv.out = cv.glmnet(X,y,alpha=1)
plot(cv.out)
bestlam.rid <- cv.out$lambda.min
bestlam.las = cv.out$lambda.min
bestlam.las

ridge.mod = glmnet(Xs,y,alpha=0,lambda=bestlam.rid)
lasso.mod = glmnet(Xs,y,alpha=1,lambda=bestlam.las)

plot(y[test],predict(lasso.mod,newx=X[test,]),xlab="Test y-values",ylab="Predicted Test y-values")

coef(lasso.mod)

ridge.MC <- glmnet.ssmc(X,y,p = .75,alpha = 0,M = 1000, lambda = bestlam.rid)
lasso.MC <- glmnet.ssmc(X,y,p = .75, alpha = 1, M = 1000, lambda = bestlam.las)

bf.en <- glmnet(X,y,alpha = .1)
cv.en <- cv.glmnet(X,y,alpha = .1)
bestlam.en <- cv.en$lambda.min
en.results <- glmnet.ssmc(X,y,M= 1000, alpha = .1, lambda = bestlam.en)
