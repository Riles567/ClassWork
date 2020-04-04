setwd("C:/Users/yz9186ci/Desktop/DSCI425/ClassWork/ClassWork/PCR Regression")

library(pls)
library(pcr)
load("mult.Rdata")

data("gasoline")
QSAR.melt <- read.csv("Melting Point QSAR.csv")

#1
gasoline.x = gasoline$NIR
dim(gasoline.x)
matplot(t(gasoline.x),type="l",xlab="Variable",ylab="Spectral Intensity")
title(main="Spectral Readings for Gasoline Data")
pairs.plus(gasoline.x[,1:10])
pairs.plus(gasoline.x[,201:210])
pairs.plus(gasoline.x[,301:310]) 


oct.pcr=pcr(octane~scale(NIR),data=gasoline,ncomp=40,validation="CV")
summary(oct.pcr)

loadingplot(oct.pcr,comps=1:2,lty=1:2,lwd=2,legendpos="topright")


gasoline.train = gasoline[1:50,]
gasoline.test = gasoline[51:60,]
attributes(gasoline.train)
dim(gasoline.train$NIR)

oct.train = pcr(octane~scale(NIR),data=gasoline.train,ncomp=6)
ypred = predict(oct.train,ncomp=6,newdata=gasoline.test)
yact = gasoline.test$octane
sqrt(mean((ypred - yact)^2)) 

oct.pls = plsr(octane~scale(NIR),data=gasoline,ncomp=40,validation="CV")
summary(oct.pls)

loadingplot(oct.pls,comps=1:2,legendpos="topright")

gasoline.train = gasoline[1:50,]
gasoline.test = gasoline[51:60,]
attributes(gasoline.train)

oct.train <- plsr(octane~scale(NIR),data=gasoline,ncomp=5)
ypred = predict(oct.train,ncomp=5,newdata=gasoline.test)
yact = gasoline.test$octane
sqrt(mean((ypred-yact)^2)) 

pls.cv = function(X,y,ncomp=2,p=.667,B=100) {
  n = length(y)
  X = scale(X)
  data = data.frame(X,y)
  cv <- rep(0,B)
  for (i in 1:B) {
    ss <- floor(n*p)
    sam <- sample(1:n,ss,replace=F)
    fit2 <- plsr(y~.,ncomp=ncomp,data=data[sam,])
    ynew <- predict(fit2,ncomp=ncomp,newdata=data[-sam,])
    cv[i] <- sqrt(mean((y[-sam]-ynew)^2,na.rm=T))
  }
  cv
}

pcr.cv = function(X,y,ncomp=2,p=.667,B=100) {
  n = length(y)
  X = scale(X)
  data = data.frame(X,y)
  cv <- rep(0,B)
  for (i in 1:B) {
    ss <- floor(n*p)
    sam <- sample(1:n,ss,replace=F)
    fit2 <- pcr(y~.,ncomp=ncomp,data=data[sam,])
    ynew <- predict(fit2,ncomp=ncomp,newdata=data[-sam,])
    cv[i] <- sqrt(mean((y[-sam]-ynew)^2,na.rm=T))
  }
  cv
}

result <- pls.cv(gasoline$NIR,gasoline$octane,ncomp = 5,p= .8)
result
mean(result)

result1 <- pcr.cv(gasoline$NIR,gasoline$octane,ncomp = 6,p= .8)
mean(result1)

#2

set.seed(1)
QSAR.melt = QSAR.melt[,-1] # remove Case column which is an ID
 train = sample(nrow(QSAR.melt),3900)
 test = -(train)
 X = QSAR.melt[,-1]   # grab all the predictors, Y = MTP is the 1st column
 Xs = scale(X)	     # scale the predictors
 QSAR = data.frame(MTP=QSAR.melt$MTP,Xs)
 qsar.train = QSAR[train,]
 qsar.test = QSAR[test,]

 qsar.pcr = pcr(MTP~.,ncomp=40,validation="CV",data=qsar.train)
 summary(qsar.pcr)
  qsar.pls = plsr(MTP~.,ncomp=40,validation="CV",data=qsar.train)
 summary(qsar.pls)
  
 qsar.train = pcr(MTP~.,data=qsar.train,ncomp=14)
 ypred = predict(qsar.train,ncomp=14,newdata=qsar.test)
 yact = qsar.test$MTP
 sqrt(mean((ypred-yact)^2)) 
 
 qsar.train <- plsr(MTP~.,data=qsar.train,ncomp=18)
 ypred = predict(qsar.train,ncomp=18,newdata=qsar.test)
 yact = gasoline.test$octane
 sqrt(mean((ypred-yact)^2)) 
  
 
 result <- pls.cv(Xs,QSAR$MTP, ncomp = 14)
summary(result)
   pls.cv = function(X,y,ncomp=2,p=.667,B=100) {
    n = length(y)
    X = scale(X)
    data = data.frame(X,y)
    cv <- rep(0,B)
    for (i in 1:B) {
      ss <- floor(n*p)
      sam <- sample(1:n,ss,replace=F)
      fit2 <- plsr(y~.,ncomp=ncomp,data=data[sam,])
      ynew <- predict(fit2,ncomp=ncomp,newdata=data[-sam,])
      cv[i] <- sqrt(mean((y[-sam]-ynew)^2,na.rm=T))
    }
    cv
  }
  
  result <- pcr.cv(Xs,QSAR$MTP,ncomp = 18)
  summary(result)
  pcr.cv = function(X,y,ncomp=2,p=.667,B=100) {
    n = length(y)
    X = scale(X)
    data = data.frame(X,y)
    cv <- rep(0,B)
    for (i in 1:B) {
      ss <- floor(n*p)
      sam <- sample(1:n,ss,replace=F)
      fit2 <- pcr(y~.,ncomp=ncomp,data=data[sam,])
      ynew <- predict(fit2,ncomp=ncomp,newdata=data[-sam,])
      cv[i] <- sqrt(mean((y[-sam]-ynew)^2,na.rm=T))
    }
    cv
  }
  
  