setwd("C:/Users/yz9186ci/Desktop/DSCI425/ClassWork/ClassWork/Neural Network")#Laptop

# packages 
  library(nnet)
  library(caret)
  load("Regression.Rdata")
  load("mult.Rdata") 
#data
  concrete <- read.csv("Concrete.csv")
  names(concrete)

#functions
  nnet.sscv <- function(x,y,fit,p=.667,B=100,size=3,decay=fit$decay,skip=T,
                      linout=T,maxit=10000){
    n = length(y)
    MSEP = rep(0,B)
    MAEP = rep(0,B)
    MAPEP = rep(0,B)
    ss = floor(n*p)
    for (i in 1:B){
      sam = sample(1:n,ss,replace=F)
      fit2 = nnet(x[sam,],y[sam],size=size,linout=linout,skip=skip,decay=decay,
                  maxit=maxit,trace=F)
      ynew = predict(fit2,newdata=x[-sam,])
      MSEP[i]=mean((y[-sam]-ynew)^2)
      MAEP[i]=mean(abs(y[-sam]-ynew))
      MAPEP[i]=mean(abs(y[-sam]-ynew)/y[-sam])
    }
    RMSEP = sqrt(mean(MSEP))
    MAE = mean(MAEP)
    MAPE = mean(MAPEP)
    cat("RMSEP\n")
    cat("===============\n")
    cat(RMSEP,"\n\n")
    cat("MAE\n")
    cat("===============\n")
    cat(MAE,"\n\n")
    cat("MAPE\n")
    cat("===============\n")
    cat(MAPE,"\n\n")
    temp = data.frame(MSEP=MSEP,MAEP=MAEP,MAPEP=MAPEP)
    return(temp)
  }
  
#a
  sam <- sample(1:1030, size = floor(.6666*1030),replace = F)
  train <- con.train[sam,]
  val <- con.train[-sam,]
  names(concrete)
  #untransformed
    concrete.nnet <- nnet(Strength ~., data = concrete, size = 10, lineout = T, skip = T, maxit = 1000, decay = .001)
    con.x <- concrete[,-1]
    con.y <- concrete[,1]    
    result.nn10 <- nnet.sscv(con.x, con.y, concrete.nnet, size = 10)
    result.nn5 <- nnet.sscv(con.x, con.y, concrete.nnet, size = 5)
    result.nn3 <- nnet.sscv(con.x, con.y, concrete.nnet, size = 3)
    result.nn10
    summary(result.nn10)
    trendscat(concrete$Strength, fitted(concrete.nnet))
    cor(concrete$Strength,fitted(concrete.nnet))^2
  #transformed
    con.train <- concrete
    con.bc <- preProcess(con.train, method = "BoxCox")
    con.bc$bc    
    con.train$Strength <- con.train$Strength^(6/10)
    con.train$Cement <- con.train$Cement^(1/5)
    con.train$Water <- con.train$Water^(8/10)
    con.train$CourseAgg <- con.train$CourseAgg^(11/10)
    con.train$FineAge <- con.train$FineAge^(18/10)
    con.train$Age <- log(con.train$Age)
    
    con.train.nn <- nnet(Strength ~., data = con.train, size = 10, lineout = T, skip = T, maxit = 10000, decay = .001)
    con.train.x <- con.train[,-1]
    con.train.y <- con.train[,1]    
    result.trainNn10 <- nnet.sscv(con.train.x, con.train.y, con.train.nn, size = 10)
    result.trainNn5 <- nnet.sscv(con.train.x, con.train.y, con.train.nn, size = 5)
    result.trainNn3 <- nnet.sscv(con.train.x, con.train.y, con.train.nn, size = 3)
    summary(result.trainNn10)
    trendscat(con.train$Strength,fitted(con.train.nn))
    cor(con.train$Strength,fitted(con.train.nn)) 
    
#b
    concrete.lm <- lm(Strength~., data = con.train)
    king.back <- step(concrete.lm, direction = "backward")
    ypred <- predict(concrete.lm, newdata = train)    
    ypred
    results <- PredAcc(val$Strength,ypred)
    results
    