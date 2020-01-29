setwd("C:/Users/yz9186ci/Desktop/DSCI425/ClassWork/ClassWork/Multi. Linear Regression")#Laptop

#Data
  king <- read.csv("King County Homes (test).csv")
  kingtest <- read.csv("King County Homes (train).csv")
  
  names(king)
  names(kingtest)
#Packages  
  load("Regression.Rdata")
  load("mult.Rdata")  
  
#changing type of data
  #Train Data
    king$waterfront <- as.factor(king$waterfront)  
    king$view <- as.factor(king$view)
    king$condition <- as.factor(king$condition)    
    king$yr_built <- as.factor(king$yr_built)  
    king$yr_renovated <- as.factor(king$yr_renovated)
    king$renovated <- as.factor(king$renovated)    
    king$zipcode <- as.factor(king$zipcode)
    king$lat <- as.factor(king$lat)    
    king$long <- as.factor(king$long)
    king$grade <- as.factor(king$grade)
    str(king)
  #Test Data
    kingtest$waterfront <- as.factor(kingtest$waterfront)
    kingtest$view <- as.factor(kingtest$view)
    kingtest$condition <- as.factor(kingtest$condition)
    kingtest$yr_built <- as.factor(kingtest$yr_built)
    kingtest$yr_renovated <- as.factor(kingtest$yr_renovated)
    kingtest$renovated <- as.factor(kingtest$renovated)
    kingtest$zipcode <- as.factor(kingtest$zipcode)
    kingtest$lat <- as.factor(kingtest$lat)
    kingtest$long <- as.factor(kingtest$long)
    kingtest$grade <- as.factor(kingtest$grade)
    str(kingtest)
# 1
  
  #a
    king <- king[,-1]
    king.lm <- lm(price~., data = kingtest)
    summary(king.lm)
    par(mfrow = c(2,2))
    plot(king.lm)    