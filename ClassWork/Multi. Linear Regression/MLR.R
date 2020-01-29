setwd("C:/Users/yz9186ci/Desktop/DSCI425/Assignments/assign 1")

#Data
  king <- read.csv("King County Homes (test).csv")
  kingtest <- read.csv("King County Homes (train).csv")
  
  names(kingtest)
#Packages  
  load("Regression.Rdata")
  load("mult.Rdata")  
  
#changing type of data
  king$waterfront <- as.factor(king$waterfront)  
  king$view <- as.factor(king$view)
  king$condition <- as.factor(king$condition)    
  king$yr_built <- as.factor(king$yr_built)  
  king$yr_renovated <- as.factor(king$yr_renovated)
  king$renovated <- as.factor(king$renovated)    
  king$zipcode <- as.factor(king$zipcode)
  king$lat <- as.factor(king$lat)    
  king$long <- as.factor(king$long)  
  str(king)
  
# 1
  
  #a
    king <- king[,-1]
    king.ols <- lm(king ~., data = king)
    