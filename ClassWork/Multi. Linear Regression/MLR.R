setwd("C:/Users/yz9186ci/Desktop/DSCI425/ClassWork/ClassWork/Multi. Linear Regression")#Laptop


#Data
  king <- read.csv("King County Homes (test).csv")
  kingtest <- read.csv("King County Homes (train).csv")
  
  names(king)
  names(kingtest)
#Packages  
  load("Regression.Rdata")
  load("mult.Rdata")  
  library(rockchalk)
#changing type of data
  #Train Data
    king$waterfront <- as.factor(king$waterfront)  
    king$view <- as.factor(king$view)
    king$condition <- as.factor(king$condition)    
    king$renovated <- as.factor(king$renovated)    
    king$zipcode <- as.factor(king$zipcode)
    king$grade <- as.factor(king$grade)
    king$floors <- as.factor(king$floors)
    king$bathrooms <- as.factor(king$bathrooms)
    king$bedrooms <- as.factor(king$bedrooms)
    str(king)
  #Test Data
    kingtest$waterfront <- as.factor(kingtest$waterfront)
    kingtest$view <- as.factor(kingtest$view)
    kingtest$condition <- as.factor(kingtest$condition)
    kingtest$renovated <- as.factor(kingtest$renovated)
    kingtest$zipcode <- as.factor(kingtest$zipcode)
    kingtest$grade <- as.factor(kingtest$grade)
    kingtest$floors <- as.factor(kingtest$floors)
    kingtest$bedrooms <- as.factor(kingtest$bedrooms)
    kingtest$bathrooms <- as.factor(kingtest$bathrooms)
    str(kingtest)
# 1
  
  #a
    king <- king[,-1]
    kingtest <- kingtest[,-1]
    king.lm <- lm(price~., data = kingtest)
    summary(king.lm)
    par(mfrow = c(2,2))
    plot(king.lm)  
    str(kingtest)
    head(kingtest)
    pairs.plus2(kingtest[,c(1,4,5,11:14,17:20)])
    
    table(king$bedrooms)
    king$bedrooms <- combineLevels(king$bedrooms, levs = c("7","8","9","6"), newLabel = "6,7,8,9")
    king$bedrooms <- combineLevels(king$bedrooms, levs = c("0","1"), newLabel = "0,1")
    table(king$bathrooms)
    king$bathrooms <- combineLevels(king$bathrooms, levs = c("0.5","0.75","1","1.25"), newLabel = "1")
    king$bathrooms <- combineLevels(king$bathrooms, levs = c("1.5","1.75","2","2.25"), newLabel = "2")
    king$bathrooms <- combineLevels(king$bathrooms, levs = c("2.75","3","3.25","2.5"), newLabel = "3")
    king$bathrooms <- combineLevels(king$bathrooms, levs = c("3.75","4","4.25","3.5"), newLabel = "4")
    king$bathrooms <- combineLevels(king$bathrooms, levs = c("4.5", "5","5.25",4.75), newLabel = "5")
    king$bathrooms <- combineLevels(king$bathrooms, levs = c("5.5","6","6.25"), newLabel = "6")
    king$bathrooms <- combineLevels(king$bathrooms, levs = c("6.75","8"),newLabel = "8")
    king$bathrooms <- combineLevels(king$bathrooms, levs = c("5","6","8"), newLabel = "5,6,8")
    table(king$floors)
    king$floors <- combineLevels(king$floors, levs = c("1","1.5"), newLabel = "1")
    king$floors <- combineLevels(king$floors, levs = c("2","2.5"), newLabel = "2")
    king$floors <- combineLevels(king$floors, levs = c("3","3.5"), newLabel = "3")
    table(king$view)
    table(king$condition)
    king$condition <- combineLevels(king$condition, levs = c("1","2"), newLabel = "1,2")
    table(king$grade) 
    king$grade <- combineLevels(king$grade,levs = c("12","13"), newLabel = "12,13")
    king$grade <- combineLevels(king$grade, levs = c("3","4","5"), newLabel = "3,4,5")
    table(king$renovated)
    table(king$zipcode)    
    
  