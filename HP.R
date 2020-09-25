library("readxl")
train <- read.csv("C:/Users/praga/Downloads/ChromDownloads/train.csv")
dim(train)

View(train)
attach(train)

install.packages("DataExplorer")
library("DataExplorer")
#Reducing the dataset to numeric variables only
train.num=dplyr::select_if(train, is.numeric)
DataExplorer::create_report(train.num)

##GarageYrblt and Lotfrontage have missing values among continuous features

#Summary
summary(train)

#To find the sum of NA in each column of dataframe.
null.sum=colSums(is.na(train))
Nulls=null.sum[which(null.sum>0)]
Nulls
##output
#LotFrontage        Alley   MasVnrType   MasVnrArea     BsmtQual     BsmtCond BsmtExposure 
#259         1369            8            8           37           37           38 
#BsmtFinType1 BsmtFinType2   Electrical  FireplaceQu   GarageType  GarageYrBlt GarageFinish 
#37           38            1          690           81           81           81 
#GarageQual   GarageCond       PoolQC        Fence  MiscFeature 
#81           81         1453         1179         1406 
##

names(train)
#Converting mssubclass from numeric to categorical feature
train$MSSubClass=as.factor(train$MSSubClass)   


#SalePrice HISTOGRAM
train$SalePrice=log(train$SalePrice)
hist(train$SalePrice)

#CORRELATION PLOT

#install.packages("corrplot")
library(corrplot)
par(mfrow=c(1,1))
cor.mat=cor(as.matrix(train.num[1:20],train.num$SalePrice))
corrplot(cor.mat, method="circle", type="upper")

cor.mat=cor(as.matrix(train.num[21:37],train.num$SalePrice))
corrplot(cor.mat, method="circle", type="upper")

library(dplyr)
library(magrittr)
library(forcats)
names(train)
train%<>% mutate(Alley = fct_explicit_na(f = train$Alley, na_level = "None"))
train%<>% mutate(BsmtCond = fct_explicit_na(f = train$BsmtCond, na_level = "None"))
train%<>% mutate(fence = fct_explicit_na(f = train$fence, na_level = "None"))
train%<>% mutate(PoolQC = fct_explicit_na(f = train$PoolQC, na_level = "None"))

any(is.na(train.num))
summary(train.num)
which(is.na(train.num$LotFrontage))
#Replacing NA with 0
train.num[is.na(train.num)]=0
train.num[]
#LotFrontage 259       ,MasVnrArea 8, GarageYrBlt 81,

#LM MODEL
lm.all.num=lm(SalePrice~.,data = train.num)
summary(lm.all.num)
plot(lm.all.num)

#Checking for outliers
summary(train.num$TotalBsmtSF)
boxplot(train.num$TotalBsmtSF)
boxplot.stats(train.num$TotalBsmtSF)
train.num=train.num[-which(train.num$TotalBsmtSF%in%c(2223,3200,2330,6110,0,3138,2633,2524,2216,2109,2392,2077,2444,2121,2136,2396,3206,2158,2078,2136,2217,3094,2076,2153,2110,2000,2006,2035,2042,2046,2002,2033)),]
dim(train.num)

summary(train.num$GrLivArea)
boxplot(train.num$GrLivArea)
boxplot.stats(train.num$GrLivArea)
train.num=train.num[-which(train.num$GrLivArea%in%c(2945, 2696, 3222, 3608, 2727 ,3112, 2794, 3493, 2730 ,2978, 2728, 2713, 2775, 3194, 3395, 2704 ,2715 ,3279, 3140
                                                     ,2822, 2872, 2898, 3082,2868,2828, 3627, 3086 ,2872, 3447, 2810, 2792, 2784)),]


dim(train.num)                           

#LM MODEL 2
lm.all.num1=lm(SalePrice~.,data = train.num)
summary(lm.all.num1)
plot(lm.all.num1)
confint(lm.all.num1)

#WHY AM I GETTING NA in the coefficients of below variables?
#Scatterplot
plot(train.num$SalePrice,train.num$GrLivArea)
plot(train.num$SalePrice,train.num$TotalBsmtSF)
any(is.na(train.num$TotalBsmtSF))
any(is.na(train.num$GrLivArea))
any(is.na(train.num$LotFrontage))


#Checking relationship of insignificant features with Target 
par(mfrow=c(2,3))

plot(train.num$SalePrice,train.num$MasVnrArea)
plot(train.num$SalePrice,train.num$LowQualFinSF)
plot(train.num$SalePrice,train.num$BsmtFullBath)
plot(train.num$SalePrice,train.num$GarageArea)
plot(train.num$SalePrice,train.num$WoodDeckSF)
plot(train.num$SalePrice,train.num$X3SsnPorch)
plot(train.num$SalePrice,train.num$PoolArea)
plot(train.num$SalePrice,train.num$MiscVal)
plot(train.num$SalePrice,train.num$MoSold)
plot(train.num$SalePrice,train.num$YrSold)



#LM MODEL 3
lm.select.num2=lm(SalePrice~LotFrontage+LotArea+OverallQual+OverallCond+YearBuilt+YearRemodAdd+BsmtFinSF1+TotalBsmtSF+KitchenAbvGr+Fireplaces+ScreenPorch,data = train.num)
summary(lm.select.num2)
plot(lm.select.num2)

#Model 4
lm.select.num2=lm(SalePrice~LotFrontage+LotArea+OverallQual+OverallCond+YearBuilt+X2ndFlrSF+X1stFlrSF+TotalBsmtSF+KitchenAbvGr+Fireplaces,data = train.num)
summary(lm.select.num2)
plot(lm.select.num2)
confint(lm.select.num2)
#Model 5 #Here F stat dropped
lm.select.num2=lm(SalePrice~LotFrontage+LotArea+OverallQual+OverallCond+YearBuilt+X2ndFlrSF+X1stFlrSF+TotalBsmtSF+KitchenAbvGr+Fireplaces+YrSold+ScreenPorch+EnclosedPorch+GarageCars+GarageYrBlt+TotRmsAbvGrd+BedroomAbvGr+HalfBath+FullBath+BsmtFullBath+LowQualFinSF+BsmtUnfSF+YearRemodAdd,data = train.num)
summary(lm.select.num2)
plot(lm.select.num2)
confint(lm.select.num2)

lm.select.num3=lm(SalePrice~LotFrontage+LotArea+OverallQual+OverallCond+YearBuilt+X2ndFlrSF+X1stFlrSF+TotalBsmtSF+KitchenAbvGr+Fireplaces+YrSold+ScreenPorch+EnclosedPorch+GarageCars+TotRmsAbvGrd+BedroomAbvGr+BsmtFullBath+LowQualFinSF+BsmtUnfSF+YearRemodAdd,data = train.num)
summary(lm.select.num3)
plot(lm.select.num3)
confint(lm.select.num3)

summary(train.num$KitchenAbvGr)
######Correlation of train.num after preprocessing
library(corrplot)
par(mfrow=c(1,1))
cor.mat=cor(as.matrix(train.num[1:20],train.num$SalePrice))
corrplot(cor.mat, method="circle", type="upper")

cor.mat=cor(as.matrix(train.num[21:37],train.num$SalePrice))
corrplot(cor.mat, method="circle", type="upper")
#==============================
 
                           
