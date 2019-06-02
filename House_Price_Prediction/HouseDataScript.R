fillMisvalue <- function(dt){
  Nas <- NaCol(dt)
  naData <- dt[,names(Nas)]
  NaClassNum <- names(sapply(naData,class)[sapply(naData,class)=="numeric"])
  NaClassChar <- names(sapply(naData,class)[sapply(naData,class)=="character"])
  for (c in NaClassNum) {
    dt[][[c]] <- if_else(is.na(dt[][[c]]),round(mean(dt[][[c]],na.rm = TRUE)),dt[][[c]])
  }
  for (d in NaClassChar) {
    dt[][[d]] <- if_else(is.na(dt[][[d]]),"none",dt[][[d]])
  }
return(dt)
}



spreadOutCol <- function(dt,dcName){
  
  for (c in dcName) {
    dt <- dt %>% spread(key = c,value = c)
  }
  
  return(dt)
}


fillMisBin <- function(dt){

  Nas <- NaCol(dt)
  NaClassNum <- names(Nas)
  
  for (c in NaClassNum) {
    dt[][[c]] <- if_else(is.na(dt[][[c]]),0,1)
  }
  return(dt)
}



NaCol <- function(dt){
  sapply(lapply(dt,is.na),sum)[sapply(lapply(dt,is.na),sum)>0]
}


JoinData <- function(data1,data2){
  rbind(data1[, 1:(dim(data1)[2] - 1)], data2)  
}


library(tidyverse)
trainPath <- file.path("C:/My Things/Kaggle/house-prices-advanced-regression-techniques/train.csv")
testPath <- file.path("C:/My Things/Kaggle/house-prices-advanced-regression-techniques/test.csv")

trainData <- read_csv(trainPath)
testData <- read_csv(testPath)

View(trainData)



combineData <- JoinData(trainData,testData)
View(combineData)


#round((NaCol(combineData)/nrow(combineData))*100,2)

#removeCol <- names(round((NaCol(combineData)/nrow(combineData))*100,2)[round((NaCol(combineData)/nrow(combineData))*100,2)>80])

#removeCol <- c(removeCol,"Id")

#View(combineData[,removeCol])

#which(names(combineData) %in% removeCol)

#dim(select(combineData,-removeCol))
#dim(combineData[,-which(names(combineData) %in% removeCol)])


#combineData <- select(combineData,-removeCol)
#View(combineData)

unique(combineData$MSSubClass)
trainData %>% ggplot(aes(MSSubClass,SalePrice)) + geom_col(aes(col=MSSubClass))


trainData %>% group_by(MSSubClass) %>% summarise(mxs = max(SalePrice)) %>% arrange(desc(mxs)) 

discreteCol <- 'MSSubClass'

glimpse(combineData$MSZoning)
summary(combineData$MSZoning)
unique(combineData$MSZoning)

trainData %>% ggplot(aes(MSZoning,SalePrice)) + geom_col(aes(fill=MSZoning))

trainData %>% group_by(MSZoning) %>% summarise(mxs = max(SalePrice)) %>% arrange(desc(mxs))
trainData %>% count(MSZoning)

modes(combineData$MSSubClass)

combineData$MSZoning[which(is.na(combineData$MSZoning))] <- "RL"

View(combineData)

discreteCol <- c(discreteCol,"MSZoning")


starts_with(match = "Lot",vars = names(combineData))

summary(combineData$LotFrontage)


combineData$LotFrontage[which(is.na(combineData$LotFrontage))] <- median(combineData$LotFrontage,na.rm = TRUE)

combineData <- combineData %>% mutate(TotalLotArea = LotFrontage + LotArea)
removeCol <- c("LotFrontage","LotArea")
combineData <- select(combineData,-removeCol)


View(combineData)

trainData %>% ggplot(aes(LotShape,SalePrice)) + geom_point() + scale_y_log10() + 
  theme(panel.grid = element_blank(),panel.background = element_blank(),
        axis.ticks = element_blank(),axis.title = element_blank(),axis.text = element_blank())

trainData %>% group_by(LotShape) %>% summarise(max(SalePrice))

View(select(.data = trainData,LotShape,SalePrice))


unique(trainData$LotConfig)

trainData %>% ggplot(aes(LotConfig,SalePrice)) + geom_point() + scale_y_log10()


discreteCol <- c(discreteCol,"LotShape","LotConfig")


NaCol(combineData)
trainData %>% ggplot(aes(Alley,SalePrice)) + geom_point()

combineData$Alley[which(is.na(combineData$Alley))] <- "NoAlley"

discreteCol <- c(discreteCol,"Alley","Street")


starts_with("Land",vars = names(combineData))

combineData %>% count(LandSlope)
combineData %>% count(LandContour)


trainData %>% ggplot(aes(LandSlope,SalePrice)) + geom_boxplot()
discreteCol <- c(discreteCol,"LandContour","LandSlope")

trainData %>% ggplot(aes(Utilities,SalePrice)) + geom_boxplot() +scale_y_log10()

combineData %>% count(Utilities)

trainData %>% group_by(Utilities) %>% summarise(median(SalePrice))

removeCol <- "Utilities"
combineData <- combineData %>% select(-removeCol)

NaCol(combineData)

combineData %>% count(Neighborhood)

trainData %>% ggplot(aes(Neighborhood)) + geom_bar(aes(fill = Neighborhood))

trainData %>% ggplot(aes(Neighborhood,SalePrice)) + geom_col(aes(fill = Neighborhood))

discreteCol <- c(discreteCol,"Neighborhood")

View(combineData %>% select(Condition1,Condition2))
sum(combineData$Condition1==combineData$Condition2)/nrow(combineData)

trainData %>% ggplot(aes(Condition2,SalePrice)) + geom_boxplot()


combineData <- combineData %>% unite(col = "Condition",... = Condition1,Condition2 ,sep = "-")

discreteCol <- c(discreteCol,"Condition")

View(combineData %>% select(MSSubClass,BldgType,HouseStyle) %>% filter(MSSubClass<=40))
View(combineData %>% select(MSSubClass,BldgType,HouseStyle) %>% filter(MSSubClass<=50))


discreteCol <- c(discreteCol,"BldgType","HouseStyle")

length(discreteCol)


combineData <- combineData %>% mutate(OverAllRate = ceiling((OverallQual+OverallCond)/2))
removeCol <- c("OverallQual","OverallCond")

combineData <- combineData %>% select(-removeCol)

contains("year",vars = names(trainData))

View(trainData[,c(20,21,77,78,81)])
Sys.time()





range(combineData$YearBuilt)
range(combineData$YrSold)



trainData %>% ggplot(aes(YrSold,SalePrice)) + geom_point()


NaCol(combineData)
summary(combineData$YearBuilt)

View(trainData %>% select(20,21,77,78,81) %>% filter(YearBuilt == 1976))

combineData <- combineData %>% mutate(YearOld = YrSold - (YearBuilt + ceiling((YearRemodAdd - YearBuilt)/2)))

removeCol <- c("YearRemodAdd","YearBuilt")

combineData <- combineData %>% select(-removeCol)

combineData <- combineData %>% select(-MoSold)

discreteCol <- c(discreteCol,"YrSold")


contains("roof",vars = names(combineData))

unique(combineData$RoofMatl)
unique(combineData[15])


combineData %>% count(RoofStyle)
combineData %>% count(RoofMatl)
trainData %>% count(RoofMatl)
testData %>% count(RoofMatl)

discreteCol <- c(discreteCol,"RoofMatl","RoofStyle")

NaCol(combineData)

combineData %>% count(Exterior1st) %>% arrange(desc(n))

which(is.na(combineData$Exterior2nd))

combineData$Exterior1st[which(is.na(combineData$Exterior1st))] <- 'VinylSd'
combineData$Exterior1st[which(is.na(combineData$Exterior2nd))] <- 'VinylSd'


combineData <- combineData %>% unite(col = "Exterior",... = Exterior1st,Exterior2nd,sep = "-")

unique(combineData$Exterior)

NaCol(combineData)

ind <- which(is.na(combineData$MasVnrType))[which(!which(is.na(combineData$MasVnrType)) %in% which(is.na(combineData$MasVnrArea)))]

combineData %>% select(Id,MasVnrType,MasVnrArea) %>% filter(Id == ind)


combineData %>% count(MasVnrType)

combineData %>% ggplot(aes(MasVnrType,MasVnrArea)) + geom_boxplot()

combineData %>% group_by(MasVnrType) %>% summarise(median(MasVnrArea))

combineData$MasVnrType[ind] <- 'BrkFace'


combineData$MasVnrType[which(is.na(combineData$MasVnrType) | combineData$MasVnrType=='None')] <- "NoMasVnr"

combineData$MasVnrArea[which(is.na(combineData$MasVnrArea))] <- 0

combineData %>% select(Id,MasVnrType,MasVnrArea) %>% filter(MasVnrType=="NoMasVnr",MasVnrArea>0)

discreteCol <- c(discreteCol,"MasVnrType")





trainData %>% ggplot(aes(ExterCond,SalePrice)) + geom_point()

combineData %>% count(ExterQual)
combineData %>% count(ExterCond)


unique(combineData %>% unite(col = ExteriorRate,ExterQual,ExterCond,sep = '-') %>% select(ExteriorRate))


combineData <- combineData %>% 
       mutate(ExterQual = case_when(ExterQual=="Po" ~ 1,ExterQual=="Fa" ~ 2,ExterQual=="TA" ~ 3,ExterQual=="Gd" ~ 4,ExterQual=="Ex" ~ 5),
              ExterCond = case_when(ExterCond=="Po" ~ 1,ExterCond=="Fa" ~ 2,ExterCond=="TA" ~ 3,ExterCond=="Gd" ~ 4,ExterCond=="Ex" ~ 5))     %>%
              mutate(ExterRate = ceiling((ExterQual+ExterCond)/2)              
                            )


#combineData <- select(combineData,-c("ExterRate","ExterCond","ExterQual"))

#combineData <- cbind(rbind(trainData %>% select(ExterCond,ExterQual), testData %>% select(ExterCond,ExterQual)),combineData)



removeCol <- c("ExterCond","ExterQual")

combineData <- combineData %>% select(-removeCol)

discreteCol <- c(discreteCol,"Foundation")


View(combineData[contains("bsmt",vars = names(combineData))])

NaCol(combineData)

nind <- which(is.na(combineData$BsmtCond))[which(!which(is.na(combineData$BsmtCond)) %in% which(is.na(combineData$BsmtQual)))]

combineData %>% select(Id,BsmtQual,BsmtCond) %>% filter(Id %in% nind)

#combineData <- combineData %>% select(-BsmtRate) 
#combineData <- cbind(rbind(trainData %>% select(BsmtCond,BsmtQual), testData %>% select(BsmtCond,BsmtQual)),combineData)

#combineData$BsmtCond[which(is.na(combineData$BsmtCond))] <- 'NoBsmt'
#combineData$BsmtQual[which(is.na(combineData$BsmtQual))] <- 'NoBsmt'


combineData <- combineData %>% 
  mutate(BsmtExposure = case_when((is.na(BsmtExposure) | BsmtExposure == "No") ~ 0,BsmtExposure == "Mn" ~ 1, BsmtExposure == "Av" ~ 2, BsmtExposure == "Gd" ~ 3))



which(is.na(combineData$BsmtFinType2))[which(!(which(is.na(combineData$BsmtFinType2)) %in% which(is.na(combineData$BsmtFinType1))))]

combineData %>% select(BsmtFinType1,BsmtFinSF1,BsmtFinType2,BsmtFinSF2,Id,TotalBsmtSF,BsmtUnfSF) %>% filter(Id %in% which(is.na(combineData$BsmtFinType2)))



combineData <- combineData %>% 
  mutate(BsmtQual = case_when(is.na(BsmtQual) ~ 0,BsmtQual=="Po" ~ 1,BsmtQual=="Fa" ~ 2,BsmtQual=="TA" ~ 3,BsmtQual=="Gd" ~ 4,BsmtQual=="Ex" ~ 5),
         BsmtCond = case_when(is.na(BsmtCond) ~ 0,BsmtCond=="Po" ~ 1,BsmtCond=="Fa" ~ 2,BsmtCond=="TA" ~ 3,BsmtCond=="Gd" ~ 4,BsmtCond=="Ex" ~ 5),
         BsmtFinType1 = case_when((is.na(BsmtFinType1) | BsmtFinType1 == "Unf") ~ 0,BsmtFinType1 == "LwQ" ~ 1,BsmtFinType1 == "Rec" ~ 2,BsmtFinType1 == "BLQ" ~ 3,BsmtFinType1 == "ALQ" ~ 4,BsmtFinType1 == "GLQ" ~ 5),
         BsmtFinType2 = case_when((is.na(BsmtFinType2) | BsmtFinType2 == "Unf") ~ 0,BsmtFinType2 == "LwQ" ~ 1,BsmtFinType2 == "Rec" ~ 2,BsmtFinType2 == "BLQ" ~ 3,BsmtFinType2 == "ALQ" ~ 4,BsmtFinType2 == "GLQ" ~ 5)
         ) %>% mutate(BsmtOverAllRate = ceiling((BsmtCond+BsmtQual+BsmtFinType1+BsmtFinType2)/4))



removeCol <- c("BsmtCond","BsmtQual","BsmtFinType1","BsmtFinType2")

combineData <- combineData %>% select(-c(BsmtFinSF1,BsmtFinSF2,BsmtUnfSF))

combineData$TotalBsmtSF[which(is.na(combineData$TotalBsmtSF))] <- 0

combineData %>% count(BsmtFullBath)
combineData %>% count(BsmtHalfBath)

combineData$BsmtFullBath[which(is.na(combineData$BsmtFullBath))] <- 0
combineData$BsmtHalfBath[which(is.na(combineData$BsmtHalfBath))] <- 0


names(combineData)[contains("heat",vars = names(combineData))]

NaCol(combineData)

discreteCol <- c(discreteCol,"Heating")

combineData <- combineData %>% 
  mutate(HeatingQC = case_when(HeatingQC == "Po" ~ 1,HeatingQC == "Fa" ~ 2, HeatingQC == "TA" ~ 3, HeatingQC == "Gd" ~ 4, HeatingQC == "Ex" ~ 5))

combineData <- combineData %>% mutate(CentralAir = if_else(CentralAir == "Y",true = 1,false = 0))

combineData$Electrical[which(is.na(combineData$Electrical))] <- as.character((combineData %>% count(Electrical) %>% arrange(desc(n)))[1,1])

discreteCol <- c(discreteCol,"Electrical")

unique(combineData$LowQualFinSF)

combineData$LowQualFinSF
removeCol <- c("1stFlrSF","2ndFlrSF","LowQualFinSF")

combineData <- combineData %>% select(-removeCol)

unique(combineData$BedroomAbvGr)
combineData %>% count(KitchenQual)

combineData <- combineData %>% 
  mutate(KitchenQual = case_when(KitchenQual == "Po" ~ 1,KitchenQual == "Fa" ~ 2, KitchenQual == "TA" ~ 3, KitchenQual == "Gd" ~ 4, KitchenQual == "Ex" ~ 5))

combineData$KitchenQual[which(is.na(combineData$KitchenQual))] <- 3

combineData %>% count(Functional)

combineData$Functional[which(is.na(combineData$Functional))] <- "Typ"

discreteCol <- c(discreteCol,"Functional")

NaCol(combineData)

contains("fire",vars = names(combineData))

unique(combineData$Fireplaces)

combineData <- combineData %>% 
  mutate(FireplaceQu = case_when(is.na(FireplaceQu) ~ 0, FireplaceQu == "Po" ~ 1,FireplaceQu == "Fa" ~ 2, FireplaceQu == "TA" ~ 3, FireplaceQu == "Gd" ~ 4, FireplaceQu == "Ex" ~ 5))

View(combineData[contains("garage", vars = names(combineData))])

unique(combineData$GarageType)
combineData$GarageType[which(is.na(combineData$GarageType))] <- "NoGarageTyp"
combineData$GarageFinish[which(is.na(combineData$GarageFinish))] <- "NoGarageFin"

trainData %>% ggplot(aes(GarageYrBlt,SalePrice)) + geom_col()
trainData %>% ggplot(aes(GarageYrBlt)) + geom_bar()

#par(mfcol = c(1,1))
#plot(trainData$GarageYrBlt,trainData$SalePrice)

#hist(trainData$GarageYrBlt)

summary(combineData$GarageYrBlt)
combineData <- combineData %>% select(-GarageYrBlt)
combineData <- combineData %>% select(-GarageCars)

summary(combineData$GarageArea)
which(is.na(combineData$GarageArea))
combineData %>% select(GarageType,GarageFinish,Id,GarageArea,GarageCond,GarageQual) %>% filter(Id == 2577) 

combineData$GarageArea[which(is.na(combineData$GarageArea))] <- 0


combineData <- combineData %>% 
  mutate(GarageCond = case_when(is.na(GarageCond) ~ 0, GarageCond == "Po" ~ 1, GarageCond == "Fa" ~ 2, GarageCond == "TA" ~ 3, GarageCond == "Gd" ~ 4, GarageCond == "Ex" ~ 5),
         GarageQual = case_when(is.na(GarageQual) ~ 0, GarageQual == "Po" ~ 1, GarageQual == "Fa" ~ 2, GarageQual == "TA" ~ 3, GarageQual == "Gd" ~ 4, GarageQual == "Ex" ~ 5)) %>% 
  mutate(GarageOverAllRate = ceiling((GarageCond+GarageQual)/2))


combineData <- combineData %>% select(-c("GarageCond","GarageQual"))

combineData %>% count(PavedDrive)

discreteCol <- c(discreteCol,"PavedDrive")

combineData <- combineData %>% mutate(TotalOutsideArea = WoodDeckSF+OpenPorchSF+EnclosedPorch+`3SsnPorch`+ScreenPorch+PoolArea) %>% 
  select(-c("WoodDeckSF","OpenPorchSF","EnclosedPorch","3SsnPorch","ScreenPorch","PoolArea"))

combineData <- combineData %>% 
  mutate(PoolQC = case_when(is.na(PoolQC) ~ 0, PoolQC == "Po" ~ 1, PoolQC == "Fa" ~ 2, PoolQC == "TA" ~ 3, PoolQC == "Gd" ~ 4, PoolQC == "Ex" ~ 5))

combineData <- combineData %>% 
  mutate(Fence = case_when(is.na(Fence) ~ 0, Fence == "MnWw" ~ 2, Fence == "GdWo" ~ 3, Fence == "MnPrv" ~ 4, Fence == "GdPrv" ~ 5))

NaCol(combineData)

combineData$MiscFeature[which(is.na(combineData$MiscFeature))] <- "NoMisc"

discreteCol <- c(discreteCol,"MiscFeature")


combineData %>% count(SaleType)
combineData$SaleType[which(is.na(combineData$SaleType))] <- "WD"

discreteCol <- c(discreteCol,"SaleType","SaleCondition")

discreteCol <- c(discreteCol,"Exterior","GarageType","GarageFinish")


combineData <- combineData %>% mutate(rowId = 1:n())

combineData <- spreadOutCol(combineData,discreteCol)
combineData <- fillMisBin(combineData)

cor(combineData)



range(combineData$CentralAir)


for (i in 21:30) {
  print(append(i,range(combineData[i])))
}

continuousCol <- c(1:4,6:10,11:20,21:27)
View(combineData[continuousCol])

combineData <- combineData %>% select(-rowId)


normaliseData <- function(dt,normCol){
  for (i in normCol) {
    dt[i] <- round((dt[i]-mean(dt[][[i]]))/sd(dt[][[i]]),digits = 2)
  }
  
  return(dt)
}


combineData <- normaliseData(combineData,continuousCol)

dim(trainData)
dim(testData)
dim(combineData)

sepData <- function(cd,td,te){
  trainData_norm <<- cbind(cd[1:nrow(td), ],td[dim(td)[2]])
  testData_norm <<- cd[nrow(td)+1:nrow(te), ]
}



sepData(combineData,trainData,testData)



write.table(trainData_norm,file = "C:\\My Things\\Kaggle\\house-prices-advanced-regression-techniques\\trainData_norm.txt", sep = "\t", row.names = FALSE, col.names = FALSE)
write.table(testData_norm,file = "C:\\My Things\\Kaggle\\house-prices-advanced-regression-techniques\\testData_norm.txt", sep = "\t", row.names = FALSE, col.names = FALSE)

quit(save = T)

