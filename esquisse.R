library(tidyverse)
library(lessR)
library(caret)
source('getData.R')

# Get and clean data
mydata <- getData()
validcols <- colSums(is.na(mydata))/length(mydata) <= 0.05
mydata <- mydata[, validcols]
sum(colSums(is.na(mydata)))
classe <- factor(mydata$classe)
mydata <- mydata %>% select(roll_belt:classe) %>% 
  select_if(is.numeric)
View(mydata)
mydata$classe <- classe
ca(mydata)

# PCA
library(FactoMineR)
res.pca <- PCA(subset(mydata, select = -classe), ncp = 12, graph = FALSE)
res.pca
library(factoextra)
fviz_screeplot(res.pca, ncp=12)
res.pca$eig
g <- NULL
for (i in 1:12) {
  g[[i]] <- fviz_contrib(res.pca, choice = "var", axes = i)
  print(g[[i]])
}
fviz_contrib(res.pca, choice = "var", axes = 1:12)
res.desc <- dimdesc(res.pca, axes = 12)
res.desc
res.pca$var$contrib
# rio::export(res.pca$var$contrib, "varcontrib.xlsx")
var.contribs <- c("roll_belt", "pitch_belt", "total_accel_belt", 
                  "accel_belt_x", "accel_belt_y", "accel_belt_z", 
                  "magnet_belt_x", "magnet_belt_z", "total_accel_arm", 
                  "gyros_arm_x", "gyros_arm_y", "magnet_arm_y", 
                  "roll_dumbbell", "pitch_dumbbell", "yaw_dumbbell", 
                  "gyros_dumbbell_x", "gyros_dumbbell_z", 
                  "gyros_forearm_y", "gyros_forearm_z", 
                  "accel_forearm_y", "accel_forearm_z", "magnet_forearm_x", 
                  "magnet_forearm_y", "magnet_forearm_z")

mydata <- mydata[var.contribs]
# mydata$classe <- classe
# mydata.cor <- Correlation(mydata, graphics = TRUE)
# mydata.cor$cors

# Correlation analysis
# rio::export(mydata.cor$cors, "cormat.xlsx")
highlyCorDescr <- findCorrelation(cor(mydata), cutoff = .7)
names(mydata[highlyCorDescr])
mydata <- mydata[, -highlyCorDescr]
mydata$classe <- classe
View(mydata)

# Random Forest
library(randomForest)
ind.train <- createDataPartition(mydata$classe, p = 0.75, list = FALSE)
mydata.train <- mydata[ind.train, ]
mydata.test <- mydata[-ind.train, ]
set.seed(300)
rf <- randomForest(classe ~ ., mydata.train)
rf
rf.pred <- predict(rf, newdata = mydata.test)
rf.acc <- confusionMatrix(rf.pred, mydata.test$classe)
rf.acc 
rf.acc$overall[[1]]
save(rf, file = "rfModel.rda")

# C5.0
library("C50")
set.seed(300)
c5 <- C5.0(classe ~ ., data = mydata.train, trials = 30)
summary(c5)
c5.pred <- predict(c5, newdata = mydata.test)
c5.acc <- confusionMatrix(c5.pred, mydata.test$classe)
c5.acc$overall[[1]]
save(c5, file = "C50Model.rda")
