## !/usr/bin/env Rstudio 1.1.419
## -*- coding: utf-8 -*-
## Coursera JHU Data Science Series
## Lecture 8 Pratical Machine Learning

setwd("/Users/yangchen/Desktop/Coursera/Data_Sci_Coursera/8_Practice_MachineL/")

library(caret); 
library(kernlab); 
data(spam)
head(spam)
inTrain <- createDataPartition(y = spam$type,
                               p = 0.75, list = FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
dim(training)

set.seed(32343)
modelFit <- train(type ~.,data = training, method = "glm")
modelFit
modelFit$finalModel

predictions <- predict(modelFit,newdata = testing)
head(predictions)
confusionMatrix(predictions,testing$type)

set.seed(32323)
folds <- createFolds(y = spam$type,k = 10,
                     list = TRUE,returnTrain = TRUE)
sapply(folds,length)
folds[[1]][1:10]


library(ISLR)
library(ggplot2)
data(Wage)
summary(Wage)

inTrain <- createDataPartition(y = Wage$wage, 
                               p = 0.7, list = FALSE)
#head(inTrain, n = 20)
training <- Wage[inTrain, ]
testing <- Wage[-inTrain, ]
dim(training); dim(testing)

featurePlot(x = training[, c('age', 'education', 'jobclass')],
            y = training$wage, 
            plot = 'pairs')

qq <- qplot(age, wage, color = education, data = training)
qq + geom_smooth(method = "lm", formula = y ~ x)

library(Hmisc)
cutWage <- cut2(training$wage, g = 3)
table(cutWage)
p1 <- qplot(cutWage, age, data = training, fill = cutWage, geom = "boxplot")
p1
p2 <- qplot(cutWage, age, data = training, fill = cutWage, geom = c("boxplot","jitter"))
library(grid)
library(gridExtra)
grid.arrange(p1, p2, ncol = 2)

t1 <- table(cutWage, training$jobclass)
prop.table(t1, 1)

qplot(wage, color = education, data = training, geom = "density")



# preprocessing
hist(training$capitalAve,main = "",xlab = "ave. capital run length")
mean(training$capitalAve)
sd(training$capitalAve)

# standardizing
trainCapAve <- training$capitalAve
trainCapAveS <- (trainCapAve  - mean(trainCapAve))/sd(trainCapAve) 
mean(trainCapAveS)
sd(trainCapAveS)

# standardizing the test set --- by using the mean and sd of training set
testCapAve <- testing$capitalAve
testCapAveS <- (testCapAve  - mean(trainCapAve))/sd(trainCapAve) 
mean(testCapAveS) # mean is not exactly 0
sd(testCapAveS) # sd is not exactly 1

# Standardizing - preProcess function
preObj <- preProcess(training[,-58],method = c("center","scale")) 
# 58 is the actual outcome we want to predict
trainCapAveS <- predict(preObj,training[,-58])$capitalAve
mean(trainCapAveS)
sd(trainCapAveS)
# Take the prepro-object from training set and apply it to the test set
testCapAveS <- predict(preObj,testing[,-58])$capitalAve
mean(testCapAveS)
sd(testCapAveS)

# Standardizing - Box-Cox transforms
preObj <- preProcess(training[,-58],method = c("BoxCox"))
# make the data looked more normal distributed
trainCapAveS <- predict(preObj,training[,-58])$capitalAve
par(mfrow = c(1,2)); hist(trainCapAveS); qqnorm(trainCapAveS)

# Standardizing - Imputing data
library(RANN)
set.seed(13343)
# Make some values NA
training$capAve <- training$capitalAve
selectNA <- rbinom(dim(training)[1],size = 1,prob = 0.05) == 1
training$capAve[selectNA] <- NA
# Impute and standardize
preObj <- preProcess(training[,-58],method = "knnImpute")
capAve <- predict(preObj,training[,-58])$capAve
# Standardize true values
capAveTruth <- training$capitalAve
capAveTruth <- (capAveTruth - mean(capAveTruth))/sd(capAveTruth)
# compare
quantile(capAve - capAveTruth)
quantile((capAve - capAveTruth)[selectNA])
quantile((capAve - capAveTruth)[!selectNA])


# Create covriates
data(Wage)
inTrain <- createDataPartition(y = Wage$wage,
                               p = 0.7, list = FALSE)
training <- Wage[inTrain,]; testing <- Wage[-inTrain,]
table(training$jobclass)
dummies <- dummyVars(wage ~ jobclass,data = training)
head(predict(dummies,newdata = training))
# Splines basis
library(splines)
bsBasis <- bs(training$age,df = 3) 
bsBasis
lm1 <- lm(wage ~ bsBasis,data = training)
plot(training$age,training$wage,pch = 19,cex = 0.5)
points(training$age,predict(lm1,newdata = training),col = "red",pch = 19,cex = 0.5)

# preprocessing with PCA
data(spam)
inTrain <- createDataPartition(y = spam$type,
                               p = 0.75, list = FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
M <- abs(cor(training[,-58]))
diag(M) <- 0
which(M > 0.8,arr.ind = T)
names(spam)[c(34,32)]
plot(spam[,34],spam[,32])
# prcomp in R
smallSpam <- spam[,c(34,32)]
prComp <- prcomp(smallSpam)
plot(prComp$x[,1],prComp$x[,2])
prComp$rotation
# PCA on SPAM data
typeColor <- ((spam$type == "spam")*1 + 1)
prComp <- prcomp(log10(spam[,-58] + 1))
plot(prComp$x[,1],prComp$x[,2],col = typeColor,xlab = "PC1",ylab = "PC2")
# PCA with caret
preProc <- preProcess(log10(spam[,-58] + 1),method = "pca",pcaComp = 2)
spamPC <- predict(preProc,log10(spam[,-58] + 1))
plot(spamPC[,1],spamPC[,2],col = typeColor)

preProc <- preProcess(log10(training[,-58] + 1),method = "pca",pcaComp = 2)
trainPC <- predict(preProc,log10(training[,-58] + 1))
modelFit <- train(training$type ~ .,method = "glm",data = trainPC)

# Quiz
#q1
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]
#q2
data(concrete)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

names <- colnames(concrete)
names <- names[-length(names)]
featurePlot(x = training[, names], y = training$CompressiveStrength, plot = "pairs")

index <- seq_along(1:nrow(training))
ggplot(data = training, aes(x = index, y = CompressiveStrength)) + geom_point() + 
        theme_bw()

cols <- colnames(training)
subCols <- cols[-length(cols)] #all but CompressiveStrength
plotCols = 2
par(mfrow = c(ceil(length(subCols)/plotCols), plotCols))
res <- sapply(subCols, function(colName){
        cut <- cut2(training[,colName])
        lab <- paste0("index: col=",colName)
        plot(training$CompressiveStrength, pch=19, col=cut, xlab=lab, ylab="CompressiveStrength")
})

#q3
data(concrete)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
par(mfrow = c(1,2))
hist(training$Superplasticizer, breaks = 50)
hist(log(training$Superplasticizer + 1), breaks = 50)
#q4
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
 #Find all the predictor variables in the training set that begin with IL.
col <- colnames(adData)
col <- col[-1] # Names of all predictors
IL_col <- grep("^IL", col, value = TRUE, ignore.case = TRUE)
prePAC <- preProcess(training[, IL_col], method = "pca", thresh = 0.9)
prePAC

#q5
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[inTrain,]
testing = adData[-inTrain,]


new_training = training[, c("diagnosis", grep("^IL", col, value = TRUE, ignore.case = TRUE))]
new_testing = testing[, c("diagnosis", grep("^IL", col, value = TRUE, ignore.case = TRUE))]

colnames(new_training)
# Non-PCA model
non_pca_model <- train(diagnosis ~ ., data = new_training, method = "glm")
non_pca_predict <- predict(non_pca_model, new_testing[, -1])
non_pca_result <- confusionMatrix(new_testing$diagnosis, non_pca_predict)
non_pca_result
# PCA model
pca_model_obj <- preProcess(new_training[, -1], method = c('center', 'scale', 'pca'), thresh = 0.8)
pca_model_obj$numComp
# Extract the data set under the pca components
# These two are training/test sets with covariates being the 7 PCA components
pca_train_predict <- predict(pca_model_obj, new_training[, -1])
pca_test_predict <- predict(pca_model_obj, new_testing[, -1])
# Now add the "diagnosis" obs into the pca training set
pca_train_predict$diagonsis <- new_training$diagnosis
# Train a new glm model with this training set
pca_model <- train(diagonsis~., data = pca_train_predict, method = 'glm' )
pca_result <- confusionMatrix(new_testing$diagnosis, predict(pca_model, pca_test_predict))
pca_result

