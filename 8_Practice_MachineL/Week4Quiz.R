
library(gbm)
library(ElemStatLearn)
library(caret)
library(AppliedPredictiveModeling)
library(elasticnet)

# Q1
  # Loading the data set
data(vowel.train)
data(vowel.test)

  # set seed 

  set.seed(33833)
  training <- vowel.train
  testing <- vowel.test

  # train the model with random forest and gbm
  RFmodel <- train(as.factor(y)~ ., data = training, method = 'rf')
  BSmodel <- train(as.factor(y)~ ., data = training, method = 'gbm', verbose = FALSE)

  # predict with test data
  RFpred <- predict(RFmodel, testing)
  BSpred <- predict(BSmodel, testing)

  # look at accuracy
  confusionMatrix(RFpred, as.factor(testing$y))
  confusionMatrix(BSpred, as.factor(testing$y))
  confusionMatrix(RFpred, BSpred)

 # Q2
  # set seed and load the data set
  set.seed(3433)
  data(AlzheimerDisease)
  adData = data.frame(diagnosis,predictors)
  inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
  training = adData[ inTrain,]
  testing = adData[-inTrain,]
  
  # train models
  set.seed(62433)
  fitRF <- train(diagnosis~ ., data = training, method = 'rf', prox = TRUE)
  fitBS <- train(diagnosis~ ., data = training, method = 'gbm', verbose = FALSE)
  fitLDA <- train(diagnosis~ ., data = training, method = 'lda')
  
  # prediction
  pRF <- predict(fitRF, testing)
  pBS <- predict(fitBS, testing)
  pLDA <- predict(fitLDA, testing)
  
  confusionMatrix(pRF, testing$diagnosis)$overall[1]
  confusionMatrix(pBS, testing$diagnosis)$overall[1]
  confusionMatrix(pLDA, testing$diagnosis)$overall[1]
  
  # stacking
  stackDF <- data.frame(rf = pRF, bs = pBS, lda = pLDA, diagnosis = testing$diagnosis)
  fit_stack <- train(diagnosis~ ., data = stackDF, method = 'rf') 
  comb.pred.test <- predict(fit_stack, stackDF)
  confusionMatrix(comb.pred.test, testing$diagnosis)$overall[1]

 # Q3
  set.seed(3523)
  data(concrete)
  inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
  training = concrete[ inTrain,]
  testing = concrete[-inTrain,]

  set.seed(233)  
  lassoFit <- train(CompressiveStrength ~.,  data = training, method = "lasso")
  lassoFit$finalModel
        
  plot.enet(lassoFit$finalModel, xvar = "penalty", use.color = TRUE)  
  
  
  # Q4
  
  
  # Q5