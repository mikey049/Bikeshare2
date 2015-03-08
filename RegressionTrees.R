
indx2 <- createFolds(Data.Munged[Data.Munged$workingday == 1 & Data.Munged$weather <3,"registered"], returnTrain = TRUE)
ctrl2 <- trainControl(method = "cv", index = indx2)

library(doParallel)
registerDoParallel(cores=6)


Data.Munged.workingday <- Training.rows[Training.rows$workingday == 1 & Training.rows$weather <3,]


#Try to model only working days



treebagTune <- train(registered ~ holiday + workingday + weather + temp + humidity + season + windspeed + dayofweek+ year + hour + month + (sin(hour) + sin(month))^2 , 
                     data = Data.Munged.workingday,
                     method = "treebag",
                     nbagg = 10,
                     trControl = ctrl2)

varImp(treebagTune)

treebagTune

treebagTune2 <- train(registered ~ holiday + workingday + weather + temp + humidity + season + windspeed + dayofweek+ year + hour + month + ((sin((hour-6)*pi/4.5)) + sin((month-4)*pi/9))^2 , 
                     data = Data.Munged.workingday,
                     method = "treebag",
                     nbagg = 10,
                     trControl = ctrl2)

treebagTune2
varImp(treebagTune2)


treebagTune3 <- train(registered ~ holiday + workingday + weather + temp + humidity + season + windspeed + dayofweek+ year + I(5 < hour & hour < 10) + I( 15<hour & hour<20) + hour + month + ((sin((hour-6)*pi/4.5)) + sin((month-4)*pi/9))^2 , 
                      data = Data.Munged.workingday,
                      method = "treebag",
                      nbagg = 10,
                      trControl = ctrl2)

treebagTune3
varImp(treebagTune3)

gbmGrid <- expand.grid(interaction.depth = seq(1, 7, by = 2),
                       n.trees = seq(100, 1000, by = 50),
                       shrinkage = seq(0.1, 1, by = .1))
set.seed(100)
gbmTune <- train(registered ~ holiday + workingday + weather + temp + humidity + season + windspeed + dayofweek+ year + I(5 < hour & hour < 10) + I( 15<hour & hour<20) + hour + month + ((sin((hour-6)*pi/4.5)) + sin((month-4)*pi/9))^2 , 
                 data = Data.Munged.workingday,
                 method = "gbm",
                 tuneGrid = gbmGrid,
                 trControl = ctrl2,
                 verbose = FALSE)


gbmTune

plot(gbmTune, auto.key = list(columns = 4, lines = TRUE))

gbmImp <- varImp(gbmTune, scale = TRUE)

gbmImp
sumtest <- data.frame(obs= c(1,1,1,1),pred = c(1,2,3,4))
RMSLESummary(sumtest)






treebagTune4 <- train(registered ~ holiday + workingday + weather + temp + humidity + season + windspeed + dayofweek+ year + I(5 < hour & hour < 10) + I( 15<hour & hour<20) + hour + month + ((sin((hour-6)*pi/4.5)) + sin((month-4)*pi/9))^2 , 
                      data = Data.Munged.workingday,
                      method = "treebag",
                      nbagg = 2,
                      maximize = FALSE,
                      metric = "RMSLE",
                      trControl = ctrl3)

treebagTune4
varImp(treebagTune4)

ctrl3 <- trainControl(method = "cv", index = indx2, summaryFunction = RMSLESummary)

gbmTune2 <- train(registered ~ holiday + workingday + weather + temp + humidity + season + windspeed + dayofweek+ year + I(5 < hour & hour < 10) + I( 15<hour & hour<20) + hour + month + ((sin((hour-6)*pi/4.5)) + sin((month-4)*pi/9))^2 , 
                 data = Data.Munged.workingday,
                 method = "gbm",
                 metric = "RMSLE",
                 maximize = FALSE,
                 tuneGrid = gbmGrid,
                 trControl = ctrl3,
                 verbose = FALSE)


gbmTune2

plot(gbmTune2, auto.key = list(columns = 4, lines = TRUE))

gbmImp2 <- varImp(gbmTune2, scale = TRUE)

gbmImp2


cbGrid <- expand.grid(committees = c(1:10, 20, 50, 75, 100), 
                      neighbors = c(0, 1, 3,5,7,9))
cbGrid2 <- expand.grid(committees = c(1:2), 
                      neighbors = c(0,9))

cubistTune <- train(registered ~ holiday + workingday + weather + temp + humidity + season + windspeed + dayofweek+ year + I(5 < hour & hour < 10) + I( 15<hour & hour<20) + hour + month + ((sin((hour-6)*pi/4.5)) + sin((month-4)*pi/9))^2 , 
                    data = Data.Munged.workingday,
                    "cubist",
                    metric = "RMSLE",
                    maximize = FALSE,
                    tuneGrid = cbGrid,
                    trControl = ctrl3)
cubistTune

plot(cubistTune, auto.key = list(columns = 4, lines = TRUE))

cbImp <- varImp(cubistTune, scale = FALSE)
cbImp



# Full Registered

treebagTune5 <- train(registered ~ holiday + workingday + weather + temp + humidity + season + windspeed + dayofweek + year + peake + peakm + hour + month + (shour.nonwork + workingday + I(weather <3) + shour +smonth)^2 , 
                      data = Training.rows,
                      method = "treebag",
                      nbagg = 50,
                      maximize = FALSE,
                      metric = "RMSLE",
                      trControl = ctrl4)
treebagTune5
varImp(treebagTune5)
?varImp
plot(varImp(treebagTune5))
#-------------------------------Final Cubist Model Registered----------------------------------------------
cbGrid <- expand.grid(committees = c(1:10, 20, 50, 75, 100), 
                      neighbors = c(0, 1, 3,5,7,9))


indx4 <- createFolds(Training.rows[,"registered"], returnTrain = TRUE)


ctrl4 <- trainControl(method = "cv", index = indx4, summaryFunction = RMSLESummary)


cubistTune2 <- train(registered ~ holiday + workingday + temp + season + windspeed  + year + peake + peakm + hour + month + shour.nonwork + I(weather <3) + shour +smonth , 
                    data = Training.rows,
                    method = "cubist",
                    metric = "RMSLE",
                    maximize = FALSE,
                    tuneGrid = cbGrid,
                    trControl = ctrl4)
cubistTune2
summary(cubistTune2)
plot(cubistTune2, auto.key = list(columns = 4, lines = TRUE))

cbImp2 <- varImp(cubistTune2, scale = FALSE)
cbImp2

cubistTune2.predict <-predict(cubistTune2,Test.rows)

cubistTune2.predict2 <-predict(cubistTune2,Test.rows, conf.int =TRUE)
head(cubistTune2.predict2)

Residual.cubist <- cubistTune2.predict -Test.rows$registered 
qplot(x = Test.rows$registered, y= Residual.cubist)

RMSLESummary(data.frame(obs= Test.rows$registered, pred = cubistTune2.predict))



#------------------------------------------------------------------------------------------------


gbmTune3 <- train(registered ~ holiday + temp + humidity + windspeed + year + peake + peakm + hour + month + (shour.nonwork + workingday + I(weather <3) + shour +smonth)^2 , 
                  data = Training.rows,
                  method = "gbm",
                  metric = "RMSLE",
                  maximize = FALSE,
                  tuneGrid = gbmGrid,
                  trControl = ctrl4,
                  verbose = FALSE)


gbmTune3

plot(gbmTune3, auto.key = list(columns = 4, lines = TRUE))

gbmImp3 <- varImp(gbmTune3, scale = TRUE)

gbmImp3

gbmTune3.predict <-predict(gbmTune3,Test.rows)

qplot(x = Test.rows$registered, y= gbmTune3.predict)

RMSLESummary(data.frame(obs= Test.rows$registered, pred = gbmTune3.predict))

treebagTune6 <- train(registered ~ holiday + temp + humidity + season + windspeed + dayofweek + year + peake + peakm + hour + month + (shour.nonwork + workingday + I(weather <3) + shour +smonth)^2 , 
                      data = Training.rows,
                      method = "treebag",
                      nbagg = 500,
                      maximize = FALSE,
                      metric = "RMSLE",
                      trControl = ctrl4)
treebagTune6

treebagTune6.predict <-predict(treebagTune6,Test.rows)

qplot(x = Test.rows$registered, y= treebagTune6.predict)

RMSLESummary(data.frame(obs= Test.rows$registered, pred = treebagTune6.predict))



#Casual 

cubistTune3 <- train(abs(casual) ~ holiday + workingday + temp + season + windspeed  + year + hour + month  + I(weather <3) + sin((hour-10)*pi/9) + sin((month-8)*pi/3), 
                    data = Training.rows,
                    method = "cubist",
                    metric = "RMSLE",
                    maximize = FALSE,
                    tuneGrid = cbGrid,
                    trControl = ctrl4)
cubistTune3

plot(cubistTune3, auto.key = list(columns = 4, lines = TRUE))

cbImp3 <- varImp(cubistTune3, scale = FALSE)
cbImp3

cubistTune3.predict <-predict(cubistTune3,Test.rows)

qplot(x = Test.rows$casual, y= cubistTune3.predict)

RMSLESummary(data.frame(obs= Test.rows$casual, pred = cubistTune3.predict))

indx4 <- createFolds(Training.rows[,"registered"], returnTrain = TRUE)


ctrl4 <- trainControl(method = "cv", index = indx4, summaryFunction = RMSLESummary)

cubistTune4 <- train(casual ~ workingday + windspeed  + year + I(weather <3) + (shour.nonwork + smonth)^2, 
                     data = Training.rows,
                     method = "cubist",
                     #metric = "RMSLE",
                     tuneGrid = cbGrid,
                     trControl = ctrl5)

train.keeps_8 <- c( "holiday", "workingday", "weather", "temp",
                   "windspeed", "year",
                   "dayofweek", "shour", "smonth", "peakm", "peake", "shour.nonwork")

cubistTune8 <- cubist(Training.rows[,train.keeps_8],Training.rows[,c(10)],
                      committees = 50)

varImp(cubistTune8)


cubistTune8.predict <-predict(cubistTune8,Test.rows)

rmse(cubistTune8.predict, Test.rows$casual )

qplot(Test.rows$casual, cubistTune8.predict- Test.rows$casual )
RMSLESummary(data.frame(obs= Test.rows$casual , pred = abs(cubistTune8.predict)))


gbmTune4 <- train(casual~ holiday + temp +  windspeed + humidity + year + peake + peakm + (shour.nonwork + I(weather <3) + shour + smonth + workingday)^2, 
                  data = Training.rows,
                  method = "gbm",
                  metric = "RMSLE",
                  maximize = FALSE,
                  tuneGrid = gbmGrid,
                  trControl = ctrl4,
                  verbose = FALSE)


gbmTune4

plot(gbmTune4, auto.key = list(columns = 4, lines = TRUE))

gbmImp4 <- varImp(gbmTune4, scale = TRUE)

# ----------------Best Causal Cubist Model--------------------------------------

cubistTune10 <- train( casual~ holiday + temp +  windspeed + humidity + year + peake + peakm + (shour.nonwork + I(weather <3) + shour + smonth + workingday)^2, 
                     data = Training.rows,
                     method = "cubist",
                     metric = "RMSLE",
                     maximize = FALSE,
                     tuneGrid = cbGrid,
                     trControl = ctrl4
                     )
varImp(cubistTune10)
cubistTune10.predict <-predict(cubistTune10,Test.rows)

qplot(Test.rows$casual , cubistTune10.predict - Test.rows$casual)

RMSLESummary(data.frame(obs= Test.rows$casual , pred = abs(cubistTune10.predict ) ))
#-------------------------------------------------------------

