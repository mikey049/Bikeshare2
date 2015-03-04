#Goals:    1. Construct a Linear model of the data for each outcome (casual + registered)
#          2. Select the best model (model selection)
#          3. Save a prediction with best linear model



library(caret)

indx <- createFolds(Data.Munged.Outcome[,"count"], returnTrain = TRUE)
ctrl <- trainControl(method = "cv", index = indx)


ctrl4 <- trainControl(method = "cv", index = indx, summaryFunction = RMSLESummary)

Lin_reg_C <- train(casual ~ holiday + workingday + weather + temp + humidity + windspeed + year + month + hour + dayofweek + sin(hour) + sin(month) , data = Data.Munged,
                 method = "lm",
                 trControl = ctrl)
Lin_reg_C

Lin_reg_C2 <- train(casual ~ holiday + workingday + weather + temp + humidity + windspeed + year + dayofweek + sin(hour) + sin(month) , data = Data.Munged,
                   method = "lm",
                   trControl = ctrl)
Lin_reg_C2

Lin_reg_R <- train(x = Data.Munged, y = Data.Munged.Outcome[,"registered"],
                   method = "lm",
                   trControl = ctrl)
Lin_reg_R



enetGrid <- expand.grid(lambda = c(0, 0.01, .1), 
                        fraction = seq(.05, 1, length = 20))

enetTune <- train(casual ~ workingday + weather + temp + humidity + windspeed + year + dayofweek + (sin(hour) + sin(month))^2 ,
                  data = Data.Munged,
                  method = "enet",
                  tuneGrid = enetGrid,
                  trControl = ctrl)
                  #preProc = c("center", "scale"))
enetTune

plot(enetTune)



### Upgraded features


Lin_reg_R2 <- train(registered ~ humidity + temp + season + windspeed+ (year) + hour + month + ((sin((hour-6)*pi/4.5)) + sin((month-4)*pi/9))^2, 
                    data = Data.Munged.workingday,
                    method = "lm",
                    trControl = ctrl2)
Lin_reg_R2



(sin((hour-6)*pi/4.5))*(sin((month-4)*pi/9))


Lin_reg_R3 <- train(registered ~ humidity + temp + season + windspeed+ (year) + I(5 < hour & hour < 10) + I( 15<hour & hour<20) + hour + month + ((sin((hour-6)*pi/4.5)) + sin((month-4)*pi/9))^2, 
                    data = Data.Munged.workingday,
                    method = "lm",
                    trControl = ctrl2)
Lin_reg_R3


Lin_reg_R4 <- train(registered ~ humidity + temp + season + I(year==2012) + I(5 < hour & hour < 10) + I( 15<hour & hour<20) + hour + month + ((sin((hour-6)*pi/4.5)) + sin((month-4)*pi/9))^2, 
                    data = Data.Munged.workingday,
                    method = "lm",
                    trControl = ctrl2)
Lin_reg_R4

varImp(Lin_reg_R4)

indx2 <- createFolds(Data.Munged[Data.Munged$workingday == 1 & Data.Munged$weather <3,"registered"], returnTrain = TRUE)
ctrl3 <- trainControl(method = "cv", index = indx2, summaryFunction = RMSLESummary)

undebug(RMSLESummary)
Lin_reg_R5 <- train(registered ~ humidity + temp + season + I(year==2012) + I(5 < hour & hour < 10) + I( 15<hour & hour<20) + hour + month + ((sin((hour-6)*pi/4.5)) + sin((month-4)*pi/9))^2, 
                    data = Data.Munged.workingday,
                    method = "lm",
                    maximize = FALSE,
                    metric = "RMSLE",
                    trControl = ctrl3)
Lin_reg_R5

varImp(Lin_reg_R5)

#Working on full Registered now

indx4 <- createFolds(Training.rows[,"registered"], returnTrain = TRUE)


ctrl4 <- trainControl(method = "cv", index = indx4, summaryFunction = RMSLESummary)
ctrl5 <-trainControl(method = "cv", index = indx4)

Lin_reg_R6 <- train(registered ~ humidity + holiday + temp + windspeed + season + I(year==2012) + peakm + peake + hour + month + (workingday + shour + smonth)^2, 
                    data = Training.rows,
                    method = "lm",
                    maximize = FALSE,
                    metric = "RMSLE",
                    trControl = ctrl4)
Lin_reg_R6


varImp(Lin_reg_R6)
undebug(RMSLESummary)

Lin_reg_R7 <- train(registered ~ holiday + temp + humidity + year + peake + peakm + (shour.nonwork + workingday + I(weather <3) + shour +smonth)^2 , 
                    data = Training.rows,
                    method = "lm",
                    trControl = ctrl5)
Lin_reg_R7

varImp(Lin_reg_R7)



# casual

ctrl5 <-trainControl(method = "cv", index = indx4)

Lin_reg_R8 <- train(casual ~ holiday + workingday + temp + season + windspeed  + year + hour + month  + I(weather <3) + sin((hour-10)*pi/9) + sin((month-8)*pi/3),
                    data = Training.rows,
                    method = "lm",
                    trControl = ctrl5)
Lin_reg_R8


varImp(Lin_reg_R8)
