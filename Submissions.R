
Data.Test <- featureEng(Test.Raw)

train.keeps_2 <- c("datetime","season", "holiday", "workingday", "weather", "temp", "humidity", "atemp",
                 "windspeed", "year", "hour", "month","day",
                 "dayofweek", "shour", "smonth", "peakm", "peake", "shour.nonwork")

Data.Test <- Data.Test[, train.keeps]

#______________________________Submission 1

#Registered
cubist.Reg.predict <-predict(cubistTune2,Data.Test)


#Casual
cubist.Cas.predict <-predict(cubistTune8,Data.Test)
Total.predict = cubist.Cas.predict + cubist.Reg.predict
Prediction <- data.frame(datetime = Data.Test[,1],count = Total.predict)
write.csv(Prediction, file ="BikeDemandSub1.csv", row.names=FALSE)
summary(Prediction)

#--------------------------Submission 2--------------------------------------

#Registered
cubist.Reg.predict <-predict(cubistTune2,Data.Test)


#Casual
cubist.Cas.predict <-predict(cubistTune10,Data.Test)
Total.predict = cubist.Cas.predict + cubist.Reg.predict
Prediction <- data.frame(datetime = Data.Test[,1],count = Total.predict)
write.csv(Prediction, file ="BikeDemandSub2.csv", row.names=FALSE)
summary(Prediction)
