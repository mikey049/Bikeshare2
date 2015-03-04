# Load Train and Test datasets

Train.Raw <- read.csv("train.csv")
Test.Raw <- read.csv("test.csv")
Data.Train <-Train.Raw
Data.Test <-Test.Raw

# Split timestamp into year, month, day, hour.
#Since the day numbers in the training set aren't in the test set, it doesn't help me.

#head(Data.Train[,1])

featureEng <- function(data){
  data$year <- as.numeric(substr(data$datetime,1,4))
  data$month <-as.numeric(substr(data$datetime,6,7))
  data$day <- as.numeric(substr(data$datetime,9,10))
  data$hour<- as.numeric(substr(data$datetime,12,13))
  data$dayofweek <-weekdays(as.Date(data$datetime))
  data$dayofweek <- as.factor(data$dayofweek)
 
  
# Add the sin terms and peak hour features for Registered
  
  data$shour <- (1.5 + sin(((data$hour-6)*pi/4.5)))
  data$smonth <- (1.5 + sin((data$month-4)*pi/9))
  data$peakm <- 0
  data$peakm[which( 5 < data$hour & data$hour < 10)] <- 1
  data$peake <- 0
  data$peake[which( 15 < data$hour & data$hour < 20)] <- 1
  data$shour.nonwork <- 1.5 + sin((data$hour-10)*pi/12)


 # add features for Casual

data$shour.casual <- (1.5 + sin((data$hour-10)*pi/9))
  
 

  
  
  
  return (data)
}




Data.Train <- featureEng(Data.Train)

train.keeps <- c("season", "holiday", "workingday", "weather", "temp", "humidity", "atemp",
                 "windspeed", "registered", "casual", "year", "hour", "month","day",
                 "dayofweek", "shour", "smonth", "peakm", "peake", "shour.nonwork")

Data.Munged <- Data.Train[, train.keeps]

offdays <- abs(Data.Train$temp - Data.Train$atemp)
offdaysindex <- which(offdays >10)
Data.Munged <- Data.Munged[-offdaysindex,]


Training.rows <- Data.Munged[Data.Munged$day < 16,]
Test.rows <- Data.Munged[Data.Munged$day >=16,]


####### Create RMSLE Statistic

RMSLESummary <-function(data, lev = NULL, model = NULL){
  
  n <- nrow(data)
  
  sum <- 0
  for (i in 1:n){
    
    x <- (log(data$pred[i] +1) - log(data$obs[i]+1))^2
    
    sum <- sum + x
    
  }
  
  out <- sqrt((1/n)*sum)
  names(out) <- "RMSLE"
  out
}




library(ggplot2)

# Look at predictor relationships to outcome

p <- qplot(hour,count, data = Data.Train, type = c("p", "g"),
       ylab = "Number of Rentals",
       main = "Rentals",
       xlab = "Hours")
p + facet_grid(. ~month)

p + facet_grid(. ~year)

p + facet_wrap( ~ dayofweek)

# Rentals increased by year. Rentals are lower 10-02.
# I might a sin term for hours

p2 <- qplot(season,count, data = Data.Train, type = c("p", "g"),
           ylab = "Number of Rentals",
           main = "Rentals",
           xlab = "Holiday")
p2 + facet_wrap( ~weather)

p3 <-qplot(holiday, data = Data.Train, type = c("p", "g"),
           geom ="histogram",
           ylab = "count",
           main = "Rentals",
           xlab = "Number of rentals")
p3 + facet_wrap( ~ dayofweek)
#There is only a small number data points for holidays
#There are no holidays on Saturday, Sunday, Thursday, Tuesday

p4 <-qplot(weather, data = Data.Train, type = c("p", "g"),
           geom ="histogram",
           ylab = "count",
           main = "Rentals",
           xlab = "Number of rentals")
p4 + facet_wrap( ~ dayofweek)

#There is only about 1 observation of weather 4 and a relatively small amount of 3. 
#2 is about 40% of 1 which has the most

p5 <-qplot(workingday,count, data = Data.Train, type = c("p", "g"),
           ylab = "count",
           main = "Rentals",
           xlab = "Number of rentals")
p5 + facet_wrap( ~ dayofweek)

p6 <-qplot(temp,atemp, data = Data.Train, type = c("p", "g"),
           ylab = "temp",
           main = "Temp",
           xlab = "feels like")
p6 + facet_wrap( ~ weather)


# I noticed a few data points that look like there may be an error with getting the temp

offdays <- abs(Data.Train$temp - Data.Train$atemp)
offdaysindex <- which(offdays >10)

p7 <-qplot(hour,atemp, data = Data.Train[offdaysindex,], type = c("p", "g"),
           ylab = "Number of rentals",
           main = "Feels Like temperature oddities",
           xlab = "Feels like")
p7 + facet_wrap( ~ hour)

#something happened on August 17 2012 where the atemp did not change the whole day
#I should either remove the day completely or imput something for atemp

p8 <- qplot(hour,atemp, data = Data.Train[Data.Train$month == 08 & Data.Train$year ==2012, ], type = c("p", "g"),
           ylab = "Number of rentals",
           main = "Feels Like temperature oddities",
           xlab = "Feels like")
p8 + facet_wrap( ~ day)

Data.Munged <- Data.Train[-offdaysindex,]



p20 <- qplot(hour,registered, data = Data.Train[Data.Train$month == 08 & Data.Train$year ==2012, ], type = c("p", "g"),
            ylab = "Number of rentals",
            main = "Feels Like temperature oddities",
            xlab = "Feels like")
p20 + facet_wrap( ~ day)

#check for missing data. Check calendar days or hours
str(Data.Train[Data.Train$year ==2011, ])

p9 <- qplot(hour,day, data = Data.Munged[Data.Munged$year ==2011 & Data.Munged$month == 01, ], type = c("p", "g"),
            ylab = "Number of rentals",
            main = "Feels Like temperature oddities",
            xlab = "Feels like")
p9 + facet_wrap( ~ month)

#There are some missing hours, but it doesn't look like it will effect the model much

#Check for collinearity and correlation between predictors
# Filter are reponse, day, and datetime
filter <- c(1,17,12,11,10)
library(caret)
featurePlot(Data.Munged[, c("temp", "humidity", "dayofweek")],
            Data.Munged[,"count"],
            between = list(x = 1, y = 1),
            type = c("g", "p", "smooth"),
            labels = rep("", 2))

library(corrplot)

corrplot::corrplot(cor(Data.Munged[,-filter]), 
                   order = "hclust", 
                   tl.cex = .8)



# atemp and season can be removed

Data.Munged <- Data.Train[-offdaysindex,]

filteroutcome <-c(7,8,9)

Data.Munged.Outcome <- Data.Munged[,filteroutcome]





#After trying to build some of the models, I realized that I need more understanding of data.
#Ideas--maybe take into account the value from the hour before
# People will look up the weather forcast for the day can plan accordingly
# manipulate sin function to match the daily cycle
#seemingly important factors (WorkingDay, hour, Bad Vs good weather, temp)
#Causal has a different hourly curve than registered. It's higher and more spread out towards the afternoon and evening
# Registered on non work days is similar to casual on work days
#Think about adding some combinations of things to see what they look like
#Peak hours Vs non peak
p10 <- qplot(hour,count, data = Data.Munged[Data.Munged$month ==01 & Data.Munged$year==2011 & Data.Munged$dayofweek =="Monday",], type = c("p", "g"),
           ylab = "Number of Rentals",
           main = "Rentals",
           xlab = "Hours")
p10 + facet_wrap( ~day)

#There are big spikes in the morning and evenings except holidays and weekends

p11 <- qplot(hour,registered, data = Data.Munged[Data.Munged$month ==01 & Data.Munged$year==2011 & Data.Munged$day <15 & Data.Munged$day >7,], type = c("p", "g"),
             ylab = "Number of Rentals",
             main = "Rentals",
             xlab = "Hours")
p11 + facet_wrap( ~dayofweek)

p12 <- qplot(hour,weather, data = Data.Munged[Data.Munged$month ==03 & Data.Munged$year==2011 & Data.Munged$workingday == 1,], type = c("p", "g"),
             ylab = "Number of Rentals",
             main = "Rentals",
             xlab = "Hours")
p12 + facet_wrap( ~day)

p12 <- qplot(hour,registered, data = Data.Munged[Data.Munged$month ==03 & Data.Munged$year==2011 & Data.Munged$workingday == 1 & Data.Munged$weather >2 ,], type = c("p", "g"),
             ylab = "Number of Rentals",
             main = "Rentals",
             xlab = "Hours")
p12 + facet_wrap( ~day)


p15 <- qplot(hour,registered, data = Data.Munged[ Data.Munged$year==2011 & Data.Munged$workingday == 0 ,], type = c("p", "g"),
             ylab = "Number of Rentals",
             main = "Rentals",
             xlab = "Hours")
p15 + facet_wrap( ~month)

p19 <- qplot(hour,registered, data = Data.Munged[ Data.Munged$year==2012 & Data.Munged$workingday == 0 ,], type = c("p", "g"),
             ylab = "Number of Rentals",
             main = "Rentals",
             xlab = "Hours")
p19 + facet_wrap( ~month)



p16 <- qplot(hour,registered, data = Data.Munged[ Data.Munged$year==2011 & Data.Munged$workingday == 1 ,], type = c("p", "g"),
             ylab = "Number of Rentals",
             main = "Rentals",
             xlab = "Hours")
p16 + facet_wrap( ~month)





# Checking Casual
p13 <- qplot(hour,casual, data = Data.Munged[ Data.Munged$month ==05 & Data.Munged$year==2011,], type = c("p", "g"),
             ylab = "Number of Rentals",
             main = "Rentals",
             xlab = "Hours")
p13 + facet_wrap( ~day)

p14 <- qplot(hour,casual, data = Data.Munged[ Data.Munged$year==2011 & Data.Munged$workingday == 1,], type = c("p", "g"),
             ylab = "Number of Rentals",
             main = "Rentals",
             xlab = "Hours")
p14 + facet_wrap( ~month)

p17 <- qplot(hour,casual, data = Data.Munged[ Data.Munged$year==2011 & Data.Munged$workingday == 0,], type = c("p", "g"),
             ylab = "Number of Rentals",
             main = "Rentals",
             xlab = "Hours")
p17 + facet_wrap( ~month)


p20 <- qplot(month,casual, data = Data.Munged[ Data.Munged$year==2012,], type = c("p", "g"),
             ylab = "Number of Rentals",
             main = "Rentals",
             xlab = "Hours")
p20 + facet_wrap( ~month)

hourfunction <- qplot(seq(0:23), 1.5 + sin((seq(0:23)-10)*pi/9),
                      ylab = "Number of Rentals",
                      main = "Rentals",
                      
                      xlab = "Hours")


monthfunction <- qplot(seq(1:12), 1.5 + sin((seq(1:12)-8)*pi/3),
                      ylab = "Number of Rentals",
                      main = "Rentals",
                      
                      xlab = "month")
#_--------------------------------------------------------------------


p18 <- qplot(month,season, data = Data.Munged[ Data.Munged$year==2011 & Data.Munged$workingday == 1,], type = c("p", "g"),
             ylab = "Number of Rentals",
             main = "Rentals",
             xlab = "Hours")
p18 + facet_wrap( ~season)

# Make a strong hour feature to put into model. Peaks are at 8 and 17

hourfunction <- qplot(seq(0:23), 1.5 + sin((seq(0:23)-6)*pi/4.5),
                      ylab = "Number of Rentals",
                      main = "Rentals",
                      
                      xlab = "Hours")

#hourly non workdays

hourfunction2 <- qplot(seq(0:23), 1.5 + sin((seq(0:23)-10)*pi/12),
                     ylab = "Number of Rentals",
                     main = "Rentals",
                     
                     xlab = "Hours")

# now come up with a monthly factor

qplot(seq(1:12), 1.5 + sin((seq(1:12)-4)*pi/9),
      ylab = "Number of Rentals",
      main = "Rentals",
      
      xlab = "month")

DailyRange <- function(data){
  

  m.avg <-matrix(0,nrow=(12),ncol =2)
  for (y in 2011:2012){
    
    for (m in 1:12){
      range <-matrix(0,nrow=(19),ncol =1)
      for (d in 1:19){
        if (sum(data[data$day == d & data$month ==m & data$year ==y & data$workingday == 1,"registered"])>0){
          daymax <- max(data[data$day == d & data$month ==m & data$year ==y & data$workingday == 1,"registered"])
          daymin <- min(data[data$day == d & data$month ==m & data$year ==y & data$workingday == 1,"registered"])
      
          range[d,1] <- (daymax - daymin)
        }               
      }
      m.avg[m,y-2010] <-mean(range[range>0])
     
    }
  }
  
  return (m.avg)
}

Maxdiff <- DailyRange(Data.Munged)

Max <- max(Data.Munged[Data.Munged$day == 1 & Data.Munged$month ==6 & Data.Munged$year ==2011,"registered"])
Min <- min(Data.Munged[Data.Munged$day == 1 & Data.Munged$month ==6 & Data.Munged$year ==2011,"registered"])

qplot(seq(1:310),Maxdiff)


library(corrplot)


corrplot::corrplot(cor(Data.Munged[,-15]), 
                   order = "hclust", 
                   tl.cex = .8)

featurePlot(Data.Munged[, c("temp", "humidity")],
            Data.Munged[,"casual"],
            between = list(x = 1, y = 1),
            type = c("g", "p", "smooth"),
            labels = rep("", 2))
