getwd()
setwd("C:/Users/shant/Desktop/Northeastern NEU/Projects/London Bike Share")

library(psych)
library(corrplot)

lbs <- read.csv("C:/Users/shant/Desktop/Northeastern NEU/Projects/London Bike Share/london_merged.csv")

#finding null values in data
is.na(lbs)

#learning more about data
summary(lbs)
describe(lbs)

#finding data type of each column
str(lbs)

#finding correlation amongst each column
corrplot(cor(lbs[-1]))

#splitting data into train and test
rownumber = sample(x=1:nrow(lbs),size = 0.75*nrow(lbs))
train = lbs[rownumber,]
test = lbs[-rownumber,]
test1 = test[,-2]

#training model
lmcnt <- lm(cnt~t1+t2+hum+wind_speed+weather_code+is_holiday+is_weekend+season, data = train)
summary(lmcnt)
par(mfrow=c(2,2))
plot(lmcnt, main="scatterplot")

#prediction model
predicted = predict(lmcnt, newdata = test1)

#actual vs predicted comparison
comtab = table(test$cnt, predicted)
View(comtab)

#finding the accuracy
accuracy = sum(diag(comtab))/sum(comtab)
accuracy

par(mfrow=c(1,1))

