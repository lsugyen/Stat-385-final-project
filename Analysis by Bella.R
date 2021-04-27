setwd("/Users/bellapatel/stat385/")

normal <- read.csv("max_normal.csv")
set.seed(2021)
sample <- sample(nrow(normal), 5883)
training <- normal[sample, ]
training1 <- training
test <- normal[-sample,]


acceleration <-as.double(training1[,2]) # android.sensor.accelerometer.mean
gyroscope <-as.double(training1[,3]) # android.sensor.gyroscope.mean
sound <- as.double(training1[,4])
gamerotation <- as.double(training1[,6]) 
linacc <- as.double(training1[,8]) #android.sensor.linear_acceleration.mean
orientation <- as.double(training1[,9])
rotationvec <- as.double(training1[,10])
speed <- as.double(training1[,11]) # speed.mean

# combine several classes into two classes, then apply binary classification iteratively
# create binary target

# bus
training1 <- training
training1$target <- as.character(training1$target)
training1$target[training1$target != "Bus"] <- 0
training1$target[training1$target == "Bus"] <- 1
training1$target <- as.integer(training1$target)
target <- as.integer(training1[,5])

# cor(as.integer(normal$target), normal)

logisticbus <-glm(target~acceleration*sound+gyroscope*speed+sound*orientation*rotationvec*gamerotation+gamerotation*linacc+orientation*linacc*gyroscope, family="binomial")
# coef(logisticbus)
with(summary(logisticbus), 1 - deviance/null.deviance) # 0.2406766

# predict(logisticbus, newdata=data.frame(acceleration=.24, gyroscope=.028, speed = .39), type="response")

# train
training1 <- training
training1$target <- as.character(training1$target)
training1$target[training1$target != "Train"] <- 0
training1$target[training1$target == "Train"] <- 1
training1$target <- as.integer(training1$target)
target <- as.integer(training1[,5])

logistictrain <-glm(target~acceleration+gyroscope*speed*sound+linacc*sound+sound*speed*rotationvec+sound+orientation*sound*linacc, family="binomial") #repeat for all targets i guess
# coef(logistictrain)
with(summary(logistictrain), 1 - deviance/null.deviance) # 0.3181661

# predict(logistictrain, newdata=data.frame(acceleration=.24, gyroscope=.028, speed = .39), type="response")

# car
training1 <- training
training1$target <- as.character(training1$target)
training1$target[training1$target != "Car"] <- 0
training1$target[training1$target == "Car"] <- 1
training1$target <- as.integer(training1$target)
target <- as.integer(training1[,5])

logisticcar <-glm(target~speed*sound+acceleration*sound+gyroscope*linacc*speed+rotationvec+orientation*speed*rotationvec, family="binomial") 
# coef(logisticcar)
with(summary(logisticcar), 1 - deviance/null.deviance) # 0.372084


# still
training1 <- training
training1$target <- as.character(training1$target)
training1$target[training1$target != "Still"] <- 0
training1$target[training1$target == "Still"] <- 1
training1$target <- as.integer(training1$target)
target <- as.integer(training1[,5])

logisticstill <-glm(target~gyroscope*sound*acceleration+speed*sound*orientation*linacc+linacc*sound+rotationvec*sound*speed, family="binomial")
# coef(logisticstill)
with(summary(logisticstill), 1 - deviance/null.deviance) #  0.5097949 

# predict(logisticstill, newdata=data.frame(acceleration=0, gyroscope=0, speed = 0), type="response")

# walking
training1 <- training
training1$target <- as.character(training1$target)
training1$target[training1$target != "Walking"] <- 0
training1$target[training1$target == "Walking"] <- 1
training1$target <- as.integer(training1$target)
target <- as.integer(training1[,5])

logisticwalking <-glm(target~acceleration+gyroscope+linacc*sound+orientation*speed*sound+speed*sound*rotationvec, family="binomial") 
# coef(logisticwalking)
with(summary(logisticwalking), 1 - deviance/null.deviance) # 0.7013405


# for testing
levels <- c("Walking", "Bus", "Still", "Train", "Car")
predictions <- c()
for (row in 1:nrow(test))
{
  acceleration1 <-as.double(test[,2][row]) # android.sensor.accelerometer.mean
  gyroscope1 <-as.double(test[,3][row]) # android.sensor.gyroscope.mean
  sound1 <- as.double(test[,4][row])
  gamerotation1 <- as.double(test[,6][row])
  linacc1 <- as.double(test[,8][row]) #android.sensor.linear_acceleration.mean
  orientation1 <- as.double(test[,9][row])
  rotationvec1 <- as.double(test[,10][row])
  speed1 <- as.double(test[,11][row]) # speed.mean

  
  busprob <- predict(logisticbus, newdata=data.frame(acceleration = acceleration1,gyroscope = gyroscope1, sound = sound1, gamerotation = gamerotation1, linacc = linacc1, orientation = orientation1, rotationvec = rotationvec1, speed = speed1), type="response")
  walkingprob <- predict(logisticwalking, newdata=data.frame(acceleration = acceleration1,sound = sound1, gyroscope = gyroscope1, linacc = linacc1, orientation = orientation1, rotationvec = rotationvec1, speed = speed1), type="response")
  stillprob <- predict(logisticstill, newdata=data.frame(gyroscope = gyroscope1, acceleration = acceleration1, linacc = linacc1, orientation = orientation1, speed = speed1, rotationvec = rotationvec1, sound = sound1), type="response")
  trainprob <- predict(logistictrain, newdata=data.frame(acceleration = acceleration1, speed = speed1,gyroscope = gyroscope1, linacc = linacc1, sound = sound1, orientation = orientation1, rotationvec = rotationvec1), type="response")
  carprob <- predict(logisticcar, newdata=data.frame(acceleration = acceleration1, speed = speed1, linacc = linacc1, sound = sound1, orientation = orientation1, rotationvec = rotationvec1, gyroscope = gyroscope1), type="response")
  
  # print(with(summary(logisticwalking), 1 - deviance/null.deviance))
  # print(with(summary(logisticcar), 1 - deviance/null.deviance))
  
  # print(max(walkingprob, busprob, stillprob, trainprob, carprob))
  predictionprobs <- c(walkingprob, busprob, stillprob, trainprob, carprob)
  # print(predictionprobs)
  # print(levels[which.max(predictionprobs)])
  predictions[row] = levels[which.max(predictionprobs)]
}

correct = 0
for (i in 1:length(predictions)){
  if(test$target[i] == predictions[i])
    correct <- correct + 100
}
correct/length(predictions)

test$target[2] == predictions[2]
predictions




logisticbus <-glm(target~acceleration*sound+gyroscope*speed+sound*orientation*rotationvec*gamerotation+gamerotation*linacc+orientation*linacc*gyroscope, family="binomial")
logistictrain <-glm(target~acceleration+gyroscope*speed*sound+linacc*sound+sound*speed*rotationvec+sound+orientation*sound*linacc, family="binomial")
logisticcar <-glm(target~speed*sound+acceleration*sound+gyroscope*linacc*speed+rotationvec+orientation*speed*rotationvec, family="binomial") 
logisticstill <-glm(target~gyroscope*sound*acceleration+speed*sound*orientation*linacc+linacc*sound+rotationvec*sound*speed, family="binomial")
logisticwalking <-glm(target~acceleration+gyroscope+linacc*sound+orientation*speed*sound+speed*sound*rotationvec, family="binomial") 







