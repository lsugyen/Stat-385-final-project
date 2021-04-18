<<<<<<< HEAD
setwd("/Users/bellapatel/Documents/GitHub/Stat-385-final-project")
setwd("C:/Users/Stephanie/Documents/STAT385/Final Project/Stat-385-final-project")
=======
setwd("/Users/bellapatel/stat385/")
>>>>>>> 80a915b722098eb8c7ee66f89abec6773a491efe

normal <- read.csv("max_normal.csv")

<<<<<<< HEAD
unique(first$target) # still, car, train, bus, walking
unique(as.integer(factor(first$target))) # 3 2 4 1 5
=======
colnames(normal)
normal$target
>>>>>>> 80a915b722098eb8c7ee66f89abec6773a491efe

gyroscope <-as.double(normal[,3]) # android.sensor.gyroscope.mean
acceleration <-as.double(normal[,2]) # android.sensor.linear_acceleration.mean
speed <- as.double(normal[,11]) # speed.mean

<<<<<<< HEAD
#<<<<<<< HEAD
cor(as.integer(factor(first$target)), first$android.sensor.accelerometer.mean) # 0.3908
cor(as.integer(factor(first$target)), first$android.sensor.gyroscope.mean) # 0.4581
cor(as.integer(factor(first$target)), first$sound.mean) # 0.0100
cor(as.integer(factor(first$target)), second$android.sensor.game_rotation_vector.mean) # 0.0377
# There is a high positive correlation between two variables when the correlation between them is closer to 1
# A value closer to 0 suggests a weak relationship between the variables
# A low correlation (-0.2 < x < 0.2) probably suggests that much of variation of the response variable (Y) is unexplained 
# by the predictor (X), in which case, we should probably look for better explanatory variables.

# logistic regression
# support vector machine
# clafficiation trees
# k means / k nearest neighborhoods

cor(as.integer(first$target), first$android.sensor.accelerometer.mean) # .390
cor(as.integer(first$target), first$android.sensor.gyroscope.mean) # .458
cor(as.integer(first$target), first$sound.mean) # .01
cor(as.integer(first$target), first$android.sensor.gyroscope.max)

# logistic regression (Bella)
# support vector machine (Stephanie)
# clafficiation trees (Albert)
# k means / k nearest neighborhoods (Albert)

=======
# combine several classes into two classes, then apply binary classification iteratively
# create binary target

# bus
normal <- read.csv("max_normal.csv")
normal$target <- as.character(normal$target)
normal$target[normal$target != "Bus"] <- 0
normal$target[normal$target == "Bus"] <- 1
normal$target <- as.integer(normal$target)
target <- as.integer(normal[,5])

logisticbus <-glm(target~acceleration+speed+gyroscope, family="binomial") #repeat for all targets i guess
# coef(logisticbus)
with(summary(logisticbus), 1 - deviance/null.deviance)

# predict(logisticbus, newdata=data.frame(acceleration=.24, gyroscope=.028, speed = .39), type="response")

# train
normal <- read.csv("max_normal.csv")
normal$target <- as.character(normal$target)
normal$target[normal$target != "Train"] <- 0
normal$target[normal$target == "Train"] <- 1
normal$target <- as.integer(normal$target)
target <- as.integer(normal[,5])

logistictrain <-glm(target~acceleration+speed+gyroscope, family="binomial") #repeat for all targets i guess
# coef(logistictrain)
with(summary(logistictrain), 1 - deviance/null.deviance)

# predict(logistictrain, newdata=data.frame(acceleration=.24, gyroscope=.028, speed = .39), type="response")

# car
normal <- read.csv("max_normal.csv")
normal$target <- as.character(normal$target)
normal$target[normal$target != "Car"] <- 0
normal$target[normal$target == "Car"] <- 1
normal$target <- as.integer(normal$target)
target <- as.integer(normal[,5])

logisticcar <-glm(target~acceleration+speed+gyroscope, family="binomial") #repeat for all targets i guess
# coef(logisticcar)
with(summary(logisticcar), 1 - deviance/null.deviance)

# predict(logisticcar, newdata=data.frame(acceleration=.24, gyroscope=.028, speed = .39), type="response")

# still
normal <- read.csv("max_normal.csv")
normal$target <- as.character(normal$target)
normal$target[normal$target != "Still"] <- 0
normal$target[normal$target == "Still"] <- 1
normal$target <- as.integer(normal$target)
target <- as.integer(normal[,5])

logisticstill <-glm(target~acceleration+speed+gyroscope, family="binomial") #repeat for all targets i guess
# coef(logisticstill)
with(summary(logisticstill), 1 - deviance/null.deviance)

# predict(logisticstill, newdata=data.frame(acceleration=0, gyroscope=0, speed = 0), type="response")

# walking
normal <- read.csv("max_normal.csv")
normal$target <- as.character(normal$target)
normal$target[normal$target != "Walking"] <- 0
normal$target[normal$target == "Walking"] <- 1
normal$target <- as.integer(normal$target)
target <- as.integer(normal[,5])

logisticwalking <-glm(target~acceleration+speed+gyroscope, family="binomial") #repeat for all targets i guess
# coef(logisticwalking)
with(summary(logisticwalking), 1 - deviance/null.deviance)

see <- predict(logisticwalking, newdata=data.frame(acceleration=.3, gyroscope=.5, speed = .5), type="response")

# for testing
levels <- c("Walking", "Bus", "Still", "Train", "Car")

for (row in 1:nrow(test))
{
  gyroscope1 <-as.double(test[,3][row]) # android.sensor.gyroscope.mean
  acceleration1 <-as.double(test[,2][row]) # android.sensor.linear_acceleration.mean
  speed1 <- as.double(test[,11][row])

  walkingprob <- predict(logisticwalking, newdata=data.frame(acceleration=acceleration1, gyroscope=gyroscope1, speed = speed1), type="response")
  busprob <- predict(logisticbus, newdata=data.frame(acceleration=acceleration1, gyroscope=gyroscope1, speed = speed1), type="response")
  stillprob <- predict(logisticstill, newdata=data.frame(acceleration=acceleration1, gyroscope=gyroscope1, speed = speed1), type="response")
  trainprob <- predict(logistictrain, newdata=data.frame(acceleration=acceleration1, gyroscope=gyroscope1, speed = speed1), type="response")
  carprob <- predict(logisticcar, newdata=data.frame(acceleration=acceleration1, gyroscope=gyroscope1, speed = speed1), type="response")
  
  # print(max(walkingprob, busprob, stillprob, trainprob, carprob))
  predictionprobs <- c(walkingprob, busprob, stillprob, trainprob, carprob)
  # print(predictionprobs)
  print(levels[which.max(predictionprobs)])
}

test$target





>>>>>>> 80a915b722098eb8c7ee66f89abec6773a491efe
