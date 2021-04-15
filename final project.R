setwd("/Users/bellapatel/stat385/")

normal <- read.csv("max_normal.csv")

colnames(normal)
normal$target

gyroscope <-as.double(normal[,3]) # android.sensor.gyroscope.mean
acceleration <-as.double(normal[,2]) # android.sensor.linear_acceleration.mean
speed <- as.double(normal[,11]) # speed.mean




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

# predict(logisticwalking, newdata=data.frame(acceleration=.3, gyroscope=.5, speed = .5), type="response")


