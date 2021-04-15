setwd("/Users/bellapatel/Documents/GitHub/Stat-385-final-project")
setwd("C:/Users/Stephanie/Documents/STAT385/Final Project/Stat-385-final-project")

first <- read.csv("Dfirst.csv")
second <- read.csv("Dsecond.csv")
third <- read.csv("Dthird.csv")

unique(first$target) # still, car, train, bus, walking
unique(as.integer(factor(first$target))) # 3 2 4 1 5

colnames(first)
colnames(second)
colnames(third)

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

