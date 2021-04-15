setwd("/Users/bellapatel/Documents/GitHub/Stat-385-final-project")

first <- read.csv("Dfirst.csv")
second <- read.csv("Dsecond.csv")
third <- read.csv("Dthird.csv")

unique(first$target) # still, car, train, bus, walking
unique(as.integer(first$target)) # 3 2 4 1 5

colnames(first)
colnames(second)
colnames(third)

cor(as.integer(first$target), first$android.sensor.accelerometer.mean) # .390
cor(as.integer(first$target), first$android.sensor.gyroscope.mean) # .458
cor(as.integer(first$target), first$sound.mean) # .01
cor(as.integer(first$target), first$android.sensor.gyroscope.max)

# logistic regression (Bella)
# support vector machine (Stephanie)
# clafficiation trees (Albert)
# k means / k nearest neighborhoods (Albert)