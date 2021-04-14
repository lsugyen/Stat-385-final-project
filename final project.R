setwd("/Users/bellapatel/Documents/GitHub/Stat-385-final-project")

first <- read.csv("Dfirst.csv")
second <- read.csv("Dsecond.csv")
third <- read.csv("Dthird.csv")

unique(first$target) # still, car, train, bus, walking
unique(as.integer(first$target)) # 3 2 4 1 5

colnames(first)
colnames(second)
colnames(third)

first$target <- as.integer(first$target)
cor(first$target, first) # highest - android.sensor.gyroscope.mean: 0.4580953

second$target <- as.integer(second$target)
cor(second$target, second) # highest - android.sensor.linear_acceleration.mean: 0.4462201

third$target <- as.integer(third$target)
cor(third$target, third) # highest - android.sensor.gyroscope.mean: 0.4580953 



# logistic regression
# support vector machine
# clafficiation trees
# k means / k nearest neighborhoods