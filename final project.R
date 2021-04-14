setwd("/Users/bellapatel/stat385/")

first <- read.csv("Dfirst.csv")
second <- read.csv("Dsecond.csv")
third <- read.csv("Dthird.csv")

which(colnames(third)%in%(colnames(first)))
which(colnames(third)%in%(colnames(second)))

total <- cbind(first, second[,c(6,7,8,9,seq(14,29))], third[,c(seq(34, 37))])
colnames(total)

unique(total$target) # still, car, train, bus, walking
unique(as.integer(total$target)) # 3 2 4 1 5

cor(as.integer(total$target), total[,-c(14)])

plot(total$target, total$speed.mean)


# logistic regression
# support vector machine
# classificiation trees
# k means / k nearest neighborhoods
