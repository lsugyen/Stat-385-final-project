setwd("/Users/bellapatel/Documents/GitHub/Stat-385-final-project")

first <- read.csv("Dfirst.csv")
second <- read.csv("Dsecond.csv")
third <- read.csv("Dthird.csv")

unique(first$target) # still, car, train, bus, walking

colnames(first)
colnames(second)
colnames(third)

length(unique(third$speed.max))
length(third$speed.max)

unique(first$target) # still, car, train, bus, walking

# logistic regression
# support vector machine
# clafficiation trees
# k means / k nearest neighborhoods