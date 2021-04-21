# Testing feature variables 

setwd("C:/Users/Stephanie/Documents/STAT385/Final Project/Stat-385-final-project") # set my working directory 

first <- read.csv("Dfirst.csv") # read Dfirst data set 
second <- read.csv("Dsecond.csv") # read Dsecond data set 
third <- read.csv("Dthird.csv") # read Dthird data set 

max_normal <- read.csv("max_normal.csv") # read max_normal data set (compact data normalized) 
z_normal <- read.csv("z_normal.csv") # read z_normal data set (compact data normalized)

head(first) # first few rows of each column of first data set 
levels(as.factor(first$target)) # levels of data set 
# "Bus"     "Car"     "Still"   "Train"   "Walking"

# compact data set by Albert (unique features, contains only means though)
second1 <- second[,which(colnames(second)%in%colnames(first)==FALSE)]
third1 <- third[,which(colnames(third)%in%colnames(first)==FALSE&colnames(third)%in%colnames(second1)==FALSE)]
third1 <- third1[,which(colnames(third1)%in%colnames(second1)==FALSE)]
data = cbind(first,second1,third1)
compact_data <- cbind(first[,c(1,2,6,10,14)],second1[,c(1,5,9,13,17)])
compact_data$speed.mean <- third1[,1]
#data = cbind(first[,c(1,2,6,10)],second[,c(6,10)])

head(compact_data) # first few lines of comoact data 

# target column moved to end of compact data frame 
compact_data <- subset(compact_data, select=c(time:sound.mean, android.sensor.game_rotation_vector.mean:speed.mean, target))

head(max_normal) # first few rows of max normal data set 

# target column moved to end of  max normal data frame and removed first column (first column was irrelevant, contained the number of each row)
max_normal <- subset(max_normal, select=c(time:sound.mean, android.sensor.game_rotation_vector.mean:speed.mean, target)) 

head(z_normal) # first few rows of z normal data set 

# target column moved to end of z normal data frame and removed first column (first column was irrelevant, contained the number of each row)
z_normal <- subset(z_normal, select=c(time:sound.mean, android.sensor.game_rotation_vector.mean:speed.mean, target))


set.seed(2021) # set seed to get same sample each time (just for testing for now)


# Training Data set 
# Create training data set 
training <- sample(1:dim(compact_data)[1], 4000, replace = FALSE) # randomly sample 4000 rows
compact_datatrain <- compact_data[training,] # compact data training set 
max_normaltrain <- max_normal[training,] # max normal training set 
z_normaltrain <- z_normal[training,] # z normal traning data set 


# Test data set 
# Create test data set 
testdata <- c(1:dim(compact_data)[1])[-training] # sample remaining rows 
compact_datatest <- compact_data[testdata,] # compact data test set 
max_normaltest <- max_normal[testdata,] # max normal test set 
z_normaltest <- z_normal[testdata,] # z normal test data set 


# Using all features (mean, min, max and std) now, instead of just compact data (means)


# Normalize all the data using third data set 

third_datatrain <-third[training,] # third training data set 

thirdtest <- third[testdata,] # third test data set 

max_normalization<-function(x)
{
  norms<-(x-min(x))/(max(x)-min(x))
  return(norms)
}

z_normalization <-function(x){
  mu <- mean(x)
  sigma <- sd(x)
  z <- (x-mu)/sigma
  return(z)
}

# Third max normal 
max_normalthird <- data.frame(cbind(sapply(third[1:(length(third)-1)], max_normalization)))
max_normalthird <- data.frame(sapply(max_normalthird, as.numeric), target = third$target) # made values as numeric 


# Third z normal 
z_normalthird <- data.frame(cbind(sapply(third[1:(length(third)-1)], z_normalization)))
z_normalthird <- data.frame(sapply(z_normalthird, as.numeric), target = third$target) # made values as numeric 


z_normalthirdtrain <- z_normalthird[training,] # z normal third traning data set 

max_normalthirdtrain <- max_normalthird[training,] # max normal third test set 

z_normalthirdtest <- z_normalthird[testdata,] # z normal third test data set

max_normalthirdtest <- max_normalthird[testdata,] # max normal third test set 


# Correlation between features and targets 

# Dfirst 
cor(first[1:(length(first)-1)], as.integer(factor(first$target)))
# time                               0.07238531 (weak positive correlation)
# android.sensor.accelerometer.mean  0.39077663 (strong positive correlation)
# android.sensor.accelerometer.min  -0.28212694 (strong negative correlation)
# android.sensor.accelerometer.max   0.33285683 (strong positive correlation)
# android.sensor.accelerometer.std   0.43906601 (strong positive correlation)
# android.sensor.gyroscope.mean      0.45809533 (strong positive correlation)
# android.sensor.gyroscope.min       0.33537893 (strong positive correlation)
# android.sensor.gyroscope.max       0.40764351 (strong positive correlation)
# android.sensor.gyroscope.std       0.39831360 (strong positive correlation)
# sound.mean                         0.01002905 (weak positive correlation)
# sound.min                          0.01015330 (weak positive correlation)
# sound.max                          0.00997791 (weak positive correlation)
# sound.std                          0.02936531 (weak positive correlation)

# android.sensor.accelerometer.mean, android.sensor.accelerometer.max, android.sensor.accelerometer.std, android.sensor.gyroscope.mean
# android.sensor.gyroscope.min, android.sensor.gyroscope.max, android.sensor.gyroscope.std have strong positive correlations 

# Dsecond 
cor(second[1:(length(second)-1)], as.integer(factor(second$target)))
# time                                        0.07238531 (weak positive correlation)
# android.sensor.accelerometer.mean           0.39077663 (strong positive correlation)
# android.sensor.accelerometer.min           -0.28212694 (strong negative correlation)
# android.sensor.accelerometer.max            0.33285683 (strong positive correlation)
# android.sensor.accelerometer.std            0.43906601 (strong positive correlation)
# android.sensor.game_rotation_vector.mean    0.03772740 (weak positive correlation)
# android.sensor.game_rotation_vector.min     0.01914994 (weak positive correlation)
# android.sensor.game_rotation_vector.max     0.04435807 (weak positive correlation)
# android.sensor.game_rotation_vector.std     0.13596744 (weak positive correlation)
# android.sensor.gyroscope.mean               0.45809533 (strong positive correlation)
# android.sensor.gyroscope.min                0.33537893 (strong positive correlation)
# android.sensor.gyroscope.max                0.40764351 (strong positive correlation)
# android.sensor.gyroscope.std                0.39831360 (strong positive correlation)
# android.sensor.gyroscope_uncalibrated.mean  0.43973652 (strong positive correlation)
# android.sensor.gyroscope_uncalibrated.min   0.36372226 (strong positive correlation)
# android.sensor.gyroscope_uncalibrated.max   0.41089559 (strong positive correlation)
# android.sensor.gyroscope_uncalibrated.std   0.38020050 (strong positive correlation)
# android.sensor.linear_acceleration.mean     0.44622005 (strong positive correlation)
# android.sensor.linear_acceleration.min      0.39928022 (strong positive correlation)
# android.sensor.linear_acceleration.max      0.31109472 (strong positive correlation)
# android.sensor.linear_acceleration.std      0.33649256 (strong positive correlation)
# android.sensor.orientation.mean             0.03456306 (weak positive correlation)
# android.sensor.orientation.min             -0.02471891 (weak negative correlation)
# android.sensor.orientation.max              0.08395114 (weak positive correlation)
# android.sensor.orientation.std              0.21366705 (strong positive correlation)
# android.sensor.rotation_vector.mean         0.06302674 (weak positive correlation)
# android.sensor.rotation_vector.min          0.04521776 (weak positive correlation)
# android.sensor.rotation_vector.max          0.07270207 (weak positive correlation)
# android.sensor.rotation_vector.std          0.15148468 (weak positive correlation)
# sound.mean                                  0.01002905 (weak positive correlation)
# sound.min                                   0.01015330 (weak positive correlation)
# sound.max                                   0.00997791 (weak positive correlation)
# sound.std                                   0.02936531 (weak positive correlation)

# android.sensor.accelerometer.mean, android.sensor.accelerometer.max, android.sensor.accelerometer.std, android.sensor.gyroscope.mean
# android.sensor.gyroscope.min, android.sensor.gyroscope.max, android.sensor.gyroscope.std, android.sensor.gyroscope_uncalibrated.mean
# android.sensor.gyroscope_uncalibrated.min, android.sensor.gyroscope_uncalibrated.max, android.sensor.gyroscope_uncalibrated.std, android.sensor.linear_acceleration.mean
# android.sensor.linear_acceleration.min, android.sensor.linear_acceleration.max, android.sensor.linear_acceleration.std, android.sensor.orientation.std
# have strong positive correlations 

# Dthird 
cor(third[1:(length(third)-1)], as.integer(factor(third$target)))
# time                                        0.07238531 (weak positive correlation)
# android.sensor.accelerometer.mean           0.39077663 (strong positive correlation)
# android.sensor.accelerometer.min           -0.28212694 (strong negative correlation)
# android.sensor.accelerometer.max            0.33285683 (strong positive correlation)
# android.sensor.accelerometer.std            0.43906601 (strong positive correlation)
# android.sensor.game_rotation_vector.mean    0.03772740 (weak positive correlation)
# android.sensor.game_rotation_vector.min     0.01914994 (weak positive correlation)
# android.sensor.game_rotation_vector.max     0.04435807 (weak positive correlation)
# android.sensor.game_rotation_vector.std     0.13596744 (weak positive correlation)
# android.sensor.gyroscope.mean               0.45809533 (strong positive correlation)
# android.sensor.gyroscope.min                0.33537893 (strong positive correlation)
# android.sensor.gyroscope.max                0.40764351 (strong positive correlation)
# android.sensor.gyroscope.std                0.39831360 (strong positive correlation)
# android.sensor.gyroscope_uncalibrated.mean  0.43973652 (strong positive correlation)
# android.sensor.gyroscope_uncalibrated.min   0.36372226 (strong positive correlation)
# android.sensor.gyroscope_uncalibrated.max   0.41089559 (strong positive correlation)
# android.sensor.gyroscope_uncalibrated.std   0.38020050 (strong positive correlation)
# android.sensor.linear_acceleration.mean     0.44622005 (strong positive correlation)
# android.sensor.linear_acceleration.min      0.39928022 (strong positive correlation)
# android.sensor.linear_acceleration.max      0.31109472 (strong positive correlation)
# android.sensor.linear_acceleration.std      0.33649256 (strong positive correlation)
# android.sensor.orientation.mean             0.03456306 (weak positive correlation)
# android.sensor.orientation.min             -0.02471891 (weak negative correlation)
# android.sensor.orientation.max              0.08395114 (weak positive correlation)
# android.sensor.orientation.std              0.21366705 (strong positive correlation)
# android.sensor.rotation_vector.mean         0.06302674 (weak positive correlation)
# android.sensor.rotation_vector.min          0.04521776 (weak positive correlation)
# android.sensor.rotation_vector.max          0.07270207 (weak positive correlation)
# android.sensor.rotation_vector.std          0.15148468 (weak positive correlation)
# sound.mean                                  0.01002905 (weak positive correlation)
# sound.min                                   0.01015330 (weak positive correlation)
# sound.max                                   0.00997791 (weak positive correlation)
# sound.std                                   0.02936531 (weak positive correlation)
# speed.mean                                 -0.15046229 (weak negative correlation)
# speed.min                                  -0.14998109 (weak negative correlation)
# speed.max                                  -0.15089156 (weak negative correlation) 
# speed.std                                  -0.15675506 (weak negative correlation)

# android.sensor.accelerometer.mean, android.sensor.accelerometer.max, android.sensor.accelerometer.std, android.sensor.gyroscope.mean
# android.sensor.gyroscope.min, android.sensor.gyroscope.max, android.sensor.gyroscope.std, android.sensor.gyroscope_uncalibrated.mean
# android.sensor.gyroscope_uncalibrated.min, android.sensor.gyroscope_uncalibrated.max, android.sensor.gyroscope_uncalibrated.std, android.sensor.linear_acceleration.mean
# android.sensor.linear_acceleration.min, android.sensor.linear_acceleration.max, android.sensor.linear_acceleration.std, android.sensor.orientation.std
# have strong positive correlations 

# There is a high positive correlation between two variables when the correlation between them is closer to 1
# A value closer to 0 suggests a weak relationship between the variables
# A low correlation (-0.2 < x < 0.2) probably suggests that much of variation of the response variable (Y) is unexplained 
# by the predictor (X), in which case, we should probably look for better explanatory variables.

# Compact data 
cor(compact_data[1:(length(compact_data)-1)], as.integer(factor(compact_data$target)))
# time                                        0.07238531 (weak positive correlation)
# android.sensor.accelerometer.mean           0.39077663 (strong positive correlation)
# android.sensor.gyroscope.mean               0.45809533 (strong positive correlation)
# sound.mean                                  0.01002905 (weak positive correlation)
# android.sensor.game_rotation_vector.mean    0.03772740 (weak positive correlation)
# android.sensor.gyroscope_uncalibrated.mean  0.43973652 (strong positive correlation)
# android.sensor.linear_acceleration.mean     0.44622005 (strong positive correlation)
# android.sensor.orientation.mean             0.03456306 (weak positive correlation)
# android.sensor.rotation_vector.mean         0.06302674 (weak positive correlation)
# speed.mean                                 -0.15046229 (weak negative correlation)

# android.sensor.accelerometer.mean, android.sensor.gyroscope.mean android.sensor.gyroscope_uncalibrated.mean  and android.sensor.linear_acceleration.mean   
# have strong positive correlation 

# Max Normal 
cor(max_normal[1:(length(max_normal)-1)], as.integer(factor(max_normal$target)))
# time                                        0.07238531 (weak positive correlation)
# android.sensor.accelerometer.mean           0.39077663 (strong positive correlation)
# android.sensor.gyroscope.mean               0.45809533 (strong positive correlation)
# sound.mean                                  0.01002905 (weak positive correlation)
# android.sensor.game_rotation_vector.mean    0.03772740 (weak positive correlation)
# android.sensor.gyroscope_uncalibrated.mean  0.43973652 (strong positive correlation)
# android.sensor.linear_acceleration.mean     0.44622005 (strong positive correlation)
# android.sensor.orientation.mean             0.03456306 (weak positive correlation)
# android.sensor.rotation_vector.mean         0.06302674 (weak positive correlation)
# speed.mean                                 -0.15046229 (weak negative correlation)

# android.sensor.accelerometer.mean, android.sensor.gyroscope.mean android.sensor.gyroscope_uncalibrated.mean  and android.sensor.linear_acceleration.mean   
# have strong positive correlation 

# Z Normal 
cor(z_normal[1:(length(z_normal)-1)], as.integer(factor(z_normal$target)))
# time                                        0.07238531 (weak positive correlation)
# android.sensor.accelerometer.mean           0.39077663 (strong positive correlation)
# android.sensor.gyroscope.mean               0.45809533 (strong positive correlation)
# sound.mean                                  0.01002905 (weak positive correlation)
# android.sensor.game_rotation_vector.mean    0.03772740 (weak positive correlation)
# android.sensor.gyroscope_uncalibrated.mean  0.43973652 (strong positive correlation)
# android.sensor.linear_acceleration.mean     0.44622005 (strong positive correlation)
# android.sensor.orientation.mean             0.03456306 (weak positive correlation)
# android.sensor.rotation_vector.mean         0.06302674 (weak positive correlation)
# speed.mean                                 -0.15046229 (weak negative correlation)

# android.sensor.accelerometer.mean, android.sensor.gyroscope.mean android.sensor.gyroscope_uncalibrated.mean  and android.sensor.linear_acceleration.mean   
# have strong positive correlation 

# Max normal third 
cor(max_normalthird[1:(length(max_normalthird)-1)], as.integer(factor(max_normalthird$target)))
# time                                        0.07238531 (weak positive correlation)
# android.sensor.accelerometer.mean           0.39077663 (strong positive correlation)
# android.sensor.accelerometer.min           -0.28212694 (weak negative correlation)
# android.sensor.accelerometer.max            0.33285683 (strong positive correlation)
# android.sensor.accelerometer.std            0.43906601 (strong positive correlation)
# android.sensor.game_rotation_vector.mean    0.03772740 (weak positive correlation)
# android.sensor.game_rotation_vector.min     0.01914994 (weak positive correlation)
# android.sensor.game_rotation_vector.max     0.04435807 (weak positive correlation)
# android.sensor.game_rotation_vector.std     0.13596744 (weak positive correlation)
# android.sensor.gyroscope.mean               0.45809533 (strong positive correlation)
# android.sensor.gyroscope.min                0.33537893 (strong positive correlation)
# android.sensor.gyroscope.max                0.40764351 (strong positive correlation)
# android.sensor.gyroscope.std                0.39831360 (strong positive correlation)
# android.sensor.gyroscope_uncalibrated.mean  0.43973652 (strong positive correlation)
# android.sensor.gyroscope_uncalibrated.min   0.36372226 (strong positive correlation)
# android.sensor.gyroscope_uncalibrated.max   0.41089559 (strong positive correlation)
# android.sensor.gyroscope_uncalibrated.std   0.38020050 (strong positive correlation)
# android.sensor.linear_acceleration.mean     0.44622005 (strong positive correlation)
# android.sensor.linear_acceleration.min      0.39928022 (strong positive correlation)
# android.sensor.linear_acceleration.max      0.31109472 (strong positive correlation)
# android.sensor.linear_acceleration.std      0.33649256 (strong positive correlation)
# android.sensor.orientation.mean             0.03456306 (weak positive correlation)
# android.sensor.orientation.min             -0.02471891 (weak negative correlation)
# android.sensor.orientation.max              0.08395114 (weak positive correlation)
# android.sensor.orientation.std              0.21366705 (strong positive correlation)
# android.sensor.rotation_vector.mean         0.06302674 (weak positive correlation)
# android.sensor.rotation_vector.min          0.04521776 (weak positive correlation)
# android.sensor.rotation_vector.max          0.07270207 (weak positive correlation)
# android.sensor.rotation_vector.std          0.15148468 (weak positive correlation)
# sound.mean                                  0.01002905 (weak positive correlation)
# sound.min                                   0.01015330 (weak positive correlation)
# sound.max                                   0.00997791 (weak positive correlation)
# sound.std                                   0.02936531 (weak positive correlation)
# speed.mean                                 -0.15046229 (weak negative correlation)
# speed.min                                  -0.14998109 (weak negative correlation)
# speed.max                                  -0.15089156 (weak negative correlation)
# speed.std                                  -0.15675506 (weak negative correlation)

# android.sensor.accelerometer.mean, android.sensor.accelerometer.max, android.sensor.accelerometer.std, android.sensor.gyroscope.mean
# android.sensor.gyroscope.min, android.sensor.gyroscope.max, android.sensor.gyroscope.std, android.sensor.gyroscope_uncalibrated.mean, android.sensor.gyroscope_uncalibrated.min
# android.sensor.gyroscope_uncalibrated.max, android.sensor.gyroscope_uncalibrated.std, android.sensor.linear_acceleration.mean, android.sensor.linear_acceleration.min
# android.sensor.linear_acceleration.max, android.sensor.linear_acceleration.std, android.sensor.orientation.std
# have strong positive correlations 

# Z normal third 
cor(z_normalthird[1:(length(z_normalthird)-1)], as.integer(factor(z_normalthird$target)))
# time                                        0.07238531 (weak positive correlation)
# android.sensor.accelerometer.mean           0.39077663 (strong positive correlation)
# android.sensor.accelerometer.min           -0.28212694 (weak negative correlation)
# android.sensor.accelerometer.max            0.33285683 (strong positive correlation)
# android.sensor.accelerometer.std            0.43906601 (strong positive correlation)
# android.sensor.game_rotation_vector.mean    0.03772740 (weak positive correlation)
# android.sensor.game_rotation_vector.min     0.01914994 (weak positive correlation)
# android.sensor.game_rotation_vector.max     0.04435807 (weak positive correlation)
# android.sensor.game_rotation_vector.std     0.13596744 (weak positive correlation) 
# android.sensor.gyroscope.mean               0.45809533 (strong positive correlation)
# android.sensor.gyroscope.min                0.33537893 (strong positive correlation)
# android.sensor.gyroscope.max                0.40764351 (strong positive correlation)
# android.sensor.gyroscope.std                0.39831360 (strong positive correlation)
# android.sensor.gyroscope_uncalibrated.mean  0.43973652 (strong positive correlation)
# android.sensor.gyroscope_uncalibrated.min   0.36372226 (strong positive correlation)
# android.sensor.gyroscope_uncalibrated.max   0.41089559 (strong positive correlation)
# android.sensor.gyroscope_uncalibrated.std   0.38020050 (strong positive correlation)
# android.sensor.linear_acceleration.mean     0.44622005 (strong positive correlation)
# android.sensor.linear_acceleration.min      0.39928022 (strong positive correlation)
# android.sensor.linear_acceleration.max      0.31109472 (strong positive correlation)
# android.sensor.linear_acceleration.std      0.33649256 (strong positive correlation)
# android.sensor.orientation.mean             0.03456306 (weak positive correlation)
# android.sensor.orientation.min             -0.02471891 (weak negative correlation)
# android.sensor.orientation.max              0.08395114 (weak positive correlation)
# android.sensor.orientation.std              0.21366705 (strong positive correlation)
# android.sensor.rotation_vector.mean         0.06302674 (weak positive correlation)
# android.sensor.rotation_vector.min          0.04521776 (weak positive correlation)
# android.sensor.rotation_vector.max          0.07270207 (weak positive correlation)
# android.sensor.rotation_vector.std          0.15148468 (weak positive correlation)
# sound.mean                                  0.01002905 (weak positive correlation)
# sound.min                                   0.01015330 (weak positive correlation)
# sound.max                                   0.00997791 (weak positive correlation)
# sound.std                                   0.02936531 (weak positive correlation)
# speed.mean                                 -0.15046229 (weak negative correlation)
# speed.min                                  -0.14998109 (weak negative correlation)
# speed.max                                  -0.15089156 (weak negative correlation)
# speed.std                                  -0.15675506 (weak negative correlation)

# android.sensor.accelerometer.mean, android.sensor.accelerometer.max, android.sensor.accelerometer.std, android.sensor.gyroscope.mean
# android.sensor.gyroscope.min, android.sensor.gyroscope.max, android.sensor.gyroscope.std, android.sensor.gyroscope_uncalibrated.mean, android.sensor.gyroscope_uncalibrated.min
# android.sensor.gyroscope_uncalibrated.max, android.sensor.gyroscope_uncalibrated.std , android.sensor.linear_acceleration.mean, android.sensor.linear_acceleration.min
# android.sensor.linear_acceleration.max, android.sensor.linear_acceleration.std, android.sensor.orientation.std
# have strong positive correlations 


# Three stars (or asterisks) represent a highly significant p-value. Consequently, 
# a small p-value for the intercept and the slope indicates that we can reject the 
# null hypothesis which allows us to conclude that there is a strong relationship 
# between mpg and weight. Typically, a p-value of 5% (.05) or less is a good cut-off 
# point. In our model example, the p-values are very close to zero


# Std. Error	Closer to zero the better
# t-statistic	Should be greater 1.96 for p-value to be less than 0.05
# F-Statistic	Higher the better
# Adj R-Squared	Higher the better
# R-Squared	Higher the better (> 0.70)


# Boxplots 

par(mfrow=c(2, 2))

# Dfirst 
for (i in 1:(length(first)-1)) {
  boxplot(first[,i]~as.factor(first$target), main=names(first[i]))
}

# Dsecond 
for (i in 1:(length(second)-1)) {
  boxplot(second[,i]~as.factor(second$target), main=names(second[i]))
}

# Dthird 
for (i in 1:(length(third)-1)) {
  boxplot(third[,i]~as.factor(third$target), main=names(third[i]))
}

# Compact Data 
for (i in 1:(length(compact_data)-1)) {
  boxplot(compact_data[,i]~as.factor(compact_data$target), main=names(compact_data[i]))
}

# Max Normal  
for (i in 1:(length(max_normal)-1)) {
  boxplot(max_normal[,i]~as.factor(max_normal$target), main=names(max_normal[i]))
}

# Z Normal  
for (i in 1:(length(z_normal)-1)) {
  boxplot(z_normal[,i]~as.factor(z_normal$target), main=names(z_normal[i]))
}

# Max Normal  
for (i in 1:(length(max_normalthird)-1)) {
  boxplot(max_normalthird[,i]~as.factor(max_normalthird$target), main=names(max_normalthird[i]))
}

# Z Normal  
for (i in 1:(length(z_normalthird)-1)) {
  boxplot(z_normalthird[,i]~as.factor(z_normalthird$target), main=names(z_normalthird[i]))
}


# ANOVA 

# Dfirst 
first$target <- factor(first$target)
formulae <- lapply(colnames(first)[2:ncol(first)-1], function(x) as.formula(paste0(x, " ~ target")))
anovafirst <- lapply(formulae, function(x) summary(aov(x, data = first)))
names(anovafirst) <- format(formulae)
anovafirst # summary of anova test for each column with target 
pfirst <- unlist(lapply(anovafirst, function(x) x[[1]]$"Pr(>F)"[1]))
pvaluesfirst <- data.frame(Sensor = sub(' ~ target', '', names(pfirst)), pvalue = pfirst)
pvaluesfirst # p-values from anova test in data frame 

# android.sensor.accelerometer.mean, android.sensor.accelerometer.min, android.sensor.accelerometer.max, android.sensor.accelerometer.std, 
# android.sensor.gyroscope.mean, android.sensor.gyroscope.min, android.sensor.gyroscope.max, android.sensor.gyroscope.std
# have very small p-values 

# Dsecond 
second$target <- factor(second$target)
formulae <- lapply(colnames(second)[2:ncol(second)-1], function(x) as.formula(paste0(x, " ~ target")))
anovasecond <- lapply(formulae, function(x) summary(aov(x, data = second)))
names(anovasecond) <- format(formulae)
anovasecond # summary of anova test for each column with target
psecond <- unlist(lapply(anovasecond, function(x) x[[1]]$"Pr(>F)"[1]))
pvaluessecond <- data.frame(Sensor = sub(' ~ target', '', names(psecond)), pvalue = psecond)
pvaluessecond # p-values from anova test in data frame

# android.sensor.accelerometer.mean, android.sensor.accelerometer.min, android.sensor.accelerometer.max,android.sensor.accelerometer.std,
# android.sensor.gyroscope.mean,android.sensor.gyroscope.min,android.sensor.gyroscope.max,android.sensor.gyroscope.std,
# android.sensor.gyroscope_uncalibrated.mean,android.sensor.gyroscope_uncalibrated.min,
# android.sensor.gyroscope_uncalibrated.max,android.sensor.gyroscope_uncalibrated.std,ndroid.sensor.linear_acceleration.mean,
# android.sensor.linear_acceleration.min, android.sensor.linear_acceleration.max, android.sensor.linear_acceleration.std,
# have small p-vlaues 

# Dthird 
third$target <- factor(third$target)
formulae <- lapply(colnames(third)[2:ncol(third)-1], function(x) as.formula(paste0(x, " ~ target")))
anovathird <- lapply(formulae, function(x) summary(aov(x, data = third)))
names(anovathird) <- format(formulae)
anovathird # summary of anova test for each column with target
pthird <- unlist(lapply(anovathird, function(x) x[[1]]$"Pr(>F)"[1]))
pvaluesthird <- data.frame(Sensor = sub(' ~ target', '', names(pthird)), pvalue = pthird)
pvaluesthird # p-values from anova test in data frame

# android.sensor.accelerometer.mean, android.sensor.accelerometer.min, android.sensor.accelerometer.max, android.sensor.accelerometer.std, android.sensor.gyroscope.mean, android.sensor.gyroscope.min, android.sensor.gyroscope.max, 
# android.sensor.gyroscope.std, android.sensor.gyroscope_uncalibrated.mean, android.sensor.gyroscope_uncalibrated.min, android.sensor.gyroscope_uncalibrated.max, android.sensor.gyroscope_uncalibrated.std, 
# android.sensor.linear_acceleration.mean, android.sensor.linear_acceleration.min, android.sensor.linear_acceleration.max, android.sensor.linear_acceleration.std, speed.mean+speed.min, speed.max
# have small p-vlaues 

# Compact data 
compact_data$target <- factor(compact_data$target)
formulae <- lapply(colnames(compact_data)[2:ncol(compact_data)-1], function(x) as.formula(paste0(x, " ~ target")))
anovacompact_data <- lapply(formulae, function(x) summary(aov(x, data = compact_data)))
names(anovacompact_data) <- format(formulae)
anovacompact_data # summary of anova test for each column with target
pcompact_data <- unlist(lapply(anovacompact_data, function(x) x[[1]]$"Pr(>F)"[1]))
pvaluescompact_data <- data.frame(Sensor = sub(' ~ target', '', names(pcompact_data)), pvalue = pcompact_data)
pvaluescompact_data # p-values from anova test in data frame

# android.sensor.accelerometer.mean,android.sensor.gyroscope.mean,android.sensor.gyroscope_uncalibrated.mean,android.sensor.linear_acceleration.mean,speed.mean
# have small p-values 

# Max Normal 
max_normal$target <- factor(max_normal$target)
formulae <- lapply(colnames(max_normal)[2:ncol(max_normal)-1], function(x) as.formula(paste0(x, " ~ target")))
anovamax_normal <- lapply(formulae, function(x) summary(aov(x, data = max_normal)))
names(anovamax_normal) <- format(formulae)
anovamax_normal # summary of anova test for each column with target
pmax_normal <- unlist(lapply(anovamax_normal, function(x) x[[1]]$"Pr(>F)"[1]))
pvaluesmax_normal <- data.frame(Sensor = sub(' ~ target', '', names(pmax_normal)), pvalue = pmax_normal)
pvaluesmax_normal # p-values from anova test in data frame

# android.sensor.accelerometer.mean, android.sensor.gyroscope.mean, android.sensor.gyroscope_uncalibrated.mean, 
# android.sensor.linear_acceleration.mean, speed.mean

# Z Normal 
z_normal$target <- factor(z_normal$target)
formulae <- lapply(colnames(z_normal)[2:ncol(z_normal)-1], function(x) as.formula(paste0(x, " ~ target")))
anovaz_normal <- lapply(formulae, function(x) summary(aov(x, data = z_normal)))
names(anovaz_normal) <- format(formulae)
anovaz_normal # summary of anova test for each column with target
pz_normal <- unlist(lapply(anovaz_normal, function(x) x[[1]]$"Pr(>F)"[1]))
pvaluesz_normal <- data.frame(Sensor = sub(' ~ target', '', names(pz_normal)), pvalue = pz_normal)
pvaluesz_normal# p-values from anova test in data frame

# android.sensor.accelerometer.mean, android.sensor.gyroscope.mean, android.sensor.gyroscope_uncalibrated.mean, 
# android.sensor.linear_acceleration.mean, speed.mean
# have small p-values 

# Max Normal third
max_normalthird$target <- factor(max_normalthird$target)
formulae <- lapply(colnames(max_normalthird)[2:ncol(max_normalthird)-1], function(x) as.formula(paste0(x, " ~ target")))
anovamax_normalthird <- lapply(formulae, function(x) summary(aov(x, data = max_normalthird)))
names(anovamax_normalthird) <- format(formulae)
anovamax_normalthird # summary of anova test for each column with target
pmax_normalthird <- unlist(lapply(anovamax_normalthird, function(x) x[[1]]$"Pr(>F)"[1]))
pvaluesmax_normalthird <- data.frame(Sensor = sub(' ~ target', '', names(pmax_normalthird)), pvalue = pmax_normalthird)
pvaluesmax_normalthird # p-values from anova test in data frame

# android.sensor.accelerometer.mean, android.sensor.accelerometer.min, android.sensor.accelerometer.max, android.sensor.accelerometer.std, 
# android.sensor.gyroscope.mean, android.sensor.gyroscope.min, android.sensor.gyroscope.max, android.sensor.gyroscope.std, 
# android.sensor.gyroscope_uncalibrated.mean, android.sensor.gyroscope_uncalibrated.min, android.sensor.gyroscope_uncalibrated.max, android.sensor.gyroscope_uncalibrated.std, 
# android.sensor.linear_acceleration.mean,android.sensor.linear_acceleration.min, android.sensor.linear_acceleration.max, android.sensor.linear_acceleration.std, 
# speed.mean, speed.min, speed.max, have small p-vlaues 

# Z Normal third
z_normalthird$target <- factor(z_normalthird$target)
formulae <- lapply(colnames(z_normalthird)[2:ncol(z_normalthird)-1], function(x) as.formula(paste0(x, " ~ target")))
anovaz_normalthird <- lapply(formulae, function(x) summary(aov(x, data = z_normalthird)))
names(anovaz_normalthird) <- format(formulae)
anovaz_normalthird # summary of anova test for each column with target
pz_normalthird <- unlist(lapply(anovaz_normalthird, function(x) x[[1]]$"Pr(>F)"[1]))
pvaluesz_normalthird <- data.frame(Sensor = sub(' ~ target', '', names(pz_normalthird)), pvalue = pz_normalthird)
pvaluesz_normalthird # p-values from anova test in data frame

# android.sensor.accelerometer.mean, android.sensor.accelerometer.min, android.sensor.accelerometer.max, android.sensor.accelerometer.std, 
# android.sensor.gyroscope.mean, android.sensor.gyroscope.min, android.sensor.gyroscope.max, android.sensor.gyroscope.std, 
# android.sensor.gyroscope_uncalibrated.mean, android.sensor.gyroscope_uncalibrated.min, android.sensor.gyroscope_uncalibrated.max, android.sensor.gyroscope_uncalibrated.std, 
# android.sensor.linear_acceleration.mean, android.sensor.linear_acceleration.min, android.sensor.linear_acceleration.max, android.sensor.linear_acceleration.std, 
# speed.mean, speed.min, speed.max have small p-vlaues 


# Support Vector Machine

# Week 5-7 for reference
# Lectures 4, 5 and 6
# R codes Week 5 Part 2 and Week 7
# Labs 5 and 6
# Homework 3 

# kernel types polynomial, radial (data set is nonlinear)

library(e1071)


# BEST ACCURACIES FROM ALL TESTING DONE FROM BELOW 

# All features for z normal data 
z_normaltrain$target <- as.factor(z_normaltrain$target)
svm3 <- svm(target~., data=z_normaltrain, method="C-classification", scale = FALSE, kernal="radial", cost=5)
summary(svm3) # summary of svm
svm3$SV # observation index and coefficients of the predictors for the support vectors
prediction3 <- predict(svm3, z_normaltest)
xtab3 <- table(z_normaltest$target, prediction3)
xtab3 # Better than above, less mis-classifications especially for all of the targets
(334+302+337+313+341)/nrow(z_normaltest) # 85.9% (better than compact data and max normal all features)

# All features for z normal data 
z_normaltrain$target <- as.factor(z_normaltrain$target)
svm3 <- svm(target~., data=z_normaltrain, method="C-classification", scale = FALSE, kernal="polynomial", cost=40, degree=5)
summary(svm3) # summary of svm
svm3$SV # observation index and coefficients of the predictors for the support vectors
prediction3 <- predict(svm3, z_normaltest)
xtab3 <- table(z_normaltest$target, prediction3)
xtab3 # Better than above, less mis-classifications especially for all of the targets
(336+321+371+327+341)/nrow(z_normaltest) # 89.4% (BEST SO FAR!!!)

# All features for third data 
z_normalthirdtrain$target <- as.factor(z_normalthirdtrain$target)
svm16 <- svm(target~., data=z_normalthirdtrain, method="C-classification", scale = FALSE, kernal="radial", cost=5)
summary(svm16) # summary of svm
svm16$SV # observation index and coefficients of the predictors for the support vectors
prediction16 <- predict(svm16, z_normalthirdtest)
xtab16 <- table(z_normalthirdtest$target, prediction16)
xtab16 # not that many are mis classified  
(321+329+336+321+344)/nrow(z_normalthirdtest) # 87.2% (pretty good)

# All features for third data 
z_normalthirdtrain$target <- as.factor(z_normalthirdtrain$target)
svm16 <- svm(target~., data=z_normalthirdtrain, method="C-classification", scale = FALSE, kernal="polynomial", cost=40, degree=5)
summary(svm16) # summary of svm
svm16$SV # observation index and coefficients of the predictors for the support vectors
prediction16 <- predict(svm16, z_normalthirdtest)
xtab16 <- table(z_normalthirdtest$target, prediction16)
xtab16 # not that many are mis classified  
(346+340+367+326+343)/nrow(z_normalthirdtest) # 90.9% (BEST OVERALL!!)




# ALL TESTING I'VE DONE

# Compact Data 

# Radial Kernel 

# All features for compact data 
compact_datatrain$target <- as.factor(compact_datatrain$target)
svm1 <- svm(target~., data=compact_datatrain, method="C-classification", scale = FALSE, kernal="radial", cost=5)
summary(svm1) # summary of svm
svm1$SV # observation index and coefficients of the predictors for the support vectors
prediction1 <- predict(svm1, compact_datatest)
xtab1 <- table(compact_datatest$target, prediction1)
xtab1 # Bad for walking a lot of mis-classifications 
(244+171+237+230+342)/nrow(compact_datatest) # 64.6% (pretty bad)

# High Positive Correlation for compact data 
svm4 <- svm(target~android.sensor.accelerometer.mean + android.sensor.gyroscope.mean + android.sensor.gyroscope_uncalibrated.mean + android.sensor.linear_acceleration.mean, data=compact_datatrain, method="C-classification", scale = FALSE, kernal="radial", cost=5)
summary(svm4) # summary of svm
svm4$SV # observation index and coefficients of the predictors for the support vectors
prediction4 <- predict(svm4, compact_datatest)
xtab4 <- table(compact_datatest$target, prediction4)
xtab4 # Good for walking, but a lot of mis-classifications for the others
(182+116+353+190+295)/nrow(compact_datatest) # 60.0% (worse than all features for compact data)

# Based on boxplot differences and anova for compact data 
svm7 <- svm(target~android.sensor.accelerometer.mean+android.sensor.gyroscope.mean+android.sensor.gyroscope_uncalibrated.mean+android.sensor.linear_acceleration.mean+speed.mean, data=compact_datatrain, method="C-classification", scale = FALSE, kernal="radial", cost=5)
summary(svm7) # summary of svm
svm7$SV # observation index and coefficients of the predictors for the support vectors
prediction7 <- predict(svm7, compact_datatest)
xtab7 <- table(compact_datatest$target, prediction7)
xtab7 # Good for walking, but a lot of mis-classifications for the others
(202+235+360+213+300)/nrow(compact_datatest) # 69.2% (better than all featrues and high positive coefs)

# Ploynomial Kernel 

# All features for compact data 
compact_datatrain$target <- as.factor(compact_datatrain$target)
svm1 <- svm(target~., data=compact_datatrain, method="C-classification", scale = FALSE, kernal="polynomial", cost=40, degree=5)
summary(svm1) # summary of svm
svm1$SV # observation index and coefficients of the predictors for the support vectors
prediction1 <- predict(svm1, compact_datatest)
xtab1 <- table(compact_datatest$target, prediction1)
xtab1 # Bad for walking a lot of mis-classifications
(243+173+237+230+342)/nrow(compact_datatest) # 64.7% (a little better than radial)

# High Positive Correlation for compact data 
svm4 <- svm(target~android.sensor.accelerometer.mean + android.sensor.gyroscope.mean + android.sensor.gyroscope_uncalibrated.mean + android.sensor.linear_acceleration.mean, data=compact_datatrain, method="C-classification", scale = FALSE, kernal="polynomial", cost=40, degree=5)
summary(svm4) # summary of svm
svm4$SV # observation index and coefficients of the predictors for the support vectors
prediction4 <- predict(svm4, compact_datatest)
xtab4 <- table(compact_datatest$target, prediction4)
xtab4 # Good for walking, but a lot of mis-classifications for the others
(187+121+330+227+298)/nrow(compact_datatest) # 61.4% (a little better than radial)

# Based on boxplot differences and anova for compact data 
svm7 <- svm(target~android.sensor.accelerometer.mean+android.sensor.gyroscope.mean+android.sensor.gyroscope_uncalibrated.mean+android.sensor.linear_acceleration.mean+speed.mean, data=compact_datatrain, method="C-classification", scale = FALSE, kernal="polynomial", cost=40, degree=5)
summary(svm7) # summary of svm
svm7$SV # observation index and coefficients of the predictors for the support vectors
prediction7 <- predict(svm7, compact_datatest)
xtab7 <- table(compact_datatest$target, prediction7)
xtab7 # Good for walking, but a lot of mis-classifications for the others 
(219+230+366+257+304)/nrow(compact_datatest) # 72.7% (a little better than radial)


# Max Normal Data 

# Radial Kernel 

# All features for max normal data 
max_normaltrain$target <- as.factor(max_normaltrain$target)
svm2 <- svm(target~., data=max_normaltrain, method="C-classification", scale = FALSE, kernal="radial", cost=5)
summary(svm2) # summary of svm
svm2$SV # observation index and coefficients of the predictors for the support vectors
prediction2 <- predict(svm2, max_normaltest)
xtab2 <- table(max_normaltest$target, prediction2)
xtab2 # Better than above, less mis-classifications especially for walking 
(272+283+255+240+321)/nrow(max_normaltest) # 72.4% (better that compact data all features)

# High Positive Correlation for max normal data 
svm5 <- svm(target~android.sensor.accelerometer.mean + android.sensor.gyroscope.mean + android.sensor.gyroscope_uncalibrated.mean + android.sensor.linear_acceleration.mean, data=max_normaltrain, method="C-classification", scale = FALSE, kernal="radial", cost=5)
summary(svm5) # summary of svm
svm5$SV # observation index and coefficients of the predictors for the support vectors
prediction5 <- predict(svm5, max_normaltest)
xtab5 <- table(max_normaltest$target, prediction5)
xtab5 # Many misclassified for train 
(137+49+2+335+286)/nrow(max_normaltest) # 42.7% (bad, worse than all features (worst of all so far))

# Based on boxplot differences and anova for max normal data 
svm8 <- svm(target~android.sensor.accelerometer.mean+android.sensor.gyroscope.mean+android.sensor.gyroscope_uncalibrated.mean+android.sensor.linear_acceleration.mean+speed.mean, data=max_normaltrain, method="C-classification", scale = FALSE, kernal="radial", cost=5)
summary(svm8) # summary of svm
svm8$SV # observation index and coefficients of the predictors for the support vectors
prediction8 <- predict(svm8, max_normaltest)
xtab8 <- table(max_normaltest$target, prediction8)
xtab8 # Many misclassified
(168+230+382+42+298)/nrow(max_normaltest) # 59.2% (pretty bad but better than high positive coefs)

# Polynomial Kernel 

# All features for max normal data 
max_normaltrain$target <- as.factor(max_normaltrain$target)
svm2 <- svm(target~., data=max_normaltrain, method="C-classification", scale = FALSE, kernal="polynomial", cost=40, degree=5)
summary(svm2) # summary of svm
svm2$SV # observation index and coefficients of the predictors for the support vectors
prediction2 <- predict(svm2, max_normaltest)
xtab2 <- table(max_normaltest$target, prediction2)
xtab2 # Many misclassified 
(283+282+290+269+326)/nrow(max_normaltest) # 76.6% (better than radial)

# High Positive Correlation for max normal data 
svm5 <- svm(target~android.sensor.accelerometer.mean + android.sensor.gyroscope.mean + android.sensor.gyroscope_uncalibrated.mean + android.sensor.linear_acceleration.mean, data=max_normaltrain, method="C-classification", scale = FALSE, kernal="polynomial", cost=40, degree=5)
summary(svm5) # summary of svm
svm5$SV # observation index and coefficients of the predictors for the support vectors
prediction5 <- predict(svm5, max_normaltest)
xtab5 <- table(max_normaltest$target, prediction5)
xtab5 # Many misclassified  
(151+127+373+91+292)/nrow(max_normaltest) # 54.6% (bad, worse than all features (worst of all so far))

# Based on boxplot differences and anova for max normal data 
svm8 <- svm(target~android.sensor.accelerometer.mean+android.sensor.gyroscope.mean+android.sensor.gyroscope_uncalibrated.mean+android.sensor.linear_acceleration.mean+speed.mean, data=max_normaltrain, method="C-classification", scale = FALSE, kernal="polynomial", cost=40, degree=5)
summary(svm8) # summary of svm
svm8$SV # observation index and coefficients of the predictors for the support vectors
prediction8 <- predict(svm8, max_normaltest)
xtab8 <- table(max_normaltest$target, prediction8)
xtab8 # Many misclassified
(168+230+382+42+298)/nrow(max_normaltest) # 59.2% (pretty bad but better than high positive coefs)


# Z Normal Data 

# Radial Kernel 

# All features for max normal data 
z_normaltrain$target <- as.factor(z_normaltrain$target)
svm3 <- svm(target~., data=z_normaltrain, method="C-classification", scale = FALSE, kernal="radial", cost=5)
summary(svm3) # summary of svm
svm3$SV # observation index and coefficients of the predictors for the support vectors
prediction3 <- predict(svm3, z_normaltest)
xtab3 <- table(z_normaltest$target, prediction3)
xtab3 # Better than above, less mis-classifications especially for all of the targets
(334+302+337+313+341)/nrow(z_normaltest) # 85.9% (better than compact data and max normal all features)

# High Positive Correlation for z normal data 
svm6 <- svm(target~android.sensor.accelerometer.mean + android.sensor.gyroscope.mean + android.sensor.gyroscope_uncalibrated.mean + android.sensor.linear_acceleration.mean, data=z_normaltrain, method="C-classification", scale = FALSE, kernal="radial", cost=5)
summary(svm6) # summary of svm
svm6$SV # observation index and coefficients of the predictors for the support vectors
prediction6 <- predict(svm6, z_normaltest)
xtab6 <- table(z_normaltest$target, prediction6)
xtab6 # Not better than above, but still less mis-classifications especially for all of the targets
(208+134+334+189+305)/nrow(z_normaltest) # 61.8% (worse than all features)

# Based on boxplot differences and anova for z normal data 
svm9 <- svm(target~android.sensor.accelerometer.mean+android.sensor.gyroscope.mean+android.sensor.gyroscope_uncalibrated.mean+android.sensor.linear_acceleration.mean+speed.mean, data=z_normaltrain, method="C-classification", scale = FALSE, kernal="radial", cost=5)
summary(svm9) # summary of svm
svm9$SV # observation index and coefficients of the predictors for the support vectors
prediction9 <- predict(svm9, z_normaltest)
xtab9 <- table(z_normaltest$target, prediction9)
xtab9 # Not better than above, but less mis-classifications especially for all of the targets
(222+228+347+217+312)/nrow(z_normaltest) # 70.0% (worse than all features but better than coefs)

# Polynomial Kernel 

# All features for max normal data 
z_normaltrain$target <- as.factor(z_normaltrain$target)
svm3 <- svm(target~., data=z_normaltrain, method="C-classification", scale = FALSE, kernal="polynomial", cost=40, degree=5)
summary(svm3) # summary of svm
svm3$SV # observation index and coefficients of the predictors for the support vectors
prediction3 <- predict(svm3, z_normaltest)
xtab3 <- table(z_normaltest$target, prediction3)
xtab3 # Better than above, less mis-classifications especially for all of the targets
(336+321+371+327+341)/nrow(z_normaltest) # 89.4% (BEST SO FAR!!!)

# High Positive Correlation for z normal data 
svm6 <- svm(target~android.sensor.accelerometer.mean + android.sensor.gyroscope.mean + android.sensor.gyroscope_uncalibrated.mean + android.sensor.linear_acceleration.mean, data=z_normaltrain, method="C-classification", scale = FALSE, kernal="polynomial", cost=40, degree=5)
summary(svm6) # summary of svm
svm6$SV # observation index and coefficients of the predictors for the support vectors
prediction6 <- predict(svm6, z_normaltest)
xtab6 <- table(z_normaltest$target, prediction6)
xtab6 # Not better than above, but still less mis-classifications especially for all of the targets
(212+129+329+206+303)/nrow(z_normaltest) # 62.3% (better than radial)

# Based on boxplot differences and anova for z normal data 
svm9 <- svm(target~android.sensor.accelerometer.mean+android.sensor.gyroscope.mean+android.sensor.gyroscope_uncalibrated.mean+android.sensor.linear_acceleration.mean+speed.mean, data=z_normaltrain, method="C-classification", scale = FALSE, kernal="polynomial", cost=40, degree=5)
summary(svm9) # summary of svm
svm9$SV # observation index and coefficients of the predictors for the support vectors
prediction9 <- predict(svm9, z_normaltest)
xtab9 <- table(z_normaltest$target, prediction9)
xtab9 # Not better than above, but less mis-classifications especially for all of the targets
(234+230+336+247+310)/nrow(z_normaltest) # 71.7% (better than radial)


# Third data 

# Radial Kernel 

# All features for third data 
third_datatrain$target <- as.factor(third_datatrain$target)
svm10 <- svm(target~., data=third_datatrain, method="C-classification", scale = FALSE, kernal="radial", cost=5)
summary(svm10) # summary of svm
svm10$SV # observation index and coefficients of the predictors for the support vectors
prediction10 <- predict(svm10, thirdtest)
xtab10 <- table(thirdtest$target, prediction10)
xtab10 # Bad for walking a lot of mis-classifications 
(238+173+281+246+360)/nrow(thirdtest) # 68.6% (a little bad)

# High positive correlation for third data set  
svm11 <- svm(target~android.sensor.accelerometer.mean+android.sensor.accelerometer.max+android.sensor.accelerometer.std+android.sensor.gyroscope.mean+android.sensor.gyroscope.min+android.sensor.gyroscope.max+android.sensor.gyroscope.std+android.sensor.gyroscope_uncalibrated.mean+android.sensor.gyroscope_uncalibrated.min+android.sensor.gyroscope_uncalibrated.max+android.sensor.gyroscope_uncalibrated.std+android.sensor.linear_acceleration.mean+android.sensor.linear_acceleration.min+android.sensor.linear_acceleration.max+android.sensor.linear_acceleration.std, data=third_datatrain, method="C-classification", scale = FALSE, kernal="radial", cost=5)
summary(svm11) # summary of svm
svm11$SV # observation index and coefficients of the predictors for the support vectors
prediction11 <- predict(svm11, thirdtest)
xtab11 <- table(thirdtest$target, prediction11)
xtab11 # many misclassified throughout 
(197+188+357+206+298)/nrow(thirdtest) # 65.8% (a little worse than above with all features for third data set)

# Small p-vlaue for anova 
svm12 <- svm(target~android.sensor.accelerometer.mean+android.sensor.accelerometer.min+android.sensor.accelerometer.max+android.sensor.accelerometer.std+android.sensor.gyroscope.mean+android.sensor.gyroscope.min+android.sensor.gyroscope.max+android.sensor.gyroscope.std+android.sensor.gyroscope_uncalibrated.mean+android.sensor.gyroscope_uncalibrated.min+android.sensor.gyroscope_uncalibrated.max+android.sensor.gyroscope_uncalibrated.std+android.sensor.linear_acceleration.mean+android.sensor.linear_acceleration.min+android.sensor.linear_acceleration.max+android.sensor.linear_acceleration.std+speed.mean+speed.min+speed.max, data=third_datatrain, method="C-classification", scale = FALSE, kernal="radial", cost=5)
summary(svm12) # summary of svm
svm12$SV # observation index and coefficients of the predictors for the support vectors
prediction12 <- predict(svm12, thirdtest)
xtab12 <- table(thirdtest$target, prediction12)
xtab12 # A decent amount are misclassified for each section 
(238+258+359+252+305)/nrow(thirdtest) # 74.6% (way better than correlation and all features)

# Polynomial Kernel 

# All features for third data 
third_datatrain$target <- as.factor(third_datatrain$target)
svm10 <- svm(target~., data=third_datatrain, method="C-classification", scale = FALSE, kernal="polynomial", cost=40, degree=5)
summary(svm10) # summary of svm
svm10$SV # observation index and coefficients of the predictors for the support vectors
prediction10 <- predict(svm10, thirdtest)
xtab10 <- table(thirdtest$target, prediction10)
xtab10 # Bad for walking a lot of mis-classifications 
(238+173+281+246+360)/nrow(thirdtest) # 68.6% (same as radial)

# High positive correlation for third data set  
svm11 <- svm(target~android.sensor.accelerometer.mean+android.sensor.accelerometer.max+android.sensor.accelerometer.std+android.sensor.gyroscope.mean+android.sensor.gyroscope.min+android.sensor.gyroscope.max+android.sensor.gyroscope.std+android.sensor.gyroscope_uncalibrated.mean+android.sensor.gyroscope_uncalibrated.min+android.sensor.gyroscope_uncalibrated.max+android.sensor.gyroscope_uncalibrated.std+android.sensor.linear_acceleration.mean+android.sensor.linear_acceleration.min+android.sensor.linear_acceleration.max+android.sensor.linear_acceleration.std, data=third_datatrain, method="C-classification", scale = FALSE, kernal="polynomial", cost=40, degree=5)
summary(svm11) # summary of svm
svm11$SV # observation index and coefficients of the predictors for the support vectors
prediction11 <- predict(svm11, thirdtest)
xtab11 <- table(thirdtest$target, prediction11)
xtab11 # many misclassified throughout 
(207+216+358+216+300)/nrow(thirdtest) # 68.5% (better than radial)

# Small p-vlaue for anova 
svm12 <- svm(target~android.sensor.accelerometer.mean+android.sensor.accelerometer.min+android.sensor.accelerometer.max+android.sensor.accelerometer.std+android.sensor.gyroscope.mean+android.sensor.gyroscope.min+android.sensor.gyroscope.max+android.sensor.gyroscope.std+android.sensor.gyroscope_uncalibrated.mean+android.sensor.gyroscope_uncalibrated.min+android.sensor.gyroscope_uncalibrated.max+android.sensor.gyroscope_uncalibrated.std+android.sensor.linear_acceleration.mean+android.sensor.linear_acceleration.min+android.sensor.linear_acceleration.max+android.sensor.linear_acceleration.std+speed.mean+speed.min+speed.max, data=third_datatrain, method="C-classification", scale = FALSE, kernal="polynomial", cost=40, degree=5)
summary(svm12) # summary of svm
svm12$SV # observation index and coefficients of the predictors for the support vectors
prediction12 <- predict(svm12, thirdtest)
xtab12 <- table(thirdtest$target, prediction12)
xtab12 # A decent amount are misclassified for each section 
(249+273+370+265+305)/nrow(thirdtest) # 77.2% (better than radial)


# Max normal data (with all features)

# Radial Kernel 

# All features for third data 
max_normalthirdtrain$target <- as.factor(max_normalthirdtrain$target)
svm13 <- svm(target~., data=max_normalthirdtrain, method="C-classification", scale = FALSE, kernal="radial", cost=5)
summary(svm13) # summary of svm
svm13$SV # observation index and coefficients of the predictors for the support vectors
prediction13 <- predict(svm13, max_normalthirdtest)
xtab13 <- table(max_normalthirdtest$target, prediction13)
xtab13 # Bad for walking a lot of mis-classifications 
(271+288+284+258+305)/nrow(max_normalthirdtest) # 74.3% (a little worse than third data small p-values anova features, but better than third data high positive coefs and all feature)

# High positive correlation for third data set  
svm14 <- svm(target~android.sensor.accelerometer.mean+android.sensor.accelerometer.max+android.sensor.accelerometer.std+android.sensor.gyroscope.mean+android.sensor.gyroscope.min+android.sensor.gyroscope.max+android.sensor.gyroscope.std+android.sensor.gyroscope_uncalibrated.mean+android.sensor.gyroscope_uncalibrated.min+android.sensor.gyroscope_uncalibrated.max+android.sensor.gyroscope_uncalibrated.std+android.sensor.linear_acceleration.mean+android.sensor.linear_acceleration.min+android.sensor.linear_acceleration.max+android.sensor.linear_acceleration.std+android.sensor.orientation.std, data=max_normalthirdtrain, method="C-classification", scale = FALSE, kernal="radial", cost=5)
summary(svm14) # summary of svm
svm14$SV # observation index and coefficients of the predictors for the support vectors
prediction14 <- predict(svm14, max_normalthirdtest)
xtab14 <- table(max_normalthirdtest$target, prediction14)
xtab14 # many misclassified throughout 
(99+114+0+337+290)/nrow(max_normalthirdtest) # 44.4% (a lot worse than all features)

# Small p-vlaue for anova 
svm15 <- svm(target~android.sensor.accelerometer.mean+android.sensor.accelerometer.min+android.sensor.accelerometer.max+android.sensor.accelerometer.std+android.sensor.gyroscope.mean+android.sensor.gyroscope.min+android.sensor.gyroscope.max+android.sensor.gyroscope.std+android.sensor.gyroscope_uncalibrated.mean+android.sensor.gyroscope_uncalibrated.min+android.sensor.gyroscope_uncalibrated.max+android.sensor.gyroscope_uncalibrated.std+android.sensor.linear_acceleration.mean+android.sensor.linear_acceleration.min+android.sensor.linear_acceleration.max+android.sensor.linear_acceleration.std+speed.mean+speed.min+speed.max, data=max_normalthirdtrain, method="C-classification", scale = FALSE, kernal="radial", cost=5)
summary(svm15) # summary of svm
svm15$SV # observation index and coefficients of the predictors for the support vectors
prediction15 <- predict(svm15, max_normalthirdtest)
xtab15 <- table(max_normalthirdtest$target, prediction15)
xtab15 # A decent amount are misclassified for each section 
(214+230+379+42+301)/nrow(max_normalthirdtest) # 61.6% (a lot worse than all features but better than coefs)

# Polynomial Kernel 

# All features for third data 
max_normalthirdtrain$target <- as.factor(max_normalthirdtrain$target)
svm13 <- svm(target~., data=max_normalthirdtrain, method="C-classification", scale = FALSE, kernal="polynomial", cost=40, degree=5)
summary(svm13) # summary of svm
svm13$SV # observation index and coefficients of the predictors for the support vectors
prediction13 <- predict(svm13, max_normalthirdtest)
xtab13 <- table(max_normalthirdtest$target, prediction13)
xtab13 # many mis-classifications 
(277+304+304+277+319)/nrow(max_normalthirdtest) # 78.2% (=better than radial)

# High positive correlation for third data set  
svm14 <- svm(target~android.sensor.accelerometer.mean+android.sensor.accelerometer.max+android.sensor.accelerometer.std+android.sensor.gyroscope.mean+android.sensor.gyroscope.min+android.sensor.gyroscope.max+android.sensor.gyroscope.std+android.sensor.gyroscope_uncalibrated.mean+android.sensor.gyroscope_uncalibrated.min+android.sensor.gyroscope_uncalibrated.max+android.sensor.gyroscope_uncalibrated.std+android.sensor.linear_acceleration.mean+android.sensor.linear_acceleration.min+android.sensor.linear_acceleration.max+android.sensor.linear_acceleration.std+android.sensor.orientation.std, data=max_normalthirdtrain, method="C-classification", scale = FALSE, kernal="polynomial", cost=40, degree=5)
summary(svm14) # summary of svm
svm14$SV # observation index and coefficients of the predictors for the support vectors
prediction14 <- predict(svm14, max_normalthirdtest)
xtab14 <- table(max_normalthirdtest$target, prediction14)
xtab14 # many misclassified throughout 
(158+194+381+118+294)/nrow(max_normalthirdtest) # 60.5% (better than radial)

# Small p-vlaue for anova 
svm15 <- svm(target~android.sensor.accelerometer.mean+android.sensor.accelerometer.min+android.sensor.accelerometer.max+android.sensor.accelerometer.std+android.sensor.gyroscope.mean+android.sensor.gyroscope.min+android.sensor.gyroscope.max+android.sensor.gyroscope.std+android.sensor.gyroscope_uncalibrated.mean+android.sensor.gyroscope_uncalibrated.min+android.sensor.gyroscope_uncalibrated.max+android.sensor.gyroscope_uncalibrated.std+android.sensor.linear_acceleration.mean+android.sensor.linear_acceleration.min+android.sensor.linear_acceleration.max+android.sensor.linear_acceleration.std+speed.mean+speed.min+speed.max, data=max_normalthirdtrain, method="C-classification", scale = FALSE, kernal="polynomial", cost=40, degree=5)
summary(svm15) # summary of svm
svm15$SV # observation index and coefficients of the predictors for the support vectors
prediction15 <- predict(svm15, max_normalthirdtest)
xtab15 <- table(max_normalthirdtest$target, prediction15)
xtab15 # A decent amount are misclassified for each section 
(235+241+378+83+309)/nrow(max_normalthirdtest) # 65.8% (better than radial)


# Z normal data (with all features)

# Radial Kernel 

# All features for third data 
z_normalthirdtrain$target <- as.factor(z_normalthirdtrain$target)
svm16 <- svm(target~., data=z_normalthirdtrain, method="C-classification", scale = FALSE, kernal="radial", cost=5)
summary(svm16) # summary of svm
svm16$SV # observation index and coefficients of the predictors for the support vectors
prediction16 <- predict(svm16, z_normalthirdtest)
xtab16 <- table(z_normalthirdtest$target, prediction16)
xtab16 # not that many are mis classified  
(321+329+336+321+344)/nrow(z_normalthirdtest) # 87.2% (pretty good)

# High positive correlation for third data set  
svm17 <- svm(target~android.sensor.accelerometer.mean+android.sensor.accelerometer.max+android.sensor.accelerometer.std+android.sensor.gyroscope.mean+android.sensor.gyroscope.min+android.sensor.gyroscope.max+android.sensor.gyroscope.std+android.sensor.gyroscope_uncalibrated.mean+android.sensor.gyroscope_uncalibrated.min+android.sensor.gyroscope_uncalibrated.max+android.sensor.gyroscope_uncalibrated.std+android.sensor.linear_acceleration.mean+android.sensor.linear_acceleration.min+android.sensor.linear_acceleration.max+android.sensor.linear_acceleration.std+android.sensor.orientation.std, data=z_normalthirdtrain, method="C-classification", scale = FALSE, kernal="radial", cost=5)
summary(svm17) # summary of svm
svm17$SV # observation index and coefficients of the predictors for the support vectors
prediction17 <- predict(svm17, z_normalthirdtest)
xtab17 <- table(z_normalthirdtest$target, prediction17)
xtab17 # many misclassified throughout 
(206+219+369+176+313)/nrow(z_normalthirdtest) # 67.8% (a lot worse than all features)

# Small p-vlaue for anova 
svm18 <- svm(target~android.sensor.accelerometer.mean+android.sensor.accelerometer.min+android.sensor.accelerometer.max+android.sensor.accelerometer.std+android.sensor.gyroscope.mean+android.sensor.gyroscope.min+android.sensor.gyroscope.max+android.sensor.gyroscope.std+android.sensor.gyroscope_uncalibrated.mean+android.sensor.gyroscope_uncalibrated.min+android.sensor.gyroscope_uncalibrated.max+android.sensor.gyroscope_uncalibrated.std+android.sensor.linear_acceleration.mean+android.sensor.linear_acceleration.min+android.sensor.linear_acceleration.max+android.sensor.linear_acceleration.std+speed.mean+speed.min+speed.max, data=z_normalthirdtrain, method="C-classification", scale = FALSE, kernal="radial", cost=5)
summary(svm18) # summary of svm
svm18$SV # observation index and coefficients of the predictors for the support vectors
prediction18 <- predict(svm18, z_normalthirdtest)
xtab18 <- table(z_normalthirdtest$target, prediction18)
xtab18 # A decent amount are misclassified for each section 
(243+268+370+214+314)/nrow(z_normalthirdtest) # 74.4% (worse than all features, better than strong positive coefs)

# Polynomial Kernel 

# All features for third data 
z_normalthirdtrain$target <- as.factor(z_normalthirdtrain$target)
svm16 <- svm(target~., data=z_normalthirdtrain, method="C-classification", scale = FALSE, kernal="polynomial", cost=40, degree=5)
summary(svm16) # summary of svm
svm16$SV # observation index and coefficients of the predictors for the support vectors
prediction16 <- predict(svm16, z_normalthirdtest)
xtab16 <- table(z_normalthirdtest$target, prediction16)
xtab16 # not that many are mis classified  
(346+340+367+326+343)/nrow(z_normalthirdtest) # 90.9% (VERY GOOD!!)

# High positive correlation for third data set  
svm17 <- svm(target~android.sensor.accelerometer.mean+android.sensor.accelerometer.max+android.sensor.accelerometer.std+android.sensor.gyroscope.mean+android.sensor.gyroscope.min+android.sensor.gyroscope.max+android.sensor.gyroscope.std+android.sensor.gyroscope_uncalibrated.mean+android.sensor.gyroscope_uncalibrated.min+android.sensor.gyroscope_uncalibrated.max+android.sensor.gyroscope_uncalibrated.std+android.sensor.linear_acceleration.mean+android.sensor.linear_acceleration.min+android.sensor.linear_acceleration.max+android.sensor.linear_acceleration.std+android.sensor.orientation.std, data=z_normalthirdtrain, method="C-classification", scale = FALSE, kernal="polynomial", cost=40, degree=5)
summary(svm17) # summary of svm
svm17$SV # observation index and coefficients of the predictors for the support vectors
prediction17 <- predict(svm17, z_normalthirdtest)
xtab17 <- table(z_normalthirdtest$target, prediction17)
xtab17 # many misclassified throughout 
(214+227+366+202+312)/nrow(z_normalthirdtest) # 66.7% (better than radial)

# Small p-vlaue for anova 
svm18 <- svm(target~android.sensor.accelerometer.mean+android.sensor.accelerometer.min+android.sensor.accelerometer.max+android.sensor.accelerometer.std+android.sensor.gyroscope.mean+android.sensor.gyroscope.min+android.sensor.gyroscope.max+android.sensor.gyroscope.std+android.sensor.gyroscope_uncalibrated.mean+android.sensor.gyroscope_uncalibrated.min+android.sensor.gyroscope_uncalibrated.max+android.sensor.gyroscope_uncalibrated.std+android.sensor.linear_acceleration.mean+android.sensor.linear_acceleration.min+android.sensor.linear_acceleration.max+android.sensor.linear_acceleration.std+speed.mean+speed.min+speed.max, data=z_normalthirdtrain, method="C-classification", scale = FALSE, kernal="polynomial", cost=40, degree=5)
summary(svm18) # summary of svm
svm18$SV # observation index and coefficients of the predictors for the support vectors
prediction18 <- predict(svm18, z_normalthirdtest)
xtab18 <- table(z_normalthirdtest$target, prediction18)
xtab18 # A decent amount are misclassified for each section 
(237+279+372+253+310)/nrow(z_normalthirdtest) # 76.6% (better than radial)
