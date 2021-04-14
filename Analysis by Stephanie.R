# Testing feature variables 

setwd("C:/Users/Stephanie/Documents/STAT385/Final Project/Stat-385-final-project")

first <- read.csv("Dfirst.csv") # Dfirst 
second <- read.csv("Dsecond.csv") # Dsecond 
third <- read.csv("Dthird.csv") # Dthird 

head(first)
levels(as.factor(first$target))

# compact data set by Albert 
second1 <- second[,which(colnames(second)%in%colnames(first)==FALSE)]
third1 <- third[,which(colnames(third)%in%colnames(first)==FALSE&colnames(third)%in%colnames(second1)==FALSE)]
third1 <- third1[,which(colnames(third1)%in%colnames(second1)==FALSE)]
data = cbind(first,second1,third1)
compact_data <- cbind(first[,c(1,2,6,10,14)],second1[,c(1,5,9,13,17)])
compact_data$speed.mean <- third1[,1]
#data = cbind(first[,c(1,2,6,10)],second[,c(6,10)])

head(compact_data)
# target column moved to end of data frame 
compact_data <- subset(compact_data, select=c(time:sound.mean, android.sensor.game_rotation_vector.mean:speed.mean, target))


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

# There is a high positive correlation between two variables when the correlation between them is closer to 1
# A value closer to 0 suggests a weak relationship between the variables
# A low correlation (-0.2 < x < 0.2) probably suggests that much of variation of the response variable (Y) is unexplained 
# by the predictor (X), in which case, we should probably look for better explanatory variables.

# gyroscope_uncalibrated.mean, gyroscope_uncalibrated.min, gyroscope_uncalibrated.max, gyroscope_uncalibrated.std, gyroscope.mean, 
# gyroscope.min, gyroscope.max, gyroscope.std, linear_acceleration.mean, linear_acceleration.min, linear_acceleration.max, linear_acceleration.std,
# and accelerometer.mean, accelerometer.min, accelerometer.max, accelerometer.std, android.sensor.orientation.std have strong positive/negative correlation

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

# time, sound, game.rotation.vector, orientation and rotation.vector seem different 


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

# Dsecond 
second$target <- factor(second$target)
formulae <- lapply(colnames(second)[2:ncol(second)-1], function(x) as.formula(paste0(x, " ~ target")))
anovasecond <- lapply(formulae, function(x) summary(aov(x, data = second)))
names(anovasecond) <- format(formulae)
anovasecond # summary of anova test for each column with target
psecond <- unlist(lapply(anovasecond, function(x) x[[1]]$"Pr(>F)"[1]))
pvaluessecond <- data.frame(Sensor = sub(' ~ target', '', names(psecond)), pvalue = psecond)
pvaluessecond # p-values from anova test in data frame

# Dthird 
third$target <- factor(third$target)
formulae <- lapply(colnames(third)[2:ncol(third)-1], function(x) as.formula(paste0(x, " ~ target")))
anovathird <- lapply(formulae, function(x) summary(aov(x, data = third)))
names(anovathird) <- format(formulae)
anovathird # summary of anova test for each column with target
pthird <- unlist(lapply(anovathird, function(x) x[[1]]$"Pr(>F)"[1]))
pvaluesthird <- data.frame(Sensor = sub(' ~ target', '', names(pthird)), pvalue = pthird)
pvaluesthird # p-values from anova test in data frame

# Compact data third$target <- factor(third$target)
compact_data$target <- factor(compact_data$target)
formulae <- lapply(colnames(compact_data)[2:ncol(compact_data)-1], function(x) as.formula(paste0(x, " ~ target")))
anovacompact_data <- lapply(formulae, function(x) summary(aov(x, data = compact_data)))
names(anovacompact_data) <- format(formulae)
anovacompact_data # summary of anova test for each column with target
pcompact_data <- unlist(lapply(anovacompact_data, function(x) x[[1]]$"Pr(>F)"[1]))
pvaluescompact_data <- data.frame(Sensor = sub(' ~ target', '', names(pcompact_data)), pvalue = pcompact_data)
pvaluescompact_data # p-values from anova test in data frame
