library(MASS)
library(leaps)

# upload data that is available online
data(Boston,package="MASS")
data<-Boston
# take a look at the data
head(data)

# calculate 50% of the sample size for training
smp_size <- floor(0.5 * nrow(data))
# set the seed to make your partition reproductible
set.seed(12)
# randomly pick 50% of the sample size for training 
train_ind <- sample(seq_len(nrow(data)), size = smp_size)
# save training/testing data
train <- data[train_ind, ]
test <- data[-train_ind, ]

# pick training data and look at it more closely
attach(train)
summary(train)
head(train)

# Use Forward/Backward Selection for preliminary filtering
Null_Model<-lm(medv~1, data=data.frame(train[,1:13]))
Full_Model<-lm(medv~., data=data.frame(train[,1:13]))
step(Null_Model, scope=list(lower=Null_Model, upper=Full_Model), direction="forward", k=log(54))
#lm(formula = medv ~ lstat + rm + ptratio + dis + nox + rad + crim + chas, data = data.frame(train[, 1:13]))

# Use Forward/Backward Selection for preliminary filtering
Full_Model<-lm(medv~., data=data.frame(train[,1:13]))
step(Full_Model, direction="backward", k=log(54))
#lm(formula = medv ~ crim + zn + chas + nox + rm + dis + rad + tax + ptratio + lstat, data = data.frame(train[, 1:13]))


# according to backward + zn, tax
# according to forward feature selection
# drop zn, indus, age, tax, black 
model<-lm(medv ~ lstat + ptratio + dis + nox + rad + crim + chas)
summary(model)
# Multiple R-squared:  0.6884
# According to the test errors are not normally distributed, p-value = 6.208e-09
shapiro.test(model$residuals)
# need to achieve p-value>0.05 for the test while keeping R^2 high enough

# Feature selection using different criteria such as R^2, adjusted R^2 and Cp 
leaps<-regsubsets(medv ~ lstat + rm + ptratio + dis + nox + rad + crim + chas, data=data.frame(train), nbest=4)
plot(leaps, scale="adjr2")
plot(leaps, scale="r2")
plot(leaps, scale="Cp")
# No need to drop any of the features according to the criterias above

# Looking at the residuals
hist(residuals(model))
qqnorm(residuals(model))
# Residuals are not normally distributed, checking for response transformation
boxcox(model)
# Since alpha is near 0 natural logarithmic transformation may be helpful

# log(medv) p-value = 5.665e-06
model<-lm(log(medv) ~ lstat + rm + ptratio + dis + nox + rad + crim + chas)
summary(model)
# Multiple R-squared:  0.7842 has increased after transformation
shapiro.test(model$residuals)
# p-value = 5.665e-06 also increased significantly

# examining rm and it's relationship with the response
hist(rm)
plot(rm, medv)

# checking conditions if we exclude rm 
model<-lm(log(medv) ~ lstat + ptratio + dis + nox + rad + crim + chas)
summary(model)
#Multiple R-squared:  0.771 one percent decrease seems to be not significant
shapiro.test(model$residuals)
# p-value = 0.01026 increased significantly


# looking at ptratio, need for transformation and it's relationship with the response
hist(ptratio)
plot(ptratio, log(medv))
plot(log(ptratio), log(medv))

# looking at dis, need for transformation and it's relationship with the response
hist(dis)
plot(dis, log(medv))
plot(log(dis), log(medv))
# we may need to apply log(dis) transformation to strengthen linear relationship between it and the response

# applying log(dis) 
model<-lm(log(medv) ~ lstat+ ptratio + log(dis) + nox + rad + crim + chas)
summary(model)
# Multiple R-squared:  0.782 has increased a bit
shapiro.test(model$residuals)
# p-value = 0.05398 increased considerably


# looking at rad, need for transformation and it's relationship with the response
hist(rad)
plot(rad, log(medv))
plot(log(rad), log(medv))
# we may need to apply log(dis) transformation to strengthen linear relationship between it and the response

# applying log(rad)
model<-lm(log(medv) ~ lstat+ ptratio + log(dis) + nox + log(rad) + crim + chas)
summary(model)
# Multiple R-squared:  0.7832 has increased a little bit
shapiro.test(model$residuals)
# normality p-value = 0.1396 also increased considerably
# We may end feature selection since both of the main criterias: high R^2 and normality p-value are satisfied

# Now let's check generalization performance of our model by matrix approach
# first load the predictors and response data into X and Y respectively
X<-matrix(c(rep(1,253), lstat, ptratio, log(dis), nox, log(rad), crim, chas), ncol=8, byrow=FALSE)
Y<-log(medv)
# then calculate and look at the weights of the model
b<-solve(t(X)%*%X)%*%t(X)%*%Y
print(b)
# then calculate hat matrix and residuals by using transformation to get original scale
H<-X%*%solve(t(X)%*%X)%*%t(X)
e<-exp(Y)-exp(H%*%Y)
# finally calculate Mean Squared Error=22.38
SSE=t(e)%*%e
MSE<-SSE/244
MSE

# Now apply obtained model on the test data
attach(test)
# initialize x and y as predictors and response
x<-matrix(c(rep(1,253), lstat, ptratio, log(dis), nox, log(rad), crim, chas), ncol=8, byrow=FALSE)
y<-log(medv)
# calculate transformed residuals and Mean Squared Prediction Error=24.18
res<-exp(y)-exp(x%*%b)
SSPR=t(res)%*%res
MSPR<-SSPR/245
MSPR
# Now check generalization performance
MSPR/MSE-1
# 8 percent difference in errors shows that generalization performance of our model is very good
