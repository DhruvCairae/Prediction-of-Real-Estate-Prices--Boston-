
library(MASS)
data(Boston)
attach(Boston)

library(tidyverse)
library(skimr)
library(glmnet)
library(ggplot2)
library(plotmo)
library(ROCR)
library(boot)
library(rpart)
library(rpart.plot)
library(caret)


library(caTools)
set.seed(14004918)
split = sample.split(Boston$medv, SplitRatio = 0.8)
training_set = subset(Boston, split == TRUE)
test_set = subset(Boston, split == FALSE)


# Full Model
Boston.full.lm <- lm(medv ~ ., data = training_set)
summary(Boston.full.lm)

# Null Model
Boston.null.lm <- lm(medv ~ 1, data = training_set)
summary(Boston.null.lm)

### best subset ###

library(leaps)
subset_result <- regsubsets(medv~.,data=training_set, nbest=1, nvmax = 14)
summary(subset_result)
plot(subset_result, scale="bic")

best_subset<- lm(medv ~ crim+zn+nox+rm+dis+rad+tax+ptratio+black+lstat, data = training_set)
summary(best_subset)


### stepwise variable selection ###
Boston.AIC.step <- step(Boston.full.lm,data=training_set) #k=2, default AIC
summary(Boston.AIC.step)
AIC(Boston.AIC.step)
BIC(Boston.AIC.step)

Boston.BIC.step <- step(Boston.full.lm,data=training_set,k=log(nrow(training_set))) #k=ln(n), BIC
summary(Boston.BIC.step)
AIC(Boston.BIC.step)
BIC(Boston.BIC.step)

install.packages('glmnet')
library(glmnet)
library(plotmo)


cv_lasso_fit <- cv.glmnet(x = as.matrix(training_set[, -c(which(colnames(training_set)=='medv'))]),
                          y = training_set$medv, alpha = 1, nfolds = 5)
cv_lasso_fit$lambda.min
cv_lasso_fit$lambda.1se

lasso_fit <- glmnet(x = as.matrix(training_set[, -c(which(colnames(training_set)=='medv'))]), y = training_set$medv, alpha = 1)
coef(lasso_fit,s=cv_lasso_fit$lambda.min)
coef(lasso_fit,s=cv_lasso_fit$lambda.1se)

lasso_model_min<- lm(medv ~ crim+zn+chas+nox+rm+dis+rad+tax+ptratio+black+lstat, data = training_set)
summary(lasso_model_min)

lasso_model_1se<- lm(medv ~ crim+chas+nox+rm+dis+ptratio+black+lstat, data = training_set)
summary(lasso_model_1se)

plot(lasso_fit, xvar = "lambda", label = TRUE)
plot_glmnet(lasso_fit, label=TRUE)  
plot_glmnet(lasso_fit, label=8, xvar ="norm")   
plot(cv_lasso_fit)


boston_test_pred_reg = predict(Boston.AIC.step, test_set)
mean((boston_test_pred_reg - test_set$medv)^2)

glm1<- glm(medv~. , family='gaussian', data=Boston)
cv_result  <- cv.glm(data=Boston, glmfit=glm1, K=5) 
cv_result$delta


boston_rpart <- rpart(formula = medv ~ ., data = training_set)
prp(boston_rpart,digits = 4, extra = 1,main="Boston Housing Regression Tree")


boston_largetree <- rpart(formula = medv ~ ., data = training_set, cp = 0.001)
plotcp(boston_largetree)

prune(boston_largetree, cp = 0.008)
prp(boston_largetree,digits = 4, extra = 1,main="Boston Housing Regression Tree")


boston_train_pred_tree = predict(boston_rpart)
boston_test_pred_tree = predict(boston_rpart,test_set)
MSE<-mean((boston_train_pred_tree - training_set$medv)^2)
MSE
MSPE<-mean((boston_test_pred_tree - test_set$medv)^2)
MSPE

boston_full_pred_reg = predict(Boston.AIC.step, Boston)
mean((boston_full_pred_reg - Boston$medv)^2)
boston_full_pred_tree = predict(boston_rpart,Boston)
mean((boston_full_pred_tree - Boston$medv)^2)




