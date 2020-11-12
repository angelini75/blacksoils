# modelling
# 
library(tidyverse)
library(caret)
library(randomForest)


data <- read_csv("data/soil_&_covariates.csv") %>% 
  na.omit() %>% 
  mutate(z = as.factor(z)) %>% 
  dplyr::select(z:VW6MOD1)
x <- data %>% dplyr::select(B02CHE3:VW6MOD1)
y <- data[,"z"]
set.seed(1234)

# Random Search
control <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 10,
  savePredictions = TRUE,
  classProbs = TRUE,
  verboseIter = TRUE)

# Create model with default paramters
control <- trainControl(method="repeatedcv", number=10, repeats=3, verboseIter = TRUE, allowParallel = T)
seed <- 1234
metric <- "Accuracy"
set.seed(seed)
mtry <- sqrt(ncol(x))
tunegrid <- expand.grid(.mtry=mtry)
rf_default <- train(z~., data=data, method="rf", metric=metric, tuneGrid=tunegrid, 
                    trControl=control)
print(rf_default)
importance(rf_default$finalModel)

mtry <- sqrt(ncol(x))
rf_random <- train(y~., data=dataset, method="rf", metric=metric, tuneLength=15, trControl=control)
print(rf_random)
plot(rf_random)


customRF <- list(type = "Classification", library = "randomForest", loop = NULL)
customRF$parameters <- data.frame(parameter = c("mtry", "ntree"), class = rep("numeric", 2), label = c("mtry", "ntree"))
customRF$grid <- function(x, y, len = NULL, search = "grid") {}
customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, y, mtry = param$mtry, ntree=param$ntree, ...)
}
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")
customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes

# train model
control <- trainControl(method="repeatedcv", number=10, repeats=3, verboseIter = T, allowParallel = T)
tunegrid <- expand.grid(.mtry=c(5,9,11,13), .ntree=c(400, 500, 600))
set.seed(1234)

custom2 <- train(z~., data=data, method=customRF, metric=metric, tuneGrid=tunegrid, trControl=control)

tunegrid <- expand.grid(.mtry=c(13), .ntree=c(500))
set.seed(1234)
custom3 <- train(z~., data=data, method=customRF, metric=metric, tuneGrid=tunegrid, trControl=control)

library(raster)
r <- stack("/home/marcos/Documents/GDB/covs_Arg/all_covariates.tif")
load("/home/marcos/Documents/GDB/covs_Arg/all_covariates_names.RData")
names(r) <- namesCovarAll
beginCluster()
x <- clusterR(r, fun = predict,  args=list(model=custom3$finalModel))
endCluster()
mapview::mapview(x)
plot(x)
