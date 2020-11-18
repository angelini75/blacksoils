# modelling
# 
library(tidyverse)
library(caret)
library(randomForest)

# Load data
data <- read_csv("data/soil_&_covariates.csv") %>% 
  na.omit() %>% 
  dplyr::select(z:VW6MOD1)

# Setting for caret model
{customRF <- list(type = "Classification", library = "randomForest", loop = NULL)
  customRF$parameters <- data.frame(parameter = c("mtry", "ntree"), class = rep("numeric", 2),
                                    label = c("mtry", "ntree"))
  customRF$grid <- function(x, y, len = NULL, search = "grid") {}
  customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
    randomForest(x, y, mtry = param$mtry, ntree=param$ntree, ...)
  }
  customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
    predict(modelFit, newdata)
  customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
    predict(modelFit, newdata, type = "prob")
  customRF$sort <- function(x) x[order(x[,1]),]
  customRF$levels <- function(x) x$classes}

control <- trainControl(method="repeatedcv", 
                        number=10, 
                        repeats=5,
                        verboseIter = TRUE,
                        classProbs = TRUE,
                        savePredictions = "all")
tunegrid <- expand.grid(.mtry=c(13), .ntree=c(500))

# raster for prediction
library(raster)
rasterOptions(tmpdir=paste0("/run/media/marcos/_home/marcos/tmp"),
              # set temp-file directory (it should existe already)
              # the directory needs more than 100 GB free
              # gsub(".tif", "",paste0(pid,"_",input[iterator]))),
              tmptime = 3, # time in hours that temp files will remain
              progress= "text", 
              timer=TRUE,
              overwrite = TRUE, 
              chunksize=2e8, 
              maxmemory=1e8)

r <- stack("/home/marcos/Documents/GDB/covs_Arg/all_covariates.tif")
load("/home/marcos/Documents/GDB/covs_Arg/all_covariates_names.RData")
names(r) <- namesCovarAll

# select the same amount of samles at each category (there are 834 samples as 1 and 2895 as 0)

x <- stack()

for(i in 1:20){
  # balance the number of samples by category
  set.seed(i)
  d <- mutate(data, 
              bs = if_else(z==2, 1, z), 
              bs = as.factor(bs)) %>% 
    dplyr::select(-z) %>% 
    group_by(bs) %>% 
    sample_n(834, replace = FALSE)
  # convert o and 1 to names
  d$bs <- as.factor(make.names(d$bs))
  
  # train the model
  set.seed(i)
  custom <- train(bs~., data = d, method=customRF, metric="Kappa", 
                  tuneGrid=tunegrid, trControl=control)
  custom$pred
  beginCluster(n = 6)
  prob <- clusterR(r, fun = predict,  args=list(model=custom$finalModel, type="prob"))
  endCluster()
  names(prob) <- paste0("layer_",i)
  x <- stack(x,prob)
} 

set.seed(1)
d <- mutate(data, 
            bs = if_else(z==2, 1, z), 
            bs = as.factor(bs)) %>% 
  dplyr::select(-z) %>% 
  group_by(bs) %>% 
  sample_n(834, replace = FALSE)

d$bs <- as.factor(make.names(d$bs))

##

##
# train model
control <- trainControl(method="repeatedcv", 
                        number=5, 
                        repeats=5,
                        verboseIter = TRUE,
                        classProbs = TRUE,
                        savePredictions = "all")
tunegrid <- expand.grid(.mtry=c(13), .ntree=c(500))
set.seed(1234)
custom <- train(bs~., data = d, method=customRF, metric="Kappa", 
                tuneGrid=tunegrid, trControl=control)
custom$pred

library(raster)
rasterOptions(tmpdir=paste0("/run/media/marcos/_home/marcos/tmp"),
              # set temp-file directory (it should existe already)
              # the directory needs more than 100 GB free
              # gsub(".tif", "",paste0(pid,"_",input[iterator]))),
              tmptime = 3, # time in hours that temp files will remain
              progress= "text", 
              timer=TRUE,
              overwrite = TRUE, 
              chunksize=2e8, 
              maxmemory=1e8)

r <- stack("/home/marcos/Documents/GDB/covs_Arg/all_covariates.tif")
load("/home/marcos/Documents/GDB/covs_Arg/all_covariates_names.RData")
names(r) <- namesCovarAll
beginCluster()
x <- clusterR(r, fun = predict,  args=list(model=custom$finalModel, type="response"))
prob <- clusterR(r, fun = predict,  args=list(model=custom$finalModel, type="prob"))
endCluster()
mapview::mapview(x)
mapview::mapview(prob)
plot(x)



##### RANGER package ####

data <- read_csv("data/soil_&_covariates.csv") %>% 
  na.omit() %>% 
  dplyr::select(z:VW6MOD1)
set.seed(1234)
d <- mutate(data, 
            bs = if_else(z==2, 1, z), 
            bs = as.factor(bs)) %>% 
  dplyr::select(-z) %>% 
  group_by(bs) %>% 
  sample_n(1000, replace = T)

library(rgdal)
r <- readGDAL(fname = "/home/marcos/Documents/GDB/covs_Arg/all_covariates.tif", )
load("/home/marcos/Documents/GDB/covs_Arg/all_covariates_names.RData")
names(r) <- namesCovarAll
beginCluster()
x <- clusterR(r, fun = predict,  args=list(model=custom$finalModel))
prob <- clusterR(r, fun = predict,  args=list(model=custom$finalModel, type="prob"))
endCluster()
mapview::mapview(x)
mapview::mapview(prob)
plot(x)