rm(list = ls())
library(doParallel)
library(foreach)
library(e1071)


hist.dat <- read.csv(
  "summarizedData/processedHistogram_0_21_target_tripLength.csv")[,-1]
names(hist.dat)[23] <- "id"
drivers <- unique(hist.dat$id) 
set.seed(25)

randomDrivers = sample(drivers, size = 5)
target = 0
names(target) = "target"

cl <- makePSOCKcluster(4)
registerDoParallel(cl)

refData <- hist.dat[hist.dat$id %in% randomDrivers,-23]
refData[,"target"] <- 0


n.trees <- 5000
target = 1
names(target) = "target"
submission = NULL
submission <- foreach(driver = iter(drivers), .combine = rbind,
                      .packages = "e1071") %dopar%
{
  print(driver)
  dirPath = paste0("drivers/", driver, '/')
  currentData = hist.dat[hist.dat$id == driver, -23]
  train = rbind(currentData, refData)
  train = as.data.frame(train)
  g = svm(target ~ ., data=train, probability = T,
          type = "C-classification")
  currentData = as.data.frame(currentData)
  p =attr(predict(g, currentData, probability = T, 
             decision.values = F), "probabilities")[,1]
  labels = sapply(1:200, function(x) paste0(driver,'_', x))
  result = cbind(labels, p)
  
}

stopCluster(cl)

colnames(submission) = c("driver_trip","prob")
write.csv(submission,
          "submissions/submission_svm_speedhistogram_tripLength.csv", row.names=F, quote=F)
