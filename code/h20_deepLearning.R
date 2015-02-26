rm(list = ls())
library(foreach)
library(iterators)
library(h2o)
localH2O <- h2o.init()  


hist.dat <- read.csv("summarizedData/processedHistogram_0_21_target_tripLength.csv")[,-1]
names(hist.dat)[23] <- "id"
drivers <- unique(hist.dat$id) 
set.seed(25)

randomDrivers = sample(drivers, size = 5)
target = 0
names(target) = "target"

refData <- hist.dat[hist.dat$id %in% randomDrivers,-23]
refData[,"target"] <- 0


n.trees <- 5000
target = 1
names(target) = "target"
submission = NULL
submission <- foreach(driver = iter(drivers), .combine = rbind) %do%
{
  print(driver)
  dirPath = paste0("drivers/", driver, '/')
  currentData = hist.dat[hist.dat$id == driver, -23]
  train.hex = as.h2o(localH2O, rbind(currentData, refData))
  model <- h2o.deeplearning(x = c(1:20, 22), y = 21, 
                            train.hex, hidden = c(50, 50, 50),
                            epochs = 100)
  p = as.data.frame(h2o.predict(model, as.h2o(localH2O, currentData)))
  labels = sapply(1:200, function(x) paste0(driver,'_', x))
  result = cbind(labels, p$X1)
}


colnames(submission) = c("driver_trip","prob")
write.csv(submission,
          "submissions/submission_deeplearning_speedhistogram_tripLength.csv", row.names=F, quote=F)
