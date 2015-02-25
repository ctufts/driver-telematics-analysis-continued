rm(list = ls())
library(doParallel)
library(foreach)
library(gbm)

speedDistributionHistogram <- function(trip)
{
  min.speed <- 0
  max.speed <- 160
  bin.width <- 8
  speed <- 3.6*sqrt(diff(trip$x,20,1)^2 + diff(trip$y,20,1)^2)/20
  speed.hist <- hist(speed[speed <= max.speed & speed >= min.speed],
                plot = F, breaks= seq(min.speed, max.speed, bin.width))$count
  return(speed.hist/sum(speed.hist))
}

set.seed(25)
drivers = list.files("drivers")
randomDrivers = sample(drivers, size = 8)

ref.data = NULL
target = 0
names(target) = "target"
# 
cl <- makePSOCKcluster(4)
registerDoParallel(cl)

refData <- foreach(driver = iter(randomDrivers), .combine = 'rbind')%dopar%
{
  dirPath = paste0("drivers/", driver, '/')
  ref.data = NULL
  for(i in 1:200)
  {    
    trip = read.csv(paste0(dirPath, i, ".csv"))
    features = c(speedDistributionHistogram(trip), target)
    ref.data = rbind(ref.data, features)
  }
  ref.data
}


n.trees <- 5000
target = 1
names(target) = "target"
submission = NULL
submission <- foreach(driver = iter(drivers), .combine = rbind,
                      .packages = "gbm") %dopar%
{
  print(driver)
  dirPath = paste0("drivers/", driver, '/')
  currentData = NULL
  for(i in 1:200)
  {
    trip = read.csv(paste0(dirPath, i, ".csv"))
    features = c(speedDistributionHistogram(trip), target)
    currentData = rbind(currentData, features)
  }
  train = rbind(currentData, refData)
  train = as.data.frame(train)
  g = gbm(target ~ ., data=train,n.trees = n.trees, distribution = "bernoulli")
  currentData = as.data.frame(currentData)
  p =predict(g, currentData, n.trees = n.trees, type = "response")
  labels = sapply(1:200, function(x) paste0(driver,'_', x))
  result = cbind(labels, p)

}

stopCluster(cl)

colnames(submission) = c("driver_trip","prob")
write.csv(submission,
          "submissions/submission_gbm_speedhistogram.csv", row.names=F, quote=F)
