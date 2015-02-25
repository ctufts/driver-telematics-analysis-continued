rm(list = ls())
library(doMC)
library(foreach)
library(gbm)

speedDistributionHistogram <- function(trip)
{
  min.speed <- 0 #km
  max.speed <- 160 #km
  bin.width <- 8 #km
  # calculate speed
  speed <- 3.6*sqrt(diff(trip$x,20,1)^2 + diff(trip$y,20,1)^2)/20
  # get histogram of speed bin size of 8km/hr 0-160km
  speed.hist <- hist(speed[speed <= max.speed & speed >= min.speed],
                plot = F, breaks= seq(min.speed, max.speed, bin.width))$count
  return(speed.hist/sum(speed.hist))
}

set.seed(25)
# get list of drivers
drivers = list.files("drivers")
# select list of random drivers
randomDrivers = sample(drivers, size = 5)
target = 0
names(target) = "target"

#register parallel backend 
registerDoMC(4)

# create sample set with class zero
refData <- foreach(driver = iter(randomDrivers), .combine = rbind)%dopar%
{
  dirPath = paste0("drivers/", driver, '/')
  ref.data <- NULL
  for(i in 1:200)
  {
    trip = read.csv(paste0(dirPath, i, ".csv"))
    features = c(speedDistributionHistogram(trip), target)
    ref.data = rbind(ref.data, features)
  }
  ref.data
}



# classify each driver 
# each driver is class 1 and 
# is compared to the class 0 data
n.trees <- 5000
target = 1
names(target) = "target"
submission = NULL
submission <- foreach(driver = iter(drivers), .combine = rbind,
                      .packages = "gbm") %dopar%
{
  
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


# write results to files
colnames(submission) = c("driver_trip","prob")
write.csv(submission,
          "submissions/submission_gbm_speedhistogram.csv", row.names=F, quote=F)
