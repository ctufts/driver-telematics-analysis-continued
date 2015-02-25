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

cl <- makePSOCKcluster(4)
registerDoParallel(cl)



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
    features = c(speedDistributionHistogram(trip), target, nrow(trip),
                 as.numeric(driver))
    currentData = rbind(currentData, features)
  }
  currentData
  
}

stopCluster(cl)
write.csv(submission, file = "summarizedData/processedHistogram_0_21_target_tripLength.csv")

