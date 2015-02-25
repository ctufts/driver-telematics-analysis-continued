rm(list = ls())
library(dplyr)
library(stringr)

#clusters using kmeans
get.clusters <- function(x){
  
  set.seed(7)
  # initialize results vector
  if(is.null( nrow(x))){
    final.result <- rep(0, length(x))
  }else{
    final.result <- rep(0, nrow(x))
  }
  # perform kmeans clustering
  result <- kmeans(x, center = 2, nstart = 100,
                   iter.max = 1000)$cluster
  
  result.a <- sum(result == 1)
  result.b <- sum(result == 2)
  # assign 1 to larger group
  if(result.a > result.b){
    final.result[result == 1] <- 1   
    return(final.result)
  }else{
    final.result[result == 2] <- 1
    return(final.result)
  }
}

# cluster using hierarchal cluster
get.hier.clusters <- function(x){
  
  set.seed(7)
  # initialize results vector
  if(is.null( nrow(x))){
    final.result <- rep(0, length(x))
  }else{
    final.result <- rep(0, nrow(x))
  }
  
  hc.complete <- hclust(dist(x), method = "complete")
  result <- cutree(hc.complete, 2)
  result.a <- sum(result == 1)
  result.b <- sum(result == 2)
  # assign 1 to larger group
  if(result.a > result.b){
    final.result[result == 1] <- 1   
    return(final.result)
  }else{
    final.result[result == 2] <- 1
    return(final.result)
  }
  
}

# read in processed data for driver trips and stops
test.data<- read.csv("driverStopStats20150219.csv")[-1]


#modify filename to show just the number of the file
file.names.sub.output <- str_extract(test.data$file.name, "/[0-9]{1,3}\\.")
file.names.sub.output <- sub("/", "", file.names.sub.output)
file.names.sub.output <- sub("\\.", "", file.names.sub.output)

# reorder array by filename
test.data$fout <- as.numeric(file.names.sub.output)
test.data <- test.data[order(test.data$id, test.data$fout),]



# apply Hierarchical clustering to: 
# stops, trip length, mean distance, median distance, 
# standard devation of distance, cumulative distance,
# and total distance
ids <- unique(test.data$id)
set.seed(7)
final.results <- data.frame()
for(i in 1:length(ids)){
  print(i)
  # get training set of positive instances
  temp <- test.data[test.data$id == ids[i],]
  neg.samples <-sample_n(test.data[test.data$id != ids[i],], 30)
  test.set <- data.frame(rbind(temp, neg.samples), 
                         man.pred = as.integer(c(rep(1, nrow(temp)),
                                      rep(0, nrow(neg.samples))))
                         )
  results <- glm(man.pred ~ trip.length, family = "binomial", 
      data = test.set)
  predictions <- predict(results, type = "response")
  final.results <- rbind(final.results, 
                         data.frame(prediction = predictions[1:nrow(temp)],
                                    id = temp$id, fout = temp$fout))
}

summary(factor(final.results$prediction))


#write results to file 
driver_trip <- paste0(final.results$id, "_", final.results$fout)
write.csv(data.frame(driver_trip, prob = final.results$prediction),
          file = "submission_2015021_6_normalized_logistic_30noise_triplength.csv",
          row.names = F)
