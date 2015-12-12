#MathieuGagnon_JulesRandolph_tp4_.R

mydata = as.matrix(read.csv("turkiye-student-evaluation_generic.csv"))
nbEvaluation <- length(mydata[,1])

#number of clusters to be used
k <- 3

#target to analyse (1 for instructors, 2 for classes)
analysisTarget <- 1

##Data Preprocessing 
  #create our partial data (if necessary)
  dataEchantillon <- mydata
  #dataEchantillon <- rbind(mydata[1:500,], mydata[801:1300,], mydata[2301:2800,])
  
  #remove the two first columns containing instructor ID and class ID
  dataWOids <- dataEchantillon[,3:33]
  
  #definition of normalizeData function. 
  #Used to normalize data so the variations of values have lesser impact (i.e. 0-3 compared to 1-5)
  normalizeData <- function(data) {
    
    #repeat
    data[,1] <- (data[,1] ) / max(data[,1])
    
    #attendance
    data[,2] <- (data[,2] ) / 4
    
    #difficulty and Q1 to Q28
    data[,3:31] <- (data[,3:31] - 1) / 4
    
    return (data)
  }
  
  #usage
  normalizedData <- normalizeData(dataWOids)

##Kmeans execution   
  #execute kmeans algorithm (from R libraries)
  set.seed(5)
  result <- kmeans(normalizedData, k, trace = FALSE)

##Data Analysis 
  #generateClusterIndexes - returns a list of evaluations associated with each cluster
  generateClusterIndexes <- function(nbClusters) {
    return (lapply(1:nbClusters, function(i) which(result$cluster == i) ))
  }
  
  clusterIndexes <- generateClusterIndexes(k)
  
  computeTargetProportions <- function(target){
    #vector associating each evaluation (index) with an instructor or class (from entry data)
    targetIndexes <- dataEchantillon[,target]
    
    #list which each slot (index correspond to an instructor or class) contains a vector of all the associated evaluations (from entry data)
    evaluationsToTargetList <- lapply(1:k, function(i) which(targetIndexes == i))
    
    #list of clusters containing classified evaluations (from clusters found by kmeans)
    evaluationClusters <- lapply(1:k, function(i) clusterIndexes[[i]])
    
    valuesInCommon <- lapply(1:k, 
                             function(i) sapply(1:k, 
                              function(j)
                                length(intersect(evaluationsToTargetList[[j]], evaluationClusters[[i]]))))
    
    for(i in 1:k ){
      valuesInCommon[[i]] <- valuesInCommon[[i]] / length(evaluationClusters[[i]])
    }
  
    return (valuesInCommon)
  }

  #compute proportions of evaluations for each instructor among all instructors, or each class among classes for all data
  computeProportionsInEntryData <- function(nbClusters, target) {
    return (unlist(lapply(1:nbClusters, function(i) length(which(dataEchantillon[,target] == i)) / nbEvaluation )))
  }
  
  #compute proportions of evaluations for each instructor in all data
  proportionsOfEachTarget <- computeProportionsInEntryData(k, 1)
  
  targetProportions <- computeTargetProportions(analysisTarget)
  
  #compute ratios 
  ratios <- lapply(1:k, function(i) targetProportions[[i]] / proportionsOfEachTarget)
  ratios
