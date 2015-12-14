#MathieuGagnon_JulesRandolph_tp4_.R



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


##Data Analysis
#generateClusterIndexes - returns a list of evaluations associated with each cluster
generateClusterIndexes <- function(nbClusters,result) {
  return (lapply(1:nbClusters, function(i) which(result$cluster == i) ))
}

computeTargetProportions <- function(target,clusterIndexes,k){
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

runAnalysis <- function(k, targetIndex, preProcessedData, seed){
  ##Kmeans execution
  #execute kmeans algorithm (from R libraries)
  set.seed(seed)
  result <- kmeans(preProcessedData, k, trace = FALSE)

  clusterIndexes <- generateClusterIndexes(k,result)
  #compute proportions of evaluations for each target in all data
  proportionsOfEachTarget <- computeProportionsInEntryData(k, targetIndex)

  #compute proportions of evaluations for each target in clusters found by kmeans
  targetProportions <- computeTargetProportions(targetIndex, clusterIndexes,k)

  #compute ratios
  ratios <- sapply(1:k, function(i) targetProportions[[i]] / proportionsOfEachTarget)
  ratios
}


mydata = as.matrix(read.csv("turkiye-student-evaluation_generic.csv"))
nbEvaluation <- length(mydata[,1])

##Data Preprocessing
#create our partial data (if necessary)
dataEchantillon <- mydata
#dataEchantillon <- rbind(mydata[1:500,], mydata[801:1300,], mydata[2301:2800,])

#remove the two first columns containing instructor ID and class ID
dataWOids <- dataEchantillon[,3:33]

#usage
normalizedData <- normalizeData(dataWOids)


#runAnalysis(3,1,normalizedData,3)
quests <- normalizedData[,4:31]
metrics <-cor(normalizedData[,1:3])
write.csv(round(cor(quests[,c(3,5,7,9,10,11,12)]),digits=2),"cor/QualiteEnseignement.csv")
write.csv(round(cor(quests[,c(1,2,4)]),digits=2),"cor/TransparenceConsistanceEnseignement.csv")
write.csv(round(cor(quests[,c(6,8)]),digits=2),"cor/QualiteRessourcesPedagogiques.csv")
write.csv(round(cor(quests[,c(13,14,15,17,26)]),digits=2),"cor/RigueurProfesseur.csv")
write.csv(round(cor(quests[,c(16,18,20,22,28)]),digits=2),"cor/ClarteEcouteNeutraliteProf.csv")
write.csv(round(cor(quests[,c(21,23,24,25,27)]),digits=2),"cor/ImplicationProf.csv")
write.csv(runAnalysis(13,2,normalizedData,5),"RatiosCoursRawNormalizedData.csv")