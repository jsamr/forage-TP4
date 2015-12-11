#MathieuGagnon_JulesRandolph_tp4_.R

mydata = as.matrix(read.csv("turkiye-student-evaluation_generic.csv"))
nbEvaluation <- length(mydata[,1])

#number of clusters to be used
k <- 13

#create our partial data
dataEchantillon <- mydata
#dataEchantillon <- rbind(mydata[1:500,], mydata[801:1300,], mydata[2301:2800,])

#compute proportions of evaluations for each instructor among all instructors, or each class among classes for all data
computeProportions <- function(nbClusters, rowNumber) {
  return (unlist(lapply(1:nbClusters, function(i) length(which(dataEchantillon[,rowNumber] == i)) / nbEvaluation )))
}

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

#execute kmeans algorithm (from R libraries)
result <- kmeans(normalizedData, k, trace = FALSE)

##definition generateClusterIndexes function
generateClusterIndexes <- function(nbClusters) {
  return (lapply(1:nbClusters, function(i) which(result$cluster == i) ))
}

clusterIndexes <- generateClusterIndexes(k)

#vector associating each evaluation (index) with an instructor (concatenation)
instructorsIndexes <- dataEchantillon[,1]

#list which each index (an instructor) is a vector containing all the associated evaluations
evaluationsToInstList <- lapply(1:k, function(i) which(instructorsIndexes == i))

#list of clusters containing classified evaluations (clusters predicted by kmeans)
evaluationClusters <- lapply(1:k, function(i) clusterIndexes[[i]])

##definition 
computeInstructorProportions <- function(evaluationsToInstList, evaluationClusters){
  
  valuesInCommon <- lapply(1:length(evaluationsToInstList), 
                           function(i) length(intersect(evaluationsToInstList[[i]], evaluationClusters)))
  
  return (unlist(valuesInCommon) / length(evaluationClusters))
}

#compute proportions of evaluations for each instructor in all data
proportionsOfEachInstructor <- computeProportions(k, 2)

#compute ratios 
ratios <- lapply(1:k, function(i) computeInstructorProportions(evaluationsToInstList, evaluationClusters[[i]]) / proportionsOfEachInstructor )

#computeInstructorProportions(evaluationsToInstList, evaluationClusters[[1]]) / proportionsOfEachInstructor
#computeInstructorProportions(evaluationsToInstList, evaluationClusters[[2]]) / proportionsOfEachInstructor
#computeInstructorProportions(evaluationsToInstList, evaluationClusters[[3]]) / proportionsOfEachInstructor

#si l'association cluster<-->instructor fonctionne, le calcul précedent devrait faire ressortir un instructeur dominant par cluster


#evaluer efficacite des regroupements 


# pour l'instant, 
# -avec 500 données de chaque groupe, le clustering n'associe pas vraiment de prof à un cluster
# -avec toutes les données, c'est pire car il y a bcp plus de données pour instructeur 3 que pour 1
