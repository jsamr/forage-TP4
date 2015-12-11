#MathieuGagnon_JulesRandolph_tp4_.R

mydata = as.matrix(read.csv("turkiye-student-evaluation_generic.csv"))
nbEvaluation <- length(mydata[,1])

#create our partial data
dataEchantillon <- mydata
#dataEchantillon <- rbind(mydata[1:500,], mydata[801:1300,], mydata[2301:2800,])

#remove the two first columns containing instructor ID and class ID
dataWOids <- dataEchantillon[,3:33]

#definition of normalizeData function
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


#number of clusters to be used
k <- 3

#execute kmeans algorithm
result <- kmeans(dataWOids, k, trace = FALSE)

##definition generateClusterIndexes function
generateClusterIndexes <- function(k) {
  return (lapply(1:k, function(i) which(result$cluster == i) ))
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

#usage (FAIRE UNE BOUCLE / LAPPLY)
computeInstructorProportions(evaluationsToInstList, evaluationClusters[[1]])
computeInstructorProportions(evaluationsToInstList, evaluationClusters[[2]])
computeInstructorProportions(evaluationsToInstList, evaluationClusters[[3]])

#evaluer efficacite des regroupements 


# pour l'instant, 
# -avec 500 données de chaque groupe, le clustering n'associe pas vraiment de prof à un cluster
# -avec toutes les données, c'est pire car il y a bcp plus de données pour instructeur 3 que pour 1
