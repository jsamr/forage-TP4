#MathieuGagnon_JulesRandolph_tp4_.R

mydata = as.matrix(read.csv("turkiye-student-evaluation_generic.csv"))

#remove the two first columns containing instructor ID and class ID
dataWOids <- mydata[,3:33]
k <- 3

#create our partial data
dataProf1 <- dataWOids[1:10,]
dataProf2 <- dataWOids[801:810,]
dataProf3 <- dataWOids[2301:2310,]
dataEchantillon <- rbind(dataProf1,dataProf2,dataProf3)

#execute kmeans algorithm
result <- kmeans(dataEchantillon, k)
result$cluster

#2) foreach liste créée, calculer la proportion de "corrélation" entre la classe prédite et
#   l'instructeur associé à chaque index
#a) DONE extraire de mydata l'info de quel instructeur va avec quel index
#b) comparer les données associées à un cluster avec le véritable instructeur
#[[1]]
#[1]  1  2  4 12 13 15 18 20 21 24 25 26 27 28 30
#fonction qui prend 1 cluster et retourne un vecteur de taille k contenant combien il y a de ins1, ins2, ins3 (en proportion, donc ins1/ ins1+ins2+ins3)

generateClusterIndexes <- function(k) {
  return (lapply(1:k, function(i) which(result$cluster == i) ))
}

clusterIndexes <- generateClusterIndexes(k)

#vector associating each student (index) with an instructor
instructorsIndexes <- c(mydata[1:10,1], mydata[801:810,1], mydata[2301:2310,1])

indexesAssociatedToInst1 <- which(instructorsIndexes == 1)
indexesAssociatedToInst2 <- which(instructorsIndexes == 2)
indexesAssociatedToInst3 <- which(instructorsIndexes == 3)

#contient les etudiants (indexes) classés dans le cluster i
cluster1 <- clusterIndexes[[1]]
cluster2 <- clusterIndexes[[2]]
cluster3 <- clusterIndexes[[3]]

calculateProportionsOfValidClusters <- function(cluster, indexesAssociatedToInst){
  
  valuesInCommon <- intersect(cluster, indexesAssociatedToInst)
  return (length(valuesInCommon) / length(cluster))
}

#usage
calculateProportionsOfValidClusters(cluster1, indexesAssociatedToInst1)


###backup###
#lapply(1:length(cluster), function(i) which(cluster == i) )
#which(cluster == 1)  
#faire un which
#return vector de 3 valeurs (proportions) 

#function
#dist(dataEchantillon, method = "euclidean")

#vector <- 1:10    
#vector[c(1,3,5)]

#plot(dataEchantillon, col = result$cluster)
#points(result$centers, col = 1:5, pch = 8)