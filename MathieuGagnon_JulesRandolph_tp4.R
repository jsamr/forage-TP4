#MathieuGagnon_JulesRandolph_tp4_.R

mydata = as.matrix(read.csv("turkiye-student-evaluation_generic.csv"))

#remove the two first columns containing instructor ID and class ID
dataWOids <- mydata[,3:33]

#create our partial data
dataProf1 <- dataWOids[1:10,]
dataProf2 <- dataWOids[801:810,]
dataProf3 <- dataWOids[2301:2310,]
dataEchantillon <- rbind(dataProf1,dataProf2,dataProf3)

#execute kmeans algorithm
result <- kmeans(dataEchantillon, 3)
result$cluster

#2) foreach liste créée, calculer la proportion de "corrélation" entre la classe prédite et
#   l'instructeur associé à chaque index
#a) extraire de mydata l'info de quel instructeur va avec quel index
#b) comparer les données associées à un cluster avec le véritable instructeur
#[[1]]
#[1]  1  2  4 12 13 15 18 20 21 24 25 26 27 28 30
#fonction qui prend 1 cluster et retourne un vecteur de taille k contenant combien il y a de ins1, ins2, ins3 (en proportion, donc ins1/ ins1+ins2+ins3)

generateClusterIndexes <- function(k) {
  return (lapply(1:k, function(i) which(result$cluster == i) ))
}

clusterIndexes <- generateClusterIndexes(3)

#vector associating each student (index) with an instructor
instructorsIndexes <- c(mydata[1:10,1], mydata[801:810,1], mydata[2301:2310,1])

###
cluster1 <- clusterIndexes[1,]

calculateProportionsOfValidClusters <- function(cluster){
    
  #faire un which
  #return vector de 3 valeurs (proportions) 
}

#usage
#calculateProportionsOfValidClusters(cluster1)


###backup###

#function
#dist(dataEchantillon, method = "euclidean")

#vector <- 1:10    
#vector[c(1,3,5)]

#plot(dataEchantillon, col = result$cluster)
#points(result$centers, col = 1:5, pch = 8)