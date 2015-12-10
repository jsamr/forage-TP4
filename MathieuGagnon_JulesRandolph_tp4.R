mydata = as.matrix(read.csv("turkiye-student-evaluation_generic.csv"))

dataWOids <- mydata[,3:33]

dataProf1 <- dataWOids[1:10,]
dataProf2 <- dataWOids[801:810,]
dataProf3 <- dataWOids[2301:2310,]

dataEchantillon <- rbind(dataProf1,dataProf2,dataProf3)
dataEchantillon

dist(dataEchantillon, method = "euclidean")

result <- kmeans(dataEchantillon, 3)
