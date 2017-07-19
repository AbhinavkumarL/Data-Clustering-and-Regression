library(readr)
testing <- read_csv("~/Documents/R-For Data Science/Project1/ForestTypes/testing.csv")
training <- read_csv("~/Documents/R-For Data Science/Project1/ForestTypes/training.csv")
totData = rbind(testing, training)
testData = as.data.frame(totData[,1:10])
myData = as.data.frame(unclass(testData[,2:10]))
summary(myData)
dim(myData)
columns <- names(myData)
columns

kc <- kmeans(myData,3)
kc$betweenss
kc$totss
table(totData$class,kc$cluster)
(kc$betweenss/kc$totss)*100

kc <- kmeans(myData,4)
kc$betweenss
kc$totss
table(totData$class,kc$cluster)
(kc$betweenss/kc$totss)*100

# Plot SS ratio for k-mean cluster of oders up t 15
# should give us an elbow graph
k.max <- 15
wss <- sapply(1:k.max,
              function(k){kmeans(myData, k, nstart=10 )$tot.withinss})
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE,
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
abline(v = 3, lty =2)

# Now let`s try H-Cluster
testing <- read_csv("~/Documents/R-For Data Science/Project1/ForestTypes/testing.csv")
training <- read_csv("~/Documents/R-For Data Science/Project1/ForestTypes/training.csv")
totData = rbind(testing, training)
testData = as.data.frame(totData[,1:10])
myData = as.data.frame(unclass(testData[,2:10]))
summary(myData)
dim(myData)

View(myData)
myData.complete = hclust(dist(myData,method = "euclidian"),method="complete")
myData.average = hclust(dist(myData,method = "euclidian"),method="average")
myData.single = hclust(dist(myData,method = "euclidian"),method="single")
myData.wardD2 = hclust(dist(myData,method = "euclidian"),method="ward.D2")

par(mfrow=c(1,4))
plot(myData.complete,main="Complete Linkage", xlab="", sub="", cex=.9,labels = FALSE)
plot(myData.average, main="Average Linkage", xlab="", sub="", cex=.9,labels = FALSE)
plot(myData.single, main="Single Linkage", xlab="", sub="", cex=.9,labels = FALSE)
plot(myData.wardD2, main="Ward D2 Linkage", xlab="", sub="", cex=.9,labels = FALSE)


table(cutree(myData.complete,4), as.factor(testData$class))
table(cutree(myData.average,4), as.factor(testData$class))
table(cutree(myData.single,4), as.factor(testData$class))
table(cutree(myData.wardD2,4), as.factor(testData$class))


