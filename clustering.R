read.csv(file="data.csv", sep=",", dec=".")
model <- kmeans(data, centers= 2, nstart=10)#j'ai tiré ce nombre du tp
model$cluster
model$betweenss/model$totss*100
model$tot.withinss
#classification hierarchique
read.csv(data.csv, sep=",", dec=".", header= TRUE)
scale(data)
head(data)
data_d_euclidean <- dist(data, method= "euclidean" )
data_single <- hclust(data_d_euclidean, method="single")
plot(data_single)
rect.hclust(data_single, k=7, border="red")
barplot(tail(data_single$height, n=20))
#algorithme de ward
data <-read.csv(file="data.csv", sep="\t", skip=1)
rownames(data)<- data$household
data$household <- NULL
data <- scale(data)
hclust_ward <-hclust(dist(data)^2/(2*nrow(data)), method="ward.D")
plot(hclust_ward)
hclust_ward$height
barplot(hclust_ward$height, ylab = "aggregation level")
plot(hclust_ward)
rect.hclust(hclust_ward, k=2, border="red")
rect.hclust(hclust_ward, k=4, border="green")
TSS <- sum(hclust_ward$height)
BSS <- sum(tail(hclust_ward$height,n=(4-1)))
BSS/TSS*100
