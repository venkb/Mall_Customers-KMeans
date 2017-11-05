#k-means clustering
library(stats)#to build k-means clusters
library(cluster)#to build cluster plot

#setwd('set the folder that contains Mall_Customers.csv')
dataset_full = read.csv('Mall_Customers.csv')

#for the ease of cluster representation lets pick two features to determine the clusters
dataset = dataset_full[,4:5]

#scale the variables 
dataset = scale(dataset)

#determine the optimal number of clusters with elbow method using wcss
set.seed(6)
wcss = vector()
for (i in 1:10) {
        wcss[i] = sum(kmeans(x = dataset, centers = i)$withinss)
}

#plot the wcss for each cluster size
plot(x = 1:10,
     y = wcss,
     type = 'b',
     main = 'WCSS vs Number of Clusters',
     xlab = 'Num of Clusters',
     ylab = 'WCSS')

#elbow is formed for num of clusters = 5. lets build 5 clusters
kmeans_cluster = kmeans(x = dataset,
                  centers = 5,
                  iter.max = 100,
                  nstart = 10)

#plot the clusters
clusplot(x = dataset,
         clus = kmeans_cluster$cluster,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 4,
         span = TRUE,
         main = 'k-means cluster plot',
         xlab = 'Salary',
         ylab = 'Spending Score')
