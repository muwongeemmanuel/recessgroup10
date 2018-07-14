
library(class)

#K-means clustering with 3 clusters of sizes 46, 54, 50
strikers <-fifa18[fifa18$Position == "FWD",c(1,2,7,10,11,22,29,30,35,36,39,43,44,46,49,51,56,59) ] 
k2 <- kmeans(strikers[,c(-1,-2)], centers = 400, nstart = 25)
str(k2)
k2
attributes(k2)
View(k2$centers)
View(k2$cluster)
View(k2$size)
library(cluster)
clusplot(strikers[,c(-1,-2)], k2$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

## list of cluster assignments
o=order(k2$cluster)
s_clusters <- data.frame(strikers$name[o],k2$cluster[o])
names(s_clusters) <- c("name", "clusters")
cluster_group <-as.numeric(s_clusters[s_clusters$name == "L. Messi",2])  #s_clusters[s_clusters$name == "L. Messi",2] 
final_cluster <- s_clusters[s_clusters$clusters == cluster_group,]
