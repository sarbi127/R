# Part 1: Distance Calculation
# TASK 1. Writing a function for distance calculation:

load("cluster.RData")

my_dist_calculator <- function(a, b, metric = "euclidean"){
  
  if(metric == "manhattan"){
    # write code for calculating manhattan distance
    dist <- sum(abs(a - b))
    
  }else{
    # write code for calculating euclidean distance
    dist <- sqrt(sum((a - b)^2))
    
  }
  
  return(dist) 
}

a=c(1.7, 5)
b=c(4, 72)

my_dist_calculator (a, b , metric = "euclidean")
my_dist_calculator (a, b , metric = "manhattan")


#Part 2: K-Means Clustering
#TASK 2a. Write a function for performing K-Means clustering

k_means <- function(x, k, max.iter = 20){
  
  random_index <- sample(1:k, nrow(x), replace = TRUE)
  data_with_cluster <- cbind(x, clusterID = random_index)
  iterations = 1
  plot(data_with_cluster[,1:2])
  
  while(TRUE){
    centroids <- matrix(rep(0, times = k * ncol(x)), nrow = k, ncol = ncol(x))
    
    for(i in 1:k){
      obs_of_cluster_i <- (data_with_cluster$clusterID == i)
      
      # find their mean and save in "centroids" matrix
      centroids[i,] <- colMeans(data_with_cluster[obs_of_cluster_i, 1:2])
    }
    
    points(centroids[,1:2], pch = 20, cex = 2)
    
    readline(prompt = "Press Enter to continue:")
    
    dist_from_centroids <- matrix(rep(0, nrow(x) * k), nrow = nrow(x), ncol = k)

    for(i in 1:nrow(x)){
      for(j in 1:nrow(centroids)){
        # Use the euclidean distance calculation function written in TASK 1.
        dist_from_centroids[i,j] <- my_dist_calculator (data_with_cluster[i,1:2], centroids[j,] , metric = "euclidean")
      }
    }
    obs_new_clusterID <- apply(dist_from_centroids, 1, which.min)
    
    # If the centroid is not changing any more for each observation, stop the iteration
    if(all(obs_new_clusterID == data_with_cluster$clusterID)){ 
      km.clusters <- obs_new_clusterID
      centroid.matrix <- centroids
      break
      # If number of iterations exceed the maximum iterations allowed, stop the iteration
    }else if(iterations > max.iter){
      break
      # Otherwise, assign the new centroids to the dataset, and continue the iteration
    }else{ 
      data_with_cluster$clusterID <- obs_new_clusterID
      iterations <- iterations + 1
    }
  }
  plot(data_with_cluster[,1:2], col = data_with_cluster$clusterID)
  points(centroid.matrix[,1:2], pch = 20, cex = 2, col = 1:k)

  return(list("clusters" = km.clusters, "centroids" = centroid.matrix))
}

km_clusters <- k_means(cluster.data, k = 3, max.iter = 15)

print(km_clusters)

#TASK 2b.

within_ss <- numeric(7)
db <- cluster.data

for(k in 1:7){
  km.cl <- kmeans(db, centers = k, nstart = 10)
  # save total within ss value for different values of k
  within_ss[k] <-  km.cl$tot.withinss
}
plot(x = 1:7, y = within_ss, type='b', xlab = "number of centers K", ylab = "total within SS" )

#The best value for k is 3 because k value in 1 and 2 more drop but in 3 less drop with remaining values.

#Part 3: K-Medoids Clustering

library('cluster')
kmed.cl <- pam(db, k = 3, metric = "manhattan")
plot(cluster.data, col = kmed.cl$clustering, pch = as.character(kmed.cl$clustering), cex = 0.7, main="k-medoids")
points(kmed.cl$medoids, pch = c("1","2","3"), cex = 2, lwd=3)

# TASK 3b.Calculate Silhouette value for clustering for K ranging from 2 to 5.

sil_info <- data.frame("K" = 2:5, "Sil" = numeric(4))
for(k in 2:5){
  kmed.cl <- pam(db, k,metric = "manhattan" )
  s <- silhouette(kmed.cl)
  sil_info$Sil[k-1] <- mean(s[,3])
  }
print(sil_info)

# the result is:
#  K       Sil
#1 2 0.4033822
#2 3 0.4314134
#3 4 0.3359872
#4 5 0.3498398
# The best value of K is 3 because Sil is a max value and near more to 1.

#Part 4: Hierarchical Clustering
#TASK 4a.
set.seed(101)
rand.sample <- sample(1:nrow(cluster.data), 20)
small.dataset <- cluster.data[rand.sample,]
rownames(small.dataset) <- 1:20
distance_matrix <- dist(small.dataset, method = "euclidean")
hierarchial.clust <- hclust(d = distance_matrix, method = "complete")
plot(hierarchial.clust, hang = 0.1, main = "Hierarchical Cluster", cex = 1)

# TASK 4b. Cut the dendrogram to get three clusters

clusters <- cutree(hierarchial.clust, k = 3)
plot(small.dataset, col = clusters, pch = as.character(clusters)) 



