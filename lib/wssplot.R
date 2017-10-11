
# This function plots the within sum of squares elbow plot for determining the optimal number of clusters in a kmeans clustering algorithm.

wssplot <- function(idata, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(idata,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(idata, centers=i)$withinss)
  }
  plot(1:nc, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares", main="Determining the Number of Clusters")
}
