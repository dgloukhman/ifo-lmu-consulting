# Elbow method for number of clusters
wss <- map_dbl(cluster_sizes, function(k) {
  dist_mat <- dist(as.matrix(ts_ccf_matrix))
  hc <- hclust(dist_mat, method = "ward.D2")
  clusters <- cutree(hc, k)
  sum(sapply(unique(clusters), function(c) {
    cluster_points <- ts_ccf_matrix[clusters == c, ]
    sum(scale(cluster_points, scale = FALSE)^2)
  }))
})

plot(cluster_sizes[20:length(cluster_sizes)], wss[20:length(cluster_sizes)], type="b", main="Elbow Method", xlab="k", ylab="Within SS")


library("cluster")
silhouette_scores <- map_dbl(cluster_sizes, function(k) {
  hc <- hclust(dist(as.matrix(ts_ccf_matrix)), method = "ward.D2")
  clusters <- cutree(hc, k)
  
  # only compute if >1 cluster
  if (length(unique(clusters)) > 1) {
    sil <- silhouette(clusters, dist(as.matrix(ts_ccf_matrix)))
    mean(sil[, "sil_width"])  # column name is safer than hard-coded index
  } else {
    NA_real_
  }
})

plot(cluster_sizes[20:length(cluster_sizes)], silhouette_scores[20:length(cluster_sizes)], type="b",main="Silhouette Scores", xlab="k", ylab="Average silhouette")