plot_dendrogram_clusters <- function(dend, clusters_df) {
  # Color the dendrogram by clusters
  dend_colored <- color_branches(dend, clusters = clusters_df$subcluster)
  
  # Plot dendrogram
  par(mar = c(5, 5, 2, 10))  # Adjust margins
  plot(dend_colored, main = "Dendrogram with Optimized Subclusters", 
       xlab = "", sub = "", horiz = TRUE)
  
  # Add legend for clusters (if not too many)
  if (length(unique(clusters_df$subcluster)) <= 20) {
    cluster_cols <- unique(get_leaves_branches_col(dend_colored))
    cluster_ids <- unique(clusters_df$subcluster)
    
    legend("topright", legend = paste("Cluster", cluster_ids), 
           fill = cluster_cols[1:length(cluster_ids)], 
           inset = c(-0.3, 0), xpd = TRUE)
  }
}