dynamic_subclustering <- function(dend, data, max_height = 700, 
                                  homogeneity_threshold = 0.3,
                                  density_threshold = 0.05,
                                  min_cluster_size = 5) {
  
  # Get all labels from the dendrogram
  all_labels <- labels(dend)
  
  # Create a dataframe to store subcluster assignments
  result_df <- data.frame(
    label = all_labels,
    subcluster = NA,
    stringsAsFactors = FALSE
  )
  
  # Function to calculate within-cluster homogeneity
  calculate_homogeneity <- function(cluster_data) {
    if (nrow(cluster_data) <= 1) return(0)
    # Use average distance to centroid as homogeneity measure
    # Lower values indicate higher homogeneity
    cluster_center <- colMeans(cluster_data)
    distances <- apply(cluster_data, 1, function(row) {
      sqrt(sum((row - cluster_center)^2))
    })
    return(mean(distances) / sqrt(ncol(cluster_data)))
  }
  
  # Function to calculate cluster density
  calculate_density <- function(cluster_data) {
    if (nrow(cluster_data) <= 1) return(0)
    # Use average pairwise distance as density measure
    # Calculate pairwise distances
    dist_matrix <- as.matrix(dist(cluster_data))
    # Get average distance
    avg_dist <- sum(dist_matrix) / (nrow(cluster_data) * (nrow(cluster_data) - 1))
    # Invert so higher values mean higher density
    return(1 / (1 + avg_dist))
  }
  
  # Cut dendrogram at multiple heights
  heights <- seq(max_height, 30, by = -30)  # Adjust step as needed
  
  # Track already assigned elements
  assigned <- rep(FALSE, length(all_labels))
  
  # Counter for subcluster IDs
  next_cluster_id <- 1
  
  # Process each height level
  for (h in heights) {
    # Cut the dendrogram at current height
    cut_result <- cut(dend, h = h)
    
    # Check each resulting cluster
    if (!is.null(cut_result$lower)) {
      for (i in 1:length(cut_result$lower)) {
        branch_labels <- labels(cut_result$lower[[i]])
        
        # Skip if all elements already assigned
        if (all(assigned[match(branch_labels, all_labels)])) {
          next
        }
        
        # Get indices of elements in this branch
        branch_indices <- match(branch_labels, all_labels)
        
        # Get data for this branch
        if (all(branch_labels %in% rownames(data))) {
          branch_data <- data[branch_labels, ]
          
          # Skip clusters that are too small
          if (length(branch_labels) < min_cluster_size) {
            next
          }
          
          # Check homogeneity
          homogeneity <- calculate_homogeneity(branch_data)
          
          # Check density
          density <- calculate_density(branch_data)
          
          # If both criteria are met and not already assigned, assign to a new cluster
          if (homogeneity <= homogeneity_threshold && 
              density >= density_threshold &&
              !all(assigned[branch_indices])) {
            
            # Only assign elements that haven't been assigned yet
            unassigned_indices <- branch_indices[!assigned[branch_indices]]
            
            # Assign cluster ID to unassigned elements
            result_df$subcluster[unassigned_indices] <- next_cluster_id
            
            # Mark as assigned
            assigned[unassigned_indices] <- TRUE
            
            # Increment cluster counter
            next_cluster_id <- next_cluster_id + 1
          }
        }
      }
    }
  }
  
  # Assign any remaining unassigned elements to their own clusters
  remaining_unassigned <- which(!assigned)
  for (i in remaining_unassigned) {
    result_df$subcluster[i] <- next_cluster_id
    next_cluster_id <- next_cluster_id + 1
  }
  
  return(result_df)
}