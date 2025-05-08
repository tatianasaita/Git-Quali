adaptive_height_clustering <- function(dend, data, n_heights = 20, 
                                       homogeneity_threshold = 0.3,
                                       min_cluster_size = 5) {
  
  # Get height range of dendrogram
  dend_heights <- heights_per_k.dendrogram(dend)
  max_height <- max(dend_heights)
  min_height <- min(dend_heights[dend_heights > 0])
  
  # Generate sequence of heights to examine
  # Use logarithmic scale to focus more on lower heights
  heights <- exp(seq(log(max_height), log(min_height), length.out = n_heights))
  
  # Store cluster counts at each height
  cluster_counts <- numeric(length(heights))
  
  # Calculate number of clusters at each height
  for (i in 1:length(heights)) {
    cut_result <- cut(dend, h = heights[i])
    if (!is.null(cut_result$lower)) {
      cluster_counts[i] <- length(cut_result$lower)
    } else {
      cluster_counts[i] <- 1
    }
  }
  
  # Calculate rate of change in cluster count
  cluster_change_rate <- c(0, diff(cluster_counts))
  
  # Identify heights with significant changes in cluster structure
  # These are good candidates for cutting points
  significant_heights <- heights[which(cluster_change_rate > mean(cluster_change_rate))]
  
  # If we didn't find significant heights, use a few spread across the range
  if (length(significant_heights) == 0) {
    significant_heights <- quantile(heights, probs = seq(0, 1, length.out = 5))
  }
  
  # Add some additional heights near the significant ones to refine clusters
  refined_heights <- sort(c(significant_heights, 
                            significant_heights * 0.8, 
                            significant_heights * 1.2))
  refined_heights <- refined_heights[refined_heights <= max_height & 
                                       refined_heights >= min_height]
  
  # Get all labels
  all_labels <- labels(dend)
  
  # Create result dataframe
  result_df <- data.frame(
    label = all_labels,
    subcluster = NA,
    stringsAsFactors = FALSE
  )
  
  # Function to validate cluster homogeneity
  validate_cluster <- function(cluster_labels) {
    if (length(cluster_labels) < min_cluster_size) return(FALSE)
    
    if (all(cluster_labels %in% rownames(data))) {
      cluster_data <- data[cluster_labels, ]
      
      # Calculate variance within cluster
      cluster_center <- colMeans(cluster_data)
      distances <- apply(cluster_data, 1, function(row) {
        sqrt(sum((row - cluster_center)^2))
      })
      homogeneity <- mean(distances) / sqrt(ncol(cluster_data))
      
      # Return TRUE if homogeneous enough
      return(homogeneity <= homogeneity_threshold)
    }
    return(FALSE)
  }
  
  # Track assigned elements
  assigned <- rep(FALSE, length(all_labels))
  
  # Next cluster ID
  next_cluster_id <- 1
  
  # Process each height
  for (h in sort(refined_heights, decreasing = TRUE)) {
    cut_result <- cut(dend, h = h)
    
    if (!is.null(cut_result$lower)) {
      for (i in 1:length(cut_result$lower)) {
        branch_labels <- labels(cut_result$lower[[i]])
        
        # Skip if all elements already assigned
        if (all(assigned[match(branch_labels, all_labels)])) {
          next
        }
        
        # Validate the cluster
        if (validate_cluster(branch_labels)) {
          # Get indices of unassigned elements in this branch
          branch_indices <- match(branch_labels, all_labels)
          unassigned_indices <- branch_indices[!assigned[branch_indices]]
          
          # Assign cluster ID
          result_df$subcluster[unassigned_indices] <- next_cluster_id
          
          # Mark as assigned
          assigned[unassigned_indices] <- TRUE
          
          # Increment cluster counter
          next_cluster_id <- next_cluster_id + 1
        }
      }
    }
  }
  
  # Assign any remaining unassigned elements
  remaining_unassigned <- which(!assigned)
  
  if (length(remaining_unassigned) > 0) {
    # For remaining elements, try to assign them to nearest existing cluster
    # or create new small clusters
    
    # If we can, group remaining points into small clusters
    if (length(remaining_unassigned) >= min_cluster_size) {
      # Use a lower height to find smaller clusters
      small_heights <- min_height * c(0.8, 0.6, 0.4, 0.2)
      
      for (h in small_heights) {
        if (all(assigned)) break
        
        cut_result <- cut(dend, h = h)
        
        if (!is.null(cut_result$lower)) {
          for (i in 1:length(cut_result$lower)) {
            branch_labels <- labels(cut_result$lower[[i]])
            branch_indices <- match(branch_labels, all_labels)
            
            # Only consider branches that contain unassigned elements
            if (any(!assigned[branch_indices])) {
              unassigned_in_branch <- branch_indices[!assigned[branch_indices]]
              
              if (length(unassigned_in_branch) >= min_cluster_size) {
                # Assign to new cluster
                result_df$subcluster[unassigned_in_branch] <- next_cluster_id
                assigned[unassigned_in_branch] <- TRUE
                next_cluster_id <- next_cluster_id + 1
              }
            }
          }
        }
      }
    }
    
    # Assign any remaining single points to their own clusters
    still_unassigned <- which(!assigned)
    for (i in still_unassigned) {
      result_df$subcluster[i] <- next_cluster_id
      next_cluster_id <- next_cluster_id + 1
    }
  }
  
  return(list(
    clusters_df = result_df,
    significant_heights = significant_heights
  ))
}