createAdjMatrix <- function(word, step, sequence, vertices_ordem) {
  n <- length(sequence)
  adj_matrix <- matrix(0L, nrow = length(vertices_ordem), ncol = length(vertices_ordem),
                       dimnames = list(vertices_ordem, vertices_ordem))
  
  max_index <- n - word * 2 + 1
  if (max_index <= 0) return(adj_matrix)
  
  indices <- seq(1, max_index, by = step)
  
  from_seq <- vapply(indices, function(i) paste(sequence[i:(i + word - 1)], collapse = ""), character(1))
  to_seq   <- vapply(indices, function(i) paste(sequence[(i + word):(i + word * 2 - 1)], collapse = ""), character(1))
  
  for (k in seq_along(from_seq)) {
    adj_matrix[from_seq[k], to_seq[k]] <- adj_matrix[from_seq[k], to_seq[k]] + 1L
  }
  
  adj_matrix
}

