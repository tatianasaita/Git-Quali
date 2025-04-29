# Matriz de distâncias e clusterização hierárquica

# Aplicar métrica euclidiana nos vetores de matriz de adjacência e criar matriz de distâncias

dist_matriz_euclidean <- matrix(0, nrow=length(vectorized_matrices), ncol=length(vectorized_matrices))
for (i in 1:length(vectorized_matrices)) {
  for (j in 1:length(vectorized_matrices)) {
    dist_matriz_euclidean[i, j] <- dist(rbind(vectorized_matrices[[i]], vectorized_matrices[[j]]), method = "euclidean")
    dist_matriz_euclidean[j, i] <- dist_matriz_euclidean[i, j]  # Garantir simetria
  }
}
#####################################################################################################################################

# Clusterização hierárquica

hc_euclidean <- hclust(as.dist(dist_matriz_euclidean), method = "ward.D2")
dend_euc <- as.dendrogram(hc_euclidean)
leaf_order_euc <- order.dendrogram(dend_euc)
species_ordered_euc <- true_labels_hiv$subtype[leaf_order_euc]
unique_subtypes <- unique(true_labels_hiv$subtype)  
colors <- rainbow(length(unique_subtypes))  # Gera 8 cores diferentes  
species_colors_euc <- colors[as.numeric(factor(species_ordered_euc, levels = unique_subtypes))]  
dend_euc <- dend_euc %>% set("labels_colors", species_colors_euc)  

plot(dend_euc)
legend("topright", legend = unique_subtypes, col = colors, pch = 15, title = "Subtipos")
