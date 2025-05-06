## ---------------------------
##
## Script name: dist_matrix_dendrogram.R
##
## Purpose of script: Generate distance matrix and hierarchical clustering dendrogram
##
## Author: Saita, T. M.
##
## Date Created: 2025-05-06
##
## Copyright (c) Saita, T. M., 2025
## Email: tatisaita@gmail.com
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

## set working directory for Mac and PC

setwd("~/Documents/doc_tatiana/Git-Quali/")

## ---------------------------

options(scipen = 6, digits = 4) # For non-scientific notation

## ---------------------------

library("dendextend") # Dendrogramas
library("stats") # Explícito para dist

## ---------------------------
##### Gerando matriz de distância
data_matrix <- do.call(rbind, vectorized_matrices)
dist_matriz_euclidean <- as.matrix(dist(data_matrix,
                                        method = "euclidean"))

##### Gerando dendrograma

hc_euclidean <- hclust(as.dist(dist_matriz_euclidean),
                       method = "ward.D2")
dend_euc <- as.dendrogram(hc_euclidean)
leaf_order_euc <- order.dendrogram(dend_euc)
species_ordered_euc <- true_labels_hiv$subtype[leaf_order_euc]
unique_subtypes <- unique(true_labels_hiv$subtype)  
colors <- rainbow(length(unique_subtypes))  # Gera 8 cores diferentes  
species_colors_euc <- colors[as.numeric(factor(species_ordered_euc,
                                               levels = unique_subtypes))]  
dend_euc <- dend_euc %>% set("labels_colors",
                             species_colors_euc)  

plot(dend_euc)
legend("topright", legend = unique_subtypes,
       col = colors,
       pch = 15,
       title = "Subtipos")
