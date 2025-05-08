## ---------------------------
##
## Script name: automatic_clusterization.R
##
## Purpose of script: Tests to automatically clusterize the data
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
##   THIS IS A WORK IN PROGRESS VERSION
##   BE CAREFUL WITH THE CODE
##   IT IS NOT FINALIZED YET
## ---------------------------

## set working directory for Mac and PC

setwd("~/Documents/doc_tatiana/Git-Quali/")

## ---------------------------

options(scipen = 6, digits = 4) # For non-scientific notation

## ---------------------------

## load up the packages we will need:

library("dendextend")
library("ggplot2")
library("dplyr")
library("cluster")

## ---------------------------

## load up our functions into memory

source("dynamic_subclustering.R")
source("adaptive_height_clustering.R")
source("plot_dendrogram_clusters.R")

## ---------------------------
original_data <- dist_matriz_euclidean

## ---------------------------
# Method 1
# Method 1: Dynamic clustering
result1 <- dynamic_subclustering(dend_euc, original_data, 
                                 max_height = 700,
                                 homogeneity_threshold = 0.1,
                                 density_threshold = 0.05,
                                 min_cluster_size = 10)

# For example, using results from method 1
true_labels_hiv1 <- merge(true_labels_hiv, 
                          result1, 
                          by.x = "row_names", 
                          by.y = "label",
                          all.x = TRUE)
plot_dendrogram_clusters(dend_euc, result1)

## ---------------------------
# Method 2
# Method 2: Adaptive height clustering
result3 <- adaptive_height_clustering(dend_euc, original_data, 
                                      n_heights = 20,
                                      homogeneity_threshold = 0.1,
                                      min_cluster_size = 10)

# For example, using results from method 2
true_labels_hiv2 <- merge(true_labels_hiv,
                          result2, 
                          by.x = "row_names",
                          by.y = "label",
                          all.x = TRUE)
plot_dendrogram_clusters(dend_euc, result2)
