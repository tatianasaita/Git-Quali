## ---------------------------
##
## Script name: create_subclusters.R
##
## Purpose of script: Generate subclusters from a hierarchical clustering object
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

## load up our functions into memory

source("assign_subclusters.R")

## ---------------------------

labels_hiv <- true_labels_hiv$subtype
clusters_hiv <- assign_subclusters(dend_euc,
                                   labels_hiv,
                                   hom_thresh = 0.9,
                                   min_size = 10)
table(clusters_hiv, labels_hiv)