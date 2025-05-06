## ---------------------------
##
## Script name: sequence-graph-adjmatrices1.R
##
## Purpose of script: Transform sequences into graphs and adjacency matrices
##
## Author: Saita, T. M.
##
## Date Created: 2025-04-28
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

## load up the packages we will need:  (uncomment as required)

library("Biostrings") #Sequências
library("seqinr") # Sequências
library("igraph") # Grafo
library("dplyr") # Manipulação de dados
library("pbmcapply") # Paralelização (com progress bar)

## ---------------------------

## load up our functions into memory

source("createNet.R") 
source("createAdjMatrix.R")
source("generateCombinations.R")

## ---------------------------


# Parametros
n_cores <- detectCores() - 1  # leave one core free
seq_name <- "HIV_6964.fasta" # arquivo .fasta com as sequências
seq <- readBStringSet(seq_name) # Leitura de sequências
seq_names <- names(seq) # Nome das sequências
lengths <- width(seq) # Tamanho das sequências

# Criar dataframe com informações das sequências
true_labels_hiv <- data.frame(name = seq_names, length = lengths, subtype = NA) # Completar a coluna com subtipo?
#write.csv(df, "HIV_6946dframe.csv", row.names = FALSE)

# Caso já tenha o arquivo pronto
true_labels_hiv <- read.csv("HIV_6946annota.csv", header = TRUE, sep = ";") # arquivo com informações das seq. - name, subtype, length

word <- 3
step <- 1
vertices_ordem <- c("AAA", "AAC", "AAG", "AAT", "ACA", "ACC", "ACG", "ACT", "AGA", "AGC", "AGG", "AGT", "ATA", "ATC", "ATG", "ATT", 
                    "CAA", "CAC", "CAG", "CAT", "CCA", "CCC", "CCG", "CCT", "CGA", "CGC", "CGG", "CGT", "CTA", "CTC", "CTG", "CTT",
                    "GAA", "GAC", "GAG", "GAT", "GCA", "GCC", "GCG", "GCT", "GGA", "GGC", "GGG", "GGT", "GTA", "GTC", "GTG", "GTT",
                    "TAA", "TAC", "TAG", "TAT", "TCA", "TCC", "TCG", "TCT", "TGA", "TGC", "TGG", "TGT", "TTA", "TTC", "TTG", "TTT")

#####################################################################################################################################

# Transformação de sequências em grafos - USAR APENAS QUANDO FOR NECESSÁRIO VISUALIZAR OS GRAFOS


graphs <- list()


# Gerar e armazenar os grafos e matrizes de adjacência de cada sequência
for (i in seq_along(seq)) {
  sequence <- strsplit(toString(seq[i]), split = '')[[1]]
  net <- createNet(word, step, sequence)
  graphs[[i]] <- net
}  
# Cominação para gerar os vértices


vertices <- generateCombinations(word) # Is the same of vertices_ordem????

#####################################################################################################################################

# Transformar sequências em matrizes de adjacência
char_seqs <- as.character(seq)
splitted_seq <- strsplit(char_seqs, split = "", fixed = TRUE)

adj_matrices <- pbmclapply(seq_along(splitted_seq), function(i) {
  # sequence <- strsplit(toString(seq[i]), split = '', fixed = TRUE)[[1]]
  createAdjMatrix(word, step, splitted_seq[[i]], vertices_ordem)
}, mc.cores = n_cores)

# Adicionar nome as matrizes de adjacência
name_adj_matrices <- setNames(adj_matrices, true_labels_hiv$name)
# Transformar matrizes de adjacência em vetores
vectorized_matrices <- lapply(adj_matrices, function(x) as.vector(t(x)))
