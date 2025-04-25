# Packages

library("Biostrings") #Sequências
library("seqinr") # Sequências
library("igraph") # Grafo
library("dplyr") 


# Arquivos

seq_name <- "E:\\Download\\Tatiana-Scriba\\HIV_6964.fasta" # arquivo .fasta com as sequências

seq <- readBStringSet(seq_name) # Leitura de sequências
seq_names <- names(seq) # Nome das sequências
lengths <- width(seq) # Tamanho das sequências

# Criar dataframe com informações das sequências
#true_labels_hiv <- data.frame(name = seq_names, length = lengths, subtype = NA) # Completar a coluna com subtipo?
#write.csv(df, "HIV_6946dframe.csv", row.names = FALSE)

# Caso já tenha o arquivo pronto
true_labels_hiv <- read.csv("E:\\Download\\Tatiana-Scriba\\HIV_6946annota.csv", header = TRUE, sep = ";") # arquivo com informações das seq. - name, subtype, length


word <- 3
step <- 1
vertices_ordem <- c("AAA", "AAC", "AAG", "AAT", "ACA", "ACC", "ACG", "ACT", "AGA", "AGC", "AGG", "AGT", "ATA", "ATC", "ATG", "ATT", 
                    "CAA", "CAC", "CAG", "CAT", "CCA", "CCC", "CCG", "CCT", "CGA", "CGC", "CGG", "CGT", "CTA", "CTC", "CTG", "CTT",
                    "GAA", "GAC", "GAG", "GAT", "GCA", "GCC", "GCG", "GCT", "GGA", "GGC", "GGG", "GGT", "GTA", "GTC", "GTG", "GTT",
                    "TAA", "TAC", "TAG", "TAT", "TCA", "TCC", "TCG", "TCT", "TGA", "TGC", "TGG", "TGT", "TTA", "TTC", "TTG", "TTT")

#####################################################################################################################################

# Transformação de sequências em grafos - USAR APENAS QUANDO FOR NECESSÁRIO VISUALIZAR OS GRAFOS


graphs <- list()


createNet <- function(word, step, sequence){
  aux <- ""
  index <- 1
  position <- 0
  cont <- length(sequence)
  x <- 0
  vector <- c()
  
  while((index-1+(word*2)) <= cont){
    
    while(x < word){
      aux <- paste(aux, sequence[index], sep = "")
      x <- x + 1
      index <- index + 1
    }
    vector <- c(vector, aux)
    aux <- ""
    x <- 0
    while(x < word){
      aux <- paste(aux, sequence[index], sep = "")
      x <- x + 1
      index <- index + 1
    }
    vector <- c(vector, aux)
    aux <- ""
    x <- 0
    position <- position + step
    index <- position + 1
  }
  net <- graph <- graph(edges = vector, directed = TRUE)
  
  return(net)
}

# Gerar e armazenar os grafos e matrizes de adjacência de cada sequência
for (i in seq_along(seq)) {
  sequence <- strsplit(toString(seq[i]), split = '')[[1]]
  net <- createNet(word, step, sequence)
  graphs[[i]] <- net
}  
# Cominação para gerar os vértices
generate_combinations <- function(word) {
  nucleotides <- c("A", "T", "C", "G")
  combinations <- expand.grid(replicate(word, nucleotides, simplify = FALSE))
  sequences<- apply(combinations, 1, paste, collapse = "")
  return(sequences)
}

vertices <- generate_combinations(word)
#####################################################################################################################################

# Transformar sequências em matrizes de adjacência

adj_matrices <- list() 

createAdjMatrix <- function(word, step, sequence){
  cont <- length(sequence)
  vertices <- vertices_ordem
  adj_matrix <- matrix(0, nrow = length(vertices_ordem), ncol = length(vertices_ordem), dimnames = list(vertices_ordem, vertices_ordem))
  
  index <- 1
  while((index-1+(word*2)) <= cont){
    from_vertex <- paste(sequence[index:(index+word-1)], collapse = "")
    to_vertex <- paste(sequence[(index+word):(index+word*2-1)], collapse = "")
    
    adj_matrix[from_vertex, to_vertex] <- adj_matrix[from_vertex, to_vertex] + 1
    
    index <- index + step
  }
  
  return(adj_matrix)
}



for (i in seq_along(seq)) {
  sequence <- strsplit(toString(seq[i]), split = '')[[1]]
  adj_matrix <- createAdjMatrix(word, step, sequence)
  adj_matrices[[i]] <- adj_matrix
}

# Adicionar nome as matrizes de adjacência
name_adj_matrices <- setNames(adj_matrices, true_labels_hiv$name)
# Transformar matrizes de adjacência em vetores
vectorized_matrices <- lapply(adj_matrices, function(x) as.vector(t(x)))