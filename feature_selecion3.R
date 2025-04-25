


# Função para obter e normalizar a matriz de adjacência para todos os subclusters  
obter_matrizes_normalizadas_todos_subclusters <- function(true_labels_hiv, name_adj_matrices) {  
  subclusters <- unique(true_labels_hiv$subcluster)  
  matrizes_normalizadas <- list()  
  for (subcluster_num in subclusters) {  
    indices_subcluster <- which(true_labels_hiv$subcluster == subcluster_num)  
    names_subcluster <- true_labels_hiv$name[indices_subcluster]  
    selected_adj_matrices <- name_adj_matrices[names_subcluster]  

    summed_matrix <- Reduce(`+`, selected_adj_matrices)  
    
    normalized_matrix <- NA  
    min_val <- min(summed_matrix, na.rm = TRUE)    
    max_val <- max(summed_matrix, na.rm = TRUE)  
    
    if (max_val > min_val) {  
      normalized_matrix <- (summed_matrix - min_val) / (max_val - min_val)  
    } else {  
      normalized_matrix <- summed_matrix  
    }  
 
    matrizes_normalizadas[[as.character(subcluster_num)]] <- normalized_matrix  
  }  
  
  matrizes_normalizadas <- matrizes_normalizadas[order(as.numeric(names(matrizes_normalizadas)))]  
  
  return(matrizes_normalizadas)  
}  

 summarized_normalized_matrices <- obter_matrizes_normalizadas_todos_subclusters(true_labels_hiv, name_adj_matrices)

 ###################################
 
 # Frequencia de motifs por cluster separadamente (pular essa etapa?)
 
 subclusters_df <- list()  
 
 for (i in seq_along(summarized_normalized_matrices)) {  
   
   subcluster_df <- as.data.frame(as.table(summarized_normalized_matrices[[i]]))  
   subcluster_df$Row_Column <- paste(subcluster_df$Var1, subcluster_df$Var2, sep = " ")  
   subcluster_df <- subcluster_df[, c("Row_Column", "Freq")]  
  
   colnames(subcluster_df)[colnames(subcluster_df) == "Freq"] <- "Value"  
   subclusters_df[[names(summarized_normalized_matrices)[i]]] <- subcluster_df  
 }  

 ######################################################################
 
 # Dataframe com a freq de motifs em todos os clusters
 
 all_motifs <- data.frame(Row_Column = character(), stringsAsFactors = FALSE)  
 for (i in seq_along(subclusters_df)) {   
   subcluster_name <- names(subclusters_df)[i]  
   subcluster_df1 <- subclusters_df[[i]] 
   colnames(subcluster_df1)[colnames(subcluster_df1) == "Value"] <- paste("Subtype_", subcluster_name, sep = "")  
   all_motifs <- merge(all_motifs, subcluster_df1, by = "Row_Column", all = TRUE)  
 }   
 all_motifs$total <- rowSums(all_motifs[, -1], na.rm = TRUE)
 all_motifs$quant_subtype <- rowSums(all_motifs[, !(names(all_motifs) %in% c("Row_Column", "total"))] != 0) 
 
 ###################################################
 # seleção de motifs exclusivos para cada subtipo
 indices_unicos <- which(all_motifs$quant_subtype == 1) # altere a quantidade de clusters  
 motifs_unicos <- all_motifs$Row_Column[indices_unicos]  
 name_vetor <- as.vector(motifs_unicos)  
 
 # seleção de motifs com frequencia maior que 0.5
 colunas_selecionadas <- names(all_motifs)[!(names(all_motifs) %in% c("Row_Column", "total", "quant_subtype"))]  
 all_motifs[, colunas_selecionadas] <- lapply(all_motifs[, colunas_selecionadas], function(x) ifelse(x > 0.5, x, NA))
 motifs_select <- all_motifs[rowSums(!is.na(all_motifs[, colunas_selecionadas])) > 0, ]  
 motifs_select_vec <- as.vector(motifs_select$Row_Column)
 
 #intersect(name_vetor,motifs_select_vec) # verificar se tem motifs em comum
 
 