library("caret")
library("dplyr")
library("randonForest") #rf
library("qcc") #SVM
library("xgboost") #XGBoost

# Conjunto de dados 
formatted_combinations <- c()  
for (first in vertices_ordem) {  
  for (second in vertices_ordem) {  
    formatted_combinations <- c(formatted_combinations, paste(first, second))  
  }  
}  

vectorized_df <- do.call(rbind, vectorized_matrices)
colnames(vectorized_df) <- formatted_combinations
rownames(vectorized_df) <- true_labels_hiv$name
vectorized_dframe <- as.data.frame(vectorized_df)
vectorized_dframe$subtype <- true_labels_hiv$subtype 



# Filtrar sequências para treino com mais de 800 pb
selected_elements800 <- true_labels_hiv %>%  
  filter(length >= 800)
summary_by_subtype <- selected_elements800 %>%  
  group_by(subtype) %>%  
  summarise(count = n())

# Para visualizar nome e tamanho das sequências selecionadas
seq800 <- vectorized_dframe[rownames(vectorized_dframe) %in% selected_elements800$name, ] %>%  
  mutate(length = true_labels_hiv$length[match(rownames(.), true_labels_hiv$name)]) %>%  
  mutate(name = rownames(.)) %>%  
  select(name, length, everything())

# Seleção de sequências para conjunto treino/teste
traintest_800 <- seq800 %>%  
  group_by(subtype) %>%                       
  arrange(desc(length)) %>%                    
  slice_sample(n = 180) %>%               
  ungroup() 
traintest_800 <- traintest_800 %>%  
  select(name, length, subtype, everything()) 

# Dataset para treino/teste - Mantendo apenas a coluna de subtype
traintest_800_df <- traintest_800[, !(colnames(traintest_800) %in% c("name", "length"))]
traintest_800_df <- traintest_800_df %>%  
  select(everything(), subtype)%>%
  select(-subtype, everything(), subtype)

# Dataset validação

vectorized_dframe$name <- rownames(vectorized_dframe)
not_in_traintest <- vectorized_dframe %>%  
  filter(!name %in% traintest_800$name)
validation_800 <- not_in_traintest %>%  
  group_by(subtype) %>%                         
  slice_sample(n = 20) %>% 
  ungroup()%>%  
  mutate(length = true_labels_hiv$length[match(name, true_labels_hiv$name)])%>%   
  select(name, subtype, length, everything())

# Visualizar o tamanho das sequências em boxplot
#boxplot(validation_800$length,  
#        main = "Boxplot do Tamanho das Sequências",  
#        ylab = "Comprimento das Sequências",  
#        col = "lightblue",  
#        border = "blue")

# Dataset para validação  
validation_800_df <- validation_800[, !(colnames(validation_800) %in% c("name", "length"))]
validation_800_df <- validation_800_df %>%  
  select(everything(), subtype)%>%
  select(-subtype, everything(), subtype)

########################################
#Incluir features 
set.seed(123) 

train_partition2 <- createDataPartition(traintest_800_df$subtype, p = 0.8, list = FALSE)
control <- trainControl(method = "cv", number = 5, savePredictions = "final")
traindata2 <- traintest_800_df[train_partition2, ]  
testdata2 <- traintest_800_df[-train_partition2, ]

# Selecionando apenas motifs com freq acima de 0.5
train2 <- traindata2[, c(motifs_select_vec, "subtype")] 
test2 <- testdata2[, c(motifs_select_vec, "subtype")]
valid2 <- validation_800_df[, c(motifs_select_vec, "subtype")]

model2 <- train(subtype ~ ., data = train2, method = "rf", trControl = control)

# Validação
pred_valid2 <- predict(model2, newdata = valid2 )
actuals_valid2 <- factor(valid2$subtype, levels = levels(factor(train2$subtype)))
confusion_matrix_valid2 <- confusionMatrix(pred_valid2, actuals_valid2)

# Feature Importance
importance_model2 <- varImp(model2, scale = FALSE)
importance_model2_df <- as.data.frame(importance_model2$importance)
importance_model2_df$Row_Column <- rownames(importance_model2_df)
importance_model2_df$Row_Column <- gsub("`", "", importance_model2_df$Row_Column)

# Inserir coluna de importance em all_motifs
all_motifs <- merge(all_motifs, 
                        importance_model2_df[,"Overall"],   
                        by = "Row_Column",   
                        all.x = TRUE)
all_motifs <- all_motifs %>%
  select(Row_Column, quant_clusters, total, Overall, everything())
#####################################
# SVM 

model2_svm <- train(subtype ~ ., data = train2, method = "svmRadial", trControl = control)
# Validação
pred_valid2_svm <- predict(model2_svm, newdata = valid2 )
actuals_valid2_svm <- factor(valid2$subtype, levels = levels(factor(train2$subtype)))
confusion_matrix_valid2_svm <- confusionMatrix(pred_valid2_svm, actuals_valid2_svm)
####################################
# XGBoost 
model2_xgb <- train(subtype ~ ., data = train2, method = "xgbTree", trControl = control)
# Validação
pred_valid2_xgb <- predict(model2_xgb, newdata = valid2 )
actuals_valid2_xgb <- factor(valid2$subtype, levels = levels(factor(train2$subtype)))
confusion_matrix_valid2_xgb <- confusionMatrix(pred_valid2_xgb, actuals_valid2_xgb)

importance_model2_xgb <- varImp(model2_xgb, scale = FALSE)
importance_model2_xgb_df <- as.data.frame(importance_model2_xgb$importance)
importance_model2_xgb_df$Row_Column <- rownames(importance_model2_xgb_df)
importance_model2_xgb_df$Row_Column <- gsub("`", "", importance_model2_xgb_df$Row_Column)

all_motifs <- merge(all_motifs,  
                    importance_model2_xgb_df[,c("Row_Column", "Overall")],
                    by = "Row_Column",   
                    all.x = TRUE)
names(all_motifs)[names(all_motifs) == "Overall"] <- "Import_xgb"
####################################

# Teste all motifs

model3 <- train(subtype ~ ., data = traindata2, method = "rf", trControl = control)
# Validação
pred_valid3 <- predict(model3, newdata = validation_800_df )
actuals_valid3 <- factor(validation_800_df$subtype, levels = levels(factor(traindata2$subtype)))
confusion_matrix_valid3 <- confusionMatrix(pred_valid3, actuals_valid3)

# Feature Importance
importance_model3 <- varImp(model3, scale = FALSE)
importance_model3_df <- as.data.frame(importance_model3$importance)
importance_model3_df$Row_Column <- rownames(importance_model3_df)
importance_model3_df$Row_Column <- gsub("`", "", importance_model3_df$Row_Column)

all_motifs <- merge(all_motifs,  
                        importance_model3_df[,c("Row_Column", "Overall")],
                        by = "Row_Column",   
                        all.x = TRUE)
names(all_motifs)[names(all_motifs) == "Overall"] <- "Import_todos"

 

all_motifs <- all_motifs %>%
  select(Row_Column, quant_cluster, total, Import_todos, Import_xgb, everything())

 