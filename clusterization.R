library("dendextend")
library("ggplot2")
library("dplyr")

# Analise de clusters - GENERALIZAR O MÉTODO PARA ENCONTRAR CLUSTER

true_labels_hiv$subcluster1 <- NA

true_labels_hiv$subcluster1[labels(cut(dend_euc, h = 700)$lower[[1]])] <- 1 #"C_1"

true_labels_hiv$subcluster1[labels(cut(dend_euc, h = 700)$lower[[2]])] <- 2  # "B_2"

branch3a <- cut(dend_euc, h = 500)$lower[[6]]
branch_3aux <- cut(dend_euc, h = 500)$lower[[7]]
branch3b <- cut(branch_3aux, h = 200)$lower[[1]]
branch3c <- cut(branch_3aux, h = 200)$lower[[2]]
branch3d <- cut(branch_3aux, h = 200)$lower[[3]]
true_labels_hiv$subcluster1[labels(branch3a %>% merge(branch3b) %>% merge(branch3c) %>% merge(branch3d))] <- 3  # "D_3"

branch_4aux <- cut(branch_3aux, h = 200)$lower[[4]]
branch4 <- cut(branch_4aux, h = 120)$lower[[1]]
true_labels_hiv$subcluster1[labels(branch4)] <- 4  # "J_4"

branch5 <- cut(branch_4aux, h = 180)$lower[[2]]
true_labels_hiv$subcluster1[labels(branch5)] <- 5  # "HA_5"


branch6 <- cut(branch_3aux, h = 450)$lower[[2]]
true_labels_hiv$subcluster1[labels(branch6)] <- 6  # "D_6"

branch7 <- cut(dend_euc, h = 500)$lower[[8]]
true_labels_hiv$subcluster1[labels(branch7)] <- 7  # "G_7"

branch8 <- cut(dend_euc, h = 500)$lower[[9]]
true_labels_hiv$subcluster1[labels(branch8)] <- 8  # "F1_8"

branch_9aux <- cut(dend_euc, h = 500)$lower[[10]]
branch9 <- cut(branch_9aux, h = 60)$lower[[1]]
true_labels_hiv$subcluster1[labels(branch9)] <- 9  # "A_9"

branch10 <- cut(branch_9aux, h = 40)$lower[[4]]
true_labels_hiv$subcluster1[labels(branch10)] <- 10  # "JH_10"

branch11a <- cut(branch_9aux, h = 40)$lower[[5]]
branch11b <- cut(branch_9aux, h = 40)$lower[[6]]
branch_11aux <- cut(branch_9aux, h = 40)$lower[[7]]
branch11c <- cut(branch_11aux, h = 40)$lower[[1]]
true_labels_hiv$subcluster1[labels(branch11a %>% merge(branch11b) %>% merge(branch11c) )] <- 11  # "HA_11"

branch12a <- cut(branch_9aux, h = 50)$lower[[3]]

branch_13 <- cut(branch_9aux, h = 40)$lower[[8]]
branch_14 <- cut(branch_9aux, h = 40)$lower[[9]]

branch6_A2_5 <-  branch6_3A2 %>%  
  merge(branch6_A_2) %>%
  merge(branch6_A_3) %>%  
  merge(branch6_A_4) %>%  
  merge(branch6_A_5) 
leaf_labels_6_A2_5 <- labels(branch6_A2_5)
sequences_in_branch6_A2_5 <- true_labels_hiv[leaf_labels_6_A2_5, ]
true_labels_hiv$subcluster[leaf_labels_6_A2_5] <- 12 #"AH_12"

branch6_HeJ4 <-branch6_h60$lower[[8]]
leaf_labels_6_HeJ4 <- labels(branch6_HeJ4)
sequences_in_branch6_HeJ4 <- true_labels_hiv[leaf_labels_6_HeJ4, ]
true_labels_hiv$subcluster[leaf_labels_6_HeJ4] <- 13 # "HJ_13"

branch6_A6_8 <- branch6_A6 %>%  
  merge(branch6_A7) %>%  
  merge(branch6_A8) 
leaf_labels_6_A6_8 <- labels(branch6_A6_8)
sequences_in_branch6_A6_8 <- true_labels_hiv[ leaf_labels_6_A6_8, ]
true_labels_hiv$subcluster[leaf_labels_6_A6_8] <- 14 #"A_14"

branch6_H1e2 <- branch6_H1 %>%  
  merge(branch6_H2) 
leaf_labels_6_H1e2 <- labels(branch6_H1e2)
sequences_in_branch6_H1e2 <- true_labels_hiv[ leaf_labels_6_H1e2, ]
true_labels_hiv$subcluster[leaf_labels_6_H1e2] <- 15 # "H_15"

branch6_F1_5 <- branch6_F1 %>%  
  merge(branch6_F2) %>%  
  merge(branch6_F3) %>%
  merge(branch6_F4) %>%
  merge(branch6_F5)
leaf_labels_6_F1_5 <- labels(branch6_F1_5)
sequences_in_branch6_F1_5 <- true_labels_hiv[ leaf_labels_6_F1_5, ]
true_labels_hiv$subcluster[leaf_labels_6_F1_5] <- 16 #"FHGA_16"

branch6_A9 <-branch6_h60$lower[[19]]
leaf_labels_6_A9 <- labels(branch6_A9)
sequences_in_branch6_A9 <- true_labels_hiv[ leaf_labels_6_A9, ]
true_labels_hiv$subcluster[leaf_labels_6_A9] <- 17 #"A_17"

branch6_20H3e4 <- branch6_20H3 %>%  
  merge(branch6_20H4)
leaf_labels_6_20H3e4 <- labels(branch6_20H3e4)
sequences_in_branch6_20H3e4 <- true_labels_hiv[leaf_labels_6_20H3e4, ]
true_labels_hiv$subcluster[leaf_labels_6_20H3e4] <- 18 #"H_18"

branch6_J20e21 <- branch6_20J %>%
  merge(branch6_21J1e2) 
leaf_labels_6_J20e21 <- labels(branch6_J20e21)
sequences_in_branch6_J20e21 <- true_labels_hiv[ leaf_labels_6_J20e21, ]
true_labels_hiv$subcluster[leaf_labels_6_J20e21] <- 19 #"JH_19"

branch6_21H <- branch6_J11_h40$lower[[3]]
leaf_labels_6_21H <- labels(branch6_21H)
sequences_in_branch6_21H <- true_labels_hiv[ leaf_labels_6_21H, ]
true_labels_hiv$subcluster[leaf_labels_6_21H] <- 20 #"HJ_20"

branch6_A11 <-branch6_h60$lower[[22]]
leaf_labels_6_A11 <- labels(branch6_A11)
sequences_in_branch6_A11 <- true_labels_hiv[ leaf_labels_6_A11, ]
true_labels_hiv$subcluster[leaf_labels_6_A11] <- 21 #"A_21"

branch6_H7a12 <- branch6_H6 %>%  
  merge(branch6_H7) %>%
  merge(branch6_H8) %>%
  merge(branch6_H9) %>%
  merge(branch6_H10) %>%
  merge(branch6_H11) %>%
  merge(branch6_H12)
leaf_labels_6_H7a12 <- labels(branch6_H7a12)
sequences_in_branch6_H7a12 <- true_labels_hiv[leaf_labels_6_H7a12, ]
true_labels_hiv$subcluster[leaf_labels_6_H7a12] <- 22 #"HJ_22"

branch6_J4 <-branch6_h60$lower[[30]]
leaf_labels_6_J4 <- labels(branch6_J4)
sequences_in_branch6_J4 <- true_labels_hiv[leaf_labels_6_J4, ]
true_labels_hiv$subcluster[leaf_labels_6_J4] <- 23 #"J_23"


branch6_H13 <-branch6_h60$lower[[31]]
leaf_labels_6_H13 <- labels(branch6_H13)
sequences_in_branch6_H13 <- true_labels_hiv[leaf_labels_6_H13, ]
true_labels_hiv$subcluster[leaf_labels_6_H13] <- 24 #"HJ_24"


branch6_32J <- branch6_32_h50$lower[[1]]
leaf_labels_6_32J <- labels(branch6_32J)
sequences_in_branch6_32J <- true_labels_hiv[leaf_labels_6_32J, ]
true_labels_hiv$subcluster[leaf_labels_6_32J] <- 25 #"J_25"

branch6_32A <- branch6_32_h50$lower[[2]]
leaf_labels_6_32A <- labels(branch6_32A)
sequences_in_branch6_32A <- true_labels_hiv[leaf_labels_6_32A, ]
true_labels_hiv$subcluster[leaf_labels_6_32A] <- 26 #"AH_26"

branch6_H15 <-branch6_h60$lower[[33]]
leaf_labels_6_H15 <- labels(branch6_H15)
sequences_in_branch6_H15 <- true_labels_hiv[leaf_labels_6_H15, ]
true_labels_hiv$subcluster[leaf_labels_6_H15] <- 27 #"HJ_27"


branch7_G1 <- branch7_h200$lower[[1]]
leaf_labels_7_G1 <- labels(branch7_G1)
sequences_in_branch7_G1 <- true_labels_hiv[leaf_labels_7_G1, ]
true_labels_hiv$subcluster[leaf_labels_7_G1] <- 28 #"G_28"

branch7_F1 <- branch7_h200$lower[[2]]
leaf_labels_7_F1 <- labels(branch7_F1)
sequences_in_branch7_F1 <- true_labels_hiv[leaf_labels_7_F1, ]
true_labels_hiv$subcluster[leaf_labels_7_F1] <- 29 #"F1_29"

branch7_G2 <- branch7_A1_h80$lower[[1]]
leaf_labels_7_G2 <- labels(branch7_G2)
sequences_in_branch7_G2 <- true_labels_hiv[leaf_labels_7_G2, ]
true_labels_hiv$subcluster[leaf_labels_7_G2] <- 30 #"G_30"

branch7_A2 <- branch7_A1_h80$lower[[2]]
leaf_labels_7_A2 <- labels(branch7_A2)
sequences_in_branch7_A2 <- true_labels_hiv[leaf_labels_7_A2, ]
true_labels_hiv$subcluster[leaf_labels_7_A2] <- 31 #"A_31"

branch7_H1 <- branch7_A1_h80$lower[[3]]
leaf_labels_7_H1 <- labels(branch7_H1)
sequences_in_branch7_H1 <- true_labels_hiv[leaf_labels_7_H1, ]
true_labels_hiv$subcluster[leaf_labels_7_H1] <- 32 #"HJ_32"

branch7_A4 <- branch7_A3_h90$lower[[1]]
leaf_labels_7_A4 <- labels(branch7_A4)
sequences_in_branch7_A4 <- true_labels_hiv[leaf_labels_7_A4, ]
true_labels_hiv$subcluster[leaf_labels_7_A4] <- 33 #"AH_33"

branch7_H2eH3 <- branch7_H2 %>%   
  merge( branch7_H3)
leaf_labels_7_H2eH3 <- labels(branch7_H2eH3)
sequences_in_branch7_H2eH3 <- true_labels_hiv[leaf_labels_7_H2eH3, ]
true_labels_hiv$subcluster[leaf_labels_7_H2eH3] <- 34 #"HJ_34"

branch7_J1 <- branch7_A3_h90$lower[[4]]
leaf_labels_7_J1 <- labels(branch7_J1)
sequences_in_branch7_J1 <- true_labels_hiv[leaf_labels_7_J1, ]
true_labels_hiv$subcluster[leaf_labels_7_J1] <- 35 #"JA_35"

branch7_H4 <- branch7_4_h180$lower[[1]]
leaf_labels_7_H4 <- labels(branch7_H4)
sequences_in_branch7_H4 <- true_labels_hiv[leaf_labels_7_H4, ]
true_labels_hiv$subcluster[leaf_labels_7_H4] <- 36 #"H_36"

branch7_A5 <- branch7_4_h180$lower[[2]]
leaf_labels_7_A5 <- labels(branch7_A5)
sequences_in_branch7_A5 <- true_labels_hiv[leaf_labels_7_A5, ]
true_labels_hiv$subcluster[leaf_labels_7_A5] <- 37 #"A_37"

branch7_F2 <- branch7_h200$lower[[6]]
leaf_labels_7_F2 <- labels(branch7_F2)
sequences_in_branch7_F2 <- true_labels_hiv[leaf_labels_7_F2, ]
true_labels_hiv$subcluster[leaf_labels_7_F2] <- 38 #"F1_38"


branch7_A6 <- branch7_7_h100$lower[[1]]
leaf_labels_7_A6 <- labels(branch7_A6)
sequences_in_branch7_A6 <- true_labels_hiv[leaf_labels_7_A6, ]
true_labels_hiv$subcluster[leaf_labels_7_A6] <- 39 #"AG_39"


branch7_H5 <- branch7_7_h100$lower[[2]]
leaf_labels_7_H5 <- labels(branch7_H5)
sequences_in_branch7_H5 <- true_labels_hiv[leaf_labels_7_H5, ]
true_labels_hiv$subcluster[leaf_labels_7_H5] <- 40 #"HJ_40"


branch7_G3 <- branch7_h200$lower[[8]]
leaf_labels_7_G3 <- labels(branch7_G3)
sequences_in_branch7_G3 <- true_labels_hiv[leaf_labels_7_G3, ]
true_labels_hiv$subcluster[leaf_labels_7_G3] <- 41 #"G_41"


branch8e9_1A <- branch8e9_1_h130$lower[[1]]
leaf_labels_8e9_1A <- labels(branch8e9_1A)
sequences_in_branch8e9_1A <- true_labels_hiv[leaf_labels_8e9_1A, ]
true_labels_hiv$subcluster[leaf_labels_8e9_1A] <- 42 #"A_42"


branch8e9_1F <- branch8e9_1_h130$lower[[2]]
leaf_labels_8e9_1F <- labels(branch8e9_1F)
sequences_in_branch8e9_1F <- true_labels_hiv[leaf_labels_8e9_1F, ]
true_labels_hiv$subcluster[leaf_labels_8e9_1F] <- 43 #"F1_43"


branch8e9_1DeD1 <- branch8e9_1D %>%  
  merge(branch8e9_1D1)
leaf_labels_8e9_1DeD1 <- labels(branch8e9_1DeD1)
sequences_in_branch8e9_1DeD1 <- true_labels_hiv[leaf_labels_8e9_1DeD1, ]
true_labels_hiv$subcluster[leaf_labels_8e9_1DeD1] <- 44 #"DAH_44"


branch8e9_1G <- branch8e9_1G_h90$lower[[2]]
leaf_labels_8e9_1G <- labels(branch8e9_1G)
sequences_in_branch8e9_11G <- true_labels_hiv[leaf_labels_8e9_1G, ]
true_labels_hiv$subcluster[leaf_labels_8e9_1G] <- 45 #"G_45"


branch8e9_2D1e2 <- branch8e9_2D1 %>%  
  merge(branch8e9_2D2)
leaf_labels_8e9_2D1e2 <- labels(branch8e9_2D1e2)
sequences_in_branch8e9_2D1e2 <- true_labels_hiv[leaf_labels_8e9_2D1e2, ]
true_labels_hiv$subcluster[leaf_labels_8e9_2D1e2] <- 46 #"DG_46"


branch8e9_2G1 <- branch8e9_2G_h270$lower[[1]]
leaf_labels_8e9_2G1 <- labels(branch8e9_2G1)
sequences_in_branch8e9_2G1 <- true_labels_hiv[leaf_labels_8e9_2G1, ]
true_labels_hiv$subcluster[leaf_labels_8e9_2G1] <- 47 #"G_47"

branch8e9_2D3 <- branch8e9_2G_h270$lower[[2]]
leaf_labels_8e9_2D3 <- labels(branch8e9_2D3)
sequences_in_branch8e9_2D3 <- true_labels_hiv[leaf_labels_8e9_2D3, ]
true_labels_hiv$subcluster[leaf_labels_8e9_2D3] <- 48 #"D_48"


branch8e9_3D4 <- branch8e9_3D_h320$lower[[1]]
leaf_labels_8e9_3D4 <- labels(branch8e9_3D4)
sequences_in_branch8e9_3D4 <- true_labels_hiv[leaf_labels_8e9_3D4, ]
true_labels_hiv$subcluster[leaf_labels_8e9_3D4] <- 49 #"D_49"


branch8e9_3G2 <- branch8e9_3D_h320$lower[[2]]
leaf_labels_8e9_3G2 <- labels(branch8e9_3G2)
sequences_in_branch8e9_3G2 <- true_labels_hiv[leaf_labels_8e9_3G2, ]
true_labels_hiv$subcluster[leaf_labels_8e9_3G2] <- 50 #"G_50"


branch8e9_3D5 <- branch8e9_3D_h320$lower[[3]]
leaf_labels_8e9_3D5 <- labels(branch8e9_3D5)
sequences_in_branch8e9_3D5 <- true_labels_hiv[leaf_labels_8e9_3D5, ]
true_labels_hiv$subcluster[leaf_labels_8e9_3D5] <- 51 #"D_51"

branch10_G1 <- branch10_G_h330$lower[[1]]
leaf_labels_10_G1 <- labels(branch10_G1)
sequences_in_branch10_G1 <- true_labels_hiv[leaf_labels_10_G1, ]
true_labels_hiv$subcluster[leaf_labels_10_G1] <- 52 #"G_52"


branch10_A <- branch10_G_h330$lower[[2]]
leaf_labels_10_A <- labels(branch10_A)
sequences_in_branch10_A <- true_labels_hiv[leaf_labels_10_A, ]
true_labels_hiv$subcluster[leaf_labels_10_A] <- 53 #"A_53"


branch10_G2 <- branch10_G_h330$lower[[3]]
leaf_labels_10_G2 <- labels(branch10_G2)
sequences_in_branch10_G2 <- true_labels_hiv[leaf_labels_10_G2, ]
true_labels_hiv$subcluster[leaf_labels_10_G2] <- 54 #"GHJ_54"


branch10_D <- as.dendrogram(branch10_h500$lower[[2]])
leaf_labels_10_D <- labels(branch10_D)
sequences_in_branch10_D <- true_labels_hiv[leaf_labels_10_D, ]
true_labels_hiv$subcluster[leaf_labels_10_D] <- 55 #"D_55"

branch10_H <- branch10_F_h380$lower[[1]]
leaf_labels_10_H <- labels(branch10_H)
sequences_in_branch10_H <- true_labels_hiv[leaf_labels_10_H, ]
true_labels_hiv$subcluster[leaf_labels_10_H] <- 56 #"H_56"


branch10_F1 <- branch10_F_h380$lower[[2]]
leaf_labels_10_F1 <- labels(branch10_F1)
sequences_in_branch10_F1 <- true_labels_hiv[leaf_labels_10_F1, ]
true_labels_hiv$subcluster[leaf_labels_10_F1] <- 57 #"F1_57"

write.csv(true_labels_hiv, file = "E:\\Download\\Tatiana-Scriba\\HIV_6946clusters.csv", row.names = FALSE)