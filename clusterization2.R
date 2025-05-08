## ---------------------------
##
## Script name: clusterization.R
##
## Purpose of script: Generate subclusters for the HIV dataset
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

## load up the packages we will need: 

library("dendextend")
library("ggplot2")
library("dplyr")

## ---------------------------


# Construção: Separei em grandes clusters, para obter os clusters 1 e 2 no corte h=700, para o restante foram separados os cluster no corte h=500. Depois foram subdivididos em clusters menores

# Analise de clusters - GENERALIZAR O MÉTODO PARA ENCONTRAR CLUSTER
true_labels_hiv$subcluster <- NA

branch1 <- cut(dend_euc, h = 700)$lower[[1]]
true_labels_hiv$subcluster[labels(cut(dend_euc, h = 700)$lower[[1]])] <- 1 #"C_1"

branch2 <- cut(dend_euc, h = 700)$lower[[2]]
true_labels_hiv$subcluster[labels(cut(dend_euc, h = 700)$lower[[2]])] <- 2  # "B_2"

branch3a <- cut(dend_euc, h = 500)$lower[[6]]
branch_3aux <- cut(dend_euc, h = 500)$lower[[7]]
branch3b <- cut(branch_3aux, h = 200)$lower[[1]]
branch3c <- cut(branch_3aux, h = 200)$lower[[2]]
branch3d <- cut(branch_3aux, h = 200)$lower[[3]]
branch3 <- branch3a %>% merge(branch3b) %>% merge(branch3c) %>% merge(branch3d)
true_labels_hiv$subcluster[labels(branch3)] <- 3  # "DGFA_3"

branch_4aux <- cut(branch_3aux, h = 200)$lower[[4]]
branch4 <- cut(branch_4aux, h = 120)$lower[[1]]
true_labels_hiv$subcluster[labels(branch4)] <- 4  # "J_4"


branch5 <- cut(branch_4aux, h = 180)$lower[[2]]
true_labels_hiv$subcluster[labels(branch5)] <- 5  # "HA_5"


branch6 <- cut(branch_3aux, h = 450)$lower[[2]]
true_labels_hiv$subcluster[labels(branch6)] <- 6  # "D_6"

branch7 <- cut(dend_euc, h = 500)$lower[[8]]
true_labels_hiv$subcluster[labels(branch7)] <- 7  # "G_7"

branch8 <- cut(dend_euc, h = 500)$lower[[9]]
true_labels_hiv$subcluster[labels(branch8)] <- 8  # "F1_8"

branch_9aux <- cut(dend_euc, h = 500)$lower[[10]]
branch9 <- cut(branch_9aux, h = 60)$lower[[1]]
true_labels_hiv$subcluster[labels(branch9)] <- 9  # "A_9"

branch10 <- cut(branch_9aux, h = 40)$lower[[4]]
true_labels_hiv$subcluster[labels(branch10)] <- 10  # "JH_10"

branch11a <- cut(branch_9aux, h = 40)$lower[[5]]
branch11b <- cut(branch_9aux, h = 40)$lower[[6]]
branch_11aux <- cut(branch_9aux, h = 40)$lower[[7]]
branch11c <- cut(branch_11aux, h = 40)$lower[[1]]
branch11 <- branch11a %>% merge(branch11b) %>% merge(branch11c)
true_labels_hiv$subcluster[labels(branch11 )] <- 11  # "HA_11"

branch12a <- cut(branch_11aux, h = 40)$lower[[2]] 
branch12b <- cut(branch_9aux, h = 40)$lower[[8]]
branch12c <- cut(branch_9aux, h = 40)$lower[[9]]
branch12d <- cut(branch_9aux, h = 40)$lower[[10]]
branch12e <- cut(branch_9aux, h = 40)$lower[[11]]
branch12f <- cut(branch_9aux, h = 40)$lower[[12]]
branch12g <- cut(branch_9aux, h = 40)$lower[[13]]
branch12 <- branch12a %>% merge(branch12b) %>% merge(branch12c) %>% merge(branch12d) %>%
 merge(branch12e) %>% merge(branch12f) %>% merge(branch12g)
true_labels_hiv$subcluster[labels(branch12 )] <- 12 #"AH_12"


branch13a <- cut(branch_9aux, h = 40)$lower[[14]] #J
branch13b <- cut(branch_9aux, h = 40)$lower[[15]] # H
branch13 <- branch13a %>% merge(branch13b)
true_labels_hiv$subcluster[labels(branch13 )] <- 13 #"HJ_13"

branch14a <- cut(branch_9aux, h = 40)$lower[[16]]
branch14b <- cut(branch_9aux, h = 40)$lower[[17]]
branch14c <- cut(branch_9aux, h = 40)$lower[[18]]
branch14d <- cut(branch_9aux, h = 40)$lower[[19]]
branch14 <- branch14a %>% merge(branch14b) %>% merge(branch14c) %>% merge(branch14d)
true_labels_hiv$subcluster[labels(branch14 )] <- 14 #"A_14"

branch15a <- cut(branch_9aux, h = 40)$lower[[20]]
branch15b <- cut(branch_9aux, h = 40)$lower[[21]]
branch15c <- cut(branch_9aux, h = 40)$lower[[22]]
branch15d <- cut(branch_9aux, h = 40)$lower[[23]]
branch15 <- branch15a %>% merge(branch15b) %>% merge(branch15c) %>% merge(branch15d)
true_labels_hiv$subcluster[labels(branch15 )] <- 15 #"H_15"

branch16a <- cut(branch_9aux, h = 80)$lower[[7]]
branch16b <- cut(branch_9aux, h = 80)$lower[[8]]
branch16 <- branch16a %>% merge(branch16b)
true_labels_hiv$subcluster[labels(branch16 )] <- 16 #"FGHA_16"

branch_17aux <- cut(branch_9aux, h = 80)$lower[[9]]
branch17 <- cut(branch_17aux, h = 50)$lower[[1]]
true_labels_hiv$subcluster[labels(branch17 )] <- 17 #"A_17"

branch_18aux <- cut(branch_17aux, h = 50)$lower[[2]]
branch18a <- cut(branch_18aux, h = 40)$lower[[1]]
branch18b <- cut(branch_18aux, h = 40)$lower[[2]]
branch18 <- branch18a %>% merge(branch18b)
true_labels_hiv$subcluster[labels(branch18 )] <- 18 #"H_18"

branch19a <- cut(branch_18aux, h = 40)$lower[[3]] #J
branch_19aux <- cut(branch_9aux, h = 80)$lower[[10]]
branch19b <- cut(branch_19aux, h = 40)$lower[[1]]
branch19c <- cut(branch_19aux, h = 40)$lower[[2]]
branch19 <- branch19a %>% merge(branch19b)%>% merge(branch19c)
true_labels_hiv$subcluster[labels(branch19 )] <- 19 #"JH_19"

branch20 <- cut(branch_19aux, h = 40)$lower[[3]]
true_labels_hiv$subcluster[labels(branch20 )] <- 20 #"HJ_20"

branch_21aux <- cut(branch_9aux, h = 80)$lower[[11]]
branch21 <- cut(branch_21aux, h = 60)$lower[[1]]
true_labels_hiv$subcluster[labels(branch21 )] <- 21 #"A_21"

branch22a <- cut(branch_21aux, h = 60)$lower[[2]] #HJ
branch22b <- cut(branch_9aux, h = 80)$lower[[12]] #H
branch22c <- cut(branch_9aux, h = 80)$lower[[13]] # HJ
branch22d <- cut(branch_9aux, h = 80)$lower[[14]] #H
branch22 <- branch22a %>% merge(branch22b) %>% merge(branch22c) %>% merge(branch22d)
true_labels_hiv$subcluster[labels(branch22 )] <- 22 #"HJ_22"

branch23 <- cut(branch_9aux, h = 80)$lower[[15]] #J
true_labels_hiv$subcluster[labels(branch23 )] <- 23 #"J_23"

branch24 <- cut(branch_9aux, h = 80)$lower[[16]]
true_labels_hiv$subcluster[labels(branch24 )] <- 24 #"HJ_24"

branch_25aux <- cut(branch_9aux, h = 80)$lower[[17]]
branch25 <- cut(branch_25aux, h = 40)$lower[[1]]
true_labels_hiv$subcluster[labels(branch25 )] <- 25 #"J_25"

branch26 <- cut(branch_25aux, h = 50)$lower[[2]]
true_labels_hiv$subcluster[labels(branch26 )] <- 26 #"AH_26"

branch27 <- cut(branch_25aux, h = 60)$lower[[2]]
true_labels_hiv$subcluster[labels(branch27 )] <- 27 #"HJ_27"

branch_28aux <- cut(dend_euc, h = 500)$lower[[11]]
branch28 <- cut(branch_28aux, h = 200)$lower[[1]]
true_labels_hiv$subcluster[labels(branch28 )] <- 28 #"G_28"
branch29 <- cut(branch_28aux, h = 200)$lower[[2]]
true_labels_hiv$subcluster[labels(branch29 )] <- 29 #"F_29"
branch30 <- cut(branch_28aux, h = 100)$lower[[3]]
true_labels_hiv$subcluster[labels(branch30 )] <- 30 #"G_30"
branch31 <- cut(branch_28aux, h = 90)$lower[[4]]
true_labels_hiv$subcluster[labels(branch31 )] <- 31 #"A_31"
branch32 <- cut(branch_28aux, h = 90)$lower[[5]]
true_labels_hiv$subcluster[labels(branch32 )] <- 32 #"HJ_32"

branch_33aux <- cut(dend_euc, h = 500)$lower[[12]]
branch33 <- cut(branch_33aux, h = 100)$lower[[1]]
true_labels_hiv$subcluster[labels(branch33 )] <- 33 #"AH_33"
branch34a <- cut(branch_33aux, h = 80)$lower[[2]]
branch34b <- cut(branch_33aux, h = 80)$lower[[3]]
branch34c <- cut(branch_33aux, h = 80)$lower[[4]]
branch34 <- branch34a %>% merge(branch34b)%>% merge(branch34c)
true_labels_hiv$subcluster[labels(branch34 )] <- 34 #"HJ_34"
branch35 <- cut(branch_33aux, h = 80)$lower[[5]]
true_labels_hiv$subcluster[labels(branch35 )] <- 35 #"JA_35"
branch36 <- cut(branch_33aux, h = 100)$lower[[4]]
true_labels_hiv$subcluster[labels(branch36 )] <- 36 #"H_36"
branch37 <- cut(branch_33aux, h = 165)$lower[[3]]
true_labels_hiv$subcluster[labels(branch37 )] <- 37 #"A_37"

branch_38aux <- cut(dend_euc, h = 500)$lower[[13]]
branch38 <- cut(branch_38aux, h = 300)$lower[[1]]
true_labels_hiv$subcluster[labels(branch38 )] <- 38 #"F_38"
branch39 <- cut(branch_38aux, h = 100)$lower[[6]]
true_labels_hiv$subcluster[labels(branch39 )] <- 39 #"AG_39"
branch40 <- cut(branch_38aux, h = 100)$lower[[7]]
true_labels_hiv$subcluster[labels(branch40 )] <- 40 #"HJ_39"
branch41 <- cut(branch_38aux, h = 210)$lower[[3]]
true_labels_hiv$subcluster[labels(branch41 )] <- 41 #"G_41"

branch_42aux <- cut(dend_euc, h = 500)$lower[[14]]
branch42 <- cut(branch_42aux, h = 200)$lower[[1]]
true_labels_hiv$subcluster[labels(branch42 )] <- 42 #"A_42"
branch43 <- cut(branch_42aux, h = 150)$lower[[2]]
true_labels_hiv$subcluster[labels(branch43 )] <- 43 #"F_43"

branch44a <- cut(branch_42aux, h = 110)$lower[[3]]
branch44b <- cut(branch_42aux, h = 80)$lower[[6]]
branch44 <- branch44a %>% merge(branch44b)
true_labels_hiv$subcluster[labels(branch44 )] <- 44 #"DAH_44"
branch45 <- cut(branch_42aux, h = 80)$lower[[7]]
true_labels_hiv$subcluster[labels(branch45 )] <- 45 #"G_45"

branch_46aux <- cut(dend_euc, h = 500)$lower[[15]]
branch46a <- cut(branch_46aux, h = 250)$lower[[1]]
branch46b <- cut(branch_46aux, h = 250)$lower[[2]]
branch46 <- branch46a %>% merge(branch46b)
true_labels_hiv$subcluster[labels(branch46 )] <- 46 #"D_46"
branch47 <- cut(branch_46aux, h = 250)$lower[[3]]
true_labels_hiv$subcluster[labels(branch47 )] <- 47 #"G_47"
branch48 <- cut(branch_46aux, h = 275)$lower[[4]]
true_labels_hiv$subcluster[labels(branch48 )] <- 48 #"DG_48"

branch_49aux <- cut(dend_euc, h = 500)$lower[[16]]
branch49 <- cut(branch_49aux, h = 370)$lower[[1]]
true_labels_hiv$subcluster[labels(branch49 )] <- 49 #"D_49"
branch50 <- cut(branch_49aux, h = 300)$lower[[2]]
true_labels_hiv$subcluster[labels(branch50 )] <- 50 #"G_50"
branch51 <- cut(branch_49aux, h = 320)$lower[[3]]
true_labels_hiv$subcluster[labels(branch51 )] <- 51 #"D_51"

branch_52aux <- cut(dend_euc, h = 500)$lower[[17]]
branch52 <- cut(branch_52aux, h = 320)$lower[[1]]
true_labels_hiv$subcluster[labels(branch52 )] <- 52 #"G_52"
branch53 <- cut(branch_52aux, h = 320)$lower[[2]]
true_labels_hiv$subcluster[labels(branch53 )] <- 53 #"A_53"
branch54 <- cut(branch_52aux, h = 320)$lower[[3]]
true_labels_hiv$subcluster[labels(branch54 )] <- 54 #"GAHJ_54"

branch55 <- cut(dend_euc, h = 500)$lower[[18]]
true_labels_hiv$subcluster[labels(branch55 )] <- 55 #"D_55"

branch_56aux <- cut(dend_euc, h = 500)$lower[[19]]
branch56 <- cut(branch_56aux, h = 400)$lower[[1]]
true_labels_hiv$subcluster[labels(branch56 )] <- 56 #"H_56"
branch57 <- cut(branch_56aux, h = 400)$lower[[2]]
true_labels_hiv$subcluster[labels(branch57 )] <- 57 #"FG_57"


write.csv(true_labels_hiv, file = "E:\\Download\\Tatiana-Scriba\\HIV_6946clusters.csv", row.names = FALSE)
