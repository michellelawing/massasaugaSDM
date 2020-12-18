###################
###--- Folding ----
###################

# Separate the original data into training (~80%) and testing (~20%) datasets

library(ggplot2)

#kfold
group <- kfold(comb_data[-90,3:2], 5)
pres_train <- comb_data[-90,3:2][group != 1, ] 
pres_test <- comb_data[-90,3:2][group == 1, ] 

#make a data frame to hold all input data
training <- c(rep("Yes", nrow(pres_train)), rep("No", nrow(pres_test)))
input_data <- cbind(rbind(pres_train, pres_test), "Training" = training, 
                    "Presence" = "Yes", "Filter" = "None", "Fold" = "Random")

#separate the environmentally filtered data at 0.15, 0.3, 0.75 into 
#training (~80%) and testing (~20%) datasets
pick_coords <- list(coords_envS_D1, coords_envS_D2, coords_envS_D3)
pick_filter <- list("Env.15", "Env.3", "Env.75")

for(i in 1:3){
  colnames(pick_coords[[i]]) <- c("Longitude", "Latitude")
  group_envS <- kfold(pick_coords[[i]], 5)
  pres_train_envS <- pick_coords[[i]][group_envS != 1, ]
  pres_test_envS <- pick_coords[[i]][group_envS == 1, ]
  training <- c(rep("Yes", nrow(pres_train_envS)), rep("No", nrow(pres_test_envS)))
  
  #add to all input_data
  temp <-cbind(rbind(pres_train_envS, pres_test_envS), "Training" = training, 
               "Presence" = "Yes", "Filter" = pick_filter[[i]], "Fold" = "Random")
  input_data <- rbind(input_data, temp)
}

#Add biased gfolds
pick_coords <- list(comb_data[,3:2], coords_envS_D1, coords_envS_D2, coords_envS_D3)
pick_filter <- list("None", "Env.15", "Env.3", "Env.75")
pick_fold <- list("West","East","North","South")

for(coords in 1:4){
  #gfold line at 20% quantile of the east edge of the distribution
  pres_train_g_w <- pick_coords[[coords]][pick_coords[[coords]][,1] < quantile(pick_coords[[coords]][,1], probs = seq(0,1,0.2))[5],] 
  pres_test_g_e <- pick_coords[[coords]][pick_coords[[coords]][,1] > quantile(pick_coords[[coords]][,1], probs = seq(0,1,0.2))[5],] 
  training <- c(rep("Yes", nrow(pres_train_g_w)), rep("No", nrow(pres_test_g_e)))
  #add to all input_data
  temp <-cbind(rbind(pres_train_g_w, pres_test_g_e), "Training" = training, 
               "Presence" = "Yes", "Filter" = pick_filter[[coords]], "Fold" = "East")
  colnames(temp)[1:2] <- colnames(input_data)[1:2]
  input_data <- rbind(input_data, temp)
  
  #gfold line at 80% quantile at the west edge of the distribution
  pres_train_g_e <- pick_coords[[coords]][pick_coords[[coords]][,1] > quantile(pick_coords[[coords]][,1], probs = seq(0,1,0.2))[2],] 
  pres_test_g_w <- pick_coords[[coords]][pick_coords[[coords]][,1] < quantile(pick_coords[[coords]][,1], probs = seq(0,1,0.2))[2],] 
  training <- c(rep("Yes", nrow(pres_train_g_e)), rep("No", nrow(pres_test_g_w)))
  #add to all input_data
  temp <-cbind(rbind(pres_train_g_e, pres_test_g_w), "Training" = training, 
               "Presence" = "Yes", "Filter" = pick_filter[[coords]], "Fold" = "West")
  colnames(temp)[1:2] <- colnames(input_data)[1:2]
  input_data <- rbind(input_data, temp)
  
  #gfold line at 20% quantile of the north edge of the distribution
  pres_train_g_s <- pick_coords[[coords]][pick_coords[[coords]][,2] < quantile(pick_coords[[coords]][,2], probs = seq(0,1,0.2))[5],] 
  pres_test_g_n <- pick_coords[[coords]][pick_coords[[coords]][,2] > quantile(pick_coords[[coords]][,2], probs = seq(0,1,0.2))[5],] 
  training <- c(rep("Yes", nrow(pres_train_g_s)), rep("No", nrow(pres_test_g_n)))
  #add to all input_data
  temp <-cbind(rbind(pres_train_g_s, pres_test_g_n), "Training" = training, 
               "Presence" = "Yes", "Filter" = pick_filter[[coords]], "Fold" = "North")
  colnames(temp)[1:2] <- colnames(input_data)[1:2]
  input_data <- rbind(input_data, temp)
  
  #gfold line at 80% quantile at the south edge of the distribution
  pres_train_g_n <- pick_coords[[coords]][pick_coords[[coords]][,2] > quantile(pick_coords[[coords]][,2], probs = seq(0,1,0.2))[2],] 
  pres_test_g_s <- pick_coords[[coords]][pick_coords[[coords]][,2] < quantile(pick_coords[[coords]][,2], probs = seq(0,1,0.2))[2],] 
  training <- c(rep("Yes", nrow(pres_train_g_n)), rep("No", nrow(pres_test_g_s)))
  #add to all input_data
  temp <-cbind(rbind(pres_train_g_n, pres_test_g_s), "Training" = training, 
               "Presence" = "Yes", "Filter" = pick_filter[[coords]], "Fold" = "South")
  colnames(temp)[1:2] <- colnames(input_data)[1:2]
  input_data <- rbind(input_data, temp)
}

summary(input_data)

ggplot(input_data, aes(Longitude, Latitude)) +
  geom_point(aes(colour = Training), size = 0.5) + 
  facet_grid(Filter ~ Fold) + 
  theme(panel.spacing=unit(0, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = 1), 
        strip.background = element_rect(color = "black", size = 1))

# ggsave("figures/occurrence points/FilterFold_20200123.pdf", dpi = 600, 
#        width = 7, height = 6, units = "in")
