############################
###--- Model Evaluation ----
############################

# Runs the model evaluation and extracts multiple evaluation statistics

#evaluate models with testing presence and background points
colnames(model_tracker) <- c("Climate", "Filters", "Folds", "Background")
model_tracker <- data.frame(model_tracker)
levels(model_tracker$Background) <- c("BE", "BE10k", "CB", "CB10k", "CL", "CL10k", "OE", "OE10k")
model_tracker$Background <- factor(model_tracker$Background, levels = c("CL", "CL10k", "CB", "CB10k", "OE", "OE10k","BE", "BE10k"))

model_eval <- list()
counter <- 1
 for (climate in 1:3){
   for (filters in 1:4){
     for (folds in 1:5) {
       for (backs in 1:8){
         input_temp <- input_data[input_data$Filter == pick_filter[[filters]] & input_data$Fold == pick_fold[[folds]] & input_data$Training == "No",1:2]
         back_temp <- background[background$Set == pick_back[[backs]] & background$Training == "No", 1:2]
         model_eval[[counter]] <- evaluate(input_temp, back_temp, model_list[[counter]], climate_list[[climate]])
         counter <- counter + 1
       }
     }
   }
 }

#save out the model evaluation
save(model_eval, file = "results/spring2020/modelruns/model_eval.RData")
save(model_tracker, file = "results/spring2020/modelruns/model_tracker.RData")

#load model list and model tracker output from a previous run
# load("results/spring2020/modelruns_20200212_largecircles/model_eval.RData")
# load("results/spring2020/modelruns_20200212_largecircles/model_tracker.RData")

#extract multiple evaluation statistics for use
auc <- unlist(lapply(model_eval, function(x) x@auc))

#max_aucdiff training - testing
aucdiff <- array(NA, dim = c(length(model_tracker[,1])))
for(i in 1:length(model_tracker[,1])){
  aucdiff[i] <- model_list[[i]]@results[5] - model_eval[[i]]@auc
}

#get thresholds
ses <- which.min(abs(model_eval[[1]]@TPR - model_eval[[1]]@TNR))
maxsss <- which.max(model_eval[[1]]@TPR + model_eval[[1]]@TNR)
#minimum presence threshold and 10% presence threshold

#kappa at thresholds
kappa_ses <- unlist(lapply(model_eval, function(x) x@kappa[ses]))
kappa_maxsss <- unlist(lapply(model_eval, function(x) x@kappa[maxsss]))
#minimum presence threshold and 10% presence threshold

#True Skill Statistic TSS at thresholds, TPR is sensititivy and TNR is specificity
tss_ses <- unlist(lapply(model_eval, function(x) x@TPR[ses] + x@TNR[ses] - 1))
tss_maxsss <- unlist(lapply(model_eval, function(x) x@TPR[maxsss] + x@TNR[maxsss] - 1))
#minimum presence threshold and 10% presence threshold

#Omission Rate, OR
or_ses <- unlist(lapply(model_eval, function(x) x@OR[ses]))
or_maxsss <- unlist(lapply(model_eval, function(x) x@OR[maxsss]))
#minimum presence threshold and 10% presence threshold
model_tracker <- cbind(model_tracker, "AUC" = auc, "AUCdiff" = aucdiff, 
                       "Kappa_SeS" = kappa_ses, "Kappa_maxSSS" = kappa_maxsss,
                       "TSS_SeS" = tss_ses, "TSS_maxSSS" = tss_maxsss,
                       "OR_SeS" = or_ses, "OR_maxSSS" = or_maxsss)
save(model_tracker, file = "results/spring2020/modelruns/model_tracker_eval.RData" )

#Plot evaluation statistics
# load("results/spring2020/modelruns_20200212_largecircles/model_tracker_eval.RData")

#plot AUC by filter, fold, background and climate
ggplot(model_tracker, aes(x = Background, y = AUC)) +
  geom_boxplot(aes(x = Background, y = AUC)) +
  geom_jitter(aes(color = Folds, shape = Climate)) + 
  facet_wrap(~Filters, nrow = 2) +
  theme(panel.spacing=unit(0, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = 1), 
        strip.background = element_rect(color = "black", size = 1),
        axis.text = element_text(color = "black"))
#ggsave("figures/evaluation metrics/largecircles/AUC_filter_fold_20200211.tiff", width = 7, height = 5, dpi = 600)

#plot OR_SeS by filter, fold, background and climate
ggplot(model_tracker, aes(x = Background, y = OR_SeS)) +
  geom_boxplot(aes(x = Background, y = OR_SeS)) +
  geom_jitter(aes(color = Folds, shape = Climate)) + 
  facet_wrap(~Filters, nrow = 2) +
  theme(panel.spacing=unit(0, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = 1), 
        strip.background = element_rect(color = "black", size = 1),
        axis.text = element_text(color = "black"))
#ggsave("figures/evaluation metrics/largecircles/ORses_filter_fold_20200211.tiff", width = 7, height = 5, dpi = 600)

#plot OR_maxSSS by filter, fold, background and climate
ggplot(model_tracker, aes(x = Background, y = OR_maxSSS)) +
  geom_boxplot(aes(x = Background, y = OR_maxSSS)) +
  geom_jitter(aes(color = Folds, shape = Climate)) + 
  facet_wrap(~Filters, nrow = 2) +
  theme(panel.spacing=unit(0, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = 1), 
        strip.background = element_rect(color = "black", size = 1),
        axis.text = element_text(color = "black"))
#ggsave("figures/evaluation metrics/largecircles/ORmax_filter_fold_20200211.tiff", width = 7, height = 5, dpi = 600)

#plot TSS SeS by filter, fold, background and climate
# Proportion of data that fall out of the area predicted as preferential 
ggplot(model_tracker, aes(x = Background, y = TSS_SeS)) +
  geom_boxplot(aes(x = Background, y = TSS_SeS)) +
  geom_jitter(aes(color = Folds, shape = Climate)) + 
  facet_wrap(~Filters, nrow = 2) +
  theme(panel.spacing=unit(0, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = 1), 
        strip.background = element_rect(color = "black", size = 1),
        axis.text = element_text(color = "black"))
#ggsave("figures/evaluation metrics/largecircles/TSSses_filter_fold_20200211.tiff", width = 7, height = 5, dpi = 600)

#plot TSS_maxSSS by filter, fold, background and climate
# Proportion of data that fall out of the area predicted as preferential 
ggplot(model_tracker, aes(x = Background, y = TSS_maxSSS)) +
  geom_boxplot(aes(x = Background, y = TSS_maxSSS)) +
  geom_jitter(aes(color = Folds, shape = Climate)) + 
  facet_wrap(~Filters, nrow = 2) +
  theme(panel.spacing=unit(0, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = 1), 
        strip.background = element_rect(color = "black", size = 1),
        axis.text = element_text(color = "black"))
#ggsave("figures/evaluation metrics/largecircles/TSSmax_filter_fold_20200211.tiff", width = 7, height = 5, dpi = 600)

#plot Kappa_SeS by filter, fold, background and climate
# Proportion of data that fall out of the area predicted as preferential 
ggplot(model_tracker, aes(x = Background, y = Kappa_SeS)) +
  geom_boxplot(aes(x = Background, y = Kappa_SeS)) +
  geom_jitter(aes(color = Folds, shape = Climate)) + 
  facet_wrap(~Filters, nrow = 2) +
  theme(panel.spacing=unit(0, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = 1), 
        strip.background = element_rect(color = "black", size = 1),
        axis.text = element_text(color = "black"))
#ggsave("figures/evaluation metrics/largecircles/KAPPAses_filter_fold_20200211.tiff", width = 7, height = 5, dpi = 600)

#plot Kappa_maxSSS by filter, fold, background and climate
# Proportion of data that fall out of the area predicted as preferential 
ggplot(model_tracker, aes(x = Background, y = Kappa_maxSSS)) +
  geom_boxplot(aes(x = Background, y = Kappa_maxSSS)) +
  geom_jitter(aes(color = Folds, shape = Climate)) + 
  facet_wrap(~Filters, nrow = 2) +
  theme(panel.spacing=unit(0, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = 1), 
        strip.background = element_rect(color = "black", size = 1),
        axis.text = element_text(color = "black"))
#ggsave("figures/evaluation metrics/largecircles/KAPPAmax_filter_fold_20200211.tiff", width = 7, height = 5, dpi = 600)
