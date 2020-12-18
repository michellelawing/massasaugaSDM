##############################
###---- Variable Response ----
##############################

#This script is a stand alone script designed to plot variable responses, variable
#contribution, and permutation importance

library(tidyverse)
library(dismo)
options(scipen = 9999) #keep from putting axes as scientific notation

#Load the needed data
load("results/spring2020/modelruns_20200212_largecircles/model_list.RData")
load("results/spring2020/modelruns_20200212_largecircles/model_tracker_eval.RData")

mod.8 <- which(model_tracker$AUC >= 0.8 & model_tracker$TSS_maxSSS > 0.4)


#Variable Response Plots
#Need to know which variables are which numbers in the different models:
#All models: Variable Set1 = 1:160 ; Variable Set2 = 161:320 ; Variable Set3 = 321:480
#mod.8: Set1 = mod.8[1:6] ; Set2 = mod.8[7:11] ; Set3 = mod.8[12:16]
model_tracker[row.names(model_tracker) %in% mod.8, 1:4]

# tiff("figures/evaluation metrics/largecircles/ResponseCurves.tiff", width = 6.5, height = 3.5, units = "in", res = 300)
# par(mfrow = c(2,4))

#Bio4 (only 1st set)
response(model_list[[1]], xlim = c(3000, 12000), var = 1)
for(i in mod.8[1:6]) {
  response(model_list[[i]], var = 1, add = T)
}

#Bio5 (1st & 3rd set)
response(model_list[[1]], xlim = c(100, 450), var = 2) #, xlim = c(100, 450)
for(i in mod.8[1:6]) {
  response(model_list[[i]], var = 2, add = T)
}
for(i in mod.8[12:16]) {
  response(model_list[[i]], col = "blue", var = 1, add = T)
}

#Bio8 (1st & 3rd set)
response(model_list[[1]], xlim = c(-50, 350), var = 3) #, xlim = c(-100, 400)
for(i in mod.8[1:6]) {
  response(model_list[[i]], var = 3, add = T)
}
for(i in mod.8[12:16]) {
  response(model_list[[i]], col = "blue", var = 2, add = T)
}

#Bio14 (2nd & 3rd set)
response(model_list[[161]], xlim = c(-20, 100), col = "green", var = 1) #, xlim = c(-20, 100)
for(i in mod.8[8:11]) {
  response(model_list[[i]], col = "green", var = 1, add = T)
}
for(i in mod.8[12:16]) {
  response(model_list[[i]], col = "blue", var = 3, add = T)
}

#Bio19 (1st set only)
response(model_list[[1]], xlim = c(-10, 325), var = 4)
for(i in mod.8[1:6]) {
  response(model_list[[i]], var = 4, add = T)
}

#arid (2nd & 3rd set)
response(model_list[[161]], col = "green", var = 2)
for(i in mod.8[8:11]) {
  response(model_list[[i]], col = "green", var = 2, add = T)
}
for(i in mod.8[12:16]) {
  response(model_list[[i]], col = "blue", var = 4, add = T)
}

#gDD5 (2nd & 3rd set)
response(model_list[[161]], xlim = c(0, 105000), col = "green", var = 3)
for(i in mod.8[8:11]) {
  response(model_list[[i]], col = "green", var = 3, add = T)
}
for(i in mod.8[12:16]) {
  response(model_list[[i]], col = "blue", var = 5, add = T)
}

#PETs (2nd & 3rd set)
response(model_list[[161]], xlim = c(2700, 7300), col = "green", var = 4)
for(i in mod.8[8:11]) {
  response(model_list[[i]], col = "green", var = 4, add = T)
}
for(i in mod.8[12:16]) {
  response(model_list[[i]], col = "blue", var = 6, add = T)
}
# dev.off


#Variable Importance Plots
#Isolate model results and retrieve model importance variables
model_list_results <- lapply(model_list[mod.8], FUN = function(x) { as.data.frame(x@results) })
model_list_results <- lapply(model_list_results, function(x) { x$Name <-rownames(x); return(x)})
perm_imp <- lapply(model_list_results, 
                   function(x) { x[grep(pattern = ".permutation.importance", x = rownames(x)),] })
perm_imp <- lapply(perm_imp, function(x) { x$Name <- gsub(".permutation.importance", "", x$Name) ; return(x) })

varPI <- data.frame(model_tracker[row.names(model_tracker) %in% mod.8, 1:4])

var1PI <- varPI[1:6,]
var1PI$bio4    <- unlist(lapply(perm_imp[1:6], function(x) { x[x$Name == "bio4", "V1"] }))
var1PI$bio5    <- unlist(lapply(perm_imp[1:6], function(x) { x[x$Name == "bio5", "V1"] }))
var1PI$bio8    <- unlist(lapply(perm_imp[1:6], function(x) { x[x$Name == "bio8", "V1"] }))
var1PI$bio19   <- unlist(lapply(perm_imp[1:6], function(x) { x[x$Name == "bio19", "V1"] }))

var2PI <- varPI[7:10,]
var2PI$PETs  <- unlist(lapply(perm_imp[7:10], function(x) { x[x$Name == "PETseasonality", "V1"] }))
var2PI$arid  <- unlist(lapply(perm_imp[7:10], function(x) { x[x$Name == "aridityIndexThornthwaite", "V1"] }))
var2PI$bio14 <- unlist(lapply(perm_imp[7:10], function(x) { x[x$Name == "bio14", "V1"] }))
var2PI$GDD5  <- unlist(lapply(perm_imp[7:10], function(x) { x[x$Name == "growingDegDays5", "V1"] }))

var3PI <- varPI[11:16,]
var3PI$bio5    <- unlist(lapply(perm_imp[11:16], function(x) { x[x$Name == "bio5", "V1"] }))
var3PI$bio8    <- unlist(lapply(perm_imp[11:16], function(x) { x[x$Name == "bio8", "V1"] }))
var3PI$PETs    <- unlist(lapply(perm_imp[11:16], function(x) { x[x$Name == "PETseasonality", "V1"] }))
var3PI$arid    <- unlist(lapply(perm_imp[11:16], function(x) { x[x$Name == "aridityIndexThornthwaite", "V1"] }))
var3PI$bio14   <- unlist(lapply(perm_imp[11:16], function(x) { x[x$Name == "bio14", "V1"] }))
var3PI$GDD5    <- unlist(lapply(perm_imp[11:16], function(x) { x[x$Name == "growingDegDays5", "V1"] }))

var1PI_long <- gather(var1PI, key = Variable, value = PermImp, bio4:bio19, factor_key=TRUE) #tri OR bio19
var2PI_long <- gather(var2PI, key = Variable, value = PermImp, PETs:GDD5, factor_key=TRUE)
var3PI_long <- gather(var3PI, key = Variable, value = PermImp, bio5:GDD5, factor_key=TRUE) #tri OR GDD5

varPI_long <- rbind(var1PI_long, var2PI_long, var3PI_long)

ggplot(data.frame(varPI_long), aes(x = Variable, y = PermImp)) +
  geom_boxplot(aes()) + 
  geom_jitter(aes(color = Climate, shape = Background)) +
  facet_wrap(~ Folds, nrow = 1) +
  #ggtitle("Default_X") +
  theme(panel.spacing=unit(0, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = 1), 
        strip.background = element_rect(color = "black", size = 1),
        axis.text = element_text(color = "black"))
# ggsave("figures/evaluation metrics/VarImp_20200218.tiff", dpi = 600,
#        height = 4, width = 6, units = "in") #width = 6 or 9


#Variable Contribution Plots
var_cont <- lapply(model_list_results, 
                   function(x) { x[grep(pattern = ".contribution", x = rownames(x)),] })
var_cont <- lapply(var_cont, function(x) { x$Name <- gsub(".contribution", "", x$Name) ; return(x) })

varVC <- data.frame(model_tracker[row.names(model_tracker) %in% mod.8, 1:4])

var1VC <- varPI[1:6,]
var1VC$bio4    <- unlist(lapply(var_cont[1:6], function(x) { x[x$Name == "bio4", "V1"] }))
var1VC$bio5    <- unlist(lapply(var_cont[1:6], function(x) { x[x$Name == "bio5", "V1"] }))
var1VC$bio8    <- unlist(lapply(var_cont[1:6], function(x) { x[x$Name == "bio8", "V1"] }))
var1VC$bio19   <- unlist(lapply(var_cont[1:6], function(x) { x[x$Name == "bio19", "V1"] }))

var2VC <- varPI[7:10,]
var2VC$PETs  <- unlist(lapply(var_cont[7:10], function(x) { x[x$Name == "PETseasonality", "V1"] }))
var2VC$arid  <- unlist(lapply(var_cont[7:10], function(x) { x[x$Name == "aridityIndexThornthwaite", "V1"] }))
var2VC$bio14 <- unlist(lapply(var_cont[7:10], function(x) { x[x$Name == "bio14", "V1"] }))
var2VC$GDD5  <- unlist(lapply(var_cont[7:10], function(x) { x[x$Name == "growingDegDays5", "V1"] }))

var3VC <- varPI[11:16,]
var3VC$bio5    <- unlist(lapply(var_cont[11:16], function(x) { x[x$Name == "bio5", "V1"] }))
var3VC$bio8    <- unlist(lapply(var_cont[11:16], function(x) { x[x$Name == "bio8", "V1"] }))
var3VC$PETs    <- unlist(lapply(var_cont[11:16], function(x) { x[x$Name == "PETseasonality", "V1"] }))
var3VC$arid    <- unlist(lapply(var_cont[11:16], function(x) { x[x$Name == "aridityIndexThornthwaite", "V1"] }))
var3VC$bio14   <- unlist(lapply(var_cont[11:16], function(x) { x[x$Name == "bio14", "V1"] }))
var3VC$GDD5    <- unlist(lapply(var_cont[11:16], function(x) { x[x$Name == "growingDegDays5", "V1"] }))

var1VC_long <- gather(var1VC, key = Variable, value = varCont, bio4:bio19, factor_key=TRUE) #tri OR bio19
var2VC_long <- gather(var2VC, key = Variable, value = varCont, PETs:GDD5, factor_key=TRUE)
var3VC_long <- gather(var3VC, key = Variable, value = varCont, bio5:GDD5, factor_key=TRUE) #tri OR GDD5

varVC_long <- rbind(var1VC_long, var2VC_long, var3VC_long)

ggplot(data.frame(varVC_long), aes(x = Variable, y = varCont)) +
  geom_boxplot(aes()) + 
  geom_jitter(aes(color = Climate, shape = Background)) +
  facet_wrap(~ Folds, nrow = 1) +
 # ggtitle("Default_X") +
  theme(panel.spacing=unit(0, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = 1), 
        strip.background = element_rect(color = "black", size = 1),
        axis.text = element_text(color = "black"))
# ggsave("figures/evaluation metrics/VarCont_20200218.tiff", dpi = 600,
#        height = 4, width = 6, units = "in") #width = 6 or 9

#Variable Contribution and Permutation Importance values for Results - Model 
#Evaluation - Paragraph 2
head(var)
##bio4
varVC_long %>% filter(Variable == "bio4") %>% summarise(b4 = mean(varCont))
varPI_long %>% filter(Variable == "bio4") %>% summarise(b4 = mean(PermImp))

##bio19
varVC_long %>% filter(Variable == "bio19") %>% summarise(b19 = mean(varCont))
varPI_long %>% filter(Variable == "bio19") %>% summarise(b19 = mean(PermImp))


varVC_long %>% filter(Variable == "bio5") %>% summarise(b5 = mean(varCont))
varVC_long %>% filter(Variable == "bio8") %>% summarise(b8 = mean(varCont))
varVC_long %>% filter(Variable == "bio14") %>% summarise(b14 = mean(varCont))
varVC_long %>% filter(Variable == "PETs") %>% summarise(pet = mean(varCont))
varVC_long %>% filter(Variable == "arid") %>% summarise(arid = mean(varCont))
varVC_long %>% filter(Variable == "GDD5") %>% summarise(gdd = mean(varCont))

varPI_long %>% filter(Variable == "bio5") %>% summarise(b5 = mean(PermImp))
varPI_long %>% filter(Variable == "bio8") %>% summarise(b8 = mean(PermImp))
varPI_long %>% filter(Variable == "bio14") %>% summarise(b14 = mean(PermImp))
varPI_long %>% filter(Variable == "PETs") %>% summarise(pet = mean(PermImp))
varPI_long %>% filter(Variable == "arid") %>% summarise(arid = mean(PermImp))
varPI_long %>% filter(Variable == "GDD5") %>% summarise(gdd = mean(PermImp))
