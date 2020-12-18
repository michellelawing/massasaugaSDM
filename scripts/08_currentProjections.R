#############################
###--- Model Predictions ----
#############################

#Add in the bioclim and crop to extent (extended the western extent further)
d_ext <- extent(c(-122, -85, yrange))
bioclim_c <- crop(bioclim, d_ext) #crop 1st so rasters are same extent
envirem_c <- crop(envirem, d_ext)

bio_all <- stack(bioclim_c[[c(4,5,8,14,19)]], envirem_c[[c(2,7,13)]])#,17,18

#Predict for each selected model (to save time, predict only on the models meeting
#the model evaluation criteria (e.g. model_list[mod.8]))
mod_pred <- lapply(model_list,  function(x) predict(x, bio_all))
save(mod_pred, file = "results/spring2020/modelruns/model_predict.RData")
# load("results/spring2020/modelruns_20200212_largecircles/model_predict.RData")
# load("results/spring2020/modelruns_20200212_largecircles/model_tracker_eval.RData")

#Vectors of model numbers under different criteria:
# mod.7  <- which(model_tracker$AUC >= 0.7 & model_tracker$TSS_maxSSS > 0.4)
# mod.75 <- which(model_tracker$AUC >= 0.75 & model_tracker$TSS_maxSSS > 0.4)
mod.8    <- which(model_tracker$AUC >= 0.8 & model_tracker$TSS_maxSSS > 0.4)

# stack predictive rasters and get mean and variance for each stack
pred_stack          <- stack(mod_pred)
pred.8_stack        <- stack(mod_pred[mod.8])
# pred.75_stack     <- stack(mod_pred[mod.75])
# pred.7_stack      <- stack(mod_pred[mod.7])

pred_mean        <- calc(pred_stack, mean, na.rm = T)
pred.8_mean      <- calc(pred.8_stack,  mean, na.rm = T)
# pred.75_mean     <- calc(pred.75_stack, mean, na.rm = T)
# pred.7_mean      <- calc(pred.7_stack,  mean, na.rm = T)

pred_var        <- calc(pred_stack, var, na.rm = T)
pred.8_var      <- calc(pred.8_stack,  var, na.rm = T)
# pred.75_var     <- calc(pred.75_stack, var, na.rm = T)
# pred.7_var      <- calc(pred.7_stack,  var, na.rm = T)


#Create mean and variance figures for the current projections

col <- c("#f7fcf5", "#e5f5e0", "#c7e9c0", "#a1d99b", "#74c476", "#41ab5d", "#238b45", "#006d2c", "#00441b")

#All 480 models 

# tiff("figures/projections/currentrun/Avg480Models_20200219.tif",
#      width = 11, height = 6, units = "in", res = 600)
# pdf("figures/projections/currentrun/Avg480Models_20200219.pdf", width = 11, height = 6)
par(mfrow = c(1, 2), mar = c(2, 2.5, 2, 5))
plot(pred_mean, col = col, main = "Mean Models (n = 480)")
maps::map("state", boundary = T, col = "darkgray", add = T)
suppressWarnings(maps::map(mex, boundary = F, col = "darkgray", add = T))
plot(pred_var, main = "Variance Models (n = 480)")
maps::map("state", boundary = T, col = "darkgray", add = T)
suppressWarnings(maps::map(mex, boundary = F, col = "darkgray", add = T))
# dev.off()


#Just the models meeting the AUC/TSS criteria

# tiff("figures/projections/currentrun/AUCpt8_TSSpt4Models_20200211.tif",
#     width = 11, height = 6, units = "in", res = 600)
# pdf("figures/projections/currentrun/AUCpt8_TSSpt4Models_20200211.pdf", width = 11, height = 6)
par(mfrow = c(1, 2), mar = c(2, 2.5, 2.5, 5))
plot(pred.8_mean, col = col, main = "AUC>0.8 TSS>0.4 (n = 16) \n Mean Models")
maps::map("state", boundary = T, col = "darkgray", add = T)
suppressWarnings(maps::map(mex, boundary = F, col = "darkgray", add = T))
plot(pred.8_var, main = "AUC>0.8 TSS>0.4 (n = 16) \n Variance Models")
maps::map("state", boundary = T, col = "darkgray", add = T)
suppressWarnings(maps::map(mex, boundary = F, col = "darkgray", add = T))
# dev.off()

# plot(pred.75_mean, col = col, main = "Mean Models AUC >= 0.75 (n = 67)")
# maps::map("state", boundary = F, col = "darkgray", add = T)
# suppressWarnings(maps::map(mex, boundary = F, col = "darkgray", add = T))
# plot(pred.75_var,  main = "Variance Models AUC >= 0.75 (n = 67)")
# maps::map("state", boundary = F, col = "darkgray", add = T)
# suppressWarnings(maps::map(mex, boundary = F, col = "darkgray", add = T))
# 
# plot(pred.7_mean, col = col, main = "Mean Models AUC >= 0.7 (n = 121)")
# maps::map("state", boundary = F, col = "darkgray", add = T)
# suppressWarnings(maps::map(mex, boundary = F, col = "darkgray", add = T))
# plot(pred.75_var,  main = "Variance Models AUC >= 0.7 (n = 121)")
# maps::map("state", boundary = F, col = "darkgray", add = T)
# suppressWarnings(maps::map(mex, boundary = F, col = "darkgray", add = T))