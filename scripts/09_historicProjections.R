##################################
###--- Past Model Predictions ----
##################################

## We decided to use models that meet the AUC > 0.8 and TSS_maxSSS > 0.4 for the 
## projections. Code for other criteria is included, but commented out.

## If you run all the scripts in order, rJava doesn't have enough memory to run 
## the historic predict code. To run stand-alone to avoid the rJava memory issue: 
## Run scripts 01_input and 02_variableEnv to get data and environmental stuff set up, then:
# load("results/spring2020/modelruns_20200212_largecircles/model_list.RData")
# load("results/spring2020/modelruns_20200212_largecircles/model_tracker_eval.RData")
## to get the models and eval stats to predict on

#SET UP THE ENVIRONMENTAL RASTERS (4 sets: LGM and mid-holocene; CC and MR)
files <- list.files(path="datainputs/gcm/LGM/cclgmbi_2-5m", pattern="tif", full.names=TRUE )
cclgmbi <- stack(files)
names(cclgmbi) <- c("bio14", "bio19", "bio4", "bio5", "bio8")
cclgmbi_reduced_ext <- crop(cclgmbi, d_ext)
files <- list.files(path="datainputs/gcm/LGM/NAmerica_lgm_ccsm4_2.5arcmin_geotiff", pattern="tif", full.names=TRUE )
cclgmen <- stack(files, quick = TRUE) #don't quite align, so use quick=T since we crop later
names(cclgmen) <- c("topoWet","tri","aridityIndexThornthwaite", "growingDegDays5", "PETseasonality")
cclgmen_reduced_ext <- crop(cclgmen, d_ext)
cclgm_reduced_ext <- stack(cclgmbi_reduced_ext, cclgmen_reduced_ext)

files <- list.files(path="datainputs/gcm/LGM/mrlgmbi_2-5m", pattern="tif", full.names=TRUE )
mrlgmbi <- stack(files)
names(mrlgmbi) <- c("bio14", "bio19", "bio4", "bio5", "bio8")
mrlgmbi_reduced_ext <- crop(mrlgmbi, d_ext)
files <- list.files(path="datainputs/gcm/LGM/NAmerica_lgm_miroc_esm_2.5arcmin_geotiff", pattern="tif", full.names=TRUE )
mrlgmen <- stack(files, quick = TRUE)
names(mrlgmen) <- c("topoWet","tri","aridityIndexThornthwaite", "growingDegDays5", "PETseasonality")
mrlgmen_reduced_ext <- crop(mrlgmen, d_ext)
mrlgm_reduced_ext <- stack(mrlgmbi_reduced_ext, mrlgmen_reduced_ext)

files <- list.files(path="datainputs/gcm/midHol/ccmidbi_2-5m", pattern="tif", full.names=TRUE )
ccmidbi <- stack(files)
names(ccmidbi) <- c("bio14", "bio19", "bio4", "bio5", "bio8")
ccmidbi_reduced_ext <- crop(ccmidbi, d_ext)
files <- list.files(path="datainputs/gcm/midHol/NAmerica_holo_ccsm4_2.5arcmin_geotiff", pattern="tif", full.names=TRUE )
ccmiden <- stack(files)
names(ccmiden) <- c("topoWet","tri","aridityIndexThornthwaite", "growingDegDays5", "PETseasonality")
ccmiden_reduced_ext <- crop(ccmiden, d_ext)
ccmid_reduced_ext <- stack(ccmidbi_reduced_ext, ccmiden_reduced_ext)

files <- list.files(path="datainputs/gcm/midHol/mrmidbi_2-5m", pattern="tif", full.names=TRUE )
mrmidbi <- stack(files)
names(mrmidbi) <- c("bio14", "bio19", "bio4", "bio5", "bio8")
mrmidbi_reduced_ext <- crop(mrmidbi, d_ext)
files <- list.files(path="datainputs/gcm/midHol/NAmerica_holo_miroc_esm_2.5arcmin_geotiff", pattern="tif", full.names=TRUE )
mrmiden <- stack(files)
names(mrmiden) <- c("topoWet","tri","aridityIndexThornthwaite", "growingDegDays5", "PETseasonality")
mrmiden_reduced_ext <- crop(mrmiden, d_ext) 
mrmid_reduced_ext <- stack(mrmidbi_reduced_ext, mrmiden_reduced_ext)

#HISTORIC MODEL PROJECTIONS
mod_pred_cclgm  <- lapply(model_list,  function(x) dismo::predict(x, cclgm_reduced_ext))
save(mod_pred_cclgm, file = "results/spring2020/modelruns/model_predict_cclgm.RData")
# load("results/spring2020/modelruns_20200212_largecircles/model_predict_cclgm.RData")

mod_pred_mrlgm  <- lapply(model_list,  function(x) predict(x, mrlgm_reduced_ext))
save(mod_pred_mrlgm, file = "results/spring2020/modelruns/model_predict_mrlgm.Rdata")
# load("results/spring2020/modelruns_20200212_largecircles/model_predict_mrlgm.Rdata")

mod_pred_ccmid  <- lapply(model_list,  function(x) predict(x, ccmid_reduced_ext))
save(mod_pred_ccmid, file = "results/spring2020/modelruns/mod_pred_ccmid.Rdata")
# load("results/spring2020/modelruns_20200212_largecircles/mod_pred_ccmid.Rdata")

mod_pred_mrmid  <- lapply(model_list,  function(x) predict(x, mrmid_reduced_ext, args = c('threads=4')))
save(mod_pred_mrmid, file = "results/spring2020/modelruns/mod_pred_mrmid.Rdata")
# load("results/spring2020/modelruns_20200212_largecircles/mod_pred_mrmid.Rdata")


#MEAN AND VARIANCE PREDICTIONS
# load("results/spring2020/modelruns_20200212_largecircles/model_tracker_eval.Rdata") #model_tracker
mod.8 <- which(model_tracker$AUC >= 0.8 & model_tracker$TSS_maxSSS > 0.4)

mod_pred.8_cclgm_stack <- stack(mod_pred_cclgm[mod.8])
mod_pred.8_cclgm_avg   <- calc(mod_pred.8_cclgm_stack,  mean, na.rm = T)
mod_pred.8_cclgm_var   <- calc(mod_pred.8_cclgm_stack,  var, na.rm = T)

# mod_pred.7_cclgm_stack <- stack(mod_pred.7_cclgm)
# mod_pred.7_cclgm_avg   <- calc( mod_pred.7_cclgm_stack,  mean, na.rm = T)
# mod_pred.7_cclgm_var   <- calc( mod_pred.7_cclgm_stack,  var, na.rm = T)

mod_pred.8_mrlgm_stack <- stack(mod_pred_mrlgm[mod.8])
mod_pred.8_mrlgm_avg   <- calc( mod_pred.8_mrlgm_stack,  mean, na.rm = T)
mod_pred.8_mrlgm_var   <- calc( mod_pred.8_mrlgm_stack,  var, na.rm = T)

# mod_pred.7_mrlgm_stack <- stack(mod_pred.7_mrlgm)
# mod_pred.7_mrlgm_avg   <- calc( mod_pred.7_mrlgm_stack,  mean, na.rm = T)
# mod_pred.7_mrlgm_var   <- calc( mod_pred.7_mrlgm_stack,  var, na.rm = T)

mod_pred.8_ccmid_stack <- stack(mod_pred_ccmid[mod.8])
mod_pred.8_ccmid_avg   <- calc( mod_pred.8_ccmid_stack,  mean, na.rm = T)
mod_pred.8_ccmid_var   <- calc( mod_pred.8_ccmid_stack,  var, na.rm = T)

# mod_pred.7_ccmid_stack <- stack(mod_pred.7_ccmid)
# mod_pred.7_ccmid_avg   <- calc( mod_pred.7_ccmid_stack,  mean, na.rm = T)
# mod_pred.7_ccmid_var   <- calc( mod_pred.7_ccmid_stack,  var, na.rm = T)

mod_pred.8_mrmid_stack <- stack(mod_pred_mrmid[mod.8])
mod_pred.8_mrmid_avg   <- calc( mod_pred.8_mrmid_stack,  mean, na.rm = T)
mod_pred.8_mrmid_var   <- calc( mod_pred.8_mrmid_stack,  var, na.rm = T)

# mod_pred.7_mrmid_stack <- stack(mod_pred.7_mrmid)
# mod_pred.7_mrmid_avg   <- calc( mod_pred.7_mrmid_stack,  mean, na.rm = T)
# mod_pred.7_mrmid_var   <- calc( mod_pred.7_mrmid_stack,  var, na.rm = T)


#MAPPING PREDICTIONS
col <- c("#f7fcf5", "#e5f5e0", "#c7e9c0", "#a1d99b", "#74c476", "#41ab5d", "#238b45", "#006d2c", "#00441b")

# tiff("figures/projections/currentrun/MRLGM_20200211.tif",
#      width = 11, height = 6, units = "in", res = 600)
# pdf("figures/projections/currentrun/MRLGM_20200211.pdf", width = 11, height = 6)
par(mfrow = c(1, 2), mar = c(2, 2.5, 2, 5))
plot(mod_pred.8_mrlgm_avg, col = col, main = "Mean MRLGM AUC>0.8 TSS<0.25 \n NoHinge Beta=1 (n = 17)")
maps::map("state", boundary = T, col = "darkgray", add = T)
suppressWarnings(maps::map(mex, boundary = F, col = "darkgray", add = T))
scalebar(d = 500, xy = c(-93, 26), type = "line")
plot(mod_pred.8_mrlgm_var, main = "Variance MRLGM AUC>0.8 TSS<0.25 \n NoHinge Beta=1 (n = 17)")
maps::map("state", boundary = T, col = "darkgray", add = T)
suppressWarnings(maps::map(mex, boundary = F, col = "darkgray", add = T))
scalebar(d = 500, xy = c(-93, 26), type = "line")
# dev.off()

# tiff("figures/projections/currentrun/CCLGM_20200211.tif",
#      width = 11, height = 6, units = "in", res = 600)
# pdf("figures/projections/currentrun/CCLGM_20200211.pdf", width = 11, height = 6)
par(mfrow = c(1, 2), mar = c(2, 2.5, 2, 5))
plot(mod_pred.8_cclgm_avg, col = col, main = "Mean CCLGM AUC>0.8 TSS<0.25 \n NoHinge Beta=1 (n = 17)")
maps::map("state", boundary = T, col = "darkgray", add = T)
suppressWarnings(maps::map(mex, boundary = F, col = "darkgray", add = T))
scalebar(d = 500, xy = c(-93, 26), type = "line")
plot(mod_pred.8_cclgm_var, main = "Variance CCLGM AUC>0.8 TSS<0.25 \n NoHinge Beta=1 (n = 17)")
maps::map("state", boundary = T, col = "darkgray", add = T)
suppressWarnings(maps::map(mex, boundary = F, col = "darkgray", add = T))
scalebar(d = 500, xy = c(-93, 26), type = "line")
# dev.off()

# tiff("figures/projections/currentrun/MRmid_20200211.tif",
#      width = 11, height = 6, units = "in", res = 600)
# pdf("figures/projections/currentrun/MRmid_20200211.pdf", width = 11, height = 6)
par(mfrow = c(1, 2), mar = c(2, 2.5, 2, 5))
plot(mod_pred.8_mrmid_avg, col = col, main = "Mean MRmid AUC>0.8 TSS<0.25 \n NoHinge Beta=1 (n = 17)")
maps::map("state", boundary = T, col = "darkgray", add = T)
suppressWarnings(maps::map(mex, boundary = F, col = "darkgray", add = T))
scalebar(d = 500, xy = c(-93, 26), type = "line")
plot(mod_pred.8_mrmid_var, main = "Variance MRmid AUC>0.8 TSS<0.25 \n NoHinge Beta=1 (n = 17)")
maps::map("state", boundary = T, col = "darkgray", add = T)
suppressWarnings(maps::map(mex, boundary = F, col = "darkgray", add = T))
scalebar(d = 500, xy = c(-93, 26), type = "line")
# dev.off()

# tiff("figures/projections/currentrun/CCmid_20200211.tif",
#      width = 11, height = 6, units = "in", res = 600)
# pdf("figures/projections/currentrun/CCmid_20200211.pdf", width = 11, height = 6)
par(mfrow = c(1, 2), mar = c(2, 2.5, 2, 5))
plot(mod_pred.8_ccmid_avg, col = col, main = "Mean CCmid AUC>0.8 TSS<0.4 \n NoHinge Beta=1 (n = 17)")
maps::map("state", boundary = T, col = "darkgray", add = T)
suppressWarnings(maps::map(mex, boundary = F, col = "darkgray", add = T))
scalebar(d = 500, xy = c(-93, 26), type = "line")
plot(mod_pred.8_ccmid_var, main = "Variance CCmid AUC>0.8 TSS<0.4 \n NoHinge Beta=1 (n = 17)")
maps::map("state", boundary = T, col = "darkgray", add = T)
suppressWarnings(maps::map(mex, boundary = F, col = "darkgray", add = T))
scalebar(d = 500, xy = c(-93, 26), type = "line")
# dev.off()
