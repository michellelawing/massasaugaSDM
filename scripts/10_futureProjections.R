################################
###--- Future Climate Models ---
################################

## For now, we have decided to use any models that meet the AUC > 0.8 and
## TSS_maxSSS > 0.4 for the projections.

## If you run all the scripts in order, rJava doesn't have enough memory to run 
## the future predict code. To run stand-alone to avoid the rJava memory issue: 
## Run scripts 01_input and 02_variableEnv to get data and environmental stuff set up, then:
# load("results/spring2020/modelruns_20200212_largecircles/model_list.RData")
# load("results/spring2020/modelruns_20200212_largecircles/model_tracker_eval.RData")
# mod.8 <- which(model_tracker$AUC >= 0.8 & model_tracker$TSS_maxSSS > 0.4)
## to get the models and eval stats to predict on

#SET UP ENVIRONMENTAL RASTERS (8 sets: 2050 and 2070; 2.6 and 8.5 W/m2; CC and MR)
files <- list.files(path="datainputs/gcm/fore_50/cc26bi50", pattern="tif", full.names=TRUE )
cc26bi50 <- stack(files)
names(cc26bi50) <- c("bio14", "bio19", "bio4", "bio5", "bio8")
cc26bi50_reduced_ext <- crop(cc26bi50, d_ext)
files <- list.files(path="datainputs/gcm/envirem/cc26en50", full.names=TRUE )
cc26en50a <- crop(stack(files[4:5]), d_ext)
cc26en50b <- stack(files[1:3]) #these were cropped when created
cc26en50_reduced_ext <- stack(cc26en50a, cc26en50b)
names(cc26en50_reduced_ext) <- c("topoWet","tri","aridityIndexThornthwaite", "growingDegDays5", "PETseasonality")
cc2650_reduced_ext <- stack(cc26bi50_reduced_ext, cc26en50_reduced_ext)

files <- list.files(path="datainputs/gcm/fore_50/cc85bi50", pattern="tif", full.names=TRUE )
cc85bi50 <- stack(files)
names(cc85bi50) <- c("bio14", "bio19", "bio4", "bio5", "bio8")
cc85bi50_reduced_ext <- crop(cc85bi50, d_ext)
files <- list.files(path="datainputs/gcm/envirem/cc85en50", pattern="tif", full.names=TRUE )
cc85en50a <- crop(stack(files[4:5]), d_ext)
cc85en50b <- stack(files[1:3]) #cropped when created
cc85en50_reduced_ext <- stack(cc85en50a, cc85en50b)
names(cc85en50_reduced_ext) <- c("topoWet","tri","aridityIndexThornthwaite", "growingDegDays5", "PETseasonality")
cc8550_reduced_ext <- stack(cc85bi50_reduced_ext, cc85en50_reduced_ext)

files <- list.files(path="datainputs/gcm/fore_50/mr26bi50", pattern="tif", full.names=TRUE )
mr26bi50 <- stack(files)
names(mr26bi50) <- c("bio14", "bio19", "bio4", "bio5", "bio8")
mr26bi50_reduced_ext <- crop(mr26bi50, d_ext)
files <- list.files(path="datainputs/gcm/envirem/mr26en50", pattern="tif", full.names=TRUE )
mr26en50a <- stack(files[1:2])
mr26en50_reduced_ext <- crop(mr26en50a, d_ext)
mr26en50b <- stack(files[3:5]) #these were cropped when created
mr26en50_reduced_ext <- stack(c(mr26en50_reduced_ext, mr26en50b))
names(mr26en50_reduced_ext) <- c("topoWet","tri","aridityIndexThornthwaite", "growingDegDays5", "PETseasonality")
mr2650_reduced_ext <- stack(mr26bi50_reduced_ext, mr26en50_reduced_ext)

files <- list.files(path="datainputs/gcm/fore_50/mr85bi50", pattern="tif", full.names=TRUE )
mr85bi50 <- stack(files)
names(mr85bi50) <- c("bio14", "bio19", "bio4", "bio5", "bio8")
mr85bi50_reduced_ext <- crop(mr85bi50, d_ext)
files <- list.files(path="datainputs/gcm/envirem/mr85en50", pattern="tif", full.names=TRUE )
mr85en50a <- crop(stack(files[1:2]), d_ext)
mr85en50b <- stack(files[3:5])
mr85en50_reduced_ext <- stack(mr85en50a, mr85en50b)
names(mr85en50_reduced_ext) <- c("topoWet","tri","aridityIndexThornthwaite", "growingDegDays5", "PETseasonality")
mr8550_reduced_ext <- stack(mr85bi50_reduced_ext, mr85en50_reduced_ext)

files <- list.files(path="datainputs/gcm/fore_70/cc26bi70", pattern="tif", full.names=TRUE )
cc26bi70 <- stack(files)
names(cc26bi70) <- c("bio14", "bio19", "bio4", "bio5", "bio8")
cc26bi70_reduced_ext <- crop(cc26bi70, d_ext)
files <- list.files(path="datainputs/gcm/envirem/cc26en70", pattern="tif", full.names=TRUE )
cc26en70a <- crop(stack(files[4:5]), d_ext)
cc26en70b <- stack(files[1:3]) #cropped when created
cc26en70_reduced_ext <- stack(cc26en70a, cc26en70b)
names(cc26en70_reduced_ext) <- c("topoWet","tri","aridityIndexThornthwaite", "growingDegDays5", "PETseasonality")
cc2670_reduced_ext <- stack(cc26bi70_reduced_ext, cc26en70_reduced_ext)

files <- list.files(path="datainputs/gcm/fore_70/cc85bi70", pattern="tif", full.names=TRUE )
cc85bi70 <- stack(files)
names(cc85bi70) <- c("bio14", "bio19", "bio4", "bio5", "bio8")
cc85bi70_reduced_ext <- crop(cc85bi70, d_ext)
files <- list.files(path="datainputs/gcm/envirem/cc85en70", pattern="tif", full.names=TRUE )
cc85en70a <- crop(stack(files[4:5]), d_ext)
cc85en70b <- stack(files[1:3]) #cropped when created
cc85en70_reduced_ext <- stack(cc85en70a, cc85en70b)
names(cc85en70_reduced_ext) <- c("topoWet","tri","aridityIndexThornthwaite", "growingDegDays5", "PETseasonality")
cc8570_reduced_ext <- stack(cc85bi70_reduced_ext, cc85en70_reduced_ext)

files <- list.files(path="datainputs/gcm/fore_70/mr26bi70", pattern="tif", full.names=TRUE )
mr26bi70 <- stack(files)
names(mr26bi70) <- c("bio14", "bio19", "bio4", "bio5", "bio8")
mr26bi70_reduced_ext <- crop(mr26bi70, d_ext)
files <- list.files(path="datainputs/gcm/envirem/mr26en70", pattern="tif", full.names=TRUE )
mr26en70a <- crop(stack(files[1:2]), d_ext)
mr26en70b <- stack(files[3:5])
mr26en70_reduced_ext <- stack(mr26en70a, mr26en70b)
names(mr26en70_reduced_ext) <- c("topoWet","tri","aridityIndexThornthwaite", "growingDegDays5", "PETseasonality")
mr2670_reduced_ext <- stack(mr26bi70_reduced_ext, mr26en70_reduced_ext)

files <- list.files(path="datainputs/gcm/fore_70/mr85bi70", pattern="tif", full.names=TRUE )
mr85bi70 <- stack(files)
names(mr85bi70) <- c("bio14", "bio19", "bio4", "bio5", "bio8")
mr85bi70_reduced_ext <- crop(mr85bi70, d_ext)
files <- list.files(path="datainputs/gcm/envirem/mr85en70", pattern="tif", full.names=TRUE )
mr85en70a <- crop(stack(files[1:2]), d_ext)
mr85en70b <- stack(files[3:5])
mr85en70_reduced_ext <- stack(mr85en70a, mr85en70b)
names(mr85en70_reduced_ext) <- c("topoWet","tri","aridityIndexThornthwaite", "growingDegDays5", "PETseasonality")
mr8570_reduced_ext <- stack(mr85bi70_reduced_ext, mr85en70_reduced_ext)

#FUTURE MODEL PROJECTIONS
mod_pred.8_cc2650 <- lapply(model_list[mod.8],  function(x) predict(x, cc2650_reduced_ext))
save(mod_pred.8_cc2650, file = "results/spring2020/modelruns/mod_pred8_cc2650.Rdata")
# load("results/spring2020/modelruns_20200212_largecircles/mod_pred8_cc2650.Rdata")

mod_pred.8_cc8550 <- lapply(model_list[mod.8],  function(x) predict(x, cc8550_reduced_ext))
save(mod_pred.8_cc8550, file = "results/spring2020/modelruns/mod_pred8_cc8550.Rdata")
# load("results/spring2020/modelruns_20200212_largecircles/mod_pred8_cc8550.Rdata")

mod_pred.8_mr2650  <- lapply(model_list[mod.8],  function(x) predict(x, mr2650_reduced_ext))
save(mod_pred.8_mr2650, file = "results/spring2020/modelruns/mod_pred8_mr2650.Rdata")
# load("results/spring2020/modelruns_20200212_largecircles/mod_pred8_mr2650.Rdata")

mod_pred.8_mr8550 <- lapply(model_list[mod.8],  function(x) predict(x, mr8550_reduced_ext))
save(mod_pred.8_mr8550, file = "results/spring2020/modelruns/mod_pred8_mr8550.Rdata")
# load("results/spring2020/modelruns_20200212_largecircles/mod_pred8_mr8550.Rdata")

mod_pred.8_cc2670 <- lapply(model_list[mod.8],  function(x) predict(x, cc2670_reduced_ext))
save(mod_pred.8_cc2670, file = "results/spring2020/modelruns/mod_pred8_cc2670.Rdata")
# load("results/spring2020/modelruns_20200212_largecircles/mod_pred8_cc2670.Rdata")

mod_pred.8_cc8570 <- lapply(model_list[mod.8],  function(x) predict(x, cc8570_reduced_ext))
save(mod_pred.8_cc8570, file = "results/spring2020/modelruns/mod_pred8_cc8570.Rdata")
# load("results/spring2020/modelruns_20200212_largecircles/mod_pred8_cc8570.Rdata")

mod_pred.8_mr2670  <- lapply(model_list[mod.8],  function(x) predict(x, mr2670_reduced_ext))
save(mod_pred.8_mr2670, file = "results/spring2020/modelruns/mod_pred8_mr2670.Rdata")
# load("results/spring2020/modelruns_20200212_largecircles/mod_pred8_mr2670.Rdata")

mod_pred.8_mr8570 <- lapply(model_list[mod.8],  function(x) predict(x, mr8570_reduced_ext))
save(mod_pred.8_mr8570, file = "results/spring2020/modelruns/mod_pred8_mr8570.Rdata")
# load("results/spring2020/modelruns_20200212_largecircles/mod_pred8_mr8570.Rdata")


###--- FUTURE MEAN AND VARIANCE MODELS ----
mod_pred.8_cc2650_stack <- stack(mod_pred.8_cc2650)
mod_pred.8_cc2650_avg   <- calc(mod_pred.8_cc2650_stack,  mean, na.rm = T)
mod_pred.8_cc2650_var   <- calc(mod_pred.8_cc2650_stack,  var, na.rm = T)

mod_pred.8_cc8550_stack <- stack(mod_pred.8_cc8550)
mod_pred.8_cc8550_avg   <- calc(mod_pred.8_cc8550_stack,  mean, na.rm = T)
mod_pred.8_cc8550_var   <- calc(mod_pred.8_cc8550_stack,  var, na.rm = T)

mod_pred.8_mr2650_stack <- stack(mod_pred.8_mr2650)
mod_pred.8_mr2650_avg   <- calc( mod_pred.8_mr2650_stack,  mean, na.rm = T)
mod_pred.8_mr2650_var   <- calc( mod_pred.8_mr2650_stack,  var, na.rm = T)

mod_pred.8_mr8550_stack <- stack(mod_pred.8_mr8550)
mod_pred.8_mr8550_avg   <- calc( mod_pred.8_mr8550_stack,  mean, na.rm = T)
mod_pred.8_mr8550_var   <- calc( mod_pred.8_mr8550_stack,  var, na.rm = T)

mod_pred.8_cc2670_stack <- stack(mod_pred.8_cc2670)
mod_pred.8_cc2670_avg   <- calc( mod_pred.8_cc2670_stack,  mean, na.rm = T)
mod_pred.8_cc2670_var   <- calc( mod_pred.8_cc2670_stack,  var, na.rm = T)

mod_pred.8_cc8570_stack <- stack(mod_pred.8_cc8570)
mod_pred.8_cc8570_avg   <- calc( mod_pred.8_cc8570_stack,  mean, na.rm = T)
mod_pred.8_cc8570_var   <- calc( mod_pred.8_cc8570_stack,  var, na.rm = T)

mod_pred.8_mr2670_stack <- stack(mod_pred.8_mr2670)
mod_pred.8_mr2670_avg   <- calc( mod_pred.8_mr2670_stack,  mean, na.rm = T)
mod_pred.8_mr2670_var   <- calc( mod_pred.8_mr2670_stack,  var, na.rm = T)

mod_pred.8_mr8570_stack <- stack(mod_pred.8_mr8570)
mod_pred.8_mr8570_avg   <- calc( mod_pred.8_mr8570_stack,  mean, na.rm = T)
mod_pred.8_mr8570_var   <- calc( mod_pred.8_mr8570_stack,  var, na.rm = T)


#PROJECTION FIGURES
col <- c("#f7fcf5", "#e5f5e0", "#c7e9c0", "#a1d99b", "#74c476", "#41ab5d", "#238b45", "#006d2c", "#00441b")

# tiff("figures/projections/currentrun/CC2650_20200211.tif",
#      width = 11, height = 6, units = "in", res = 600)
# pdf("figures/projections/currentrun/CC2650_20200211.pdf", width = 11, height = 6)
par(mfrow = c(1, 2), mar = c(2, 2.5, 2, 5))
plot(mod_pred.8_cc2650_avg, col = col, main = "Mean CC2650 AUC>0.8 TSS<0.4 \n NoHinge Beta=1 (n = 16)")
maps::map("state", boundary = T, col = "darkgray", add = T)
suppressWarnings(maps::map(mex, boundary = F, col = "darkgray", add = T))
scalebar(d = 500, xy = c(-93, 26), type = "line")
plot(mod_pred.8_cc2650_var, main = "Variance CC2650 AUC>0.8 TSS<0.4 \n NoHinge Beta=1 (n = 16)")
maps::map("state", boundary = T, col = "darkgray", add = T)
suppressWarnings(maps::map(mex, boundary = F, col = "darkgray", add = T))
scalebar(d = 500, xy = c(-93, 26), type = "line")
# dev.off()
 
# tiff("figures/projections/currentrun/CC8550_20200211.tif",
#      width = 11, height = 6, units = "in", res = 600)
# pdf("figures/projections/currentrun/CC8550_20200211.pdf", width = 11, height = 6)
par(mfrow = c(1, 2), mar = c(2, 2.5, 2, 5))
plot(mod_pred.8_cc8550_avg, col = col, main = "Mean CC8550 AUC>0.8 TSS<0.4 \n NoHinge Beta=1 (n = 16)")
maps::map("state", boundary = T, col = "darkgray", add = T)
suppressWarnings(maps::map(mex, boundary = F, col = "darkgray", add = T))
scalebar(d = 500, xy = c(-93, 26), type = "line")
plot(mod_pred.8_cc8550_var, main = "Variance CC8550 AUC>0.8 TSS<0.4 \n NoHinge Beta=1 (n = 16)")
maps::map("state", boundary = T, col = "darkgray", add = T)
suppressWarnings(maps::map(mex, boundary = F, col = "darkgray", add = T))
scalebar(d = 500, xy = c(-93, 26), type = "line")
# dev.off()

# tiff("figures/projections/currentrun/MR2650_20200211.tif",
#      width = 11, height = 6, units = "in", res = 600)
# pdf("figures/projections/currentrun/MR2650_20200211.pdf", width = 11, height = 6)
par(mfrow = c(1, 2), mar = c(2, 2.5, 2, 5))
plot(mod_pred.8_mr2650_avg, col = col, main = "Mean MR2650 AUC>0.8 TSS<0.4 \n NoHinge Beta=1 (n = 16)")
maps::map("state", boundary = T, col = "darkgray", add = T)
suppressWarnings(maps::map(mex, boundary = F, col = "darkgray", add = T))
scalebar(d = 500, xy = c(-93, 26), type = "line")
plot(mod_pred.8_mr2650_var, main = "Variance MR2650 AUC>0.8 TSS<0.4 \n NoHinge Beta=1 (n = 16)")
maps::map("state", boundary = T, col = "darkgray", add = T)
suppressWarnings(maps::map(mex, boundary = F, col = "darkgray", add = T))
scalebar(d = 500, xy = c(-93, 26), type = "line")
# dev.off()

# tiff("figures/projections/currentrun/MR8550_20200211.tif",
#      width = 11, height = 6, units = "in", res = 600)
# pdf("figures/projections/currentrun/MR8550_20200211.pdf", width = 11, height = 6)
par(mfrow = c(1, 2), mar = c(2, 2.5, 2, 5))
plot(mod_pred.8_mr8550_avg, col = col, main = "Mean MR8550 AUC>0.8 TSS<0.4 \n NoHinge Beta=1 (n = 16)")
maps::map("state", boundary = T, col = "darkgray", add = T)
suppressWarnings(maps::map(mex, boundary = F, col = "darkgray", add = T))
scalebar(d = 500, xy = c(-93, 26), type = "line")
plot(mod_pred.8_mr8550_var, main = "Variance MR8550 AUC>0.8 TSS<0.4 \n NoHinge Beta=1 (n = 16)")
maps::map("state", boundary = T, col = "darkgray", add = T)
suppressWarnings(maps::map(mex, boundary = F, col = "darkgray", add = T))
scalebar(d = 500, xy = c(-93, 26), type = "line")
# dev.off()

# tiff("figures/projections/currentrun/CC2670_20200211.tif",
#      width = 11, height = 6, units = "in", res = 600)
# pdf("figures/projections/currentrun/CC2670_20200211.pdf", width = 11, height = 6)
par(mfrow = c(1, 2), mar = c(2, 2.5, 2, 5))
plot(mod_pred.8_cc2670_avg, col = col, main = "Mean CC2670 AUC>0.8 TSS<0.4 \n NoHinge Beta=1 (n = 16)")
maps::map("state", boundary = T, col = "darkgray", add = T)
suppressWarnings(maps::map(mex, boundary = F, col = "darkgray", add = T))
scalebar(d = 500, xy = c(-93, 26), type = "line")
plot(mod_pred.8_cc2670_var, main = "Variance CC2670 AUC>0.8 TSS<0.4 \n NoHinge Beta=1 (n = 16)")
maps::map("state", boundary = T, col = "darkgray", add = T)
suppressWarnings(maps::map(mex, boundary = F, col = "darkgray", add = T))
scalebar(d = 500, xy = c(-93, 26), type = "line")
# dev.off()

# tiff("figures/projections/currentrun/CC8570_20200211.tif",
#      width = 11, height = 6, units = "in", res = 600)
# pdf("figures/projections/currentrun/CC8570_20200211.pdf", width = 11, height = 6)
par(mfrow = c(1, 2), mar = c(2, 2.5, 2, 5))
plot(mod_pred.8_cc8570_avg, col = col, main = "Mean CC8570 AUC>0.8 TSS<0.4 \n NoHinge Beta=1 (n = 16)")
maps::map("state", boundary = T, col = "darkgray", add = T)
suppressWarnings(maps::map(mex, boundary = F, col = "darkgray", add = T))
scalebar(d = 500, xy = c(-93, 26), type = "line")
plot(mod_pred.8_cc8570_var, main = "Variance CC8570 AUC>0.8 TSS<0.4 \n NoHinge Beta=1 (n = 16)")
maps::map("state", boundary = T, col = "darkgray", add = T)
suppressWarnings(maps::map(mex, boundary = F, col = "darkgray", add = T))
scalebar(d = 500, xy = c(-93, 26), type = "line")
# dev.off()

# tiff("figures/projections/currentrun/MR2670_2020021.tif",
#      width = 11, height = 6, units = "in", res = 600)
# pdf("figures/projections/currentrun/MR2670_20200211.pdf", width = 11, height = 6)
par(mfrow = c(1, 2), mar = c(2, 2.5, 2, 5))
plot(mod_pred.8_mr2670_avg, col = col, main = "Mean MR2670 AUC>0.8 TSS<0.4 \n NoHinge Beta=1 (n = 16)")
maps::map("state", boundary = T, col = "darkgray", add = T)
suppressWarnings(maps::map(mex, boundary = F, col = "darkgray", add = T))
scalebar(d = 500, xy = c(-93, 26), type = "line")
plot(mod_pred.8_mr2670_var, main = "Variance MR2670 AUC>0.8 TSS<0.4 \n NoHinge Beta=1 (n = 16)")
maps::map("state", boundary = T, col = "darkgray", add = T)
suppressWarnings(maps::map(mex, boundary = F, col = "darkgray", add = T))
scalebar(d = 500, xy = c(-93, 26), type = "line")
# dev.off()

# tiff("figures/projections/currentrun/MR8570_20200211.tif",
#      width = 11, height = 6, units = "in", res = 600)
# pdf("figures/projections/currentrun/MR8570_20200211.pdf", width = 11, height = 6)
par(mfrow = c(1, 2), mar = c(2, 2.5, 2, 5))
plot(mod_pred.8_mr8570_avg, col = col, main = "Mean MR8570 AUC>0.8 TSS<0.4 \n NoHinge Beta=1 (n = 16)")
maps::map("state", boundary = T, col = "darkgray", add = T)
suppressWarnings(maps::map(mex, boundary = F, col = "darkgray", add = T))
scalebar(d = 500, xy = c(-93, 26), type = "line")
plot(mod_pred.8_mr8570_var, main = "Variance MR8570 AUC>0.8 TSS<0.4 \n NoHinge Beta=1 (n = 16)")
maps::map("state", boundary = T, col = "darkgray", add = T)
suppressWarnings(maps::map(mex, boundary = F, col = "darkgray", add = T))
scalebar(d = 500, xy = c(-93, 26), type = "line")
# dev.off()
