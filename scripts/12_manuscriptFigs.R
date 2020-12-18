library(rgdal)
library(maptools)
library(maps)
library(raster)
library(envirem)
library(cowplot)
library(sf)
library(ggsn)
library(ggpubr)
library(gridExtra)
library(tidyverse)

options(scipen = 9999)

### Fig 1 - Distribution Points -----------
comb_data <- read.csv("datainputs/comb_data_28Jun19.csv")

data(wrld_simpl)
states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
mex <- readOGR("datainputs/mexstates", "mexstates")
mex_df <- st_as_sf(mex)

head(comb_data)

r_ext <- extent(c(-117, -86, 20.92, 48))

ggplot() +
  geom_point(data = comb_data, aes(x = Longitude, y = Latitude), alpha = 0.5, cex = 0.7) +
  geom_sf(data = states, fill = NA, size = 0.2) +
  geom_sf(data = mex_df, fill = NA, size = 0.2) +
  coord_sf(xlim = r_ext[1:2], ylim = r_ext[3:4], expand = FALSE) +
  ggsn::scalebar(dist = 350, dist_unit = "km", transform = T, model = "WGS84",
                 st.bottom = F, anchor = c(x = -88.5, y = 22.7), st.dist = 0.025,
                 st.size = 2, height = 0.02, border.size = 0.2,
                 x.min = r_ext[1], x.max = r_ext[2], y.min = r_ext[3], y.max = r_ext[4]) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        panel.border = element_rect(colour = "black", size = 0.5),
        legend.title = element_blank(),
        legend.key.size = unit(4, "mm"), legend.text = element_text(size = 6),
        axis.text = element_text(colour = "black", size = 7))

ggsave("figures/manuscript/F1_distribution.tiff", height = 89, width = 79, units = "mm",
       dpi = 1200)

#
### Fig 2 - Model Evals --------------
load("results/spring2020/modelruns_20200212_largecircles/model_tracker_eval.RData")

head(model_tracker, 2)
model_tracker$Climate <- as.factor(model_tracker$Climate)
model_tracker$Folds   <- as.factor(model_tracker$Folds)
levels(model_tracker$Background)
levels(model_tracker$Background) <- c("C1k", "C10k", "BC1k", "BC10k", "E1k", 
                                      "E10k", "BE1k", "BE10k")
model_tracker$Filters <- recode(model_tracker$Filters, None = "No Filter", 
                                Env.15 = "Narrow Filter", Env.3 = "Mid Filter",
                                Env.75 = "Broad Filter")
model_tracker$Climate <- recode(model_tracker$Climate, predictors1 = "Variable Set 1", 
                                predictors2 = "Variable Set 2", predictors3 = "Variable Set 3")

cbPalette <- c("#333333", "#D55E00", "#56B4E9", "#009E73", "#AA4499")

auc_pl <- ggplot(model_tracker, aes(x = Background, y = AUC)) +
  geom_boxplot(cex = 0.4, outlier.shape = NA, color = "grey30") +
  geom_jitter(aes(color = Folds, shape = Climate), cex = 0.8) + 
  facet_wrap(~Filters) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1, by = 0.2), expand = c(0,0)) +
  scale_color_manual(values = cbPalette) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(colour = "black", size = 6, angle = -45, hjust = 0), 
        axis.text.y = element_text(colour = "black", size = 6),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 6),
        axis.title.x = element_blank(), axis.title.y = element_text(size = 8),
        legend.text = element_text(size = 6), 
        legend.title = element_text(size = 6),
        legend.key.size = unit(3,"mm"))
auc_pl
tss_pl <-  ggplot(model_tracker, aes(x = Background, y = TSS_maxSSS)) +
  geom_boxplot(outlier.shape = NA, cex = 0.4, color = "grey30") +
  geom_jitter(aes(color = Folds, shape = Climate), cex = 0.8) + 
  facet_wrap(~Filters) +
  scale_y_continuous("TSS", limits = c(-0.8, 0.8), breaks = seq(-0.8, 0.8, by = 0.2), expand = c(0,0)) +
  scale_color_manual(values = cbPalette) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(colour = "black", size = 6, angle = -45, hjust = 0),
        axis.text.y = element_text(colour = "black", size = 6),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 6),
        axis.title.x = element_blank(), axis.title.y = element_text(size = 8),
        legend.text = element_text(size = 6), 
        legend.title = element_text(size = 6),
        legend.key.size = unit(3,"mm"))

F2 <- ggarrange(auc_pl, tss_pl, labels = c("(a)","(b)"), common.legend = T,
                legend = "bottom")
ggsave("figures/manuscript/F2_ModEval_v2.tiff", height = 100, width = 168, units = "mm",
       dpi = 1200)
#
### Fig 3 - Variable contribution and permutation importance -----------------
load("results/spring2020/modelruns_20200212_largecircles/model_list.RData")

mod.8 <- which(model_tracker$AUC >= 0.8 & model_tracker$TSS_maxSSS > 0.4)
mod.8

## Variable Importance Plots
# Isolate the model results and Pull out the model importance variables
model_list_results <- lapply(model_list[mod.8], FUN = function(x) { as.data.frame(x@results) })
model_list_results <- lapply(model_list_results, function(x) { x$Name <-rownames(x); return(x)})
perm_imp <- lapply(model_list_results, 
                   function(x) { x[grep(pattern = ".permutation.importance", x = rownames(x)),] })
perm_imp <- lapply(perm_imp, function(x) { x$Name <- gsub(".permutation.importance", "", x$Name) ; return(x) })

varPI <- data.frame(model_tracker[row.names(model_tracker) %in% mod.8, 1:4])

var1PI <- varPI[1:6,]
var1PI$Bio4    <- unlist(lapply(perm_imp[1:6], function(x) { x[x$Name == "bio4", "V1"] }))
var1PI$Bio5    <- unlist(lapply(perm_imp[1:6], function(x) { x[x$Name == "bio5", "V1"] }))
var1PI$Bio8    <- unlist(lapply(perm_imp[1:6], function(x) { x[x$Name == "bio8", "V1"] }))
var1PI$Bio19   <- unlist(lapply(perm_imp[1:6], function(x) { x[x$Name == "bio19", "V1"] }))
var1PI

var2PI <- varPI[7:10,]
var2PI$PETs  <- unlist(lapply(perm_imp[7:10], function(x) { x[x$Name == "PETseasonality", "V1"] }))
var2PI$Arid  <- unlist(lapply(perm_imp[7:10], function(x) { x[x$Name == "aridityIndexThornthwaite", "V1"] }))
var2PI$Bio14 <- unlist(lapply(perm_imp[7:10], function(x) { x[x$Name == "bio14", "V1"] }))
var2PI$GDD5  <- unlist(lapply(perm_imp[7:10], function(x) { x[x$Name == "growingDegDays5", "V1"] }))
var2PI

var3PI <- varPI[11:16,]
var3PI$Bio5    <- unlist(lapply(perm_imp[11:16], function(x) { x[x$Name == "bio5", "V1"] }))
var3PI$Bio8    <- unlist(lapply(perm_imp[11:16], function(x) { x[x$Name == "bio8", "V1"] }))
var3PI$PETs    <- unlist(lapply(perm_imp[11:16], function(x) { x[x$Name == "PETseasonality", "V1"] }))
var3PI$Arid    <- unlist(lapply(perm_imp[11:16], function(x) { x[x$Name == "aridityIndexThornthwaite", "V1"] }))
var3PI$Bio14   <- unlist(lapply(perm_imp[11:16], function(x) { x[x$Name == "bio14", "V1"] }))
var3PI$GDD5    <- unlist(lapply(perm_imp[11:16], function(x) { x[x$Name == "growingDegDays5", "V1"] }))
var3PI

var1PI_long <- gather(var1PI, key = Variable, value = PermImp, Bio4:Bio19, factor_key=TRUE) #tri OR bio19
var2PI_long <- gather(var2PI, key = Variable, value = PermImp, PETs:GDD5, factor_key=TRUE)
var3PI_long <- gather(var3PI, key = Variable, value = PermImp, Bio5:GDD5, factor_key=TRUE) #tri OR GDD5

varPI_long <- rbind(var1PI_long, var2PI_long, var3PI_long)
#varPI_long$Variable <- str_to_title(varPI_long$Variable)

PI_pl <- ggplot(data.frame(varPI_long), aes(x = Variable, y = PermImp)) +
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(color = Climate, shape = Background)) +
  scale_y_continuous("Permutation Importance", limits = c(0, 55), breaks = seq(0, 50, by = 10)) +
  #facet_wrap(~ Folds, nrow = 1) +
  scale_color_manual(values = cbPalette) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(colour = "black", size = 8, angle = -45, hjust = 0), 
        axis.text.y = element_text(colour = "black", size = 8),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 6),
        axis.title.x = element_blank(), axis.title.y = element_text(size = 8),
        legend.text = element_text(size = 8), 
        legend.title = element_text(size = 8),
        legend.key.size = unit(3,"mm"))

## Variable Contribution Plots
var_cont <- lapply(model_list_results, 
                   function(x) { x[grep(pattern = ".contribution", x = rownames(x)),] })
var_cont <- lapply(var_cont, function(x) { x$Name <- gsub(".contribution", "", x$Name) ; return(x) })

varVC <- data.frame(model_tracker[row.names(model_tracker) %in% mod.8, 1:4])

var1VC <- varPI[1:6,]
var1VC$Bio4    <- unlist(lapply(var_cont[1:6], function(x) { x[x$Name == "bio4", "V1"] }))
var1VC$Bio5    <- unlist(lapply(var_cont[1:6], function(x) { x[x$Name == "bio5", "V1"] }))
var1VC$Bio8    <- unlist(lapply(var_cont[1:6], function(x) { x[x$Name == "bio8", "V1"] }))
var1VC$Bio19   <- unlist(lapply(var_cont[1:6], function(x) { x[x$Name == "bio19", "V1"] }))
var1VC

var2VC <- varPI[7:10,]
var2VC$PETs  <- unlist(lapply(var_cont[7:10], function(x) { x[x$Name == "PETseasonality", "V1"] }))
var2VC$Arid  <- unlist(lapply(var_cont[7:10], function(x) { x[x$Name == "aridityIndexThornthwaite", "V1"] }))
var2VC$Bio14 <- unlist(lapply(var_cont[7:10], function(x) { x[x$Name == "bio14", "V1"] }))
var2VC$GDD5  <- unlist(lapply(var_cont[7:10], function(x) { x[x$Name == "growingDegDays5", "V1"] }))
var2VC

var3VC <- varPI[11:16,]
var3VC$Bio5    <- unlist(lapply(var_cont[11:16], function(x) { x[x$Name == "bio5", "V1"] }))
var3VC$Bio8    <- unlist(lapply(var_cont[11:16], function(x) { x[x$Name == "bio8", "V1"] }))
var3VC$PETs    <- unlist(lapply(var_cont[11:16], function(x) { x[x$Name == "PETseasonality", "V1"] }))
var3VC$Arid    <- unlist(lapply(var_cont[11:16], function(x) { x[x$Name == "aridityIndexThornthwaite", "V1"] }))
var3VC$Bio14   <- unlist(lapply(var_cont[11:16], function(x) { x[x$Name == "bio14", "V1"] }))
var3VC$GDD5    <- unlist(lapply(var_cont[11:16], function(x) { x[x$Name == "growingDegDays5", "V1"] }))
var3VC

var1VC_long <- gather(var1VC, key = Variable, value = varCont, Bio4:Bio19, factor_key=TRUE) #tri OR bio19
var2VC_long <- gather(var2VC, key = Variable, value = varCont, PETs:GDD5, factor_key=TRUE)
var3VC_long <- gather(var3VC, key = Variable, value = varCont, Bio5:GDD5, factor_key=TRUE) #tri OR GDD5

varVC_long <- rbind(var1VC_long, var2VC_long, var3VC_long)
#varVC_long$Variable <- str_to_title(varVC_long$Variable)

VCpl <- ggplot(data.frame(varVC_long), aes(x = Variable, y = varCont)) +
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(color = Climate, shape = Background)) +
  scale_y_continuous("Variable Contribution", limits = c(0, 55), breaks = seq(0, 50, by = 10)) +
 # facet_wrap(~ Folds, nrow = 1) +
  scale_color_manual(values = cbPalette) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(colour = "black", size = 8, angle = -45, hjust = 0), 
        axis.text.y = element_text(colour = "black", size = 8),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 6),
        axis.title.x = element_blank(), axis.title.y = element_text(size = 8),
        legend.text = element_text(size = 8), 
        legend.title = element_text(size = 8),
        legend.key.size = unit(3,"mm"))

F3 <- ggarrange(VCpl, PI_pl, labels = c("(a)","(b)"), common.legend = T,
                legend = "right")
ggsave("figures/manuscript/F3_VarCont_PermImp_v2.tiff", height = 79, width = 168, units = "mm",
       dpi = 1200)
#
### Figs 4-7 Raster figs ----------

load("results/spring2020/modelruns_20200212_largecircles/model_predict.RData")

load("results/spring2020/modelruns_20200212_largecircles/model_predict_cclgm.RData")
load("results/spring2020/modelruns_20200212_largecircles/model_predict_mrlgm.Rdata")
load("results/spring2020/modelruns_20200212_largecircles/mod_pred_ccmid.Rdata")
load("results/spring2020/modelruns_20200212_largecircles/mod_pred_mrmid.Rdata")

load("results/spring2020/modelruns_20200212_largecircles/mod_pred8_cc2650.Rdata")
load("results/spring2020/modelruns_20200212_largecircles/mod_pred8_cc8550.Rdata")
load("results/spring2020/modelruns_20200212_largecircles/mod_pred8_mr2650.Rdata")
load("results/spring2020/modelruns_20200212_largecircles/mod_pred8_mr8550.Rdata")
load("results/spring2020/modelruns_20200212_largecircles/mod_pred8_cc2670.Rdata")
load("results/spring2020/modelruns_20200212_largecircles/mod_pred8_cc8570.Rdata")
load("results/spring2020/modelruns_20200212_largecircles/mod_pred8_mr2670.Rdata")
load("results/spring2020/modelruns_20200212_largecircles/mod_pred8_mr8570.Rdata")

mod.8 <- which(model_tracker$AUC >= 0.8 & model_tracker$TSS_maxSSS > 0.4) #same for TSS > 0.4

## Set them up  
pred.8_stack <- stack(mod_pred[mod.8])
pred.8_mean  <- calc(pred.8_stack, mean, na.rm = T)
pred.8_var   <- calc(pred.8_stack, var, na.rm = T)

cclgm_m <- calc(stack(mod_pred_cclgm[mod.8]), mean, na.rm = T)
cchol_m <- calc(stack(mod_pred_ccmid[mod.8]), mean, na.rm = T)
mrlgm_m <- calc(stack(mod_pred_mrlgm[mod.8]), mean, na.rm = T)
mrhol_m <- calc(stack(mod_pred_mrmid[mod.8]), mean, na.rm = T)
cclgm_v <- calc(stack(mod_pred_cclgm[mod.8]), var, na.rm = T)
cchol_v <- calc(stack(mod_pred_ccmid[mod.8]), var, na.rm = T)
mrlgm_v <- calc(stack(mod_pred_mrlgm[mod.8]), var, na.rm = T)
mrhol_v <- calc(stack(mod_pred_mrmid[mod.8]), var, na.rm = T)

cc2650_m <- calc(stack(mod_pred.8_cc2650[-7]), mean, na.rm = T)
cc2670_m <- calc(stack(mod_pred.8_cc2670[-7]), mean, na.rm = T)
cc8550_m <- calc(stack(mod_pred.8_cc8550[-7]), mean, na.rm = T)
cc8570_m <- calc(stack(mod_pred.8_cc8570[-7]), mean, na.rm = T)
mr2650_m <- calc(stack(mod_pred.8_mr2650[-7]), mean, na.rm = T)
mr2670_m <- calc(stack(mod_pred.8_mr2670[-7]), mean, na.rm = T)
mr8550_m <- calc(stack(mod_pred.8_mr8550[-7]), mean, na.rm = T)
mr8570_m <- calc(stack(mod_pred.8_mr8570[-7]), mean, na.rm = T)
cc2650_v <- calc(stack(mod_pred.8_cc2650[-7]), var, na.rm = T)
cc2670_v <- calc(stack(mod_pred.8_cc2670[-7]), var, na.rm = T)
cc8550_v <- calc(stack(mod_pred.8_cc8550[-7]), var, na.rm = T)
cc8570_v <- calc(stack(mod_pred.8_cc8570[-7]), var, na.rm = T)
mr2650_v <- calc(stack(mod_pred.8_mr2650[-7]), var, na.rm = T)
mr2670_v <- calc(stack(mod_pred.8_mr2670[-7]), var, na.rm = T)
mr8550_v <- calc(stack(mod_pred.8_mr8550[-7]), var, na.rm = T)
mr8570_v <- calc(stack(mod_pred.8_mr8570[-7]), var, na.rm = T)

## Crop all rasters to the same extent
r_ext <- extent(c(-117, -86, 20.92, 48))

pred.8_mean  <- crop(pred.8_mean, r_ext)
pred.8_var   <- crop(pred.8_var, r_ext)

cclgm_m <- crop(cclgm_m, r_ext)
cchol_m <- crop(cchol_m, r_ext)
mrlgm_m <- crop(mrlgm_m, r_ext)
mrhol_m <- crop(mrhol_m, r_ext)
cclgm_v <- crop(cclgm_v, r_ext)
cchol_v <- crop(cchol_v, r_ext)
mrlgm_v <- crop(mrlgm_v, r_ext)
mrhol_v <- crop(mrhol_v, r_ext)

cc2650_m <- crop(cc2650_m, r_ext)
cc2670_m <- crop(cc2670_m, r_ext)
cc8550_m <- crop(cc8550_m, r_ext)
cc8570_m <- crop(cc8570_m, r_ext)
mr2650_m <- crop(mr2650_m, r_ext)
mr2670_m <- crop(mr2670_m, r_ext)
mr8550_m <- crop(mr8550_m, r_ext)
mr8570_m <- crop(mr8570_m, r_ext)
cc2650_v <- crop(cc2650_v, r_ext)
cc2670_v <- crop(cc2670_v, r_ext)
cc8550_v <- crop(cc8550_v, r_ext)
cc8570_v <- crop(cc8570_v, r_ext)
mr2650_v <- crop(mr2650_v, r_ext)
mr2670_v <- crop(mr2670_v, r_ext)
mr8550_v <- crop(mr8550_v, r_ext)
mr8570_v <- crop(mr8570_v, r_ext)

cur_lgm <- pred.8_mean - mrlgm_m
cur_8570 <- pred.8_mean - mr8570_m

## Let's plot it all
col <- c("white","#f7fcf5", "#e5f5e0", "#c7e9c0", "#a1d99b", "#74c476", "#41ab5d",
         "#238b45", "#006d2c", "#00441b")

## switch raster to df to plot using ggplot
pred8_mdf <- data.frame(rasterToPoints(pred.8_mean), type = "mean")
pred8_vdf <- data.frame(rasterToPoints(pred.8_var), type = "vari")

cclgm_mdf <- data.frame(rasterToPoints(cclgm_m), type = "mean")
cchol_mdf <- data.frame(rasterToPoints(cchol_m), type = "mean")
mrlgm_mdf <- data.frame(rasterToPoints(mrlgm_m), type = "mean")
mrhol_mdf <- data.frame(rasterToPoints(mrhol_m), type = "mean")
cclgm_vdf <- data.frame(rasterToPoints(cclgm_v), type = "vari")
cchol_vdf <- data.frame(rasterToPoints(cchol_v), type = "vari")
mrlgm_vdf <- data.frame(rasterToPoints(mrlgm_v), type = "vari")
mrhol_vdf <- data.frame(rasterToPoints(mrhol_v), type = "vari")

cc2650_mdf <- data.frame(rasterToPoints(cc2650_m), type = "mean")
cc2670_mdf <- data.frame(rasterToPoints(cc2670_m), type = "mean")
cc8550_mdf <- data.frame(rasterToPoints(cc8550_m), type = "mean")
cc8570_mdf <- data.frame(rasterToPoints(cc8570_m), type = "mean")
mr2650_mdf <- data.frame(rasterToPoints(mr2650_m), type = "mean")
mr2670_mdf <- data.frame(rasterToPoints(mr2670_m), type = "mean")
mr8550_mdf <- data.frame(rasterToPoints(mr8550_m), type = "mean")
mr8570_mdf <- data.frame(rasterToPoints(mr8570_m), type = "mean")
cc2650_vdf <- data.frame(rasterToPoints(cc2650_v), type = "vari")
cc2670_vdf <- data.frame(rasterToPoints(cc2670_v), type = "vari")
cc8550_vdf <- data.frame(rasterToPoints(cc8550_v), type = "vari")
cc8570_vdf <- data.frame(rasterToPoints(cc8570_v), type = "vari")
mr2650_vdf <- data.frame(rasterToPoints(mr2650_v), type = "vari")
mr2670_vdf <- data.frame(rasterToPoints(mr2670_v), type = "vari")
mr8550_vdf <- data.frame(rasterToPoints(mr8550_v), type = "vari")
mr8570_vdf <- data.frame(rasterToPoints(mr8570_v), type = "vari")

cur_lgm_df  <- data.frame(rasterToPoints(cur_lgm), type = "mean")
cur_8570_df <- data.frame(rasterToPoints(cur_8570), type = "mean") 

## GGplot individual rasters 
mpredpl <- 
  ggplot() +
  geom_raster(data = pred8_mdf, aes(x = x, y = y, fill = layer)) +
  scale_fill_distiller(palette = "YlGnBu", direction = 1, limits = c(0,1), 
                       breaks = seq(0,1, by = 0.2)) +
  ggsn::scalebar(dist = 350, dist_unit = "km", transform = T, model = "WGS84",
                 st.bottom = F, anchor = c(x = -88.5, y = 22.7), st.dist = 0.025,
                 st.size = 2, height = 0.02, border.size = 0.2,
                 x.min = r_ext[1], x.max = r_ext[2], y.min = r_ext[3], y.max = r_ext[4]) +
  geom_rect(mapping = aes(xmin = -92 , xmax = r_ext[2], ymin = r_ext[3], ymax =22.6), fill = "white") +
  geom_sf(data = states, fill = NA, size = 0.2) +
  geom_sf(data = mex_df, fill = NA, size = 0.2) +
  coord_sf(xlim = r_ext[1:2], ylim = r_ext[3:4], expand = FALSE) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        panel.border = element_rect(colour = "black", size = 0.5),
        legend.title = element_blank(),
        legend.key.size = unit(4, "mm"), legend.text = element_text(size = 6),
        axis.text = element_text(colour = "black", size = 7))

ggsave("figures/manuscript/present_mean.tiff", height = 79, width = 79, units = "mm",
       dpi = 1200)

vpredpl <- 
  ggplot() +
  geom_raster(data = pred8_vdf, aes(x = x, y = y, fill = layer)) +
  scale_fill_gradientn(colours = col, limits = c(0,0.25), breaks = seq(0,0.25, by = 0.05)) +
  ggsn::scalebar(dist = 350, dist_unit = "km", transform = T, model = "WGS84",
                 st.bottom = F, anchor = c(x = -88.5, y = 22.7), st.dist = 0.025,
                 st.size = 2, height = 0.02, border.size = 0.2,
                 x.min = r_ext[1], x.max = r_ext[2], y.min = r_ext[3], y.max = r_ext[4]) +
  geom_rect(mapping = aes(xmin = -92 , xmax = r_ext[2], ymin = r_ext[3], ymax =22.6), fill = "white") +
  geom_sf(data = states, fill = NA, size = 0.2) +
  geom_sf(data = mex_df, fill = NA, size = 0.2) +
  coord_sf(xlim = r_ext[1:2], ylim = r_ext[3:4], expand = FALSE) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        panel.border = element_rect(colour = "black", size = 0.5),
        legend.title = element_blank(),
        legend.key.size = unit(4, "mm"), legend.text = element_text(size = 5),
        axis.text = element_text(colour = "black", size = 7))

ggsave("figures/manuscript/present_var.tiff", height = 79, width = 79, units = "mm",
       dpi = 1200)

## Historic Distribution
CLMpl <- 
  ggplot() +
  geom_raster(data = cclgm_mdf, aes(x = x, y = y, fill = layer)) +
  scale_fill_distiller(palette = "YlGnBu", direction = 1, limits = c(0,1), 
                       breaks = seq(0,1, by = 0.2)) +  ggsn::scalebar(dist = 350, dist_unit = "km", transform = T, model = "WGS84",
                 st.bottom = F, anchor = c(x = -88.4, y = 24.7), st.dist = 0.025,
                 st.size = 2, height = 0.02, border.size = 0.2,
                 x.min = r_ext[1], x.max = r_ext[2], y.min = r_ext[3], y.max = r_ext[4]) +
  geom_rect(mapping = aes(xmin = -92 , xmax = r_ext[2], ymin = r_ext[3], ymax =24.6), fill = "white") +
  geom_sf(data = states, fill = NA, size = 0.2) +
  geom_sf(data = mex_df, fill = NA, size = 0.2) +
  coord_sf(xlim = r_ext[1:2], ylim = r_ext[3:4], expand = FALSE) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        panel.border = element_rect(colour = "black", size = 0.5),
        legend.title = element_blank(),
        legend.key.size = unit(4, "mm"), legend.text = element_text(size = 6),
        axis.text = element_text(colour = "black", size = 7))

ggsave("figures/manuscript/cclgm_mean.tiff", height = 79, width = 79, units = "mm",
       dpi = 1200)

CLVpl <- 
  ggplot() +
  geom_raster(data = cclgm_vdf, aes(x = x, y = y, fill = layer)) +
  scale_fill_gradientn(colours = col, limits = c(0,0.25), breaks = seq(0,0.25, by = 0.05)) +
  ggsn::scalebar(dist = 350, dist_unit = "km", transform = T, model = "WGS84",
                 st.bottom = F, anchor = c(x = -88.4, y = 24.7), st.dist = 0.025,
                 st.size = 2, height = 0.02, border.size = 0.2,
                 x.min = r_ext[1], x.max = r_ext[2], y.min = r_ext[3], y.max = r_ext[4]) +
  geom_rect(mapping = aes(xmin = -92 , xmax = r_ext[2], ymin = r_ext[3], ymax =24.6), fill = "white") +
  geom_sf(data = states, fill = NA, size = 0.2) +
  geom_sf(data = mex_df, fill = NA, size = 0.2) +
  coord_sf(xlim = r_ext[1:2], ylim = r_ext[3:4], expand = FALSE) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        panel.border = element_rect(colour = "black", size = 0.5),
        legend.title = element_blank(),
        legend.key.size = unit(4, "mm"), legend.text = element_text(size = 5),
        axis.text = element_text(colour = "black", size = 7))

ggsave("figures/manuscript/cclgm_var.tiff", height = 79, width = 79, units = "mm",
       dpi = 1200)

MLMpl <- 
  ggplot() +
  geom_raster(data = mrlgm_mdf, aes(x = x, y = y, fill = layer)) +
  scale_fill_distiller(palette = "YlGnBu", direction = 1, limits = c(0,1), 
                       breaks = seq(0,1, by = 0.2)) +  ggsn::scalebar(dist = 350, dist_unit = "km", transform = T, model = "WGS84",
                 st.bottom = F, anchor = c(x = -88.4, y = 24.7), st.dist = 0.025,
                 st.size = 2, height = 0.02, border.size = 0.2,
                 x.min = r_ext[1], x.max = r_ext[2], y.min = r_ext[3], y.max = r_ext[4]) +
  geom_rect(mapping = aes(xmin = -92 , xmax = r_ext[2], ymin = r_ext[3], ymax =24.6), fill = "white") +
  geom_sf(data = states, fill = NA, size = 0.2) +
  geom_sf(data = mex_df, fill = NA, size = 0.2) +
  coord_sf(xlim = r_ext[1:2], ylim = r_ext[3:4], expand = FALSE) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        panel.border = element_rect(colour = "black", size = 0.5),
        legend.title = element_blank(),
        legend.key.size = unit(4, "mm"), legend.text = element_text(size = 6),
        axis.text = element_text(colour = "black", size = 7))

ggsave("figures/manuscript/mrlgm_mean.tiff", height = 79, width = 79, units = "mm",
       dpi = 1200)

MLVpl <- 
  ggplot() +
  geom_raster(data = mrlgm_vdf, aes(x = x, y = y, fill = layer)) +
  scale_fill_gradientn(colours = col, limits = c(0,0.25), breaks = seq(0,0.25, by = 0.05)) +
  ggsn::scalebar(dist = 350, dist_unit = "km", transform = T, model = "WGS84",
                 st.bottom = F, anchor = c(x = -88.4, y = 24.7), st.dist = 0.025,
                 st.size = 2, height = 0.02, border.size = 0.2,
                 x.min = r_ext[1], x.max = r_ext[2], y.min = r_ext[3], y.max = r_ext[4]) +
  geom_rect(mapping = aes(xmin = -92 , xmax = r_ext[2], ymin = r_ext[3], ymax =24.6), fill = "white") +
  geom_sf(data = states, fill = NA, size = 0.2) +
  geom_sf(data = mex_df, fill = NA, size = 0.2) +
  coord_sf(xlim = r_ext[1:2], ylim = r_ext[3:4], expand = FALSE) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        panel.border = element_rect(colour = "black", size = 0.5),
        legend.title = element_blank(),
        legend.key.size = unit(4, "mm"), legend.text = element_text(size = 5),
        axis.text = element_text(colour = "black", size = 7))

ggsave("figures/manuscript/mrlgm_var.tiff", height = 79, width = 79, units = "mm",
       dpi = 1200)

## Future Distribution
C8570Mpl <- 
  ggplot() +
  geom_raster(data = cc8570_mdf, aes(x = x, y = y, fill = layer)) +
  scale_fill_distiller(palette = "YlGnBu", direction = 1, limits = c(0,1), 
                       breaks = seq(0,1, by = 0.2)) +  
  ggsn::scalebar(dist = 350, dist_unit = "km", transform = T, model = "WGS84",
                 st.bottom = F, anchor = c(x = -88.4, y = 24.7), st.dist = 0.025,
                 st.size = 2, height = 0.02, border.size = 0.2,
                 x.min = r_ext[1], x.max = r_ext[2], y.min = r_ext[3], y.max = r_ext[4]) +
  geom_rect(mapping = aes(xmin = -92 , xmax = r_ext[2], ymin = r_ext[3], ymax =24.6), fill = "white") +
  geom_sf(data = states, fill = NA, size = 0.2) +
  geom_sf(data = mex_df, fill = NA, size = 0.2) +
  coord_sf(xlim = r_ext[1:2], ylim = r_ext[3:4], expand = FALSE) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        panel.border = element_rect(colour = "black", size = 0.5),
        legend.title = element_blank(),
        legend.key.size = unit(4, "mm"), legend.text = element_text(size = 6),
        axis.text = element_text(colour = "black", size = 7))

ggsave("figures/manuscript/cc8570_mean.tiff", height = 79, width = 79, units = "mm",
       dpi = 1200)

C8570Vpl <- 
  ggplot() +
  geom_raster(data = cc8570_vdf, aes(x = x, y = y, fill = layer)) +
  scale_fill_gradientn(colours = col, limits = c(0,0.25), breaks = seq(0,0.25, by = 0.05)) +
  ggsn::scalebar(dist = 350, dist_unit = "km", transform = T, model = "WGS84",
                 st.bottom = F, anchor = c(x = -88.4, y = 24.7), st.dist = 0.025,
                 st.size = 2, height = 0.02, border.size = 0.2,
                 x.min = r_ext[1], x.max = r_ext[2], y.min = r_ext[3], y.max = r_ext[4]) +
  geom_rect(mapping = aes(xmin = -92 , xmax = r_ext[2], ymin = r_ext[3], ymax =24.6), fill = "white") +
  geom_sf(data = states, fill = NA, size = 0.2) +
  geom_sf(data = mex_df, fill = NA, size = 0.2) +
  coord_sf(xlim = r_ext[1:2], ylim = r_ext[3:4], expand = FALSE) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        panel.border = element_rect(colour = "black", size = 0.5),
        legend.title = element_blank(),
        legend.key.size = unit(4, "mm"), legend.text = element_text(size = 5),
        axis.text = element_text(colour = "black", size = 7))

ggsave("figures/manuscript/cc8570_var.tiff", height = 79, width = 79, units = "mm",
       dpi = 1200)

M8570Mpl <- 
  ggplot() +
  geom_raster(data = mr8570_mdf, aes(x = x, y = y, fill = layer)) +
  scale_fill_distiller(palette = "YlGnBu", direction = 1, limits = c(0,1), 
                       breaks = seq(0,1, by = 0.2)) +
  ggsn::scalebar(dist = 350, dist_unit = "km", transform = T, model = "WGS84",
                 st.bottom = F, anchor = c(x = -88.4, y = 24.7), st.dist = 0.025,
                 st.size = 2, height = 0.02, border.size = 0.2,
                 x.min = r_ext[1], x.max = r_ext[2], y.min = r_ext[3], y.max = r_ext[4]) +
  geom_rect(mapping = aes(xmin = -92 , xmax = r_ext[2], ymin = r_ext[3], ymax =24.6), fill = "white") +
  geom_sf(data = states, fill = NA, size = 0.2) +
  geom_sf(data = mex_df, fill = NA, size = 0.2) +
  coord_sf(xlim = r_ext[1:2], ylim = r_ext[3:4], expand = FALSE) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        panel.border = element_rect(colour = "black", size = 0.5),
        legend.title = element_blank(),
        legend.key.size = unit(4, "mm"), legend.text = element_text(size = 6),
        axis.text = element_text(colour = "black", size = 7))

ggsave("figures/manuscript/mr8570_mean.tiff", height = 79, width = 79, units = "mm",
       dpi = 1200)

M8570Vpl <- 
  ggplot() +
  geom_raster(data = mr8570_vdf, aes(x = x, y = y, fill = layer)) +
  scale_fill_gradientn(colours = col, limits = c(0,0.25), breaks = seq(0,0.25, by = 0.05)) +
  ggsn::scalebar(dist = 350, dist_unit = "km", transform = T, model = "WGS84",
                 st.bottom = F, anchor = c(x = -88.4, y = 24.7), st.dist = 0.025,
                 st.size = 2, height = 0.02, border.size = 0.2,
                 x.min = r_ext[1], x.max = r_ext[2], y.min = r_ext[3], y.max = r_ext[4]) +
  geom_rect(mapping = aes(xmin = -92 , xmax = r_ext[2], ymin = r_ext[3], ymax =24.6), fill = "white") +
  geom_sf(data = states, fill = NA, size = 0.2) +
  geom_sf(data = mex_df, fill = NA, size = 0.2) +
  coord_sf(xlim = r_ext[1:2], ylim = r_ext[3:4], expand = FALSE) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        panel.border = element_rect(colour = "black", size = 0.5),
        legend.title = element_blank(),
        legend.key.size = unit(4, "mm"), legend.text = element_text(size = 5),
        axis.text = element_text(colour = "black", size = 7))

ggsave("figures/manuscript/mr8570_var.tiff", height = 79, width = 79, units = "mm",
       dpi = 1200)

## Anomaly maps
cur_lgm_pl <- ggplot() +
  geom_raster(data = cur_lgm_df, aes(x = x, y = y, fill = layer)) +
  scale_fill_distiller(palette = "RdBu", direction = 1, limits = c(-1, 1), breaks = seq(-1,1, by = 0.25)) +
  ggsn::scalebar(dist = 350, dist_unit = "km", transform = T, model = "WGS84",
                 st.bottom = F, anchor = c(x = -88.4, y = 24.7), st.dist = 0.025,
                 st.size = 2, height = 0.02, border.size = 0.2,
                 x.min = r_ext[1], x.max = r_ext[2], y.min = r_ext[3], y.max = r_ext[4]) +
  geom_rect(mapping = aes(xmin = -92 , xmax = r_ext[2], ymin = r_ext[3], ymax =24.6), fill = "white") +
  geom_sf(data = states, fill = NA, size = 0.2) +
  geom_sf(data = mex_df, fill = NA, size = 0.2) +
  coord_sf(xlim = r_ext[1:2], ylim = r_ext[3:4], expand = FALSE) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        panel.border = element_rect(colour = "black", size = 0.5),
        legend.title = element_blank(),
        legend.key.size = unit(6, "mm"), legend.text = element_text(size = 5),
        axis.text = element_text(colour = "black", size = 7))

ggsave("figures/manuscript/anom_cur_lgm.tiff", height = 79, width = 79, units = "mm",
       dpi = 1200)

cur_8570_pl <- ggplot() +
  geom_raster(data = cur_8570_df, aes(x = x, y = y, fill = layer)) +
  scale_fill_distiller(palette = "RdBu", direction = 1, limits = c(-1, 1), breaks = seq(-1,1, by = 0.25)) +
  ggsn::scalebar(dist = 350, dist_unit = "km", transform = T, model = "WGS84",
                 st.bottom = F, anchor = c(x = -88.4, y = 24.7), st.dist = 0.025,
                 st.size = 2, height = 0.02, border.size = 0.2,
                 x.min = r_ext[1], x.max = r_ext[2], y.min = r_ext[3], y.max = r_ext[4]) +
  geom_rect(mapping = aes(xmin = -92 , xmax = r_ext[2], ymin = r_ext[3], ymax =24.6), fill = "white") +
  geom_sf(data = states, fill = NA, size = 0.2) +
  geom_sf(data = mex_df, fill = NA, size = 0.2) +
  coord_sf(xlim = r_ext[1:2], ylim = r_ext[3:4], expand = FALSE) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        panel.border = element_rect(colour = "black", size = 0.5),
        legend.title = element_blank(),
        legend.key.size = unit(6, "mm"), legend.text = element_text(size = 5),
        axis.text = element_text(colour = "black", size = 7))

ggsave("figures/manuscript/anom_cur_8570.tiff", height = 79, width = 79, units = "mm",
       dpi = 1200)

## Manuscript figures

F4 <- ggarrange(mpredpl, vpredpl, nrow = 1, labels = c("(a)","(b)"))#, legend = "bottom")
ggsave("figures/manuscript/F4_present.tiff", F4, height = 79, width = 168, units = "mm",
       dpi = 1200)

F5 <- ggarrange(CLMpl, CLVpl, MLMpl, MLVpl, nrow = 2, ncol = 2, 
                labels = c("(a)","(b)","(c)","(d)"), legend = "bottom")
ggsave("figures/manuscript/F5_LGM2.tiff", F5, height = 168, width = 168, units = "mm",
       dpi = 1200)

F6 <- ggarrange(C8570Mpl, C8570Vpl, M8570Mpl, M8570Vpl, nrow = 2, ncol = 2, 
                labels = c("(a)","(b)","(c)","(d)"), legend = "bottom")
ggsave("figures/manuscript/F6_8570_2.tiff", F6, height = 168, width = 168, units = "mm",
       dpi = 1200)

F7_top <- ggarrange(MLMpl, mpredpl, M8570Mpl, nrow = 1, common.legend = T, legend = "right")
ggsave("figures/manuscript/F7_top.tiff", F7_top, height = 79, width = 200, units = "mm",
       dpi = 1200)
ph <- ggplot() + theme_void()
F7_bottom <- ggarrange(cur_lgm_pl, ph, cur_8570_pl, nrow = 1, widths = c(1, 0.2, 1), 
                   common.legend = T, legend = "right")
ggsave("figures/manuscript/F7_test2.tiff", F7_bottom, height = 79, width = 168, units = "mm",
       dpi = 1200)


### Supplement Fig 1.3-6 Maps -----
##mid-Holocene
CHMpl <- ggplot() +
  geom_raster(data = cchol_mdf, aes(x = x, y = y, fill = layer)) +
  scale_fill_distiller(palette = "YlGnBu", direction = 1, limits = c(0,1), 
                       breaks = seq(0,1, by = 0.2)) +  ggsn::scalebar(dist = 350, dist_unit = "km", transform = T, model = "WGS84",
                                                                      st.bottom = F, anchor = c(x = -88.4, y = 24.7), st.dist = 0.025,
                                                                      st.size = 2, height = 0.02, border.size = 0.2,
                                                                      x.min = r_ext[1], x.max = r_ext[2], y.min = r_ext[3], y.max = r_ext[4]) +
  geom_rect(mapping = aes(xmin = -92 , xmax = r_ext[2], ymin = r_ext[3], ymax =24.6), fill = "white") +
  geom_sf(data = states, fill = NA, size = 0.2) +
  geom_sf(data = mex_df, fill = NA, size = 0.2) +
  coord_sf(xlim = r_ext[1:2], ylim = r_ext[3:4], expand = FALSE) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        panel.border = element_rect(colour = "black", size = 0.5),
        legend.title = element_blank(),
        legend.key.size = unit(4, "mm"), legend.text = element_text(size = 6),
        axis.text = element_text(colour = "black", size = 7))

CHVpl <- ggplot() +
  geom_raster(data = cchol_vdf, aes(x = x, y = y, fill = layer)) +
  scale_fill_gradientn(colours = col, limits = c(0,0.25), breaks = seq(0,0.25, by = 0.05)) +
  ggsn::scalebar(dist = 350, dist_unit = "km", transform = T, model = "WGS84",
                 st.bottom = F, anchor = c(x = -88.4, y = 24.7), st.dist = 0.025,
                 st.size = 2, height = 0.02, border.size = 0.2,
                 x.min = r_ext[1], x.max = r_ext[2], y.min = r_ext[3], y.max = r_ext[4]) +
  geom_rect(mapping = aes(xmin = -92 , xmax = r_ext[2], ymin = r_ext[3], ymax =24.6), fill = "white") +
  geom_sf(data = states, fill = NA, size = 0.2) +
  geom_sf(data = mex_df, fill = NA, size = 0.2) +
  coord_sf(xlim = r_ext[1:2], ylim = r_ext[3:4], expand = FALSE) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        panel.border = element_rect(colour = "black", size = 0.5),
        legend.title = element_blank(),
        legend.key.size = unit(4, "mm"), legend.text = element_text(size = 5),
        axis.text = element_text(colour = "black", size = 7))

MHMpl <- ggplot() +
  geom_raster(data = mrhol_mdf, aes(x = x, y = y, fill = layer)) +
  scale_fill_distiller(palette = "YlGnBu", direction = 1, limits = c(0,1), 
                       breaks = seq(0,1, by = 0.2)) +  ggsn::scalebar(dist = 350, dist_unit = "km", transform = T, model = "WGS84",
                                                                      st.bottom = F, anchor = c(x = -88.4, y = 24.7), st.dist = 0.025,
                                                                      st.size = 2, height = 0.02, border.size = 0.2,
                                                                      x.min = r_ext[1], x.max = r_ext[2], y.min = r_ext[3], y.max = r_ext[4]) +
  geom_rect(mapping = aes(xmin = -92 , xmax = r_ext[2], ymin = r_ext[3], ymax =24.6), fill = "white") +
  geom_sf(data = states, fill = NA, size = 0.2) +
  geom_sf(data = mex_df, fill = NA, size = 0.2) +
  coord_sf(xlim = r_ext[1:2], ylim = r_ext[3:4], expand = FALSE) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        panel.border = element_rect(colour = "black", size = 0.5),
        legend.title = element_blank(),
        legend.key.size = unit(4, "mm"), legend.text = element_text(size = 6),
        axis.text = element_text(colour = "black", size = 7))

MHVpl <- ggplot() +
  geom_raster(data = mrhol_vdf, aes(x = x, y = y, fill = layer)) +
  scale_fill_gradientn(colours = col, limits = c(0,0.25), breaks = seq(0,0.25, by = 0.05)) +
  ggsn::scalebar(dist = 350, dist_unit = "km", transform = T, model = "WGS84",
                 st.bottom = F, anchor = c(x = -88.4, y = 24.7), st.dist = 0.025,
                 st.size = 2, height = 0.02, border.size = 0.2,
                 x.min = r_ext[1], x.max = r_ext[2], y.min = r_ext[3], y.max = r_ext[4]) +
  geom_rect(mapping = aes(xmin = -92 , xmax = r_ext[2], ymin = r_ext[3], ymax =24.6), fill = "white") +
  geom_sf(data = states, fill = NA, size = 0.2) +
  geom_sf(data = mex_df, fill = NA, size = 0.2) +
  coord_sf(xlim = r_ext[1:2], ylim = r_ext[3:4], expand = FALSE) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        panel.border = element_rect(colour = "black", size = 0.5),
        legend.title = element_blank(),
        legend.key.size = unit(4, "mm"), legend.text = element_text(size = 5),
        axis.text = element_text(colour = "black", size = 7))

S1.3 <- ggarrange(CHMpl, CHVpl, MHMpl, MHVpl, nrow = 2, ncol = 2, 
                labels = c("(a)","(b)","(c)","(d)"))#, legend = "bottom")
ggsave("figures/manuscript/FS1_3_Hol_v2.tiff", S1.3, height = 168, width = 168, units = "mm",
       dpi = 1200)

## 2050 2.6
C2650Mpl <- ggplot() +
  geom_raster(data = cc2650_mdf, aes(x = x, y = y, fill = layer)) +
  scale_fill_distiller(palette = "YlGnBu", direction = 1, limits = c(0,1), 
                       breaks = seq(0,1, by = 0.2)) +  ggsn::scalebar(dist = 350, dist_unit = "km", transform = T, model = "WGS84",
                                                                      st.bottom = F, anchor = c(x = -88.4, y = 24.7), st.dist = 0.025,
                                                                      st.size = 2, height = 0.02, border.size = 0.2,
                                                                      x.min = r_ext[1], x.max = r_ext[2], y.min = r_ext[3], y.max = r_ext[4]) +
  geom_rect(mapping = aes(xmin = -92 , xmax = r_ext[2], ymin = r_ext[3], ymax =24.6), fill = "white") +
  geom_sf(data = states, fill = NA, size = 0.2) +
  geom_sf(data = mex_df, fill = NA, size = 0.2) +
  coord_sf(xlim = r_ext[1:2], ylim = r_ext[3:4], expand = FALSE) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        panel.border = element_rect(colour = "black", size = 0.5),
        legend.title = element_blank(),
        legend.key.size = unit(4, "mm"), legend.text = element_text(size = 6),
        axis.text = element_text(colour = "black", size = 7))

C2650Vpl <- ggplot() +
  geom_raster(data = cc2650_vdf, aes(x = x, y = y, fill = layer)) +
  scale_fill_gradientn(colours = col, limits = c(0,0.25), breaks = seq(0,0.25, by = 0.05)) +
  ggsn::scalebar(dist = 350, dist_unit = "km", transform = T, model = "WGS84",
                 st.bottom = F, anchor = c(x = -88.4, y = 24.7), st.dist = 0.025,
                 st.size = 2, height = 0.02, border.size = 0.2,
                 x.min = r_ext[1], x.max = r_ext[2], y.min = r_ext[3], y.max = r_ext[4]) +
  geom_rect(mapping = aes(xmin = -92 , xmax = r_ext[2], ymin = r_ext[3], ymax =24.6), fill = "white") +
  geom_sf(data = states, fill = NA, size = 0.2) +
  geom_sf(data = mex_df, fill = NA, size = 0.2) +
  coord_sf(xlim = r_ext[1:2], ylim = r_ext[3:4], expand = FALSE) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        panel.border = element_rect(colour = "black", size = 0.5),
        legend.title = element_blank(),
        legend.key.size = unit(4, "mm"), legend.text = element_text(size = 5),
        axis.text = element_text(colour = "black", size = 7))

M2650Mpl <- ggplot() +
  geom_raster(data = mr2650_mdf, aes(x = x, y = y, fill = layer)) +
  scale_fill_distiller(palette = "YlGnBu", direction = 1, limits = c(0,1), 
                       breaks = seq(0,1, by = 0.2)) +  ggsn::scalebar(dist = 350, dist_unit = "km", transform = T, model = "WGS84",
                                                                      st.bottom = F, anchor = c(x = -88.4, y = 24.7), st.dist = 0.025,
                                                                      st.size = 2, height = 0.02, border.size = 0.2,
                                                                      x.min = r_ext[1], x.max = r_ext[2], y.min = r_ext[3], y.max = r_ext[4]) +
  geom_rect(mapping = aes(xmin = -92 , xmax = r_ext[2], ymin = r_ext[3], ymax =24.6), fill = "white") +
  geom_sf(data = states, fill = NA, size = 0.2) +
  geom_sf(data = mex_df, fill = NA, size = 0.2) +
  coord_sf(xlim = r_ext[1:2], ylim = r_ext[3:4], expand = FALSE) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        panel.border = element_rect(colour = "black", size = 0.5),
        legend.title = element_blank(),
        legend.key.size = unit(4, "mm"), legend.text = element_text(size = 6),
        axis.text = element_text(colour = "black", size = 7))

M2650Vpl <- ggplot() +
  geom_raster(data = mr2650_vdf, aes(x = x, y = y, fill = layer)) +
  scale_fill_gradientn(colours = col, limits = c(0,0.25), breaks = seq(0,0.25, by = 0.05)) +
  ggsn::scalebar(dist = 350, dist_unit = "km", transform = T, model = "WGS84",
                 st.bottom = F, anchor = c(x = -88.4, y = 24.7), st.dist = 0.025,
                 st.size = 2, height = 0.02, border.size = 0.2,
                 x.min = r_ext[1], x.max = r_ext[2], y.min = r_ext[3], y.max = r_ext[4]) +
  geom_rect(mapping = aes(xmin = -92 , xmax = r_ext[2], ymin = r_ext[3], ymax =24.6), fill = "white") +
  geom_sf(data = states, fill = NA, size = 0.2) +
  geom_sf(data = mex_df, fill = NA, size = 0.2) +
  coord_sf(xlim = r_ext[1:2], ylim = r_ext[3:4], expand = FALSE) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        panel.border = element_rect(colour = "black", size = 0.5),
        legend.title = element_blank(),
        legend.key.size = unit(4, "mm"), legend.text = element_text(size = 5),
        axis.text = element_text(colour = "black", size = 7))

S1.4 <- ggarrange(C2650Mpl, C2650Vpl, M2650Mpl, M2650Vpl, nrow = 2, ncol = 2, 
                  labels = c("(a)","(b)","(c)","(d)"))#, legend = "bottom")
ggsave("figures/manuscript/FS1_4_2650.tiff", S1.4, height = 168, width = 168, units = "mm",
       dpi = 1200)

##2050 8.5
C8550Mpl <- ggplot() +
  geom_raster(data = cc8550_mdf, aes(x = x, y = y, fill = layer)) +
  scale_fill_distiller(palette = "YlGnBu", direction = 1, limits = c(0,1), 
                       breaks = seq(0,1, by = 0.2)) +  ggsn::scalebar(dist = 350, dist_unit = "km", transform = T, model = "WGS84",
                                                                      st.bottom = F, anchor = c(x = -88.4, y = 24.7), st.dist = 0.025,
                                                                      st.size = 2, height = 0.02, border.size = 0.2,
                                                                      x.min = r_ext[1], x.max = r_ext[2], y.min = r_ext[3], y.max = r_ext[4]) +
  geom_rect(mapping = aes(xmin = -92 , xmax = r_ext[2], ymin = r_ext[3], ymax =24.6), fill = "white") +
  geom_sf(data = states, fill = NA, size = 0.2) +
  geom_sf(data = mex_df, fill = NA, size = 0.2) +
  coord_sf(xlim = r_ext[1:2], ylim = r_ext[3:4], expand = FALSE) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        panel.border = element_rect(colour = "black", size = 0.5),
        legend.title = element_blank(),
        legend.key.size = unit(4, "mm"), legend.text = element_text(size = 6),
        axis.text = element_text(colour = "black", size = 7))

C8550Vpl <- ggplot() +
  geom_raster(data = cc8550_vdf, aes(x = x, y = y, fill = layer)) +
  scale_fill_gradientn(colours = col, limits = c(0,0.25), breaks = seq(0,0.25, by = 0.05)) +
  ggsn::scalebar(dist = 350, dist_unit = "km", transform = T, model = "WGS84",
                 st.bottom = F, anchor = c(x = -88.4, y = 24.7), st.dist = 0.025,
                 st.size = 2, height = 0.02, border.size = 0.2,
                 x.min = r_ext[1], x.max = r_ext[2], y.min = r_ext[3], y.max = r_ext[4]) +
  geom_rect(mapping = aes(xmin = -92 , xmax = r_ext[2], ymin = r_ext[3], ymax =24.6), fill = "white") +
  geom_sf(data = states, fill = NA, size = 0.2) +
  geom_sf(data = mex_df, fill = NA, size = 0.2) +
  coord_sf(xlim = r_ext[1:2], ylim = r_ext[3:4], expand = FALSE) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        panel.border = element_rect(colour = "black", size = 0.5),
        legend.title = element_blank(),
        legend.key.size = unit(4, "mm"), legend.text = element_text(size = 5),
        axis.text = element_text(colour = "black", size = 7))

M8550Mpl <- ggplot() +
  geom_raster(data = mr8550_mdf, aes(x = x, y = y, fill = layer)) +
  scale_fill_distiller(palette = "YlGnBu", direction = 1, limits = c(0,1), 
                       breaks = seq(0,1, by = 0.2)) +  ggsn::scalebar(dist = 350, dist_unit = "km", transform = T, model = "WGS84",
                                                                      st.bottom = F, anchor = c(x = -88.4, y = 24.7), st.dist = 0.025,
                                                                      st.size = 2, height = 0.02, border.size = 0.2,
                                                                      x.min = r_ext[1], x.max = r_ext[2], y.min = r_ext[3], y.max = r_ext[4]) +
  geom_rect(mapping = aes(xmin = -92 , xmax = r_ext[2], ymin = r_ext[3], ymax =24.6), fill = "white") +
  geom_sf(data = states, fill = NA, size = 0.2) +
  geom_sf(data = mex_df, fill = NA, size = 0.2) +
  coord_sf(xlim = r_ext[1:2], ylim = r_ext[3:4], expand = FALSE) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        panel.border = element_rect(colour = "black", size = 0.5),
        legend.title = element_blank(),
        legend.key.size = unit(4, "mm"), legend.text = element_text(size = 6),
        axis.text = element_text(colour = "black", size = 7))

M8550Vpl <- ggplot() +
  geom_raster(data = mr8550_vdf, aes(x = x, y = y, fill = layer)) +
  scale_fill_gradientn(colours = col, limits = c(0,0.25), breaks = seq(0,0.25, by = 0.05)) +
  ggsn::scalebar(dist = 350, dist_unit = "km", transform = T, model = "WGS84",
                 st.bottom = F, anchor = c(x = -88.4, y = 24.7), st.dist = 0.025,
                 st.size = 2, height = 0.02, border.size = 0.2,
                 x.min = r_ext[1], x.max = r_ext[2], y.min = r_ext[3], y.max = r_ext[4]) +
  geom_rect(mapping = aes(xmin = -92 , xmax = r_ext[2], ymin = r_ext[3], ymax =24.6), fill = "white") +
  geom_sf(data = states, fill = NA, size = 0.2) +
  geom_sf(data = mex_df, fill = NA, size = 0.2) +
  coord_sf(xlim = r_ext[1:2], ylim = r_ext[3:4], expand = FALSE) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        panel.border = element_rect(colour = "black", size = 0.5),
        legend.title = element_blank(),
        legend.key.size = unit(4, "mm"), legend.text = element_text(size = 5),
        axis.text = element_text(colour = "black", size = 7))

S1.5 <- ggarrange(C8550Mpl, C8550Vpl, M8550Mpl, M8550Vpl, nrow = 2, ncol = 2, 
                  labels = c("(a)","(b)","(c)","(d)"))#, legend = "bottom")
ggsave("figures/manuscript/FS1_5_8550.tiff", S1.5, height = 168, width = 168, units = "mm",
       dpi = 1200)

##2070 2.6
C2670Mpl <- ggplot() +
  geom_raster(data = cc2670_mdf, aes(x = x, y = y, fill = layer)) +
  scale_fill_distiller(palette = "YlGnBu", direction = 1, limits = c(0,1), 
                       breaks = seq(0,1, by = 0.2)) +  ggsn::scalebar(dist = 350, dist_unit = "km", transform = T, model = "WGS84",
                                                                      st.bottom = F, anchor = c(x = -88.4, y = 24.7), st.dist = 0.025,
                                                                      st.size = 2, height = 0.02, border.size = 0.2,
                                                                      x.min = r_ext[1], x.max = r_ext[2], y.min = r_ext[3], y.max = r_ext[4]) +
  geom_rect(mapping = aes(xmin = -92 , xmax = r_ext[2], ymin = r_ext[3], ymax =24.6), fill = "white") +
  geom_sf(data = states, fill = NA, size = 0.2) +
  geom_sf(data = mex_df, fill = NA, size = 0.2) +
  coord_sf(xlim = r_ext[1:2], ylim = r_ext[3:4], expand = FALSE) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        panel.border = element_rect(colour = "black", size = 0.5),
        legend.title = element_blank(),
        legend.key.size = unit(4, "mm"), legend.text = element_text(size = 6),
        axis.text = element_text(colour = "black", size = 7))

C2670Vpl <- ggplot() +
  geom_raster(data = cc2670_vdf, aes(x = x, y = y, fill = layer)) +
  scale_fill_gradientn(colours = col, limits = c(0,0.25), breaks = seq(0,0.25, by = 0.05)) +
  ggsn::scalebar(dist = 350, dist_unit = "km", transform = T, model = "WGS84",
                 st.bottom = F, anchor = c(x = -88.4, y = 24.7), st.dist = 0.025,
                 st.size = 2, height = 0.02, border.size = 0.2,
                 x.min = r_ext[1], x.max = r_ext[2], y.min = r_ext[3], y.max = r_ext[4]) +
  geom_rect(mapping = aes(xmin = -92 , xmax = r_ext[2], ymin = r_ext[3], ymax =24.6), fill = "white") +
  geom_sf(data = states, fill = NA, size = 0.2) +
  geom_sf(data = mex_df, fill = NA, size = 0.2) +
  coord_sf(xlim = r_ext[1:2], ylim = r_ext[3:4], expand = FALSE) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        panel.border = element_rect(colour = "black", size = 0.5),
        legend.title = element_blank(),
        legend.key.size = unit(4, "mm"), legend.text = element_text(size = 5),
        axis.text = element_text(colour = "black", size = 7))

M2670Mpl <- ggplot() +
  geom_raster(data = mr2670_mdf, aes(x = x, y = y, fill = layer)) +
  scale_fill_distiller(palette = "YlGnBu", direction = 1, limits = c(0,1), 
                       breaks = seq(0,1, by = 0.2)) +  ggsn::scalebar(dist = 350, dist_unit = "km", transform = T, model = "WGS84",
                                                                      st.bottom = F, anchor = c(x = -88.4, y = 24.7), st.dist = 0.025,
                                                                      st.size = 2, height = 0.02, border.size = 0.2,
                                                                      x.min = r_ext[1], x.max = r_ext[2], y.min = r_ext[3], y.max = r_ext[4]) +
  geom_rect(mapping = aes(xmin = -92 , xmax = r_ext[2], ymin = r_ext[3], ymax =24.6), fill = "white") +
  geom_sf(data = states, fill = NA, size = 0.2) +
  geom_sf(data = mex_df, fill = NA, size = 0.2) +
  coord_sf(xlim = r_ext[1:2], ylim = r_ext[3:4], expand = FALSE) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        panel.border = element_rect(colour = "black", size = 0.5),
        legend.title = element_blank(),
        legend.key.size = unit(4, "mm"), legend.text = element_text(size = 6),
        axis.text = element_text(colour = "black", size = 7))

M2670Vpl <- ggplot() +
  geom_raster(data = mr2670_vdf, aes(x = x, y = y, fill = layer)) +
  scale_fill_gradientn(colours = col, limits = c(0,0.25), breaks = seq(0,0.25, by = 0.05)) +
  ggsn::scalebar(dist = 350, dist_unit = "km", transform = T, model = "WGS84",
                 st.bottom = F, anchor = c(x = -88.4, y = 24.7), st.dist = 0.025,
                 st.size = 2, height = 0.02, border.size = 0.2,
                 x.min = r_ext[1], x.max = r_ext[2], y.min = r_ext[3], y.max = r_ext[4]) +
  geom_rect(mapping = aes(xmin = -92 , xmax = r_ext[2], ymin = r_ext[3], ymax =24.6), fill = "white") +
  geom_sf(data = states, fill = NA, size = 0.2) +
  geom_sf(data = mex_df, fill = NA, size = 0.2) +
  coord_sf(xlim = r_ext[1:2], ylim = r_ext[3:4], expand = FALSE) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        panel.border = element_rect(colour = "black", size = 0.5),
        legend.title = element_blank(),
        legend.key.size = unit(4, "mm"), legend.text = element_text(size = 5),
        axis.text = element_text(colour = "black", size = 7))

S1.6 <- ggarrange(C2670Mpl, C2670Vpl, M2670Mpl, M2670Vpl, nrow = 2, ncol = 2, 
                  labels = c("(a)","(b)","(c)","(d)"))#, legend = "bottom")
ggsave("figures/manuscript/FS1_6_2670.tiff", S1.6, height = 168, width = 168, units = "mm",
       dpi = 1200)
#
### Supplement Fig 1.2 Variable Response Plots ------------

#Need to figure out which variables are which numbers in the different models
#Set1 = 1:160 ; Set2 = 161:320 ; Set3 = 321:480
#DefaultTriTopo ; Set1 = mod.8[1:7] ; Set2 = mod.8[8:10] ; Set3 = mod.8[11:17]
#Default ; Set1 = mod.8[1:6] ; Set2 = mod.8[7:11] ; Set3 = mod.8[12:17]
#DefaultTriTopo ; Set1 = mod.8[1:7] ; Set2 = mod.8[8:10] ; Set3 = mod.8[11:17]
#DefaultTriTopo ; Set1 = mod.8[1:7] ; Set2 = mod.8[8:10] ; Set3 = mod.8[11:17]

model_tracker[row.names(model_tracker) %in% mod.8, 1:4]

png("FS1_2_ResponsePlots.png", width = 8, height = 5, 
    res = 300, units = "in")
par(mfrow = c(2,4)) #2,5 #2,4 

#Bio4 (only 1st set)
response(model_list[[1]], col = "#D55E00", xlim = c(3000, 12000), var = 1)
for(i in mod.8[2:6]) {
  response(model_list[[i]], col = "#D55E00", var = 1, add = T)
}

#Bio5 (1st & 3rd set)
response(model_list[[1]], col = "#D55E00", xlim = c(100, 450), var = 2) #, xlim = c(100, 450)
for(i in mod.8[2:6]) {
  response(model_list[[i]], col = "#D55E00", var = 2, add = T)
}
for(i in mod.8[12:16]) {
  response(model_list[[i]], col = "#56B4E9", var = 1, add = T)
}

#Bio8 (1st & 3rd set)
response(model_list[[1]], col = "#D55E00", xlim = c(-50, 350), var = 3) #, xlim = c(-100, 400)
for(i in mod.8[2:6]) {
  response(model_list[[i]], col = "#D55E00", var = 3, add = T)
}
for(i in mod.8[12:16]) {
  response(model_list[[i]], col = "#56B4E9", var = 2, add = T)
}

#Bio14 (2nd & 3rd set)
response(model_list[[161]], xlim = c(-20, 100), col = "#CC79A7", var = 1) #, xlim = c(-20, 100)
for(i in mod.8[8:11]) {
  response(model_list[[i]], col = "#CC79A7", var = 1, add = T)
}
for(i in mod.8[12:16]) {
  response(model_list[[i]], col = "#56B4E9", var = 3, add = T)
}

#Bio19 (1st set only)
response(model_list[[1]], col = "#D55E00", xlim = c(-10, 325), var = 4)
for(i in mod.8[2:6]) {
  response(model_list[[i]], col = "#D55E00", var = 4, add = T)
}

#arid (2nd & 3rd set)
response(model_list[[161]], col = "#CC79A7", var = 2)
for(i in mod.8[8:11]) {
  response(model_list[[i]], col = "#CC79A7", var = 2, add = T)
}
for(i in mod.8[12:16]) {
  response(model_list[[i]], col = "#56B4E9", var = 4, add = T)
}

#gDD5 (2nd & 3rd set)
response(model_list[[161]], xlim = c(0, 105000), col = "#CC79A7", var = 3)
for(i in mod.8[8:11]) {
  response(model_list[[i]], col = "#CC79A7", var = 3, add = T)
}
for(i in mod.8[12:16]) {
  response(model_list[[i]], col = "#56B4E9", var = 5, add = T)
}

#PETs (2nd & 3rd set)
response(model_list[[161]], xlim = c(2700, 7300), col = "#CC79A7", var = 4)
for(i in mod.8[8:11]) {
  response(model_list[[i]], col = "#CC79A7", var = 4, add = T)
}
for(i in mod.8[12:16]) {
  response(model_list[[i]], col = "#56B4E9", var = 6, add = T)
}

dev.off()

png("FS1_2_ResponseLegend.png", width = 6, height = 1, res = 300, units = "in")
par(mar=c(1,1,1,1))
plot(NULL ,xaxt='n',yaxt='n',ylab='',xlab='', bty = "n", xlim=0:1, ylim=0:1)
legend("bottom", legend = c('Variable Set 1', 'Variable Set 2', 
                            'Variable Set 3'), 
       pch=16, pt.cex=3, cex=1.1, bty='n', horiz = T,
       col = c('#D55E00', '#CC79A7', "#56B4E9"))
dev.off()

#TopoWet (1st & 3rd set)
# response(model_list[[1]], col = "#D55E00", var = 5)
# for(i in mod.8[2:6]) {
#   response(model_list[[i]], col = "#D55E00", var = 5, add = T)
# }
# for(i in mod.8[12:16]) {
#   response(model_list[[i]], col = "#56B4E9", var = 7, add = T)
# }

#tri (1st & 3rd set)
# response(model_list[[1]], xlim = c(-10, 200), col = "#D55E00", var = 6)
# for(i in mod.8[2:6]) {
#   response(model_list[[i]], col = "#D55E00", var = 6, add = T)
# }
# for(i in mod.8[12:16]) {
#   response(model_list[[i]], col = "#56B4E9", var = 8, add = T)
# }

par(mfrow = c(1,1))

### Supplement Fig 1.1 Full VC and PI -----
#
model_list_results <- lapply(model_list, FUN = function(x) { as.data.frame(x@results) })
model_list_results <- lapply(model_list_results, function(x) { x$Name <-rownames(x); return(x)})
perm_imp <- lapply(model_list_results, 
                   function(x) { x[grep(pattern = ".permutation.importance", x = rownames(x)),] })
perm_imp <- lapply(perm_imp, function(x) { x$Name <- gsub(".permutation.importance", "", x$Name) ; return(x) })

## For each model set (3 different climate sets)
clim1_PI <- data.frame(model_tracker[1:160,])
clim1_PI$Bio4 <- unlist(lapply(perm_imp[1:160], function(x) { x[x$Name == "bio4", "V1"] }))
clim1_PI$Bio19 <- unlist(lapply(perm_imp[1:160], function(x) { x[x$Name == "bio19", "V1"] }))
clim1_PI$Bio5 <- unlist(lapply(perm_imp[1:160], function(x) { x[x$Name == "bio5", "V1"] }))
clim1_PI$Bio8 <- unlist(lapply(perm_imp[1:160], function(x) { x[x$Name == "bio8", "V1"] }))
#clim1_PI$topoWet <- unlist(lapply(perm_imp[1:160], function(x) { x[x$Name == "topoWet", "V1"] }))
#clim1_PI$tri <- unlist(lapply(perm_imp[1:160], function(x) { x[x$Name == "tri", "V1"] }))
head(clim1_PI)
#
clim2_PI <- data.frame(model_tracker[161:320, ])
clim2_PI$PETs <- unlist(lapply(perm_imp[161:320], function(x) { x[x$Name == "PETseasonality", "V1"] }))
clim2_PI$Arid <- unlist(lapply(perm_imp[161:320], function(x) { x[x$Name == "aridityIndexThornthwaite", "V1"] }))
clim2_PI$Bio14 <- unlist(lapply(perm_imp[161:320], function(x) { x[x$Name == "bio14", "V1"] }))
clim2_PI$GDD5 <- unlist(lapply(perm_imp[161:320], function(x) { x[x$Name == "growingDegDays5", "V1"] }))
head(clim2_PI)
#
clim3_PI <- data.frame(model_tracker[321:480,])
clim3_PI$Bio14 <- unlist(lapply(perm_imp[321:480], function(x) { x[x$Name == "bio14", "V1"] }))
clim3_PI$Bio5 <- unlist(lapply(perm_imp[321:480], function(x) { x[x$Name == "bio5", "V1"] }))
clim3_PI$Bio8 <- unlist(lapply(perm_imp[321:480], function(x) { x[x$Name == "bio8", "V1"] }))
clim3_PI$PETs <- unlist(lapply(perm_imp[321:480], function(x) { x[x$Name == "PETseasonality", "V1"] }))
clim3_PI$Arid <- unlist(lapply(perm_imp[321:480], function(x) { x[x$Name == "aridityIndexThornthwaite", "V1"] }))
clim3_PI$GDD5 <- unlist(lapply(perm_imp[321:480], function(x) { x[x$Name == "growingDegDays5", "V1"] }))
#clim3_PI$topoWet <- unlist(lapply(perm_imp[321:480], function(x) { x[x$Name == "topoWet", "V1"] }))
#clim3_PI$tri     <- unlist(lapply(perm_imp[321:480], function(x) { x[x$Name == "tri", "V1"] }))
head(clim3_PI)

## Permutation Importance
clim1_PI_long <- gather(clim1_PI, key = Variable, value = PermImp, Bio4:Bio8, factor_key=TRUE)
clim2_PI_long <- gather(clim2_PI, key = Variable, value = PermImp, PETs:GDD5, factor_key=TRUE)
clim3_PI_long <- gather(clim3_PI, key = Variable, value = PermImp, Bio14:GDD5, factor_key=TRUE)


## Variable Contribution Setup
var_cont <- lapply(model_list_results, 
                   function(x) { x[grep(pattern = ".contribution", x = rownames(x)),] })
var_cont <- lapply(perm_imp, function(x) { x$Name <- gsub(".contribution", "", x$Name) ; return(x) })

## For each model set (3 different climate sets)
clim1_VC <- data.frame(model_tracker[1:160,])
clim1_VC$Bio4 <- unlist(lapply(var_cont[1:160], function(x) { x[x$Name == "bio4", "V1"] }))
clim1_VC$Bio19 <- unlist(lapply(var_cont[1:160], function(x) { x[x$Name == "bio19", "V1"] }))
clim1_VC$Bio5 <- unlist(lapply(var_cont[1:160], function(x) { x[x$Name == "bio5", "V1"] }))
clim1_VC$Bio8 <- unlist(lapply(var_cont[1:160], function(x) { x[x$Name == "bio8", "V1"] }))
#clim1_VC$topoWet <- unlist(lapply(var_cont[1:160], function(x) { x[x$Name == "topoWet", "V1"] }))
#clim1_VC$tri <- unlist(lapply(var_cont[1:160], function(x) { x[x$Name == "tri", "V1"] }))
head(clim1_VC)
#
clim2_VC <- data.frame(model_tracker[161:320, ])
clim2_VC$PETs <- unlist(lapply(var_cont[161:320], function(x) { x[x$Name == "PETseasonality", "V1"] }))
clim2_VC$Arid <- unlist(lapply(var_cont[161:320], function(x) { x[x$Name == "aridityIndexThornthwaite", "V1"] }))
clim2_VC$Bio14 <- unlist(lapply(var_cont[161:320], function(x) { x[x$Name == "bio14", "V1"] }))
clim2_VC$GDD5 <- unlist(lapply(var_cont[161:320], function(x) { x[x$Name == "growingDegDays5", "V1"] }))
head(clim2_VC)
#
clim3_VC <- data.frame(model_tracker[321:480,])
clim3_VC$Bio14 <- unlist(lapply(var_cont[321:480], function(x) { x[x$Name == "bio14", "V1"] }))
clim3_VC$Bio5 <- unlist(lapply(var_cont[321:480], function(x) { x[x$Name == "bio5", "V1"] }))
clim3_VC$Bio8 <- unlist(lapply(var_cont[321:480], function(x) { x[x$Name == "bio8", "V1"] }))
clim3_VC$PETs <- unlist(lapply(var_cont[321:480], function(x) { x[x$Name == "PETseasonality", "V1"] }))
clim3_VC$Arid <- unlist(lapply(var_cont[321:480], function(x) { x[x$Name == "aridityIndexThornthwaite", "V1"] }))
clim3_VC$GDD5 <- unlist(lapply(var_cont[321:480], function(x) { x[x$Name == "growingDegDays5", "V1"] }))
#clim3_VC$topoWet <- unlist(lapply(var_cont[321:480], function(x) { x[x$Name == "topoWet", "V1"] }))
#clim3_VC$tri     <- unlist(lapply(var_cont[321:480], function(x) { x[x$Name == "tri", "V1"] }))
head(clim3_PI)

## Plot Permutation Importance
clim1_VC_long <- gather(clim1_VC, key = Variable, value = VarCont, Bio4:Bio8, factor_key=TRUE)
clim2_VC_long <- gather(clim2_VC, key = Variable, value = VarCont, PETs:GDD5, factor_key=TRUE)
clim3_VC_long <- gather(clim3_VC, key = Variable, value = VarCont, Bio14:GDD5, factor_key=TRUE)


## Plots
#Climate 1
Clim1_PI_all <- ggplot(data.frame(clim1_PI_long), aes(x = Variable)) +
  geom_boxplot(aes(y = PermImp)) + 
  geom_jitter(aes(y = PermImp, color = Folds, shape = Folds), size = 3) +
  xlab("") + ylab("Permutation Importance") +
  ggtitle("Variable Set 1") + theme_bw() +
  theme(panel.spacing=unit(0, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = 1), 
        strip.background = element_rect(color = "black", size = 1),
        axis.text = element_text(color = "black", size = 14),
        axis.title = element_text(color = "black", size = 14),
        legend.text = element_text(size = 14),        
        legend.title = element_text(size = 14),
        panel.grid = element_blank())

ggplot(data.frame(clim1_PI_long), aes(x = Variable)) +
  geom_boxplot(aes(y = PermImp)) + 
  geom_jitter(aes(y = PermImp, color = Folds, shape = Folds)) +
  facet_wrap(~Background, nrow = 2) +
  ggtitle("Variable Set 1") +
  theme(panel.spacing=unit(0, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = 1), 
        strip.background = element_rect(color = "black", size = 1),
        axis.text = element_text(color = "black"))

Clim1_VC_all <- ggplot(data.frame(clim1_VC_long), aes(x = Variable)) +
  geom_boxplot(aes(y = VarCont)) + 
  geom_jitter(aes(y = VarCont, color = Folds, shape = Folds), size = 3) +
  xlab("") + ylab("Variable Contribution") +
  ggtitle("Variable Set 1") + theme_bw() +
  theme(panel.spacing=unit(0, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = 1), 
        strip.background = element_rect(color = "black", size = 1),
        axis.text = element_text(color = "black", size = 14),
        axis.title = element_text(color = "black", size = 14),
        legend.text = element_text(size = 14),        
        legend.title = element_text(size = 14),
        panel.grid = element_blank())

#Climate 2
Clim2_PI_all <- ggplot(data.frame(clim2_PI_long), aes(x = Variable)) +
  geom_boxplot(aes(y = PermImp)) + 
  geom_jitter(aes(y = PermImp, color = Folds, shape = Folds), size = 3) +
  xlab("") + ylab("Permutation Importance") +
  ggtitle("Variable Set 2") + theme_bw() +
  theme(panel.spacing=unit(0, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = 1), 
        strip.background = element_rect(color = "black", size = 1),
        axis.text = element_text(color = "black", size = 14),
        axis.title = element_text(color = "black", size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        panel.grid = element_blank())

ggplot(data.frame(clim2_PI_long), aes(x = Variable)) +
  geom_boxplot(aes(y = PermImp)) + 
  geom_jitter(aes(y = PermImp, color = Folds, shape = Folds)) +
  facet_wrap(~Background, nrow = 2) +
  ggtitle("Variable Set 2") +
  theme(panel.spacing=unit(0, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = 1), 
        strip.background = element_rect(color = "black", size = 1))

Clim2_VC_all <- ggplot(data.frame(clim2_VC_long), aes(x = Variable)) +
  geom_boxplot(aes(y = VarCont)) + 
  geom_jitter(aes(y = VarCont, color = Folds, shape = Folds), size = 3) +
  xlab("") + ylab("Variable Contribution") +
  ggtitle("Variable Set 2") + theme_bw() +
  theme(panel.spacing=unit(0, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = 1), 
        strip.background = element_rect(color = "black", size = 1),
        axis.text = element_text(color = "black", size = 14),
        axis.title = element_text(color = "black", size = 14),
        legend.text = element_text(size = 14),        
        legend.title = element_text(size = 14),
        panel.grid = element_blank())


#Climate 3
Clim3_PI_all <- ggplot(data.frame(clim3_PI_long), aes(x = Variable)) +
  geom_boxplot(aes(y = PermImp)) + 
  geom_jitter(aes(y = PermImp, color = Folds, shape = Folds), size = 3) +
  xlab("") + ylab("Permutation Importance") +
  ggtitle("Variable Set 3") + theme_bw() +
  theme(panel.spacing=unit(0, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = 1), 
        strip.background = element_rect(color = "black", size = 1),
        axis.text = element_text(color = "black", size = 14),
        axis.title = element_text(color = "black", size = 14),
        legend.text = element_text(size = 14), 
        legend.title = element_text(size = 14),
        panel.grid = element_blank())

ggplot(data.frame(clim3_PI_long), aes(x = Variable)) +
  geom_boxplot(aes(y = PermImp)) + 
  geom_jitter(aes(y = PermImp, color = Folds, shape = Folds)) +  
  facet_wrap(~Background, nrow = 2) +
  ggtitle("Variable Set 3") +
  theme(panel.spacing=unit(0, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = 1), 
        strip.background = element_rect(color = "black", size = 1))

Clim3_VC_all <- ggplot(data.frame(clim3_VC_long), aes(x = Variable)) +
  geom_boxplot(aes(y = VarCont)) + 
  geom_jitter(aes(y = VarCont, color = Folds, shape = Folds), size = 3) +
  xlab("") + ylab("Variable Contribution") +
  ggtitle("Variable Set 3") + theme_bw() +
  theme(panel.spacing=unit(0, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = 1), 
        strip.background = element_rect(color = "black", size = 1),
        axis.text = element_text(color = "black", size = 14),
        axis.title = element_text(color = "black", size = 14),
        legend.text = element_text(size = 14),        
        legend.title = element_text(size = 14),
        panel.grid = element_blank())

FS1_1 <- ggarrange(Clim1_VC_all, Clim2_VC_all, Clim3_VC_all, 
                   Clim1_PI_all, Clim2_PI_all, Clim3_PI_all, 
                   labels = c("(a)","(b)", "(c)", "(d)", "(e)", "(f)"), 
                   common.legend = T, nrow = 2, ncol = 3, legend = "right")
ggsave("figures/manuscript/FS1_1_PermImpAll.tiff", plot = FS1_1, height = 16, 
       width = 22, units = "in", dpi = 300)
#
### Base plot figs -----

#tiff("figures/manuscript/mr8570.tiff", height = 168, width = 168, units = "mm", res = 1200, bg = "transparent")
par(mar = c(2,2,0.5,2), xpd = F)
plot(NULL, xlim = r_ext[1:2], ylim = r_ext[3:4], xaxs = "i", yaxs = "i", xlab = NA, ylab = NA)
plot(mr8570_m, col = col, zlim = c(0,1), legend = F, asp = 1, add = T)
rect(-91, 20.93, -86.2, 25, col = "white", border = "white")
maps::map("state", boundary = T, col = "darkgray", add = T)
suppressWarnings(maps::map(mex, boundary = F, col = "darkgray", add = T))
scalebar(d = 500, xy = c(-94, 25), type = "line")
text(x = -89.6, y = 25.69, labels = "km")
box(lwd = 2)
dev.off()

#tiff("figures/manuscript/F4_Current.tiff", height = 78, width = 168, units = "mm", res = 1200, bg = "transparent")

layout(matrix(c(1,2), 1, 2, byrow = TRUE), widths=c(1,1.5))
par(mar = c(2,2,0.5,3), xpd = F)
plot(NULL, xlim = r_ext[1:2], ylim = r_ext[3:4], xaxs = "i", yaxs = "i", xlab = NA, ylab = NA)
plot(pred.8_var, col = col, zlim = c(0,1), legend = F, asp = 1, add = T)
rect(-91, 20.93, -86.2, 25, col = "white", border = "white")
maps::map("state", boundary = T, col = "darkgray", add = T)
suppressWarnings(maps::map(mex, boundary = F, col = "darkgray", add = T))
scalebar(d = 500, xy = c(-94, 25), type = "line")
text(x = -89.6, y = 25.69, labels = "km")
text(x = -120, y = 47, labels = "(a)", xpd = NA)
box(lwd = 2)

par(mar = c(2,2,0.5,3), xpd = F)
plot(NULL, xlim = r_ext[1:2], ylim = r_ext[3:4], xaxs = "i", yaxs = "i", xlab = NA, ylab = NA)
plot(pred.8_mean, col = col, zlim = c(0,1), legend = F, asp = 1, add = T)
rect(-91, 20.93, -86.2, 25, col = "white", border = "white")
maps::map("state", boundary = T, col = "darkgray", add = T)
suppressWarnings(maps::map(mex, boundary = F, col = "darkgray", add = T))
scalebar(d = 500, xy = c(-94, 25), type = "line")
text(x = -89.6, y = 25.69, labels = "km")
text(x = -120, y = 47, labels = "(b)", xpd = NA)
box(lwd = 2)

plot(mr8570_m, col = col, zlim = c(0,1), horizontal = F, legend.only = T)

dev.off()
#
#png("pptmaps/legend.png", height = 6, width = 6, units = "in", res = 1200, bg = "transparent")
plot(mr8570_m, col = col, zlim = c(0,1), horizontal = T, legend.only = T)
dev.off()

#tiff("figures/paper/currentPred.tiff", height = 60, width = 168, units = "mm", res = 1200, bg = "transparent")
par(mfrow = c(1, 2), mar = c(2, 2.5, 2, 5.5))
plot(NULL, xlim = r_ext[1:2], ylim = r_ext[3:4], cex.axis = 0.8, xaxs = "i", yaxs = "i")
plot(pred.8_mean, col = col, zlim = c(0,1), cex.axis = 0.8, add = T)
maps::map("state", boundary = T, col = "darkgray", cex.axis = 0.8, add = T)
suppressWarnings(maps::map(mex, boundary = F, col = "darkgray", cex.axis = 0.8, add = T))
plot(pred.8_mean, legend.only=TRUE, col=col, legend.width=1, legend.shrink=0.6,
     cex.axis=0.6)
rect(-93, 19.9, -86, 25, col = "white", border = NA)
scalebar(d = 500, xy = c(-93, 24), divs = 4, type = "bar", below = "km", cex = 0.52)
text(x = -128, y = 48, labels = "(a)", xpd = NA)
box(lwd = 2)

plot(NULL, xlim = d_ext[1:2], ylim = d_ext[3:4])
plot(pred.8_var, zlim = c(0,0.25), add = T)
maps::map("state", boundary = T, col = "darkgray", add = T)
suppressWarnings(maps::map(mex, boundary = F, col = "darkgray", add = T))
rect(-93, 19.9, -86, 25, col = "white", border = NA)
scalebar(d = 500, xy = c(-93, 24), divs = 4, type = "bar", below = "km", cex = 0.52)
text(x = -128, y = 48, labels = "(b)", xpd = NA)
box(lwd = 2)
dev.off()

#tiff("figures/paper/LGMpred.tiff", height = 120, width = 168, units = "mm", res = 1200, bg = "transparent")
par(mfrow = c(2, 2), mar = c(2, 2.5, 2, 5.5))
plot(NULL, xlim = d_ext[1:2], ylim = d_ext[3:4])
plot(cclgm_m, col = col, zlim = c(0,1), add = T)
maps::map("state", boundary = T, col = "darkgray", add = T)
suppressWarnings(maps::map(mex, boundary = F, col = "darkgray", add = T))
rect(-93, 19.9, -86, 25, col = "white", border = NA)
scalebar(d = 500, xy = c(-93, 24), divs = 4, type = "bar", below = "km", cex = 0.52)
text(x = -128, y = 48, labels = "(a)", xpd = NA)
box(lwd = 2)

plot(NULL, xlim = d_ext[1:2], ylim = d_ext[3:4])
plot(cclgm_v, zlim = c(0,0.25), add = T)
maps::map("state", boundary = T, col = "darkgray", add = T)
suppressWarnings(maps::map(mex, boundary = F, col = "darkgray", add = T))
rect(-93, 19.9, -86, 25, col = "white", border = NA)
scalebar(d = 500, xy = c(-93, 24), divs = 4, type = "bar", below = "km", cex = 0.52)
text(x = -128, y = 48, labels = "(b)", xpd = NA)
box(lwd = 2)

plot(NULL, xlim = d_ext[1:2], ylim = d_ext[3:4])
plot(mrlgm_m, col = col, zlim = c(0,1), asp = 1, add = T)
maps::map("state", boundary = T, col = "darkgray", add = T)
suppressWarnings(maps::map(mex, boundary = F, col = "darkgray", add = T))
rect(-93, 19.9, -86, 25, col = "white", border = NA)
scalebar(d = 500, xy = c(-93, 24), divs = 4, type = "bar", below = "km", cex = 0.52)
text(x = -128, y = 48, labels = "(c)", xpd = NA)
box(lwd = 2)

plot(NULL, xlim = d_ext[1:2], ylim = d_ext[3:4])
plot(mrlgm_v, zlim = c(0,0.25), asp = 1, add = T)
maps::map("state", boundary = T, col = "darkgray", add = T)
suppressWarnings(maps::map(mex, boundary = F, col = "darkgray", add = T))
rect(-93, 19.9, -86, 25, col = "white", border = NA)
scalebar(d = 500, xy = c(-93, 24), divs = 4, type = "bar", below = "km", cex = 0.52)
text(x = -128, y = 48, labels = "(d)", xpd = NA)
box(lwd = 2)
dev.off()

###--- Anomaly maps ------###
cur_lgm <- pred.8_mean - mrlgm_m
cur_8570 <- pred.8_mean - mr8570_m

#tiff("figures/paper/anomCurMRLGM.tiff", height = 60, width = 79, units = "mm", res = 1200, bg = "transparent")
par(mar = c(2,2,0.5,2))#, xpd = T
#plot(NULL, xlim = d_ext[1:2], ylim = d_ext[3:4])
plot(cur_lgm, col=cm.colors(5), xlim = d_ext[1:2], ylim = d_ext[3:4], zlim = c(-1,1), asp = 1) #, col = col, legend = F
maps::map("state", boundary = T, col = "darkgray", add = T)
suppressWarnings(maps::map(mex, boundary = F, col = "darkgray", add = T))
scalebar(d = 500, xy = c(-93, 26), type = "line")
box(lwd = 2)
dev.off()

#tiff("figures/paper/anomCurMR8570.tiff", height = 60, width = 79, units = "mm", res = 1200, bg = "transparent")
par(mar = c(2,2,0.5,2))#, xpd = T
#plot(NULL, xlim = d_ext[1:2], ylim = d_ext[3:4])
plot(cur_8570, col=cm.colors(5), xlim = d_ext[1:2], ylim = d_ext[3:4], zlim = c(-1,1), asp = 1) #, col = col, legend = F
maps::map("state", boundary = T, col = "darkgray", add = T)
suppressWarnings(maps::map(mex, boundary = F, col = "darkgray", add = T))
scalebar(d = 500, xy = c(-93, 26), type = "line")
box(lwd = 2)
dev.off()

