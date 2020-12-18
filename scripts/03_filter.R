###################################
###--- Environmental Filtering ----
###################################

# We used the Varela method here to sample based on environment by sourcing the 
# script below (loads several packages in the sourced script)

source("scripts/envSample.R")

# Varela, S., Anderson, R.P., García‐Valdés, R. and Fernández‐González, F., 2014. 
# Environmental filters reduce the effects of sampling bias and improve predictions 
# of ecological niche models. Ecography, 37(11), pp.1084-1091.

#Run filters at 0.15, 0.3, and 0.75 bin sizes, check evaluation statistics and projections
envS_data2.5_1 <- envSample(comb_data[-90,3:2],
                            filters = list(comb_pr$scores[,1], comb_pr$scores[,2],
                                           comb_pr$scores[,3], comb_pr$scores[,4]),
                            res = list(0.15, 0.15, 0.15, 0.15), do.plot = TRUE)
envS_data2.5_2 <- envSample(comb_data[-90,3:2],
                            filters = list(comb_pr$scores[,1], comb_pr$scores[,2],
                                           comb_pr$scores[,3], comb_pr$scores[,4]),
                            res = list(0.3, 0.3, 0.3, 0.3), do.plot = TRUE)
envS_data2.5_3 <- envSample(comb_data[-90,3:2],
                            filters = list(comb_pr$scores[,1], comb_pr$scores[,2],
                                           comb_pr$scores[,3], comb_pr$scores[,4]),
                            res = list(0.75, 0.75, 0.75, 0.75), do.plot = TRUE)

save(envS_data2.5_1, file = "datainputs/envS_data25_1.RData")
save(envS_data2.5_2, file = "datainputs/envS_data25_2.RData")
save(envS_data2.5_3, file = "datainputs/envS_data25_3.RData")

#Once the data has been run comment out the above and just load it
# load("datainputs/envS_data25_1.RData") #0.15
# load("datainputs/envS_data25_2.RData") #0.3
# load("datainputs/envS_data25_3.RData") #0.75

envS_data_1 <- envS_data2.5_1
envS_data_2 <- envS_data2.5_2
envS_data_3 <- envS_data2.5_3

#get the coords for the reduced environmental set without NA values, 
#we will use bioclim_1 only here for now, if it is a problem we can revisit
presvals_envS_D1 <- raster::extract(bioclim_1, envS_data_1)
coords_envS_D1   <- envS_data_1[!is.na(presvals_envS_D1[,1]),]
presvals_envS_D1 <- presvals_envS_D1[!is.na(presvals_envS_D1[,1]),]

presvals_envS_D2 <- raster::extract(bioclim_1, envS_data_2)
coords_envS_D2   <- envS_data_2[!is.na(presvals_envS_D2[,1]),]
presvals_envS_D2 <- presvals_envS_D2[!is.na(presvals_envS_D2[,1]),]

presvals_envS_D3 <- raster::extract(bioclim_1, envS_data_3)
coords_envS_D3   <- envS_data_3[!is.na(presvals_envS_D3[,1]),]
presvals_envS_D3 <- presvals_envS_D3[!is.na(presvals_envS_D3[,1]),]
