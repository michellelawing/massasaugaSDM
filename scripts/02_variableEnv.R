##############################
###--- Variable Selection ----
##############################

# This script checks the correlation between the climate variables, puts them 
# into a principle components analysis, and extracts the final variables chosen

library(corrplot)

#Check the variable correlations
env_cor <- cor(cbind(bc_points, env_points), use = "pairwise.complete.obs")
corrplot(env_cor)
which((env_cor < 0.75) & (env_cor > -0.75), arr.ind = T)

#principal components analysis to help variable selection and assess collinearity
comb_pr <- princomp(cbind(bc_points, env_points)[-90,], cor = T)
summary(comb_pr)
options(max.print=10000)
comb_pr$loadings

#Tried bioclim variables (3,19,5,8), (18,19,5,8), and (1,4,12,15) - went with 
#variables 4, 19, 5, and 8 after comparing the three for initial model evaluation
#Tried envirem variable (2,7,13) and (17,18) after an intial through everything
#at the model run

#Setting three suites of variables for further model evaluation
#Rasters w/ just the 4 interested variables, limited to the extent determined above
d_ext <- extent(c(xrange, yrange))

bioclim_c <- crop(bioclim, d_ext)
envirem_c <- crop(envirem, d_ext)

#Removed envirem 17,18 for final models because there were no historic or future
#datasets for them
bioclim_1 <- stack(bioclim_c[[c(4,5,8,19)]])#, envirem_c[[c(17,18)]]) 
bioclim_2 <- stack(bioclim_c[[14]], envirem_c[[c(2,7,13)]])
bioclim_3 <- stack(bioclim_c[[c(5,8,14)]], envirem_c[[c(2,7,13)]]) #envirem 17,18
