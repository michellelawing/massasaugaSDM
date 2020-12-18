library(envirem)
library(raster)
library(rgeos)
library(maps)
library(viridis)
library(rgdal)

future.clims.dir <- "datainputs/gcm/envirem/FutureClim" #/Users/alawing/Desktop/FutureClim/
outputDir <- "./datainputs/gcm/envirem/"
future.clims <- list.files(future.clims.dir)

##Note: I cleared the SolarRad and SolarRad70 folders manually between each climate 
##b/c it wouldn't let me overwrite them.

#######mr2650#######

#mr26tn50
model.num <- 15
inputDir.min <- paste("datainputs/gcm/envirem/FutureClim/",future.clims[[model.num]], sep = "")
files.tmin <- list.files(inputDir.min, pattern = '.tif$', full.names = TRUE)

#mr26tx50
model.num <- 17
inputDir.max <- paste("datainputs/gcm/envirem/FutureClim/",future.clims[[model.num]], sep = "")
files.tmax <- list.files(inputDir.max, pattern = '.tif$', full.names = TRUE)

########################
# Growing Degree Days 5
########################

# Here, we will take the mean of max and min temperature, 
# although if mean temp was available, that would be better.
mintempstack <- stack(files.tmin)
maxtempstack <- stack(files.tmax)

mintempstack <- crop(mintempstack, d_ext)
maxtempstack <- crop(maxtempstack, d_ext)

# We need to be sure the months are in chronological order.
names(mintempstack)
names(maxtempstack)

#determine the order by extracting the month from each file name and using it in sort.list
order.vars <- sort.list(as.numeric(sub(".*tn50", "", names(mintempstack))))

#order the variables
mintempstack <- mintempstack[[order.vars]]

#check the order
names(mintempstack)

#repeat for the max temp variables
order.vars <- sort.list(as.numeric(sub(".*tx50", "", names(maxtempstack))))
maxtempstack <- maxtempstack[[order.vars]]
names(maxtempstack)

# calculate a proxy for mean temperature
meantempstack <- (mintempstack + maxtempstack) / 2

#calculate growing degree days
gdd <- growingDegDays(meantempstack, baseTemp = 5)

#plot
plot(gdd, col = inferno(100))

writeRaster(gdd, file = paste(outputDir, future.clims[[model.num]], "gdd", sep = ""),
            format = "GTiff")
#save(gdd, file = paste(outputDir, future.clims[[model.num]], "gdd", sep = ""))

########################
#PET Seasonality
########################

#To calculate this, we need solar radiation rasters

# read in a climatic raster for use as a template
rasterTemplate <- mintempstack[[1]]

# calculate monthly solar radiation, defined for the year 2050, output to the current directory (1950 is the index year)
ETsolradRasters(rasterTemplate = rasterTemplate, year = 100, outputDir = "datainputs/gcm/envirem/FutureClim/SolarRad/")

# calculate monthly solar radiation, defined for the year 2070, output to the current directory (1950 is the index year)
#ETsolradRasters(rasterTemplate = rasterTemplate, year = 120, outputDir = "/Users/alawing/Desktop/FutureClim/SolarRad70/")

files.solrad <- list.files("datainputs/gcm/envirem/FutureClim/SolarRad/", pattern = '.tif$', full.names = TRUE)
#files.solrad <- list.files("/Users/alawing/Desktop/FutureClim/SolarRad70/", pattern = '.tif$', full.names = TRUE)
solradstack <- stack(files.solrad)

solradstack <- crop(solradstack, d_ext)

#check order
names(solradstack)

# calculate temperature range
rangetempstack <- maxtempstack - mintempstack

#calculate monthly PET for PET seasonality
PETstack <- monthlyPET(meantempstack, solradstack, rangetempstack, tempScale = 10)

#PETseasonality
PETseason <- PETseasonality(PETstack)

writeRaster(PETseason, file = paste(outputDir, future.clims[[model.num]], "PETseason", sep = ""),
            format = "GTiff")
#save(PETseason, file = paste(outputDir, future.clims[[model.num]], "PETseason", sep = ""))

########################
#aridityIndexThornthwaite
########################

#mr26pr50
model.num <- 13
inputDir.pr <- paste("datainputs/gcm/envirem/FutureClim/",future.clims[[model.num]], sep = "")
files.pr <- list.files(inputDir.pr, pattern = '.tif$', full.names = TRUE)

precipStack <- stack(files.pr)
precipStack <- crop(precipStack, d_ext)

# We need to be sure the months are in chronological order.
names(precipStack)

#determine the order by extracting the month from each file name and using it in sort.list
order.vars <- sort.list(as.numeric(sub(".*pr50", "", names(precipStack))))

#order the variables
precipStack <- precipStack[[order.vars]]

#check the order
names(precipStack)

# Convert layer names for compatibiity with the aridity function
names(precipStack) <- paste("prec", 1:12, sep = "_")
names(PETstack) <- paste("PET", 1:12, sep = "_")

# assign names for compatibility with aridity function
precip <- precipStack
assignNames(precip = 'prec_##')

thornwaite <- aridityIndexThornthwaite(precip, PETstack, precipScale = 1)

writeRaster(thornwaite, file = paste(outputDir, future.clims[[model.num]], "thornwaite", sep = ""),
            format = "GTiff")
#save(thornwaite, file = paste(outputDir, future.clims[[model.num]], "thornwaite", sep = ""))


#######mr2670#######

#mr26tn70
model.num <- 16
inputDir.min <- paste("datainputs/gcm/envirem/FutureClim/",future.clims[[model.num]], sep = "")
files.tmin <- list.files(inputDir.min, pattern = '.tif$', full.names = TRUE)

#mr26tx70
model.num <- 18
inputDir.max <- paste("datainputs/gcm/envirem/FutureClim/",future.clims[[model.num]], sep = "")
files.tmax <- list.files(inputDir.max, pattern = '.tif$', full.names = TRUE)

########################
# Growing Degree Days 5
########################

# Here, we will take the mean of max and min temperature, 
# although if mean temp was available, that would be better.
mintempstack <- stack(files.tmin)
maxtempstack <- stack(files.tmax)

mintempstack <- crop(mintempstack, d_ext)
maxtempstack <- crop(maxtempstack, d_ext)

# We need to be sure the months are in chronological order.
names(mintempstack)
names(maxtempstack)

#determine the order by extracting the month from each file name and using it in sort.list
order.vars <- sort.list(as.numeric(sub(".*tn70", "", names(mintempstack))))

#order the variables
mintempstack <- mintempstack[[order.vars]]

#check the order
names(mintempstack)

#repeat for the max temp variables
order.vars <- sort.list(as.numeric(sub(".*tx70", "", names(maxtempstack))))
maxtempstack <- maxtempstack[[order.vars]]
names(maxtempstack)

# calculate a proxy for mean temperature
meantempstack <- (mintempstack + maxtempstack) / 2

#calculate growing degree days
gdd <- growingDegDays(meantempstack, baseTemp = 5)

#plot
plot(gdd, col = inferno(100))

writeRaster(gdd, file = paste(outputDir, future.clims[[model.num]], "gdd", sep = ""),
            format = "GTiff")
#save(gdd, file = paste(outputDir, future.clims[[model.num]], "gdd", sep = ""))

########################
#PET Seasonality
########################

#To calculate this, we need solar radiation rasters

# read in a climatic raster for use as a template
rasterTemplate <- mintempstack[[1]]

# calculate monthly solar radiation, defined for the year 2050, output to the current directory (1950 is the index year)
#ETsolradRasters(rasterTemplate = rasterTemplate, year = 100, outputDir = "/Users/alawing/Desktop/FutureClim/SolarRad/")

# calculate monthly solar radiation, defined for the year 2070, output to the current directory (1950 is the index year)
ETsolradRasters(rasterTemplate = rasterTemplate, year = 120, outputDir = "datainputs/gcm/envirem/FutureClim/SolarRad70/")

#files.solrad <- list.files("/Users/alawing/Desktop/FutureClim/SolarRad/", pattern = '.tif$', full.names = TRUE)
files.solrad <- list.files("datainputs/gcm/envirem/FutureClim/SolarRad70", pattern = '.tif$', full.names = TRUE)
solradstack <- stack(files.solrad)

solradstack <- crop(solradstack, d_ext)

#check order
names(solradstack)

# calculate temperature range
rangetempstack <- maxtempstack - mintempstack

#calculate monthly PET for PET seasonality
PETstack <- monthlyPET(meantempstack, solradstack, rangetempstack, tempScale = 10)

#PETseasonality
PETseason <- PETseasonality(PETstack)
plot(PETseason, col = inferno(100))

writeRaster(PETseason, file = paste(outputDir, future.clims[[model.num]], "PETseason", sep = ""),
            format = "GTiff")
#save(PETseason, file = paste(outputDir, future.clims[[model.num]], "PETseason", sep = ""))

########################
#aridityIndexThornthwaite
########################

#mr26pr70
model.num <- 14
inputDir.pr <- paste("datainputs/gcm/envirem/FutureClim",future.clims[[model.num]], sep = "")
files.pr <- list.files(inputDir.pr, pattern = '.tif$', full.names = TRUE)

precipStack <- stack(files.pr)
precipStack <- crop(precipStack, d_ext)

# We need to be sure the months are in chronological order.
names(precipStack)

#determine the order by extracting the month from each file name and using it in sort.list
order.vars <- sort.list(as.numeric(sub(".*pr70", "", names(precipStack))))

#order the variables
precipStack <- precipStack[[order.vars]]

#check the order
names(precipStack)

# Convert layer names for compatibiity with the aridity function
names(precipStack) <- paste("prec", 1:12, sep = "_")
names(PETstack) <- paste("PET", 1:12, sep = "_")

# assign names for compatibility with aridity function
precip <- precipStack
assignNames(precip = 'prec_##')

thornwaite <- aridityIndexThornthwaite(precip, PETstack, precipScale = 1)
plot(thornwaite, col = inferno(100))

writeRaster(thornwaite, file = paste(outputDir, future.clims[[model.num]], "thornwaite", sep = ""),
            format = "GTiff")
#save(thornwaite, file = paste(outputDir, future.clims[[model.num]], "thornwaite", sep = ""))


############################
############################
#######  mr85 50  #######
############################
############################

#mr85tn50
model.num <- 21
inputDir.min <- paste("datainputs/gcm/envirem/FutureClim",future.clims[[model.num]], sep = "")
files.tmin <- list.files(inputDir.min, pattern = '.tif$', full.names = TRUE)

#mr85tx50
model.num <- 23
inputDir.max <- paste("datainputs/gcm/envirem/FutureClim",future.clims[[model.num]], sep = "")
files.tmax <- list.files(inputDir.max, pattern = '.tif$', full.names = TRUE)

########################
# Growing Degree Days 5
########################

# Here, we will take the mean of max and min temperature, 
# although if mean temp was available, that would be better.
mintempstack <- stack(files.tmin)
maxtempstack <- stack(files.tmax)

mintempstack <- crop(mintempstack, d_ext)
maxtempstack <- crop(maxtempstack, d_ext)

# We need to be sure the months are in chronological order.
names(mintempstack)
names(maxtempstack)

#determine the order by extracting the month from each file name and using it in sort.list
order.vars <- sort.list(as.numeric(sub(".*tn50", "", names(mintempstack))))

#order the variables
mintempstack <- mintempstack[[order.vars]]

#check the order
names(mintempstack)

#repeat for the max temp variables
order.vars <- sort.list(as.numeric(sub(".*tx50", "", names(maxtempstack))))
maxtempstack <- maxtempstack[[order.vars]]
names(maxtempstack)

# calculate a proxy for mean temperature
meantempstack <- (mintempstack + maxtempstack) / 2

#calculate growing degree days
gdd <- growingDegDays(meantempstack, baseTemp = 5)

#plot
plot(gdd, col = inferno(100))

writeRaster(gdd, file = paste(outputDir, future.clims[[model.num]], "gdd", sep = ""), 
            format = "GTiff")
#save(gdd, file = paste(outputDir, future.clims[[model.num]], "gdd", sep = ""))

########################
#PET Seasonality
########################

#To calculate this, we need solar radiation rasters

# read in a climatic raster for use as a template
rasterTemplate <- mintempstack[[1]]

# calculate monthly solar radiation, defined for the year 2050, output to the current directory (1950 is the index year)
ETsolradRasters(rasterTemplate = rasterTemplate, year = 100, 
                outputDir = "datainputs/gcm/envirem/FutureClim/SolarRad/")

# calculate monthly solar radiation, defined for the year 2070, output to the current directory (1950 is the index year)
#ETsolradRasters(rasterTemplate = rasterTemplate, year = 120, outputDir = "/Users/alawing/Desktop/FutureClim/SolarRad70/")

files.solrad <- list.files("datainputs/gcm/envirem/FutureClim/SolarRad", pattern = '.tif$', full.names = TRUE)
#files.solrad <- list.files("/Users/alawing/Desktop/FutureClim/SolarRad70/", pattern = '.tif$', full.names = TRUE)
solradstack <- stack(files.solrad)

solradstack <- crop(solradstack, d_ext)

#check order
names(solradstack)

# calculate temperature range
rangetempstack <- maxtempstack - mintempstack

#calculate monthly PET for PET seasonality
PETstack <- monthlyPET(meantempstack, solradstack, rangetempstack, tempScale = 10)

#PETseasonality
PETseason <- PETseasonality(PETstack)
plot(PETseason, col = inferno(100))

writeRaster(PETseason, file = paste(outputDir, future.clims[[model.num]], "PETseason", sep = ""),
            format = "GTiff")
#save(PETseason, file = paste(outputDir, future.clims[[model.num]], "PETseason", sep = ""))

########################
#aridityIndexThornthwaite
########################

#mr85pr50
model.num <- 19
inputDir.pr <- paste("datainputs/gcm/envirem/FutureClim",future.clims[[model.num]], sep = "")
files.pr <- list.files(inputDir.pr, pattern = '.tif$', full.names = TRUE)

precipStack <- stack(files.pr)
precipStack <- crop(precipStack, d_ext)

# We need to be sure the months are in chronological order.
names(precipStack)

#determine the order by extracting the month from each file name and using it in sort.list
order.vars <- sort.list(as.numeric(sub(".*pr50", "", names(precipStack))))

#order the variables
precipStack <- precipStack[[order.vars]]

#check the order
names(precipStack)

# Convert layer names for compatibiity with the aridity function
names(precipStack) <- paste("prec", 1:12, sep = "_")
names(PETstack) <- paste("PET", 1:12, sep = "_")

# assign names for compatibility with aridity function
precip <- precipStack
assignNames(precip = 'prec_##')

thornwaite <- aridityIndexThornthwaite(precip, PETstack, precipScale = 1)
plot(thornwaite, col = inferno(100))

writeRaster(thornwaite, file = paste(outputDir, future.clims[[model.num]], "thornwaite", sep = ""),
            format = "GTiff")
#save(thornwaite, file = paste(outputDir, future.clims[[model.num]], "thornwaite", sep = ""))



#######mr8570#######

#mr85tn70
model.num <- 22
inputDir.min <- paste("datainputs/gcm/envirem/FutureClim",future.clims[[model.num]], sep = "")
files.tmin <- list.files(inputDir.min, pattern = '.tif$', full.names = TRUE)

#mr85tx70
model.num <- 24
inputDir.max <- paste("datainputs/gcm/envirem/FutureClim",future.clims[[model.num]], sep = "")
files.tmax <- list.files(inputDir.max, pattern = '.tif$', full.names = TRUE)

########################
# Growing Degree Days 5
########################

# Here, we will take the mean of max and min temperature, 
# although if mean temp was available, that would be better.
mintempstack <- stack(files.tmin)
maxtempstack <- stack(files.tmax)

mintempstack <- crop(mintempstack, d_ext)
maxtempstack <- crop(maxtempstack, d_ext)

# We need to be sure the months are in chronological order.
names(mintempstack)
names(maxtempstack)

#determine the order by extracting the month from each file name and using it in sort.list
order.vars <- sort.list(as.numeric(sub(".*tn70", "", names(mintempstack))))

#order the variables
mintempstack <- mintempstack[[order.vars]]

#check the order
names(mintempstack)

#repeat for the max temp variables
order.vars <- sort.list(as.numeric(sub(".*tx70", "", names(maxtempstack))))
maxtempstack <- maxtempstack[[order.vars]]
names(maxtempstack)

# calculate a proxy for mean temperature
meantempstack <- (mintempstack + maxtempstack) / 2

#calculate growing degree days
gdd <- growingDegDays(meantempstack, baseTemp = 5)

#plot
plot(gdd, col = inferno(100))

writeRaster(gdd, file = paste(outputDir, future.clims[[model.num]], "gdd", sep = ""),
            format = "GTiff")
#save(gdd, file = paste(outputDir, future.clims[[model.num]], "gdd", sep = ""))

########################
#PET Seasonality
########################

#To calculate this, we need solar radiation rasters

# read in a climatic raster for use as a template
rasterTemplate <- mintempstack[[1]]

# calculate monthly solar radiation, defined for the year 2050, output to the current directory (1950 is the index year)
#ETsolradRasters(rasterTemplate = rasterTemplate, year = 100, outputDir = "/Users/alawing/Desktop/FutureClim/SolarRad/")

# calculate monthly solar radiation, defined for the year 2070, output to the current directory (1950 is the index year)
ETsolradRasters(rasterTemplate = rasterTemplate, year = 120, outputDir = "datainputs/gcm/envirem/FutureClim/SolarRad70/")

#files.solrad <- list.files("/Users/alawing/Desktop/FutureClim/SolarRad/", pattern = '.tif$', full.names = TRUE)
files.solrad <- list.files("datainputs/gcm/envirem/FutureClim/SolarRad70", pattern = '.tif$', full.names = TRUE)
solradstack <- stack(files.solrad)

solradstack <- crop(solradstack, d_ext)

#check order
names(solradstack)

# calculate temperature range
rangetempstack <- maxtempstack - mintempstack

#calculate monthly PET for PET seasonality
PETstack <- monthlyPET(meantempstack, solradstack, rangetempstack, tempScale = 10)

#PETseasonality
PETseason <- PETseasonality(PETstack)
plot(PETseason, col = inferno(100))

writeRaster(PETseason, file = paste(outputDir, future.clims[[model.num]], "PETseason", sep = ""),
            format = "GTiff")
#save(PETseason, file = paste(outputDir, future.clims[[model.num]], "PETseason", sep = ""))

########################
#aridityIndexThornthwaite
########################

#mr85pr70
model.num <- 20
inputDir.pr <- paste("datainputs/gcm/envirem/FutureClim",future.clims[[model.num]], sep = "")
files.pr <- list.files(inputDir.pr, pattern = '.tif$', full.names = TRUE)

precipStack <- stack(files.pr)
precipStack <- crop(precipStack, d_ext)

# We need to be sure the months are in chronological order.
names(precipStack)

#determine the order by extracting the month from each file name and using it in sort.list
order.vars <- sort.list(as.numeric(sub(".*pr70", "", names(precipStack))))

#order the variables
precipStack <- precipStack[[order.vars]]

#check the order
names(precipStack)

# Convert layer names for compatibiity with the aridity function
names(precipStack) <- paste("prec", 1:12, sep = "_")
names(PETstack) <- paste("PET", 1:12, sep = "_")

# assign names for compatibility with aridity function
precip <- precipStack
assignNames(precip = 'prec_##')

thornwaite <- aridityIndexThornthwaite(precip, PETstack, precipScale = 1)
plot(thornwaite, col = inferno(100))

writeRaster(thornwaite, file = paste(outputDir, future.clims[[model.num]], "thornwaite", sep = ""),
            format = "GTiff")
#save(thornwaite, file = paste(outputDir, future.clims[[model.num]], "thornwaite", sep = ""))

################################################################################

#######cc2650#######

#cc26tn50
model.num <- 3
inputDir.min <- paste("datainputs/gcm/envirem/FutureClim/",future.clims[[model.num]], sep = "")
files.tmin <- list.files(inputDir.min, pattern = '.tif$', full.names = TRUE)

#cc26tx50
model.num <- 5
inputDir.max <- paste("datainputs/gcm/envirem/FutureClim/",future.clims[[model.num]], sep = "")
files.tmax <- list.files(inputDir.max, pattern = '.tif$', full.names = TRUE)

########################
# Growing Degree Days 5
########################

# Here, we will take the mean of max and min temperature, 
# although if mean temp was available, that would be better.
mintempstack <- stack(files.tmin)
maxtempstack <- stack(files.tmax)

mintempstack <- crop(mintempstack, d_ext)
maxtempstack <- crop(maxtempstack, d_ext)

# We need to be sure the months are in chronological order.
names(mintempstack)
names(maxtempstack)

#determine the order by extracting the month from each file name and using it in sort.list
order.vars <- sort.list(as.numeric(sub(".*tn50", "", names(mintempstack))))

#order the variables
mintempstack <- mintempstack[[order.vars]]

#check the order
names(mintempstack)

#repeat for the max temp variables
order.vars <- sort.list(as.numeric(sub(".*tx50", "", names(maxtempstack))))
maxtempstack <- maxtempstack[[order.vars]]
names(maxtempstack)

# calculate a proxy for mean temperature
meantempstack <- (mintempstack + maxtempstack) / 2

#calculate growing degree days
gdd <- growingDegDays(meantempstack, baseTemp = 5)

#plot
plot(gdd, col = inferno(100))

writeRaster(gdd, file = paste(outputDir, future.clims[[model.num]], "gdd", sep = ""),
            format = "GTiff")
#save(gdd, file = paste(outputDir, future.clims[[model.num]], "gdd", sep = ""))

########################
#PET Seasonality
########################

#To calculate this, we need solar radiation rasters

# read in a climatic raster for use as a template
rasterTemplate <- mintempstack[[1]]

# calculate monthly solar radiation, defined for the year 2050, output to the current directory (1950 is the index year)
ETsolradRasters(rasterTemplate = rasterTemplate, year = 100, outputDir = "datainputs/gcm/envirem/FutureClim/SolarRad/")

# calculate monthly solar radiation, defined for the year 2070, output to the current directory (1950 is the index year)
#ETsolradRasters(rasterTemplate = rasterTemplate, year = 120, outputDir = "/Users/alawing/Desktop/FutureClim/SolarRad70/")

files.solrad <- list.files("datainputs/gcm/envirem/FutureClim/SolarRad/", pattern = '.tif$', full.names = TRUE)
#files.solrad <- list.files("/Users/alawing/Desktop/FutureClim/SolarRad70/", pattern = '.tif$', full.names = TRUE)
solradstack <- stack(files.solrad)

solradstack <- crop(solradstack, d_ext)

#check order
names(solradstack)

# calculate temperature range
rangetempstack <- maxtempstack - mintempstack

#calculate monthly PET for PET seasonality
PETstack <- monthlyPET(meantempstack, solradstack, rangetempstack, tempScale = 10)

#PETseasonality
PETseason <- PETseasonality(PETstack)

writeRaster(PETseason, file = paste(outputDir, future.clims[[model.num]], "PETseason", sep = ""),
            format = "GTiff")
#save(PETseason, file = paste(outputDir, future.clims[[model.num]], "PETseason", sep = ""))

########################
#aridityIndexThornthwaite
########################

#cc26pr50
model.num <- 1
inputDir.pr <- paste("datainputs/gcm/envirem/FutureClim/",future.clims[[model.num]], sep = "")
files.pr <- list.files(inputDir.pr, pattern = '.tif$', full.names = TRUE)

precipStack <- stack(files.pr)
precipStack <- crop(precipStack, d_ext)

# We need to be sure the months are in chronological order.
names(precipStack)

#determine the order by extracting the month from each file name and using it in sort.list
order.vars <- sort.list(as.numeric(sub(".*pr50", "", names(precipStack))))

#order the variables
precipStack <- precipStack[[order.vars]]

#check the order
names(precipStack)

# Convert layer names for compatibiity with the aridity function
names(precipStack) <- paste("prec", 1:12, sep = "_")
names(PETstack) <- paste("PET", 1:12, sep = "_")

# assign names for compatibility with aridity function
precip <- precipStack
assignNames(precip = 'prec_##')

thornwaite <- aridityIndexThornthwaite(precip, PETstack, precipScale = 1)

writeRaster(thornwaite, file = paste(outputDir, future.clims[[model.num]], "thornwaite", sep = ""),
            format = "GTiff")
#save(thornwaite, file = paste(outputDir, future.clims[[model.num]], "thornwaite", sep = ""))


#######cc2670#######

#cc26tn70
model.num <- 4
inputDir.min <- paste("datainputs/gcm/envirem/FutureClim/",future.clims[[model.num]], sep = "")
files.tmin <- list.files(inputDir.min, pattern = '.tif$', full.names = TRUE)

#cc26tx70
model.num <- 6
inputDir.max <- paste("datainputs/gcm/envirem/FutureClim/",future.clims[[model.num]], sep = "")
files.tmax <- list.files(inputDir.max, pattern = '.tif$', full.names = TRUE)

########################
# Growing Degree Days 5
########################

# Here, we will take the mean of max and min temperature, 
# although if mean temp was available, that would be better.
mintempstack <- stack(files.tmin)
maxtempstack <- stack(files.tmax)

mintempstack <- crop(mintempstack, d_ext)
maxtempstack <- crop(maxtempstack, d_ext)

# We need to be sure the months are in chronological order.
names(mintempstack)
names(maxtempstack)

#determine the order by extracting the month from each file name and using it in sort.list
order.vars <- sort.list(as.numeric(sub(".*tn70", "", names(mintempstack))))

#order the variables
mintempstack <- mintempstack[[order.vars]]

#check the order
names(mintempstack)

#repeat for the max temp variables
order.vars <- sort.list(as.numeric(sub(".*tx70", "", names(maxtempstack))))
maxtempstack <- maxtempstack[[order.vars]]
names(maxtempstack)

# calculate a proxy for mean temperature
meantempstack <- (mintempstack + maxtempstack) / 2

#calculate growing degree days
gdd <- growingDegDays(meantempstack, baseTemp = 5)

#plot
plot(gdd, col = inferno(100))

writeRaster(gdd, file = paste(outputDir, future.clims[[model.num]], "gdd", sep = ""),
            format = "GTiff")
#save(gdd, file = paste(outputDir, future.clims[[model.num]], "gdd", sep = ""))

########################
#PET Seasonality
########################

#To calculate this, we need solar radiation rasters

# read in a climatic raster for use as a template
rasterTemplate <- mintempstack[[1]]

# calculate monthly solar radiation, defined for the year 2050, output to the current directory (1950 is the index year)
#ETsolradRasters(rasterTemplate = rasterTemplate, year = 100, outputDir = "/Users/alawing/Desktop/FutureClim/SolarRad/")

# calculate monthly solar radiation, defined for the year 2070, output to the current directory (1950 is the index year)
ETsolradRasters(rasterTemplate = rasterTemplate, year = 120, outputDir = "datainputs/gcm/envirem/FutureClim/SolarRad70")

#files.solrad <- list.files("/Users/alawing/Desktop/FutureClim/SolarRad/", pattern = '.tif$', full.names = TRUE)
files.solrad <- list.files("datainputs/gcm/envirem/FutureClim/SolarRad70", pattern = '.tif$', full.names = TRUE)
solradstack <- stack(files.solrad)

solradstack <- crop(solradstack, d_ext)

#check order
names(solradstack)

# calculate temperature range
rangetempstack <- maxtempstack - mintempstack

#calculate monthly PET for PET seasonality
PETstack <- monthlyPET(meantempstack, solradstack, rangetempstack, tempScale = 10)

#PETseasonality
PETseason <- PETseasonality(PETstack)
plot(PETseason, col = inferno(100))

writeRaster(PETseason, file = paste(outputDir, future.clims[[model.num]], "PETseason", sep = ""),
            format = "GTiff")
#save(PETseason, file = paste(outputDir, future.clims[[model.num]], "PETseason", sep = ""))

########################
#aridityIndexThornthwaite
########################

#cc26pr70
model.num <- 2
inputDir.pr <- paste("datainputs/gcm/envirem/FutureClim",future.clims[[model.num]], sep = "")
files.pr <- list.files(inputDir.pr, pattern = '.tif$', full.names = TRUE)

precipStack <- stack(files.pr)
precipStack <- crop(precipStack, d_ext)

# We need to be sure the months are in chronological order.
names(precipStack)

#determine the order by extracting the month from each file name and using it in sort.list
order.vars <- sort.list(as.numeric(sub(".*pr70", "", names(precipStack))))

#order the variables
precipStack <- precipStack[[order.vars]]

#check the order
names(precipStack)

# Convert layer names for compatibiity with the aridity function
names(precipStack) <- paste("prec", 1:12, sep = "_")
names(PETstack) <- paste("PET", 1:12, sep = "_")

# assign names for compatibility with aridity function
precip <- precipStack
assignNames(precip = 'prec_##')

thornwaite <- aridityIndexThornthwaite(precip, PETstack, precipScale = 1)
plot(thornwaite, col = inferno(100))

writeRaster(thornwaite, file = paste(outputDir, future.clims[[model.num]], "thornwaite", sep = ""),
            format = "GTiff")
#save(thornwaite, file = paste(outputDir, future.clims[[model.num]], "thornwaite", sep = ""))


############################
############################
#######  cc85 50  #######
############################
############################

#cc85tn50
model.num <- 9
inputDir.min <- paste("datainputs/gcm/envirem/FutureClim",future.clims[[model.num]], sep = "")
files.tmin <- list.files(inputDir.min, pattern = '.tif$', full.names = TRUE)

#cc85tx50
model.num <- 11
inputDir.max <- paste("datainputs/gcm/envirem/FutureClim",future.clims[[model.num]], sep = "")
files.tmax <- list.files(inputDir.max, pattern = '.tif$', full.names = TRUE)

########################
# Growing Degree Days 5
########################

# Here, we will take the mean of max and min temperature, 
# although if mean temp was available, that would be better.
mintempstack <- stack(files.tmin)
maxtempstack <- stack(files.tmax)

mintempstack <- crop(mintempstack, d_ext)
maxtempstack <- crop(maxtempstack, d_ext)

# We need to be sure the months are in chronological order.
names(mintempstack)
names(maxtempstack)

#determine the order by extracting the month from each file name and using it in sort.list
order.vars <- sort.list(as.numeric(sub(".*tn50", "", names(mintempstack))))

#order the variables
mintempstack <- mintempstack[[order.vars]]

#check the order
names(mintempstack)

#repeat for the max temp variables
order.vars <- sort.list(as.numeric(sub(".*tx50", "", names(maxtempstack))))
maxtempstack <- maxtempstack[[order.vars]]
names(maxtempstack)

# calculate a proxy for mean temperature
meantempstack <- (mintempstack + maxtempstack) / 2

#calculate growing degree days
gdd <- growingDegDays(meantempstack, baseTemp = 5)

#plot
plot(gdd, col = inferno(100))

writeRaster(gdd, file = paste(outputDir, future.clims[[model.num]], "gdd", sep = ""), 
            format = "GTiff")
#save(gdd, file = paste(outputDir, future.clims[[model.num]], "gdd", sep = ""))

########################
#PET Seasonality
########################

#To calculate this, we need solar radiation rasters

# read in a climatic raster for use as a template
rasterTemplate <- mintempstack[[1]]

# calculate monthly solar radiation, defined for the year 2050, output to the current directory (1950 is the index year)
ETsolradRasters(rasterTemplate = rasterTemplate, year = 100, 
                outputDir = "datainputs/gcm/envirem/FutureClim/SolarRad/")

# calculate monthly solar radiation, defined for the year 2070, output to the current directory (1950 is the index year)
#ETsolradRasters(rasterTemplate = rasterTemplate, year = 120, outputDir = "/Users/alawing/Desktop/FutureClim/SolarRad70/")

files.solrad <- list.files("datainputs/gcm/envirem/FutureClim/SolarRad", pattern = '.tif$', full.names = TRUE)
#files.solrad <- list.files("/Users/alawing/Desktop/FutureClim/SolarRad70/", pattern = '.tif$', full.names = TRUE)
solradstack <- stack(files.solrad)

solradstack <- crop(solradstack, d_ext)

#check order
names(solradstack)

# calculate temperature range
rangetempstack <- maxtempstack - mintempstack

#calculate monthly PET for PET seasonality
PETstack <- monthlyPET(meantempstack, solradstack, rangetempstack, tempScale = 10)

#PETseasonality
PETseason <- PETseasonality(PETstack)
plot(PETseason, col = inferno(100))

writeRaster(PETseason, file = paste(outputDir, future.clims[[model.num]], "PETseason", sep = ""),
            format = "GTiff")
#save(PETseason, file = paste(outputDir, future.clims[[model.num]], "PETseason", sep = ""))

########################
#aridityIndexThornthwaite
########################

#cc85pr50
model.num <- 7
inputDir.pr <- paste("datainputs/gcm/envirem/FutureClim",future.clims[[model.num]], sep = "")
files.pr <- list.files(inputDir.pr, pattern = '.tif$', full.names = TRUE)

precipStack <- stack(files.pr)
precipStack <- crop(precipStack, d_ext)

# We need to be sure the months are in chronological order.
names(precipStack)

#determine the order by extracting the month from each file name and using it in sort.list
order.vars <- sort.list(as.numeric(sub(".*pr50", "", names(precipStack))))

#order the variables
precipStack <- precipStack[[order.vars]]

#check the order
names(precipStack)

# Convert layer names for compatibiity with the aridity function
names(precipStack) <- paste("prec", 1:12, sep = "_")
names(PETstack) <- paste("PET", 1:12, sep = "_")

# assign names for compatibility with aridity function
precip <- precipStack
assignNames(precip = 'prec_##')

thornwaite <- aridityIndexThornthwaite(precip, PETstack, precipScale = 1)
plot(thornwaite, col = inferno(100))

writeRaster(thornwaite, file = paste(outputDir, future.clims[[model.num]], "thornwaite", sep = ""),
            format = "GTiff")
#save(thornwaite, file = paste(outputDir, future.clims[[model.num]], "thornwaite", sep = ""))



#######cc8570#######

#cc85tn70
model.num <- 10
inputDir.min <- paste("datainputs/gcm/envirem/FutureClim",future.clims[[model.num]], sep = "")
files.tmin <- list.files(inputDir.min, pattern = '.tif$', full.names = TRUE)

#cc85tx70
model.num <- 12
inputDir.max <- paste("datainputs/gcm/envirem/FutureClim",future.clims[[model.num]], sep = "")
files.tmax <- list.files(inputDir.max, pattern = '.tif$', full.names = TRUE)

########################
# Growing Degree Days 5
########################

# Here, we will take the mean of max and min temperature, 
# although if mean temp was available, that would be better.
mintempstack <- stack(files.tmin)
maxtempstack <- stack(files.tmax)

mintempstack <- crop(mintempstack, d_ext)
maxtempstack <- crop(maxtempstack, d_ext)

# We need to be sure the months are in chronological order.
names(mintempstack)
names(maxtempstack)

#determine the order by extracting the month from each file name and using it in sort.list
order.vars <- sort.list(as.numeric(sub(".*tn70", "", names(mintempstack))))

#order the variables
mintempstack <- mintempstack[[order.vars]]

#check the order
names(mintempstack)

#repeat for the max temp variables
order.vars <- sort.list(as.numeric(sub(".*tx70", "", names(maxtempstack))))
maxtempstack <- maxtempstack[[order.vars]]
names(maxtempstack)

# calculate a proxy for mean temperature
meantempstack <- (mintempstack + maxtempstack) / 2

#calculate growing degree days
gdd <- growingDegDays(meantempstack, baseTemp = 5)

#plot
plot(gdd, col = inferno(100))

writeRaster(gdd, file = paste(outputDir, future.clims[[model.num]], "gdd", sep = ""),
            format = "GTiff")
#save(gdd, file = paste(outputDir, future.clims[[model.num]], "gdd", sep = ""))

########################
#PET Seasonality
########################

#To calculate this, we need solar radiation rasters

# read in a climatic raster for use as a template
rasterTemplate <- mintempstack[[1]]

# calculate monthly solar radiation, defined for the year 2050, output to the current directory (1950 is the index year)
#ETsolradRasters(rasterTemplate = rasterTemplate, year = 100, outputDir = "/Users/alawing/Desktop/FutureClim/SolarRad/")

# calculate monthly solar radiation, defined for the year 2070, output to the current directory (1950 is the index year)
ETsolradRasters(rasterTemplate = rasterTemplate, year = 120, outputDir = "datainputs/gcm/envirem/FutureClim/SolarRad70/")

#files.solrad <- list.files("/Users/alawing/Desktop/FutureClim/SolarRad/", pattern = '.tif$', full.names = TRUE)
files.solrad <- list.files("datainputs/gcm/envirem/FutureClim/SolarRad70", pattern = '.tif$', full.names = TRUE)
solradstack <- stack(files.solrad)

solradstack <- crop(solradstack, d_ext)

#check order
names(solradstack)

# calculate temperature range
rangetempstack <- maxtempstack - mintempstack

#calculate monthly PET for PET seasonality
PETstack <- monthlyPET(meantempstack, solradstack, rangetempstack, tempScale = 10)

#PETseasonality
PETseason <- PETseasonality(PETstack)
plot(PETseason, col = inferno(100))

writeRaster(PETseason, file = paste(outputDir, future.clims[[model.num]], "PETseason", sep = ""),
            format = "GTiff")
#save(PETseason, file = paste(outputDir, future.clims[[model.num]], "PETseason", sep = ""))

########################
#aridityIndexThornthwaite
########################

#cc85pr70
model.num <- 8
inputDir.pr <- paste("datainputs/gcm/envirem/FutureClim",future.clims[[model.num]], sep = "")
files.pr <- list.files(inputDir.pr, pattern = '.tif$', full.names = TRUE)

precipStack <- stack(files.pr)
precipStack <- crop(precipStack, d_ext)

# We need to be sure the months are in chronological order.
names(precipStack)

#determine the order by extracting the month from each file name and using it in sort.list
order.vars <- sort.list(as.numeric(sub(".*pr70", "", names(precipStack))))

#order the variables
precipStack <- precipStack[[order.vars]]

#check the order
names(precipStack)

# Convert layer names for compatibiity with the aridity function
names(precipStack) <- paste("prec", 1:12, sep = "_")
names(PETstack) <- paste("PET", 1:12, sep = "_")

# assign names for compatibility with aridity function
precip <- precipStack
assignNames(precip = 'prec_##')

thornwaite <- aridityIndexThornthwaite(precip, PETstack, precipScale = 1)
plot(thornwaite, col = inferno(100))

writeRaster(thornwaite, file = paste(outputDir, future.clims[[model.num]], "thornwaite", sep = ""),
            format = "GTiff")
#save(thornwaite, file = paste(outputDir, future.clims[[model.num]], "thornwaite", sep = ""))
