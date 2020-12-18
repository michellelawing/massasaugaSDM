##################################
###--- Input Data and Figures ----
##################################

# Takes the cleaned occurrence data (csv available upon request) and maps it,
# and gathers climate data from bioclim (https://www.worldclim.org/data/bioclim.html_
# and envirem (https://envirem.github.io/)

# Install these libraries before loading them with the code below.
library(rgdal)
library(maptools)
library(maps)
library(raster)
library(envirem)

# Save the occurrence data in a file called data.csv
# Read in data.csv, make sure the directory is pointing to the correct working directory
comb_data <- read.csv("datainputs/data_ex.csv")
comb_data <- comb_data[,-2]

#read in MX map data
mex <- readOGR("datainputs/mexstates", "mexstates")

#calculate the extent of the map
xrange <- c(min(comb_data[,3]) - 7, max(comb_data[,3]) + 7) # get bounding box
yrange <- c(min(comb_data[,2]) - 5, max(comb_data[,2]) + 7)

#load in data for maps
data(wrld_simpl)

#pdf("figures/OccurrencePointsMap.pdf", width = 5, height = 8)
#par(family = "Arial")
map("state", boundary = F, xlim=xrange, ylim=yrange, fill = F)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "light blue")
map("state", boundary = F, xlim=xrange, ylim=yrange, fill = T, col="lightgray", add = T )
suppressWarnings(map(mex, boundary = F, fill = T, col="lightgray", add = TRUE))
map("lakes", add=TRUE, fill=TRUE, col='lightblue', boundary='black')
box()
points(comb_data$Longitude, comb_data$Latitude, col = "red", pch = 1, cex = 0.5)
scalebar(d = 500, xy = c(-95.5, 23.25), type = "line")
points(-95.5, 22.25, col = "red", pch = 1, cex = 0.75)
text(-95.5, 22.25, labels = "Occurrences", pos = 4, cex = 0.75)
#dev.off()

#mapping climate data
bioclim <- getData(name = "worldclim", var = "bio", res = 2.5, download = T, path = "datainputs/")
bc_points <- raster::extract(bioclim, data.frame(comb_data$Longitude, comb_data$Latitude))
bc_points <- bc_points[!is.na(bc_points[,1]),]

#mapping environment data
file_names <- as.list(paste("datainputs/env2-5/" , list.files("datainputs/env2-5/"), sep = ""))
envirem <- stack(file_names)
names(envirem) <- gsub("current_2.5arcmin_","", names(envirem))
env_points <- raster::extract(envirem, data.frame(comb_data$Longitude, comb_data$Latitude))
env_points <- env_points[!is.na(env_points[,1]),]

