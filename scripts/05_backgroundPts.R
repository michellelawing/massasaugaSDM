#############################
###--- Background points ----
#############################

library(raster)
library(rgdal)
library(rgeos)
library(adehabitatHR)
library(maps)

#set up MCP background and points
#convert coordinates to a spatial data frame (transformed to estimate area)
coords_spdf <- comb_data[,3:2]
coordinates(coords_spdf) <- ~ Longitude + Latitude
projection(coords_spdf) <- CRS('+proj=longlat +datum=WGS84')
coords_spdf_trans <- spTransform(coords_spdf, 
                                 CRS("+proj=utm +zone=14 +ellps=GRS80 +datum=NAD83"))

coords_mcp <- mcp(coords_spdf_trans, percent = 100)
coords_mcp_buff <- gBuffer(mcp(coords_spdf_trans, percent = 100), width = 61548)

#calculate the change in extent - aiming for 20%
(gArea(coords_mcp_buff) - gArea(coords_mcp))/gArea(coords_mcp) #19.99%

#crop mcp and mask to turns variables outside the mask to NA
coords_mcp <- spTransform(coords_mcp, CRS('+proj=longlat +datum=WGS84'))
coords_mcp_buff <- spTransform(coords_mcp_buff, CRS('+proj=longlat +datum=WGS84'))

r1 <- mask(x = crop(bioclim_1, extent(coords_spdf)), mask = coords_mcp)
#plot(r1)
r2 <- mask(x = crop(bioclim_1, extent(coords_mcp_buff)), mask = coords_mcp_buff)
#plot(r2)

#extract 1k points from mcp of occurrence points, add to kfold dataset
backg_mcp <- randomPoints(r1, n=1000, extf = 1.25)
colnames(backg_mcp) <- c('Longitude', 'Latitude')
group <- kfold(backg_mcp, 5)
backg_mcp_train <- backg_mcp[group != 1, ] 
backg_mcp_test <- backg_mcp[group == 1, ] 
training <- c(rep("Yes", nrow(backg_mcp_train)), rep("No", nrow(backg_mcp_test)))

background <- data.frame(rbind(backg_mcp_train, backg_mcp_test), "Training" = training, 
                         "Presence" = "No", "Set" = "OccurrenceMCP")
                         
#extract 10k points from mcp of occurrence points, add to kfold dataset
backg_mcp10k <- randomPoints(r1, n=10000, extf = 1.25)
colnames(backg_mcp10k) <- c('Longitude', 'Latitude')
group <- kfold(backg_mcp10k, 5)
backg_mcp_train10k <- backg_mcp10k[group != 1, ] 
backg_mcp_test10k <- backg_mcp10k[group == 1, ]  
training <- c(rep("Yes", nrow(backg_mcp_train10k)), rep("No", nrow(backg_mcp_test10k)))

temp <-data.frame(rbind(backg_mcp_train10k, backg_mcp_test10k), "Training" = training, 
                  "Presence" = "No", "Set" = "OccurrenceMCP10k")
background <- rbind(background, temp)

#extract 1k points from 20% buffered mcp of occurrence points, add to kfold dataset
backg_mcp_buffer <- randomPoints(r2, n=1000, extf = 1.25)
colnames(backg_mcp_buffer) <- c('Longitude', 'Latitude')
group <- kfold(backg_mcp_buffer, 5)
backg_mcp_buffer_train <- backg_mcp_buffer[group != 1, ] 
backg_mcp_buffer_test <- backg_mcp_buffer[group == 1, ]  
training <- c(rep("Yes", nrow(backg_mcp_buffer_train)), rep("No", nrow(backg_mcp_buffer_test)))

temp <-data.frame(rbind(backg_mcp_buffer_train, backg_mcp_buffer_test), 
                  "Training" = training, "Presence" = "No", "Set" = "BufferedMCP")
background <- rbind(background, temp)

#extract 10k points from 20% buffered mcp of occurrence points, add to kfold dataset
backg_mcp_buffer10k <- randomPoints(r2, n=10000, extf = 1.25)
colnames(backg_mcp_buffer10k) <- c('Longitude', 'Latitude')
group <- kfold(backg_mcp_buffer10k, 5)
backg_mcp_buffer_train10k <- backg_mcp_buffer10k[group != 1, ]  
backg_mcp_buffer_test10k <- backg_mcp_buffer10k[group == 1, ] 
training <- c(rep("Yes", nrow(backg_mcp_buffer_train10k)), rep("No", nrow(backg_mcp_buffer_test10k)))

temp <-data.frame(rbind(backg_mcp_buffer_train10k, backg_mcp_buffer_test10k), 
                  "Training" = training, "Presence" = "No", "Set" = "BufferedMCP10k")
background <- rbind(background, temp)

#Setup circle buffered points
#extract 1k points from small circle polygon of occurrence points, add to kfold dataset
x <- circles(coords_spdf, d=100000, lonlat=TRUE) #200 km diameter circles
pol_small <- polygons(x)
samp1 <- sampleRandom(mask(bioclim_1, pol_small), size = 1000, xy = T, sp = T, na.rm = T)
cells <- unique(cellFromXY(bioclim_1, samp1)) # get unique cells
backg_smcircles <- xyFromCell(bioclim_1, cells)
group <- kfold(backg_smcircles, 5)
backg_smcircles_train <- backg_smcircles[group != 1, ] 
backg_smcircles_test <- backg_smcircles[group == 1, ] 
training <- c(rep("Yes", nrow(backg_smcircles_train)), rep("No", nrow(backg_smcircles_test)))

temp <-data.frame(rbind(backg_smcircles_train, backg_smcircles_test), 
                  "Training" = training, "Presence" = "No", "Set" = "CirclesSmalldiam")
colnames(temp)[1:2] <- c("Longitude", "Latitude")
background <- rbind(background, temp)

#extract 10k points from small circle polygon of occurrence points, add to kfold dataset
x <- circles(coords_spdf, d=100000, lonlat=TRUE) #200 km diameter circles
pol_small <- polygons(x)
samp1_10k <- sampleRandom(mask(bioclim_1, pol_small), size = 10000, xy = T, sp = T, na.rm = T)
cells <- unique(cellFromXY(bioclim_1, samp1_10k)) # get unique cells
backg_smcircles <- xyFromCell(bioclim_1, cells)
group <- kfold(backg_smcircles, 5)
backg_smcircles_train <- backg_smcircles[group != 1, ] 
backg_smcircles_test <- backg_smcircles[group == 1, ] 
training <- c(rep("Yes", nrow(backg_smcircles_train)), rep("No", nrow(backg_smcircles_test)))

temp <-data.frame(rbind(backg_smcircles_train, backg_smcircles_test), 
                  "Training" = training, "Presence" = "No", "Set" = "CirclesSmalldiam10kpts")
colnames(temp)[1:2] <- c("Longitude", "Latitude")
background <- rbind(background, temp)

#extract 1k points from large circle polygon of occurrence points, add to kfold dataset
x <- circles(coords_spdf, d=200000, lonlat=TRUE) #400 km diameter circles
pol_big <- polygons(x)
samp2 <- sampleRandom(mask(bioclim_1, pol_big), size = 1000, xy = T, sp = T, na.rm = T)
cells <- unique(cellFromXY(bioclim_1, samp2)) # get unique cells
backg_bigcircles <- xyFromCell(bioclim_1, cells)
group <- kfold(backg_bigcircles, 5)
backg_bigcircles_train <- backg_bigcircles[group != 1, ] 
backg_bigcircles_test <- backg_bigcircles[group == 1, ]  
training <- c(rep("Yes", nrow(backg_bigcircles_train)), rep("No", nrow(backg_bigcircles_test)))

temp <-data.frame(rbind(backg_bigcircles_train, backg_bigcircles_test), "Training" = training, "Presence" = "No", "Set" = "CirclesBig")
colnames(temp)[1:2] <- c("Longitude", "Latitude")
background <- rbind(background, temp)

#extract 10k points from large circle polygon of occurrence points, add to kfold dataset
x <- circles(coords_spdf, d=200000, lonlat=TRUE) #400 km diameter circles
pol_big <- polygons(x)
samp2_10k <- sampleRandom(mask(bioclim_1, pol_big), size = 10000, xy = T, sp = T, na.rm = T)
cells <- unique(cellFromXY(bioclim_1, samp2_10k)) # get unique cells
backg_bigcircles <- xyFromCell(bioclim_1, cells)
group <- kfold(backg_bigcircles, 5)
backg_bigcircles_train <- backg_bigcircles[group != 1, ]
backg_bigcircles_test <- backg_bigcircles[group == 1, ] 
training <- c(rep("Yes", nrow(backg_bigcircles_train)), rep("No", nrow(backg_bigcircles_test)))

temp <-data.frame(rbind(backg_bigcircles_train, backg_bigcircles_test), "Training" = training, "Presence" = "No", "Set" = "CirclesBigdiam10kpts")
colnames(temp)[1:2] <- c("Longitude", "Latitude")
background <- rbind(background, temp)

summary(background)
summary(background$Set)

## Calculate %increase for the circle buffers, the difference in (dec.deg) areas:
(pol_big@polygons[[1]]@area - pol_small@polygons[[1]]@area)/
  pol_small@polygons[[1]]@area  #==76% w/ the 200km & 400km buffers
#was 19% with the 60 & 73km buffers

###--- PLOT BACKGROUND POINTS ----
#tiff("figures/predictors/BackgroundPoints_20200123.tif", width = 8, height = 8, units = "in",
#     bg = "transparent", res = 1200)
#pdf("figures/predictors/BackgroundPoints_20200123.pdf", width = 8, height = 8, bg = "transparent")
#par(mfrow = c(2,2), mar = c(2,2,2,2))
plot(backg_mcp_buffer, cex=0.5, pch=20, col='blue', xlim = xrange, ylim = yrange,
     main = "1k Pts, MCP Buffers", axes = T, xlab = "", ylab = "")
points(backg_mcp, cex=0.5, pch=20, col='green')
points(coords_spdf, cex=0.5, pch=16, col='purple')
map("state", boundary = T, col = "lightgray", xlim = xrange, ylim = yrange, 
    fill = F, lwd = 2, add = T)
suppressWarnings(map(mex, boundary = T, col = "lightgray", xlim = xrange, 
                     ylim = yrange, fill = F, lwd = 2, add = T))
box()

plot(samp2, cex = 0.5, pch = 20, col = 'blue', xlim = xrange, ylim = yrange, 
     main = "1k Pts, Circle Buffers", axes = T)
points(samp1, cex = 0.5, pch = 20, col = 'green')
points(coords_spdf, cex = 0.5, pch = 16, col = 'purple')
map("state", boundary = T, col = "lightgray", xlim = xrange, ylim = yrange,
    fill = F, lwd = 2, add = T)
suppressWarnings(map(mex, boundary = T, col = "lightgray", xlim = xrange, 
                     ylim = yrange, fill = F, lwd = 2, add = T))
box()

plot(backg_mcp_buffer10k, cex=0.5, pch=20, col='blue', xlim=xrange, ylim=yrange,
     main = "10k Pts, MCP Buffers", xlab = "", ylab = "")
points(backg_mcp10k, cex=0.5, pch=20, col='green')
points(coords_spdf, cex=0.5, pch=16, col='purple')
map("state", boundary = T, col = "lightgray", xlim = xrange, ylim = yrange, 
    fill = F, lwd = 2, add = T)
suppressWarnings(map(mex, boundary = T, col = "lightgray", xlim = xrange, 
                     ylim = yrange, fill = F, lwd = 2, add = T))
box()

plot(samp2_10k, cex=0.5, pch=20, col='blue', xlim=xrange, ylim=yrange,
     main = "10k Pts, Circle Buffers", axes = T)
points(samp1_10k, cex=0.5, pch=20, col='green')
points(coords_spdf, cex=0.5, pch=16, col='purple')
map("state", boundary = T, col = "lightgray", xlim = xrange, ylim = yrange,
    fill = F, lwd = 2, add = T)
suppressWarnings(map(mex, boundary = T, col = "lightgray", xlim = xrange, 
                     ylim = yrange, fill = F, lwd = 2, add = T))
box()
#dev.off()