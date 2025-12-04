#load package
library(terra)

#working with only one tile for now, will replicate this process for all tiles (3 cities, different years) later
#read the MODIS HDF file
t <- rast("Z:\\jchamria\\project\\MOD13A2.A2024001.h08v05.061.2024022141941.hdf")

#check structure
t
names(t)

#extract useful bands
#Band 1 = NDVI, Band 2 = EVI
ndvi <- t[[1]]
evi  <- t[[2]]

#MODIS NDVI/EVI are scaled by 0.0001
ndvi_scaled <- ndvi * 0.0001
evi_scaled  <- evi  * 0.0001

#plot vegetation indices
plot(ndvi_scaled, main = "NDVI (scaled)")
plot(evi_scaled, main = "EVI (scaled)")

lonPh <- 112.0725 #Phoenix
latPh <- 33.4483 

print(crs(ndvi_scaled))

point <- vect(cbind(lonPh, latPh), crs = "EPSG:4326")

ndvi_val <- extract(ndvi_scaled, point)

print(ndvi_val[[1]])

