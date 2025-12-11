library(terra)
library(sf)

#11/13/25
#working with the GHS-UCDB data first to define my urban and rural polygons (doing this first makes more sense than NVDI and LST)

ucdb <- vect("Z:\\jchamria\\project\\GHS_UCDB_REGION_NORTHERN_AMERICA_R2024A.gpkg")
st_layers("Z:\\jchamria\\project\\GHS_UCDB_REGION_NORTHERN_AMERICA_R2024A.gpkg") #listing layers to find the urban centre polygons

#this has all layers
#using the layer i need (polygons)

ucdb_poly <- st_read(
  "Z:\\jchamria\\project\\GHS_UCDB_REGION_NORTHERN_AMERICA_R2024A.gpkg",
  layer = "GHSL_UCDB_THEME_GENERAL_CHARACTERISTICS_GLOBE_R2024A"
)
names(ucdb_poly) #getting the column name that stores city names by checking all column names
unique(ucdb_poly$GC_UCN_MAI_2025) #reading the unique city names (i put New York instead of New York City at first)

target_cities <- c("Phoenix", "Chicago", "New York City") #the cities i wish to analyze

cities <- ucdb_poly[ucdb_poly$GC_UCN_MAI_2025 %in% target_cities, ]

cities #checking if i have the cities needed

st_crs(cities) #checking which projection they are in

#reprojecting to a metric CRS for rural buffer distances in meters
cities_metric <- st_transform(cities, 3857)  #converting to metric CRS (EPSG:3857) for buffers (in km)
urban_poly_metric <- cities_metric$geom  #extracting city polygons (reporjected)
urban_only <- vect(urban_poly_metric)

#creating buffers for the rural areas surrounding cities
outer <- st_buffer(urban_poly_metric, 50000)   # 50 km
inner <- st_buffer(urban_poly_metric, 10000)   # 10 km

#subtracting inner buffer from outer to get the rural ring
rural_ring_metric <- st_difference(outer, inner)

rural_only <- mask(vect(rural_ring_metric), crop(vect(rural_ring_metric), urban_only), inverse=TRUE)

#rural
plot(rural_only, col = "lightgreen", border = "darkgreen", main = "Urban vs Rural Polygons")

#urban
plot(urban_only, col = "red", border = "darkred", add = TRUE)


#11/20/25
#land surface temperature
#for a recent phoenix tile for now, will be adding the two cities and time after checking if this works

s <- sds("Z:\\jchamria\\project\\MOD11A2.A2025313.h08v05.061.2025322165642.hdf")
sds("Z:\\jchamria\\project\\MOD11A2.A2025313.h08v05.061.2025322165642.hdf")
#lst <- rast(s[[1]])
#lst <- rast("Z:\\jchamria\\project\\MOD11A2.A2025313.h08v05.061.2025322165642.hdf",
#           subdataset = "LST_Day_1km")

lst <- rast("HDF4_EOS:EOS_GRID:\"Z:/jchamria/project/MOD11A2.A2025313.h08v05.061.2025322165642.hdf\":MODIS_Grid_8Day_1km_LST:LST_Day_1km")
plot(lst)

#minmax(lst)

#lst[lst < 7500] <- NA   #mask out invalid pixels

vals_raw <- values(lst, mat = FALSE)
summary(vals_raw)   

#vals_valid <- vals_raw[vals_raw >= 7500 & vals_raw <= 65535]
#summary(vals_valid)

#lst_c <- vals_raw - 273.15
#summary(lst_c)
#hist(lst_c, breaks=50, main="LST in Celsius (valid pixels only)", xlab="°C")

#--------------------------------------
lst <- s[[1]]
#applying scale factor and then converting from Kelvin to Fahrenheit
lst_f <- (((lst-273.15) * 9/5) + 32)
hist(lst_f, breaks=50, main="LST in Fahrenheit", xlab="°F")

#checking projection of landsat data
crs(lst_f)

summary(lst_f)
#reprojecing to the same CRS
lst_f_metric <- project(lst_f, "epsg:3857")  #converting to metric CRS (EPSG:3857) 
#-----------------------------
summary(values(lst_f_metric))


#crop raster to combined extent to save memory
all_polygons <- c(urban_only, rural_only)

combined_extent <- ext(all_polygons)

lst_crop <- crop(lst_f_metric, combined_extent)

#mask urban area to get urban raster
urban_raster <- mask(lst_crop, urban_only)

#mask rural area first with rural polygon
rural_raster <- mask(lst_crop, rural_only)

#remove any overlapping urban pixels from rural raster
rural_raster <- mask(rural_raster, urban_only, inverse=TRUE)

#check plots
plot(rural_raster, main="Rural and Urban LST")
plot(urban_raster, add=TRUE, main="Urban LST")

#mean LST
urban_mean <- global(urban_raster, fun="mean", na.rm=TRUE)[1,1]
rural_mean <- global(rural_raster, fun="mean", na.rm=TRUE)[1,1]

#UHI
uhi_value <- urban_mean - rural_mean
cat("Urban mean:", round(urban_mean,2), "°F\n")
cat("Rural mean:", round(rural_mean,2), "°F\n")
cat("UHI:", round(uhi_value,2), "°F\n")

#UHI raster
#plot(uhi_raster,
#     main="Urban Heat Island (Pixel-wise) Map",
#     col=terrain.colors(20))
#plot(urban_only, border="red", lwd=2, add=TRUE)
#plot(rural_only, border="blue", lwd=2, add=TRUE)

#histogram of pixel-wise UHI
#hist(values(uhi_raster),
#     main="Pixel-wise UHI Distribution",
#     xlab="Temperature Difference (°F)",
#     ylab="Number of Pixels",
#     col="orange",
#     breaks=20)

# --------------------------------------------------------------
# HISTOGRAM OF PIXEL-WISE UHI VALUES
# --------------------------------------------------------------

# -------------------------
# Histogram of pixel-wise UHI (urban pixel - rural mean)
# Make axes explicit so they are 'complete'
# -------------------------
urban_vals <- values(urban_raster)
urban_vals <- urban_vals[is.finite(urban_vals)]

uhi_pixelwise <- urban_vals - rural_mean
uhi_pixelwise <- uhi_pixelwise[is.finite(uhi_pixelwise)]

# Define nice breakpoints and axis limits
xlims_hist <- range(uhi_pixelwise, na.rm = TRUE)
xpad <- diff(xlims_hist) * 0.04
xlims_hist <- c(xlims_hist[1] - xpad, xlims_hist[2] + xpad)

# Choose breaks (40 bins or fewer if very narrow range)
nbins <- 40
breaks_vec <- pretty(xlims_hist, n = nbins)

# Make histogram without axes, then add complete axes
hist_obj <- hist(uhi_pixelwise,
                 breaks = breaks_vec,
                 plot = FALSE)

# Set up plotting margins if needed
op <- par(no.readonly = TRUE)
par(mar = c(5, 5, 4, 2) + 0.1)  # more space for axis labels

# draw histogram with explicit xlim/ylim and no axes
hist(uhi_pixelwise,
     breaks = breaks_vec,
     col = "orange",
     main = "Histogram of UHI Values (Pixel-wise): Phoenix in 2025",
     xlab = "UHI (°F): Urban pixels - Rural mean",
     ylab = "Number of Pixels",
     xlim = xlims_hist,
     ylim = c(0, max(hist_obj$counts, na.rm = TRUE) * 1.05),
     axes = FALSE)

# add x and y axes with pretty ticks
axis(1, at = pretty(xlims_hist))
axis(2, at = pretty(c(0, hist_obj$counts)), las = 1)

# add a vertical line at mean UHI and label
abline(v = mean(uhi_pixelwise, na.rm = TRUE), col = "red", lwd = 2, lty = 2)
mtext(sprintf("Mean = %.2f °F", mean(uhi_pixelwise, na.rm = TRUE)), side = 3, line = -1, adj = 0.99, col = "red")

# restore par
par(op)

#NDVI

#working with only one tile for now, will replicate this process for all tiles (3 cities, different years) later
#read the MODIS HDF file
t <- rast("Z:\\jchamria\\project\\MOD13A2.A2024001.h08v05.061.2024022141941.hdf")

#check structure
t
names(t)

s <- sds(t)
print(t)
terra::describe(t)

#extract useful bands
#Band 1 = NDVI
ndvi_raw <- t[[1]]

ndvi_raw_metric <- project(ndvi_raw, "EPSG:3857")

all_proj <- c(urban_only, rural_only) 
combined_extent <- ext(all_proj)

ndvi_raw_crop <- crop(ndvi_raw_metric, combined_extent)
class(combined_extent)
print(combined_extent)
#mask with urban/rural polygons
urban_ndvi_raw  <- mask(ndvi_raw_crop, urban_only)
rural_ndvi_raw  <- mask(ndvi_raw_crop, rural_only)
rural_ndvi_raw  <- mask(rural_ndvi_raw, urban_only, inverse=TRUE)

#apply scaling (MODIS so multiply by 0.0001)
urban_ndvi  <- urban_ndvi_raw * 0.0001
rural_ndvi  <- rural_ndvi_raw * 0.0001

#means
urban_ndvi_mean <- global(urban_ndvi, fun="mean", na.rm=TRUE)[1,1]
rural_ndvi_mean <- global(rural_ndvi, fun="mean", na.rm=TRUE)[1,1]

cat("Urban NDVI mean:", urban_ndvi_mean, "\n")
cat("Rural NDVI mean:", rural_ndvi_mean, "\n")
cat("NDVI difference:", rural_ndvi_mean - urban_ndvi_mean, "\n")

plot(urban_ndvi, main="Rural and Urban NDVI", col=terrain.colors(20))
plot(rural_ndvi, main="Rural NDVI", add=TRUE, col=terrain.colors(20))

hist(values(urban_ndvi),
     main="Urban NDVI Distribution",
     col="darkgreen", breaks=30)

hist(values(rural_ndvi),
     main="Rural NDVI Distribution",
     col="lightgreen", breaks=30)


#built-up density

built <- rast("Z:\\jchamria\\project\\GHS_BUILT_S_NRES_E2025_GLOBE_R2023A_4326_30ss_V1_0\\GHS_BUILT_S_NRES_E2025_GLOBE_R2023A_4326_30ss_V1_0.tif")
built_valid <- built
values(built_valid)[values(built_valid) < -2000 | values(built_valid) > 10000] <- NA

built_frac <- built / 10000
built_3857 <- project(built_frac, "EPSG:3857")
built_crop <- crop(built_3857, combined_extent)
minmax(built)

urban_built  <- mask(built_crop, urban_only)
rural_built  <- mask(built_crop, rural_only)

#remove overlap pixels from rural
rural_built  <- mask(rural_built, urban_only, inverse = TRUE)

urban_built_mean <- global(urban_built, "mean", na.rm = TRUE)[1,1]
rural_built_mean <- global(rural_built, "mean", na.rm = TRUE)[1,1]

cat("Urban built-up (%):", round(urban_built_mean * 100, 2), "\n")
cat("Rural built-up (%):", round(rural_built_mean * 100, 2), "\n")
cat("Difference:", 
    round(urban_built_mean * 100 - rural_built_mean * 100, 2), "%\n")

zlim_vals <- c(0, 100)  

#rural raster (with legend)
plot(rural_built,
     col = terrain.colors(30),
     zlim = zlim_vals,
     main = "Urban (overlay) and Rural Built-Up Density",
     xlab = "Easting (m)",
     ylab = "Northing (m)")

#overlay urban raster (without legend)
plot(urban_built,
     col = heat.colors(30, alpha = 0.5),  
     add = TRUE,
     zlim = zlim_vals,
     legend = FALSE)