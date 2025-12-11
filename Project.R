#load packages
library(terra)
library(sf)
library(dplyr)
library(ggplot2)
#install.packages("tidyr")
library(tidyr)

opar <- par(no.readonly = TRUE)

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
lst
plot(lst)

#minmax(lst)

#lst[lst < 7500] <- NA   #mask out invalid pixels

vals_raw <- values(lst, mat = FALSE)
summary(vals_raw)   

#vals_valid <- vals_raw[vals_raw >= 7500 & vals_raw <= 65535]
#summary(vals_valid)

lst_c <- vals_raw - 273.15
summary(lst_c)
hist(lst_c, breaks=50, main="LST in Celsius (valid pixels only)", xlab="°C")

#--------------------------------------
lst <- s[[1]]
#applying scale factor and then converting from Kelvin to Fahrenheit
lst_f <- (lst - 273.15) * 9/5 + 32

#checking projection of landsat data
crs(lst_f)

#reprojecing to the same CRS
lst_f_metric <- project(lst_f, "epsg:3857")  #converting to metric CRS (EPSG:3857) 
#-----------------------------

#crop raster to combined extent
#combined_extent <- union(ext(urban_only), ext(rural_only))
#summary(combined_extent)

#combine the urban and rural polygons into a single SpatVector
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
plot(urban_raster, add=TRUE, main="Urban LST overlay")

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


#NDVI

#working with only one tile for now, will replicate this process for all tiles (3 cities, different years) later
#read the MODIS HDF file
t <- rast("Z:\\jchamria\\project\\MOD13A2.A2024001.h08v05.061.2024022141941.hdf")

#check structure
t
names(t)

#extract useful bands
#Band 1 = NDVI
ndvi_raw <- t[[1]]

ndvi_raw_metric <- project(ndvi_raw, "EPSG:3857")

all_polygons <- c(urban_only, rural_only)

combined_extent <- ext(all_polygons)

ndvi_raw_crop <- crop(ndvi_raw_metric, combined_extent)

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
#built_valid <- built
#values(built_valid)[values(built_valid) < -2000 | values(built_valid) > 10000] <- NA

all_polygons <- c(urban_only, rural_only)

combined_extent <- ext(all_polygons)

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
     main = "Urban and Rural Built-Up Density",
     xlab = "Easting (m)",
     ylab = "Northing (m)")

#overlay urban raster (without legend)
plot(urban_built,
     col = heat.colors(30, alpha = 0.5),  
     add = TRUE,
     zlim = zlim_vals,
     legend = FALSE)

#compiling
target_cities <- c("Phoenix", "Chicago", "New York City") #the cities i wish to analyze
years_lst <- 2000:2025              
years_ndvi <- 2000:2025             
years_built <- c(2000,2005,2010,2015,2020,2025)  #global built-up raster epochs available

#load GHS-UCDB to define urban and rural polygons
ucdb_poly <- st_read("Z:/jchamria/project/GHS_UCDB_REGION_NORTHERN_AMERICA_R2024A.gpkg",
                     layer = "GHSL_UCDB_THEME_GENERAL_CHARACTERISTICS_GLOBE_R2024A")

cities_sf <- ucdb_poly[ucdb_poly$GC_UCN_MAI_2025 %in% target_cities, ]
cities_metric <- st_transform(cities_sf, 3857)  # metric CRS

urban_list <- lapply(1:nrow(cities_metric), function(i) vect(cities_metric$geom[i]))
names(urban_list) <- target_cities

#create rural buffers
rural_list <- lapply(1:nrow(cities_metric), function(i){
  outer <- st_buffer(cities_metric$geom[i], 50000)
  inner <- st_buffer(cities_metric$geom[i], 10000)
  rural <- st_difference(outer, inner)
  mask(vect(rural), crop(vect(rural), urban_list[[i]]), inverse=TRUE)
})
names(rural_list) <- target_cities

#checking all my results
urban_list
rural_list

lapply(urban_list, class)
lapply(rural_list, class)

lapply(urban_list, function(x) geomtype(x))
lapply(rural_list, function(x) geomtype(x))

lapply(urban_list, ext)
lapply(rural_list, ext)

par(mfrow=c(1,3))
for(i in names(urban_list)){
  plot(urban_list[[i]], main=paste(i, "Urban"))
}

par(mfrow=c(1,3))
for(i in names(rural_list)){
  plot(rural_list[[i]], main=paste(i, "Rural Ring"))
}

#polygons have been obtained
#moving on to built-up density

#load all global GHSL built-up files
built_means_list <- list()

#folder with global built-up TIFs
built_files <- list.files("Z:/jchamria/project/builtupdata/",
                          pattern = "\\.tif$", full.names = TRUE)

for (f in built_files) {
  
  #extract year from filename
  year <- as.numeric(sub(".*E(\\d{4}).*", "\\1", basename(f)))
  cat("Processing year:", year, "\n")  #needed because of long processing times and for debugging
  
  #safe read (no bad_alloc which i was getting earlier because of large files)
  built_global <- try(rast(f), silent = TRUE)
  if (inherits(built_global, "try-error")) {
    cat("Failed to read raster:", f, "\n")
    next
  }
  
  #remove invalid pixels
  built_valid <- classify(
    built_global,
    rcl = matrix(c(-Inf, -2000, NA,
                   10000, Inf, NA), ncol = 3, byrow = TRUE)
  )
  
  #convert to 0–1 scale
  built_frac <- built_valid / 10000
  
  crs(built_frac) <- "EPSG:4326"
  
  for (city in target_cities) {
    cat("  City:", city, "\n")
    
    #use the combined extent of urban+rural in native CRS of raster
    #transform urban/rural polygons to built CRS
    urban_proj <- project(urban_list[[city]], crs(built_frac))
    rural_proj <- project(rural_list[[city]], crs(built_frac))
    
    #combine the projected urban and rural vectors
    all_proj <- c(urban_proj, rural_proj) 
    #get the extent of the combined vector
    combined_extent <- ext(all_proj)
    
    #crop first in native CRS
    built_crop <- crop(built_frac, combined_extent)
    
    #reproject cropped raster to metric CRS 3857
    built_3857 <- project(built_crop, "EPSG:3857")
    
    #mask urban/rural areas (already in 3857)
    urban_built  <- mask(built_3857, urban_list[[city]])
    rural_built  <- mask(built_3857, rural_list[[city]])
    rural_built  <- mask(rural_built, urban_list[[city]], inverse = TRUE)
    
    #compute means
    urban_mean <- global(urban_built, "mean", na.rm = TRUE)[1,1]
    rural_mean <- global(rural_built, "mean", na.rm = TRUE)[1,1]
    diff_pct <- (urban_mean - rural_mean) * 100
    
    #store
    built_means_list[[length(built_means_list)+1]] <- data.frame(
      city = city,
      year = year,
      urban_built = urban_mean,
      rural_built = rural_mean,
      diff = diff_pct
    )
    
    # print
    cat("    Urban built-up:", round(urban_mean*100,2), "%\n")
    cat("    Rural built-up:", round(rural_mean*100,2), "%\n")
    cat("    Difference (urban-rural):", round(diff_pct,2), "%\n\n")
  }
}

#combine into one data frame
built_means <- do.call(rbind, built_means_list)
built_means

#to reset plotting window
par(opar)

#set up plot
plot(NULL, xlim = c(2000, 2025), ylim = c(0, 0.35),
     xlab = "Year", ylab = "Built-Up Fraction",
     main = "Urban vs Rural Built-Up Density")

#define colors for cities
city_colors <- c("Phoenix" = "red", "Chicago" = "blue", "New York City" = "green")

#loop through cities
for(city in unique(built_means$city)) {
  city_data <- built_means[built_means$city == city, ]
  # urban
  lines(city_data$year, city_data$urban_built, col = city_colors[city], lwd = 2)
  points(city_data$year, city_data$urban_built, col = city_colors[city], pch = 16)
  # rural (dashed)
  lines(city_data$year, city_data$rural_built, col = city_colors[city], lwd = 2, lty = 2)
}

legend("topright", legend = unique(built_means$city),
       col = city_colors, lwd = 2, lty = 1)
legend("bottomright", legend = "Rural (dashed)", lty = 2)

#interpolation for other years in between

#new data frame to store interpolated values
built_full <- data.frame()

for(city in target_cities) {
  
  city_data <- built_means[built_means$city == city, ]
  
  #years to interpolate
  all_years <- 2000:2025
  
  #interpolate urban_built
  urban_interp <- approx(x = city_data$year, 
                         y = city_data$urban_built, 
                         xout = all_years)$y
  
  #interpolate rural_built
  rural_interp <- approx(x = city_data$year, 
                         y = city_data$rural_built, 
                         xout = all_years)$y
  
  #compute difference
  diff_interp <- (urban_interp - rural_interp) * 100
  
  #combine into a data frame
  built_full <- rbind(built_full,
                      data.frame(
                        city = city,
                        year = all_years,
                        urban_built = urban_interp,
                        rural_built = rural_interp,
                        diff = diff_interp
                      ))
}

#check
head(built_full)

#reshape for plotting
built_long <- built_full %>%
  pivot_longer(cols = c("urban_built", "rural_built"),
               names_to = "zone",
               values_to = "built_frac") %>%
  mutate(built_pct = built_frac * 100)  #scale 0–1 to percentage

#plot
ggplot(built_long, aes(x = year, y = built_pct, color = zone)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  facet_wrap(~ city, ncol = 1) +
  labs(
    title = "Urban vs Rural Built-Up Density (2000–2025)",
    x = "Year",
    y = "Built-Up Density (%)",
    color = "Zone"
  ) +
  theme_minimal(base_size = 14) +
  scale_color_manual(values = c("urban_built" = "red", "rural_built" = "green"))

base_plot <- function(data, plot_title) {
  ggplot(data, aes(x = year, y = built_pct, color = zone)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    labs(
      title = plot_title,
      x = "Year",
      y = "Built-Up Density (%)",
      color = "Zone"
    ) +
    theme_minimal(base_size = 14) +
    scale_color_manual(values = c("urban_built" = "red", "rural_built" = "green"))
}

phoenix_data <- built_long %>%
  filter(city == "Phoenix")
base_plot(phoenix_data, "Phoenix Built-Up Density (2000–2025)")

chicago_data <- built_long %>%
  filter(city == "Chicago")
base_plot(chicago_data, "Chicago Built-Up Density (2000–2025)")

nyc_data <- built_long %>%
  filter(city == "New York City")
base_plot(nyc_data, "New York City Built-Up Density (2000–2025)")

#NDVI processing

city_paths <- list(
  "Chicago"       = "Z:/jchamria/project/earthdata/NDVI/Chicago",
  "Phoenix"       = "Z:/jchamria/project/earthdata/NDVI/Phoenix",
  "New York City" = "Z:/jchamria/project/earthdata/NDVI/New York City"
)

ndvi_means_list <- list()
modis_sinu_crs <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs"

for (city in target_cities) {
  
  cat("\n", toupper(city), "\n")
  
  files <- list.files(city_paths[[city]], pattern = "\\.hdf$", full.names = TRUE)
  
  #extract year
  years <- as.numeric(sub(".*A(\\d{4})(\\d{3}).*", "\\1", basename(files)))
  unique_years <- sort(unique(years))
  
  for (yr in unique_years) {
    
    cat("\nYear:", yr, "\n")
    
    #files for this year
    yearly_files <- files[years == yr]
    
    if (length(yearly_files) == 0) {
      cat("No files for this year.\n")
      next
    }
    
    ndvi_stack <- list()
    
    #calculate the extent in the target CRS (EPSG:3857) once per city/year
    #we reuse the urban_list and rural_list objects which are in 3857
    all_polygons_3857 <- c(urban_list[[city]], rural_list[[city]])
    combined_extent_3857 <- ext(all_polygons_3857)
   
    for (f in yearly_files) {
      
      cat("  File:", basename(f), "\n")
      
      ndvi_raw <- try(rast(f, subds = 1), silent = TRUE)
      crs(ndvi_raw) <- modis_sinu_crs
      
      if (inherits(ndvi_raw, "try-error")) {
        cat("    Failed to read NDVI band or subdataset not found.\n")
        next
      }
      
      #reproject the whole tile to the metric CRS first
      #slow, but works
      ndvi_3857 <- try(project(ndvi_raw, "EPSG:3857"), silent = TRUE)
      
      if (inherits(ndvi_3857, "try-error")) {
        cat("    Projection failed (likely due to missing CRS metadata).\n")
        next
      }
      
      #check for non-valid data 
      if (all(is.nan(values(ndvi_3857)))) {
        cat("    Reprojection complete, but all values are NaN (Polygon likely outside tile).\n")
        next
      }
      
      #remove invalid pixels and apply scale factor
      ndvi_3857[ndvi_3857 < -2000 | ndvi_3857 > 10000] <- NA
      ndvi_3857 <- ndvi_3857 * 0.0001
      
      #crop using the 3857 extent
      ndvi_crop <- crop(ndvi_3857, combined_extent_3857)
      
      ndvi_stack[[length(ndvi_stack) + 1]] <- ndvi_crop
      
    }
    
    if (length(ndvi_stack) == 0) {
      cat("  No valid NDVI rasters for this year.\n")
      next
    }
    
    ndvi_mean_year <- mean(rast(ndvi_stack))
    
    urban_ndvi <- mask(ndvi_mean_year, urban_list[[city]])
    rural_ndvi <- mask(ndvi_mean_year, rural_list[[city]])
    rural_ndvi <- mask(rural_ndvi, urban_list[[city]], inverse = TRUE)
    
    urban_mean <- global(urban_ndvi, "mean", na.rm = TRUE)[1,1]
    rural_mean <- global(rural_ndvi, "mean", na.rm = TRUE)[1,1]
    diff_val <- urban_mean - rural_mean
    
    cat("  Urban NDVI:", round(urban_mean, 3), "\n")
    cat("  Rural NDVI:", round(rural_mean, 3), "\n")
    cat("  Difference:", round(diff_val, 3), "\n")
    
    ndvi_means_list[[length(ndvi_means_list) + 1]] <- data.frame(
      city = city,
      year = yr,
      urban_ndvi = urban_mean,
      rural_ndvi = rural_mean,
      diff = diff_val
    )
  }
}

ndvi_means <- do.call(rbind, ndvi_means_list)
ndvi_means

