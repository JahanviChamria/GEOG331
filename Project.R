#load packages
library(terra)
library(sf)
library(dplyr)
library(ggplot2)
#install.packages("tidyr")
library(tidyr)

opar <- par(no.readonly = TRUE)

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
  
  for (city in target_cities) {
    cat("  City:", city, "\n")
    
    #use the combined extent of urban+rural in native CRS of raster
    #transform urban/rural polygons to built CRS
    urban_proj <- project(urban_list[[city]], crs(built_frac))
    rural_proj <- project(rural_list[[city]], crs(built_frac))
    combined_extent <- union(ext(urban_proj), ext(rural_proj))
    
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


