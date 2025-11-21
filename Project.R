#load packages
library(terra)
library(sf)

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

#plot vegetation indices
plot(ndvi, main = "NDVI")
plot(evi, main = "EVI")

lonPh <- 112.0725 #Phoenix
latPh <- 33.4483 

print(crs(ndvi))

point <- vect(cbind(lonPh, latPh), crs = "EPSG:4326")

ndvi_val <- extract(ndvi, point)

print(ndvi_val[[1]])

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

#creating buffers for the rural areas surrounding cities
outer <- st_buffer(urban_poly_metric, 50000)   # 50 km
inner <- st_buffer(urban_poly_metric, 10000)   # 10 km

#subtracting inner buffer from outer to get the rural ring
rural_ring_metric <- st_difference(outer, inner)

#projecting back to the latitude/longtitude 4326 CRS
urban <- st_transform(urban_poly_metric, 4326)
rural <- st_transform(rural_ring_metric, 4326)

#while checking if the polygons are valid (fully closed borders, no intersections), i ran into an error for NYC
#so i did some research to separate the polygons and fix NYC
urban_single <- st_cast(urban, "POLYGON")
urban_fixed <- st_make_valid(urban_single)
st_is_valid(urban_fixed)
st_is_valid(rural)

urban <- urban_fixed

#rural
plot(rural, col = "lightgreen", border = "darkgreen", main = "Urban vs Rural Polygons")

#urban polygons on top
plot(urban, col = "red", border = "darkred", add = TRUE)


#11/20/25
#land surface temperature
#for a recent phoenix tile for now, will be adding the two cities and time after checking if this works

s <- sds("Z:\\jchamria\\project\\MOD11A2.A2025313.h08v05.061.2025322165642.hdf")
s
lst <- s[[1]]
#lst <- rast("Z:\\jchamria\\project\\MOD11A2.A2025313.h08v05.061.2025322165642.hdf",
 #           subdataset = "LST_Day_1km")

#applying scale factor and then converting from Kelvin to Fahrenheit
lst_f <- (lst * 0.02 - 273.15) * 9/5 + 32

#reprojecting polygons to raster CRS
urban_proj <- st_transform(urban, crs(lst_f))
rural_proj <- st_transform(rural, crs(lst_f))

#converting sf -> SpatVector for terra functions
urban_v <- vect(urban_proj)
rural_v <- vect(rural_proj)

#cropping raster for each area
lst_urban <- mask(crop(lst_f, urban_v), urban_v)
lst_rural <- mask(crop(lst_f, rural_v), rural_v)

#mean rural LST
rural_mean <- global(lst_rural, fun="mean", na.rm=TRUE)[1,1]

#UHI raster
uhi <- lst_urban - rural_mean

#ploting UHI map

plot(uhi,
     main = "Urban Heat Island Map: Phoenix, AZ",
     xlab = "Longitude (°)",     
     ylab = "Latitude (°)",      
     col = terrain.colors(20),
     zlim = c(-5, 10)  
)

#urban and rural boundaries
plot(urban_v, border="red", lwd=2, add=TRUE)
plot(rural_v, border="blue", lwd=2, add=TRUE)

hist(uhi,
     main = "Distribution of Urban Heat Island (Phoenix)",
     xlab = "Temperature Difference (°F, Urban - Rural)",
     ylab = "Number of Pixels",
     col = "orange",
     breaks = 20)  

