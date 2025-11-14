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
cities_metric <- st_transform(cities_modis, 3857)  #converting to metric CRS (EPSG:3857) for buffers (in km)
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

