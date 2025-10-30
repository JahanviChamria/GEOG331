#install.packages("terra")
#install.packages("tidyterra")
#install.packages("FedData")

library(terra)
library(tidyterra)
library(FedData)

nlcd_meve16 <-get_nlcd(template = FedData::meve,
                       label = "meve",
                       year = 2016,
                       extraction.dir = "Z:/GEOG331_F25/jchamria/data/")
nlcd_meve16

cavm <- vect("Z:/GEOG331_F25/mloranty/data/cp_veg_la_shp/")
cavm

head(cavm)

terra::plot(cavm, y = "PHYSIOG")

