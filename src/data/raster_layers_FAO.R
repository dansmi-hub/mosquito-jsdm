library(raster)
library(sf)


# FAO Livestock Layers ----------------------------------------------------

cattle <- "/home/daniel/Datasets/LivbestockLayers_FAO/cattle_global_5arcmin/5_Ct_2010_Da.tif"
pig <- "/home/daniel/Datasets/LivbestockLayers_FAO/pig_global_5arcmin/5_Pg_2010_Da.tif"
chicken <- "/home/daniel/Datasets/LivbestockLayers_FAO/chicken_global_5arcmin/5_Ch_2010_Da.tif"
sheep <- "/home/daniel/Datasets/LivbestockLayers_FAO/sheep_global_5arcmin/5_Sh_2010_Da.tif"

livestock_paths <- c(cattle, pig, chicken, sheep)

livestock_geotiffs <- lapply(livestock_paths, raster)

crs(livestock_geotiffs)

raster::stack(livestock_geotiffs) %>% plot



CRS(read_sf("data/raw/VectorNetData/"))
