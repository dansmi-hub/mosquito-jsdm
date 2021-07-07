# A more basic script to bugcheck?


library(tidyverse)
library(sf)
library(raster)
library(spatstat)
library(moranfast)
library(ggridges)


# Loading Data ------------------------------------------------------------

## Rasters

# 2km, 5km, 10km
raster_2km <- raster("data/interim/presenceraster_2km_EPSG3035.nc")
raster_5km <- raster("data/interim/presenceraster_5km_EPSG3035.nc")
raster_10km <- raster("data/interim/presenceraster_10km_EPSG3035.nc")

# raster_2km <- raster(ncol = 300, nrow = 300)
# raster_2km = setValues(raster_2km, runif(ncell(raster_2km)))
# crs(raster_2km) = 3035
# 
# raster_5km <- raster(ncol = 200, nrow = 200)
# raster_5km = setValues(raster_5km, runif(ncell(raster_5km)))
# crs(raster_5km) = 3035
# 
# raster_10km <- raster(ncol = 100, nrow = 100)
# raster_10km = raster::setValues(raster_10km, runif(ncell(raster_10km)))
# crs(raster_10km) = 3035


## Points

# Raw data
raw <- read_sf("data/interim/all_points.nc")
st_crs(raw) = 3035
raw <- st_transform(raw, 3035)

# Over 100 Presences per species only and Only Presences
raw <- raw %>% 
  filter(DistributionStatus == "Present") %>% 
  group_by(SpeciesName) %>% filter(n() >= 100) %>% 
  ungroup()


distance_names = c(
  "Raw",
  "Raster 10km",
  "Raster 5km", 
  "Raster 2km"
)


##
nn = list()

ppp_points = as.ppp(raw)
nn[[1]] = nndist(ppp_points, k = 1)

poly = st_as_sf(rasterToPoints(raster_10km, fun = function(x){x!=0}, spatial = TRUE))
ppp_centroids = as.ppp(poly)
nn[[2]] = nndist(ppp_centroids, k = 1)

poly = st_as_sf(rasterToPoints(raster_5km, fun = function(x){x!=0}, spatial = TRUE))
ppp_centroids = as.ppp(poly)
nn[[3]] = nndist(ppp_centroids, k = 1)

poly = st_as_sf(rasterToPoints(raster_2km, fun = function(x){x!=0}, spatial = TRUE))
ppp_centroids = as.ppp(poly)
nn[[4]] = nndist(ppp_centroids, k = 1)

# Apply over all our layers 
# nn_list = lapply(list_distances, ppp_nn, k = 1)

nn_list = nn

# Create DF
nn_df = 
  map(nn_list, as_tibble) %>% 
  map2_df(distance_names, ~ mutate(.x, Type = .y)) %>% 
  rename(KNN = value) %>% 
  bind_rows()


## Plot these
p_nn_box = 
  nn_df %>% 
  ggplot(aes(x = KNN, group = Type, fill = Type), alpha = 0.3) +
  geom_boxplot(alpha = 0.3)

p_nn_density = 
  nn_df %>% 
  ggplot(aes(x = KNN, group = Type, fill = Type), alpha = 0.3) +
  geom_density(alpha = 0.3)

p_nn_ridge = 
  nn_df %>% 
  ggplot(aes(x = KNN, y = Type, group = Type, fill = Type), alpha = 0.5) +
  ggridges::geom_density_ridges(alpha = 0.5)


# Save All
save.image(compress = "xz", file = "notebooks/distance-matrix/distance-matrix.RData")
