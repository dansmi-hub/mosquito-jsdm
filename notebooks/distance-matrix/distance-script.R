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



#######
raw = sample_n(raw, 200)

# Morans I ----------------------------------------------------------------

# Simple Morans using queens case weighting
# Queens Case Morans for the raster gridded data
M_2km <- Moran(raster_2km, w=matrix(c(1,1,1,1,0,1,1,1,1), 3,3))
M_5km <- Moran(raster_5km, w=matrix(c(1,1,1,1,0,1,1,1,1), 3,3))
M_10km <- Moran(raster_10km, w=matrix(c(1,1,1,1,0,1,1,1,1), 3,3))

# Spatial correlation 
# Increasing grid size increases spatial autocorrelation. 
# Though 0.25 doesn't seem that much?

# 2km: 0.09526147
# 5km: 0.1675565
# 10km: 0.259602

# For all the raw points 29k points ...
# Data set is very large so need a memory efficent way of calculating this
# remotes::install_github('mcooper/moranfast')

xy <- st_coordinates(raw)
bound <- cbind(raw, xy)
bound$id <- 1:nrow(bound)

# Queens matrix weighted?
M_all_points <- moranfast(bound$id, bound$X, bound$Y)

# 0.24 coefficient. So samples are positively correlated but not greatly so.
# Very similar to the 10km resolution. So we are increasing correlation slightly
# when grid to 10km. but reducing when species are sampled in grids at 2-5km.
M_all_points


# Distance Between All Points ---------------------------------------------

# Function to do the work for us...
# Calculates distance between all points for sf point objects.
# For raster objects it converts each cell toa  polygon and then measures 
# the distance between all polygon centres that have a value above 1 (i.e.
# they have species present in them rather than blank cells)

spatial_dist <- function(x) {
  
  # if classs is sf
  if (inherits(x, "sf")) {
    distmat = st_distance(x)
    vec_dist = as.vector(t(distmat)[lower.tri(t(distmat))])
    return(vec_dist)
  }
  # Raster option
  if (inherits(x, "RasterLayer")) {
    poly = st_as_sf(rasterToPolygons(x))
    grid_dist = as.vector(st_distance(filter(poly, layer != 0)))
    return(grid_dist)
    }
  else {
    errorCondition("Unknown Class Type. Must be SF object or Raster Object...")
  }
}

list_distances = list(
  raw,
  raster_10km,
  raster_5km,
  raster_2km
  )
  

distnaces_m = parallel::mclapply(list_distances, spatial_dist, mc.cores = 4)
distnaces_m = lapply(list_distances, spatial_dist)


distance_names = c(
  "Raw",
  "Raster 10km",
  "Raster 5km", 
  "Raster 2km"
)

# Create data frame from these listed vectors ready for plotting in ggplot
# need to do some fancy dplyr and purr to keep it efficent (50 mill vals)

distances_df = 
  map(distnaces_m, as_tibble) %>% 
  map2_df(distance_names, ~ mutate(.x, Type = .y)) %>%
  mutate(Dist = as.integer(name)) %>%
  bind_rows()


## Plot these
p_distances_box = 
  distances_df %>% 
  ggplot(aes(x = Dist, group = Type, fill = Type), alpha = 0.3) +
  geom_boxplot(alpha = 0.3)

p_distances_density = 
  distances_df %>% 
  ggplot(aes(x = Dist, group = Type, fill = Type), alpha = 0.3) +
  geom_density(alpha = 0.3)

p_distances_ridge = 
  distances_df %>% 
  ggplot(aes(x = Dist, y = Type, group = Type, fill = Type), alpha = 0.5) +
  ggridges::geom_density_ridges(alpha = 0.5)


# Nearest Neighbour Analysis ----------------------------------------------

## Convert to ppp object for spatstat

ppp_nn = function(x, k) {
  
  if (inherits(x, "RasterLayer")) {
    # poly = st_as_sf(rasterToPolygons(x))
    # ppp_centroids = as.ppp(st_centroid(filter(poly, layer != 0)))
    # nn = nndist(ppp_centroids, k = k)
    # return(nn)
    poly = st_as_sf(rasterToPoints(x, fun = function(x){x!=0}, spatial = TRUE))
    ppp_centroids = as.ppp(poly)
    nn = nndist(ppp_centroids, k = 1)
    return(nn)
  }
  
  if (inherits(x, "sf")) {
    ppp_points = as.ppp(x)
    nn = nndist(ppp_points, k = k)
    return(nn)
  }
  
  else {
    errorCondition("Unkown object class. Must be 'sf' or 'RasterLayer'...")
  }
}


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


