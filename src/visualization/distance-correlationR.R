library(tidyverse)
library(raster)
library(spdep)
library(stars)
library(spatstat)
library(sf)


# RasterToPoint ----------------------------------------------------------

raster_2km <- raster("data/interim/presenceraster_2km_EPSG3035.nc")
raster_5km <- raster("data/interim/presenceraster_5km_EPSG3035.nc")
raster_10km <- raster("data/interim/presenceraster_10km_EPSG3035.nc")

points_2km <- rasterToPoints(raster_2km)
points_5km <- rasterToPoints(raster_5km)
points_10km <- rasterToPoints(raster_10km)

# Queens Case Morans
M_2km <- Moran(raster_2km, w=matrix(c(1,1,1,1,0,1,1,1,1), 3,3))
M_5km <- Moran(raster_5km, w=matrix(c(1,1,1,1,0,1,1,1,1), 3,3))
M_10km <- Moran(raster_10km, w=matrix(c(1,1,1,1,0,1,1,1,1), 3,3))

nndist(as.ppp(points_2km), k=1) %>% hist()

stars_nc <- read_stars("data/interim/presenceraster_10km_EPSG3035.nc")

stars_raster <- st_as_sf(stars_nc)
st_distance(stars_raster)

xyFromCell(raster_10km, 213123)



points_10km <- st_as_sf(as.data.frame(points_10km), coords = c(1,2))
nndist(as.ppp(points_10km))



raw <- read_sf("data/interim/all_points.nc")
raw <- st_transform(raw, 3035)

st_geometry(raw)

# Over 100 Presences per species only and Only Presences
raw <- raw %>% 
  # filter(DistributionStatus == "Present") %>% 
  group_by(SpeciesName) %>% filter(n() >= 100) %>% 
  ungroup()


# Sampled dataset (much smaller) - 1000 points?
data <- sample_n(raw, 1000)

# Distance matrix
spatial_dist <- function(x) {
  
  distmat <- st_distance(x)
  
  vec_dist <- as.vector(t(distmat)[lower.tri(t(distmat))])
  
  # hist(vec_dist)
  
  p = 
    ggplot() + 
    geom_histogram(aes(vec_dist), binwidth = 10) +
    geom_vline(xintercept = 2, col = "red") +
    geom_vline(xintercept = 5, col = "orange") +
    geom_vline(xintercept = 10, col = "pink") +
    ggtitle(
      
      ifelse(
      
      length(unique(x$SpeciesName)) != 1,
      
      "All Species",
      
      paste(x$SpeciesName))
    )
    
    
  
  # qplot(vec_dist, geom = "auto")
  
  return(p)
  
  }

spatial_dist(data)

data_split <- group_split(data, SpeciesName)

plots_split <- lapply(data_split, spatial_dist)

lapply(plots_split, plot)

ggplot() + geom_histogram(aes())


