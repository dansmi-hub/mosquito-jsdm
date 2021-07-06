library(tidyverse)
library(sf)
library(raster)
library(spatstat)
# 2km, 5km, 10km
raster_2km <- raster("data/interim/presenceraster_2km_EPSG3035.nc")
raster_5km <- raster("data/interim/presenceraster_5km_EPSG3035.nc")
raster_10km <- raster("data/interim/presenceraster_10km_EPSG3035.nc")

# Simple Morans using queens case weighting
# Queens Case Morans
M_2km <- Moran(raster_2km, w=matrix(c(1,1,1,1,0,1,1,1,1), 3,3))
M_5km <- Moran(raster_5km, w=matrix(c(1,1,1,1,0,1,1,1,1), 3,3))
M_10km <- Moran(raster_10km, w=matrix(c(1,1,1,1,0,1,1,1,1), 3,3))

# Spatial correlation 
# Increasing grid size increases spatial autocorrelation. 
# Though 0.25 doesn't seem that much?

# 2km: 0.09526147
# 5km: 0.1675565
# 10km: 0.259602
## Points

# Raw data
raw <- read_sf("data/interim/all_points.nc")
raw <- st_transform(raw, 3035)

# Over 100 Presences per species only and Only Presences
raw <- raw %>% 
  filter(DistributionStatus == "Present") %>% 
  group_by(SpeciesName) %>% filter(n() >= 100) %>% 
  ungroup()
# Morans I for the points (All species)

# Data set is very large so need a memory efficent way of calculating this
# install_github('mcooper/moranfast')

library(moranfast)
xy <- st_coordinates(raw)
bound <- cbind(raw, xy)
bound$id <- 1:nrow(bound)

# Queens matrix weighted?
M_all_points <- moranfast(bound$id, bound$X, bound$Y)

# 0.24 coefficient. So samples are positively correlated but not greatly so.
# Very similar to the 10km resolution. So we are increasing correlation slightly
# when grid to 10km. but reducing when species are sampled in grids at 2-5km.
M_all_points
# Distance to all other points for all species

# This can be very-very memory intensive so we have to make 
# do with nearest neighbor analysis


# Function to measure spatial distances between all points and plot
# Will it run?

spatial_dist <- function(x) {
  
  distmat <- st_distance(x)
  
  vec_dist <- as.vector(t(distmat)[lower.tri(t(distmat))])
  
  # hist(vec_dist)
  
  p = 
    ggplot() + 
    geom_histogram(aes(vec_dist)) +
    geom_vline(xintercept = 2000, col = "red") +
    geom_vline(xintercept = 5000, col = "orange") +
    geom_vline(xintercept = 100000, col = "pink") +
    ggtitle(
      
      ifelse(
        
        length(unique(x$SpeciesName)) != 1,
        
        "All Species",
        
        paste(x$SpeciesName))
    )
  
  
  
  # qplot(vec_dist, geom = "auto")
  
  return(p)
  
}


raw_dist <- spatial_dist(raw)