# Padding the data


# Libraries ---------------------------------------------------------------

library(sf)
library(tidyverse)
library(raster)
library(letsR)

# Read Points -------------------------------------------------------------

mosquito_raw <- read_sf("data/interim/all_points.nc")

# As linear projection
mosquito_km <- st_transform(mosquito_raw, crs = 3035)


# Presence Data
presence <- mosquito_km %>% 
  filter(DistributionStatus == "Present")

# Absent Data
absent <- mosquito_km %>% 
  filter(DistributionStatus == "Absent")

# 1. Given a set of presence and absences for a mosquito community

# 2. Turn the presences of that dataset into a matrix of presences
# Ideally have a function that generates a bunch of resolutions for this.

# Function to make this all work from an orginal dataset

get_xy <- function(dataframe, lon, lat) {
  # get cols
  xy <- dplyr::select(dataframe, {{lon}}, {{lat}})
  # turn into a matrix or data matrix
  xy <- data.matrix(xy)
  # return
  return(xy)
}

points2presab <- function(dataframe, lon, lat, species, ...) {
  # Matrix
  xy = get_xy(dataframe, {{lon}}, {{lat}})
  # Species Names
  species_vector = get_species(dataframe, {{species}})
  # Give me a presabs object back
  return(lets.presab.points(xy, species_vector))
}

# Use Functions -----------------------------------------------------------

# Geometry to lon lat columns
separated_coord <- presence %>%
  mutate(lat = unlist(purrr::map(presence$geometry, 1)),
         long = unlist(purrr::map(presence$geometry, 2)))


# presence_matrix <-
#   lets.presab.points(data.matrix(cbind(
#     separated_coord$long,
#     separated_coord$lat
#   )),
#   presence$SpeciesName)
# 
# 
# presence_matrix <-
#   lets.presab.points(
#     data.matrix(cbind(
#       separated_coord$long,
#       separated_coord$lat
#     )),
#     presence$SpeciesName,
#     resol = 10000,
#     show.matrix = FALSE,
#     count = TRUE
#   )

## bbox

worldmap <- st_read("data/interim/worldmap_3035.nc")
worldmap_laea <- st_transform(worldmap, crs = crs(mosquito_km))

## WGS84 bounds
# -16.1 32.88
# 40.18 84.17

## Projected bounds
# 1896628.62 1507846.05
# 4662111.45 6829874.45

xy <- data.matrix(cbind(separated_coord$long,
                        separated_coord$lat))

spp_name <- presence$SpeciesName

presence_obj <- lets.presab.points(xy, spp_name, resol = 50000,
                   xmn = 476139.3, xmx = 5982555,
                   ymn = 1600838, ymx = 7572415,
                   crs = crs(mosquito_km))

presence_obj$Richness_Raster$layer %>% plot()

plot(worldmap_laea, add=T)
                  



                  
extent(mosquito_km) + 10000
                  
                  
                  
                  
                  
                  
                  
                  