# Padding the data


# Libraries ---------------------------------------------------------------

library(sf)
library(tidyverse)
library(letsR)

# Read Points -------------------------------------------------------------

mosquito_raw <- read_sf("data/interim/all_points.nc") %>% as.data.frame()

# Presence Data
presence <- mosquito_raw %>% 
  filter(DistributionStatus == "Present")

# Absent Data
absent <- mosquito_raw %>% 
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
  mutate(lat = unlist(purrr::map(presence$geometry,1)),
         long = unlist(purrr::map(presence$geometry,2)))


presence_matrix <- 
  lets.presab.points(data.matrix(cbind(
  separated_coord$long,
  separated_coord$lat
)),
presence$SpeciesName)


presence_matrix <- 
  lets.presab.points(data.matrix(cbind(
    separated_coord$long,
    separated_coord$lat
  )),
  presence$SpeciesName, resol = 0.008, show.matrix = FALSE, count = TRUE)

## bbox
-40, 20, 75, 75



