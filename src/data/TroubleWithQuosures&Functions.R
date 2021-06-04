# Workflow 

library(tidyverse)
library(letsR)

# Get a clean dataset to work with first
data <- ecdc_2019_accepted %>% as.data.frame() %>% filter(!is.na(longitude))

data_clean <- data %>% 
  mutate(pa = case_when(
    statusterm == "Absent" ~ "Absent", 
    statusterm == "Anticipated Absent" ~ "Absent", 
    statusterm == "Confirmed Absent" ~ "Absent", 
    statusterm == "Introduced" ~ "Present", 
    statusterm == "Established" ~ "Present",
    statusterm == "Present" ~ "Present",
    TRUE ~ "Other"
    )) 

# Presence Data
presence <- data_clean %>% 
  filter(pa == "Present")

# Absence Data
absence <- data_clean %>% 
  filter(pa == "Absent")

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

get_xy(presence, longitude, latitude)

get_species <- function(dataframe, species) {
  species <- pull(dataframe, {{species}})
  return(species)
}

get_species(presence, VectorSpecies)


points2presab <- function(dataframe, lon, lat, species, ...) {
  

  xy = get_xy(dataframe, {{lon}}, {{lat}})
  
  species_vector = get_species(dataframe, {{species}})
  
  return(lets.presab.points(xy, species_vector))

  }

# test <- points2presab(presence, "longitude", "latitude", "VectorSpecies")


 

# 3. Fill the matrix with true absences from the original points data
# This will involve turning all none-true absences to NA
# Then refilling those with a 0 for a TRUE absence

# 4. Then run the JSDM
