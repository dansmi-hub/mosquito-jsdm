## Cleaning initial mosquito dataset

library(tidyverse)
library(CoordinateCleaner)
library(sf)

# Functions ---------------------------------------------------------------

# Read all multiple layers from ESRI file or something similar
read_multi_layers <- function(file) {
  # Get the names of all the layers in the file
  layers <- st_layers(file)$name
  # Read each individual layer
  lapply(layers, function(x){
    st_read(file, layer = as.character(x))
  })
}

# Cleaning 2019 Dataset ---------------------------------------------------

ecdc_2019 <- read_multi_layers("data/raw/exportedreportdata-mosquitoesnonrvf2019-09-23_xlsx.gdb/")

# Can't bind rows because of conflicting classes of columns - fix:

  # Characters for this
ecdc_2019 <- lapply(ecdc_2019, function(x) mutate_at(x, .vars = vars(curationdate), as.character))
  # Doubles for these vars
ecdc_2019 <- lapply(ecdc_2019, function(x) mutate_at(x, .vars = vars(studysamplingsize, curatorid, number, Identifications, numbpersamp, abundance, latitude, longitude), as.double))
  # Bind the rows together
ecdc_2019 <- bind_rows(ecdc_2019)

# Take only accepted values
ecdc_2019_accepted <- filter(ecdc_2019, accepted == "Y")

# RVF Dataset -------------------------------------------------------------

### Haitus on this for now until we get a large dataset file.
### for now just use the edc 2019 to test functions for turning
### points into 10km community grids



