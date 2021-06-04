# Libraries ---------------------------------------------------------------

library(tidyverse)
library(raster)
library(sf)
library(rworldmap)
library(inlmisc)
library(cowplot)
library(janitor)
library(readxl)
library(CoordinateCleaner)



# Initial Data Import -----------------------------------------------------

imutable <- read_xlsx("data/raw/VectorNetData/ref.dVectorDistributionchk.xlsx", 
                      sheet = 5)

glimpse(imutable)

mutable <- imutable %>% filter(!is.na(Latitude), !is.na(Longitude))

# Distribution Status Conversion ------------------------------------------

distribution_codes <- read_xlsx("data/interim/DistributionStatusCodesConversion.xlsx")

# Mutate the columns in mutable dependent on matching codes between dataframes

mutable <- inner_join(mutable, distribution_codes, by = c("AssessedDistributionStatus" = "VectorDistributionStatusCode"))

# CoordinateCleaner -------------------------------------------------------

mutable <- clean_coordinates(mutable, lon = "Longitude", lat = "Latitude", species = "SpeciesName", value = "clean")

# Create spatial dataframes file with all data points 
# with a cut off of 30 absences and 100 presences

mutable %>% 
  count(SpeciesName, DistributionStatus) %>% 
  pivot_wider(names_from = DistributionStatus, values_from = n) %>% 
  filter(Present >= 100)

all_points <- mutable %>% 
  filter() %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), 
           # Standard Proj
           crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

write_sf(all_points, "data/interim/all_points.nc")

# WNV Vectors -------------------------------------------------------------

wnv_vectors <- read_lines("data/interim/WNV_Vectors.txt")

# RVF Vectors -------------------------------------------------------------

rvf_vectors <- read_lines("data/interim/RVF_Vectors.txt")

# Count Species -----------------------------------------------------------

cbind(wnv_vectors, (wnv_vectors %in% mutable$SpeciesName))

cbind(rvf_vectors, (rvf_vectors %in% mutable$SpeciesName))

# Summarise Species Counts  -----------------------------------------------

## WNV 
available_wnv_vectors <- cbind(wnv_vectors, (wnv_vectors %in% mutable$SpeciesName)) %>% 
  as_tibble() %>% 
  filter(V2 == "TRUE") %>% 
  pull(wnv_vectors)

mutable %>% 
  filter(SpeciesName %in% available_wnv_vectors) %>% 
  count(SpeciesName, DistributionStatus) %>% 
  pivot_wider(names_from = DistributionStatus, values_from = n) %>% 
  write.table("reports/exports/wnv_vectors_count.txt", row.names = F, quote = F)

## RVF 
available_rvf_vectors <- cbind(rvf_vectors, (rvf_vectors %in% mutable$SpeciesName)) %>% 
  as_tibble() %>% 
  filter(V2 == "TRUE") %>% 
  pull(rvf_vectors)

mutable %>% 
  filter(SpeciesName %in% available_rvf_vectors) %>% 
  count(SpeciesName, DistributionStatus) %>% 
  pivot_wider(names_from = DistributionStatus, values_from = n) %>% 
  write.table("reports/exports/rvf_vectors_count.txt", row.names = F, quote = F)


# Quick and Dirty Plot ----------------------------------------------------


# Create spatialDataframes
rvf_points <- mutable %>% filter(SpeciesName %in% available_rvf_vectors) %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), 
           # Standard Proj
           crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

wnv_points <- mutable %>% filter(SpeciesName %in% available_wnv_vectors) %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), 
           # Standard Proj
           crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")


## Save as shapefiles - NetCDF is jsut one file (less clutter)
rvf_points %>% st_write("data/interim/rvf_vector_points.nc")
wnv_points %>% st_write("data/interim/wnv_vector_points.nc")

## All data

library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")

mutable %>% as_tibble() %>% 
  ggplot2::ggplot(data = ., aes(x = Longitude, y = Latitude)) +
  geom_sf(data = world, inherit.aes = F, fill = "antiquewhite") +
  ggpointdensity::geom_pointdensity(size = .7) +
  # geom_hex() +
  xlim(c(min(mutable$Longitude) + 5, max(mutable$Longitude) + 5)) +
  ylim(c(min(mutable$Latitude) + 5, max(mutable$Latitude) + 5)) + 
  theme(panel.background = element_rect(fill = "aliceblue")) +
  labs(colour = "Density", title = "VectorNet Points Density")

ggsave("reports/exports/AllVectorNetPoints.png", dpi = 300, height = 6, width = 8)

## RVF Vectors
ggplot2::ggplot() +
  geom_sf(data = world, inherit.aes = F, fill = "antiquewhite") +
  geom_sf(data = rvf_points, aes(col = SpeciesName), size = .3) +
  xlim(c(min(mutable$Longitude) + 5, max(mutable$Longitude) + 5)) +
  ylim(c(min(mutable$Latitude) + 5, max(mutable$Latitude) + 5)) + 
  theme(panel.background = element_rect(fill = "aliceblue")) +
  labs(colour = "Species", title = "RVF Vector Points Density")

ggsave("reports/exports/RVFVectorNetPoints.png", dpi = 300, height = 6, width = 8)

## WNV Vectors
ggplot2::ggplot() +
  geom_sf(data = world, inherit.aes = F, fill = "antiquewhite") +
  geom_sf(data = wnv_points, aes(col = SpeciesName), size = .3) +
  xlim(c(min(mutable$Longitude) + 5, max(mutable$Longitude) + 5)) +
  ylim(c(min(mutable$Latitude) + 5, max(mutable$Latitude) + 5)) + 
  theme(panel.background = element_rect(fill = "aliceblue")) +
  labs(colour = "Species", title = "WNV Vector Points Density")

ggsave("reports/exports/WNV_VectorNetPoints.png", dpi = 300, height = 6, width = 8)














