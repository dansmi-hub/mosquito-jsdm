library(tidyverse)
library(sf)

###########################################################################
# get all layers from ECDC data 2019
###########################################################################

layers <- st_layers("data/raw/exportedreportdata-mosquitoesnonrvf2019-09-23_xlsx.gdb/")$name

sf::st_read("data/raw/exportedreportdata-mosquitoesnonrvf2019-09-23_xlsx.gdb/")

ecdc_2019 <- lapply(layers, function(x) {
  sf::st_read("data/raw/exportedreportdata-mosquitoesnonrvf2019-09-23_xlsx.gdb/", layer = as.character(x))
})

# species recorded
unique(unlist(lapply(ecdc_2019, function(x) unique(x$VectorSpecies))))

# can't bind rows because of conflicting classes of columns - fix:
ecdc_2019_test <- lapply(ecdc_2019, function(x) mutate_at(x, .vars = vars(curationdate), as.character))

ecdc_2019_test <- lapply(ecdc_2019_test, function(x) mutate_at(x, 
.vars = vars(studysamplingsize, curatorid, number, Identifications, numbpersamp, abundance, latitude, longitude), as.double))

# fixed
ecdc_2019_clean <- bind_rows(ecdc_2019_test)

# filter only accepted values

# only three possible values for `accepted`
unique(ecdc_2019_clean$accepted)

ecdc_2019_clean <- ecdc_2019_clean %>% filter(accepted == "Y")

# what species in this dataset?
spp_2019 <- ecdc_2019_clean$VectorSpecies %>% unique()

###########################################################################
# get all layers from Willy and Francis' RVF work -------------------------
###########################################################################

layers <- st_layers("data/raw/rvfselecetdmodeltrainingdatafeb20_xlsx.gdb/")$name

sf::st_read("data/raw/rvfselecetdmodeltrainingdatafeb20_xlsx.gdb//")

rvf_2020 <- lapply(layers, function(x) {
  sf::st_read("data/raw/rvfselecetdmodeltrainingdatafeb20_xlsx.gdb/", layer = as.character(x))
})

rvf_2020 <- flatten_df(rvf_2020)

# what species in this dataset?
spp_2020 <- rvf_2020$species %>% unique()

# change names from abbreviated values
rvf_2020 <- rvf_2020 %>% mutate(
  species = case_when(
    species == "Albo" ~ "Aedes albopictus",
    species == "Casp" ~ "Aedes caspius",
    species == "Jap" ~ "Aedes japonicus",
    species == "Pip" ~ "Culex pipiens",
    species == "The" ~ "Culex theileri",
    species == "Vex" ~ "Aedes vexans",
  ))


###########################################################################
# get all the points from the gbif search for mosquitoes in Europe --------
###########################################################################

gbif_2021 <- 
  data.table::fread("data/raw/gbif_occ.csv")
  # read_csv("data/gbif_occ.csv")
  # read.csv("data/gbif_occ.csv")

# what sepcies are there? Some are blank
gbif_2021$species %>% unique()

# some of these are fossil specimines too - remove, and only species taxanomic ranks
gbif_2021_clean <- gbif_2021 %>% filter(basisOfRecord != "FOSSIL_SPECIMEN" & taxonRank == "SPECIES")

###########################################################################
# Test making a huge dataframe with all points included for all spp -------
###########################################################################

mosquito_points <- data.frame(
  species = c(ecdc_2019_clean$VectorSpecies, gbif_2021_clean$species, rvf_2020$species),
  lon = c(ecdc_2019_clean$longitude, gbif_2021_clean$decimalLongitude, rvf_2020$lonX),
  lat = c(ecdc_2019_clean$latitude, gbif_2021_clean$decimalLatitude, rvf_2020$latY)
) %>% na.omit()

# This is how many records for each species with this rough clean of data
mosquito_points %>% count(species)

# Remove any syntax relating to complexes etc
mosquito_points <- mosquito_points %>% mutate(species = str_remove(species, " s.s."))

# We would like to get more than 50 points for each species for it to be worth including
spp_100 <- mosquito_points %>% group_by(species) %>% count() %>% filter(n > 100) %>% pull(species)

# Filter based on these species worth modelling
mosquito_100 <- mosquito_points %>% filter(species %in% spp_100)

###########################################################################
# rough plot --------------------------------------------------------------
###########################################################################

library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")

mosquito_100 %>% as_tibble() %>% 
  ggplot2::ggplot(data = ., aes(x = lon, y = lat)) +
  geom_sf(data = world, inherit.aes = F) +
  geom_hex() +
  xlim(c(min(mosquito_100$lon) + 5, max(mosquito_100$lon) + 5)) +
  ylim(c(min(mosquito_100$lat) + 5, max(mosquito_100$lat) + 5)) +
  facet_wrap(~ species)


mosquito_points <- st_as_sf(mosquito_100, coords = c("lon", "lat"), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

# Make sure CRS is actually correct: WGS1984 = EPSG:4326
st_crs(mosquito_points) = 4326

mosquito_points

sf::write_sf(mosquito_points, "data/interim/mosquito_points.shp")



