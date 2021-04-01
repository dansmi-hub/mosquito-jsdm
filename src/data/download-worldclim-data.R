# Downlaod BioClimdata

library(raster)

if (!dir.exists("data/external/worldclim")) {
  dir.create("data/external/worldclim")
}

getData("worldclim", var = "bio", res = "2.5", path = "data/external/worldclim")
getData("worldclim", var = "bio", res = "5", path = "data/external/worldclim")
getData("worldclim", var = "bio", res = "10", path = "data/external/worldclim")


















