## A reformatted version and functional approach to "rasterize_points.R"


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(raster)
library(sf)
library(rworldmap)
library(inlmisc)
library(cowplot)

# Functions ---------------------------------------------------------------

# Get a worldmap shape layer and save it to a directory ----
getworldmap <- function(res, dir) {
  # Get the map
  worldmap <- getMap(resolution = res)
  # As sf object
  worldmap <- st_as_sf(worldmap)
  # Change the CRS to the European 3035 projection
  worldmap <- st_transform(worldmap, 3035)
  # Write as netCDF, append = F overwrites .nc files
  st_write(worldmap, dir, append = FALSE)
}

# Get the Map
getworldmap(res = "low", dir = "data/interim/worldmap_3035.nc")

# Rasterize points file and get sampling effort for diff resolutions ----


points2raster <- function(points, crs, res, method) {
  
  # # test block
  # points = mosquito_points
  # crs = 3035
  # res = 10000 # 10km
  # method = "count"

  # Print
  print("Please make sure to use a CRS that is projected in metres (m)")
  # Get points in correct format and CRS
  points <- st_as_sf(points)
  points <- st_transform(points, crs = crs)
  # Add an attribute for counting the points occurence
  points$value <- 1
  
  # Generate a blank raster from those points
  blank_raster <- raster(extent(points), res = res)
  crs(blank_raster) <- 3035
  proj4string(blank_raster) <- "+init=epsg:3035"
  
  # Do X dependent on input value
  if (method == "count") {
    # Count the points per geo unit (km on works)
    raster <- rasterize(points, blank_raster, "value", fun = "count")
  } else if (method == "log") {
    raster <- rasterize(
      points,
      blank_raster,
      "value",
      fun = function(x, ...)
        log(length(x))
    )
  } else {
    stop("Method must be one of 'count' or 'log'")
  }
  print("The error is arising due to the transition to WKT projection definitions and the changes in GDAL and PROJ. Just ignore them.")
  return(raster)
}

# count_10km <- points2raster(mosquito_points, 3035, 10000, "log")
# plot(count_10km)


# Convert any raster file to a shapefile; easy plotting in ggplot ----
raster2shape <- function(raster) {
  shp <- Grid2Polygons(raster)
  shp <- st_as_sf(shp)
  # Do we need to do this? Is CRS inherited through this func?
  # shp <- st_transform(shp, crs = crs)
  return(shp)
}

# Works!
# raster2shape(count_10km) %>% plot()


# Methods -----------------------------------------------------------------

# load in mosquito points fiel
mosquito_points <- st_read("data/interim/mosquito_points.shp")

# Get log and count values for 20, 15, 10 and 5km density
# of mosquito recorder effort

# Resolutions in Km
resolutions <- c(5, 10, 25, 50)
# Convert m to km
resolutions <- resolutions * 1000

# loop over list of res
counts <- lapply(resolutions, function(x) {
  raster <- points2raster(mosquito_points, 3035, x, method = "count")
  shapes <- raster2shape(raster)
})

logs <- lapply(resolutions, function(x) {
  raster <- points2raster(mosquito_points, 3035, x, method = "log")
  shapes <- raster2shape(raster)
})


# Plots -------------------------------------------------------------------

# Load shapefile

worldmap <- st_read("data/interim/worldmap_3035.nc")

# Raw count plots
counts_plots <- list()

for (i in seq_along(resolutions)) {
  p = ggplot() +
  geom_sf(data = worldmap, fill = "antiquewhite") +
  geom_sf(data = counts[[i]], aes(col = z, fill = z)) +
  coord_sf(xlim = c(1652265, 7567265), ylim = c(523584, 4878584)) +
  theme_bw() +
  theme(panel.background = element_rect(fill = "aliceblue")) +
  scale_color_viridis_c() +
  scale_fill_viridis_c() +
  labs(colour = "Raw\nSamples", fill = "Raw\nSamples") +
  ggtitle(paste0("Resolution: ", resolutions[i]/1000, "Km"))

  counts_plots[[i]] = p
  }

# Save
plot_grid(plotlist = counts_plots, nrow = 2)
ggsave("reports/figures/RawSamplingEffort.png", width = 8.3, height = 11.7, dpi = 300)

# log plots
log_plots <- list()

for (i in seq_along(resolutions)) {
  p = ggplot() +
    geom_sf(data = worldmap, fill = "antiquewhite") +
    geom_sf(data = logs[[i]], aes(col = z, fill = z)) +
    coord_sf(xlim = c(1652265, 7567265), ylim = c(523584, 4878584)) +
    theme_bw() +
    theme(panel.background = element_rect(fill = "aliceblue")) +
    scale_color_viridis_c() +
    scale_fill_viridis_c() +
    labs(colour = "Log\nSamples", fill = "Log\nSamples") +
    ggtitle(paste0("Resolution: ", resolutions[i]/1000, "Km"))
  
  counts_plots[[i]] = p
}

# Save
plot_grid(plotlist = counts_plots, nrow = 2)
ggsave("reports/figures/LogSamplingEffort.png", width = 8.3, height = 11.7, dpi = 300)




















