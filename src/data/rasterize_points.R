# Points to Raster

library(raster)
library(tidyverse)
library(sf)

# the points
mosquito_points <- st_read("data/interim/mosquito_points.shp")

# A blank raster layer
blank_raster <- raster(extent(mosquito_points),
                       # 30 arc minutes
                       res = (1/60)*30)
                       # 2.5 arcminutes 
                       # res = (1/60)*2.5)

# Add a value to the mosquito points (just one)
mosquito_points$value <- 1

# A very brief count of points per geo unit...
rsum <- rasterize(mosquito_points, blank_raster, "value", fun = "count")
plot(rsum)

# Logged version of the values above...
rlog <- rasterize(mosquito_points, blank_raster, "value", fun=function(x,...)log(length(x)))
plot(rlog)

## get a world map
library(rworldmap)

worldmap <- getMap(resolution = "low")
worldmap <- st_as_sf(worldmap)

# for converting my raster to a shape file
library(inlmisc)

# log vlaue plotting
rlog_shape <- Grid2Polygons(rlog)
rlog_shape <- st_as_sf(rlog_shape)
st_crs(rlog_shape) <- st_crs(worldmap)


logeffort <- ggplot() + 
  geom_sf(data = worldmap, fill = "antiquewhite") + 
  geom_sf(data = rlog_shape, aes(col = z, fill = z)) +
  coord_sf(xlim = c(-20, 65), ylim = c(35, 73)) +
  theme_bw() +
  theme(panel.background = element_rect(fill = "aliceblue")) +
  scale_color_viridis_c() +
  scale_fill_viridis_c() +
  labs(colour = "Log\nSamples", fill = "Log\nSamples") +
  ggtitle("Logged VectorNET Sampling Effort")


# count plotting
rsum_shape <- Grid2Polygons(rsum)
rsum_shape <- st_as_sf(rsum_shape)
st_crs(rsum_shape) <- st_crs(worldmap)


raweffort <- ggplot() + 
  geom_sf(data = worldmap, fill = "antiquewhite") + 
  geom_sf(data = rsum_shape, aes(col = z, fill = z)) +
  coord_sf(xlim = c(-20, 65), ylim = c(35, 73)) +
  theme_bw() +
  theme(panel.background = element_rect(fill = "aliceblue")) +
  scale_color_viridis_c() +
  scale_fill_viridis_c() +
  labs(colour = "Raw\nSamples", fill = "Raw\nSamples") +
  ggtitle("Raw VectorNET Sampling Effort")

library(patchwork)

raweffort / logeffort

ggsave("reports/figures/SamplingEffort.pdf", width = 8.3, height = 11.7)
ggsave("reports/figures/SamplingEffort.png", width = 8.3, height = 11.7, dpi = 300)










