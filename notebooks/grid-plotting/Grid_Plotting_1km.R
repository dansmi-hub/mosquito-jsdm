## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())


## ----Libraries----------------------------------------------------------------
library(sf) # Also required to install ncdf4, gdal etc
library(raster)
library(tidyverse)
library(cowplot)

# This is the package with the community from points functions
library(letsR)


## ----Mosquito Points----------------------------------------------------------
# Read in the mosquito points data
mosquito_raw <- read_sf("data/interim/all_points.nc")

# Projection is :
st_crs(mosquito_raw) <- 4326

# Change to Lazmuth Equal Area European
# EPSG 3035

mosquito_km <- st_transform(mosquito_raw, crs = 3035)

# Check new projection:
st_crs(mosquito_km)


## ----Presence Only------------------------------------------------------------
# Simplify the padding - take only the species that are present for now
present <- mosquito_km %>% filter(DistributionStatus == "Present")



## ----Worldmap-----------------------------------------------------------------
## Worldmap for overlay plotting (sanity checking)

worldmap <- st_read("data/interim/worldmap_3035.nc")
# Worldmap_3035 is already transformed spatial vector object
# into EPSG:3035 LAEA for Europe. but retransform to double check

worldmap <- st_transform(worldmap, crs = 3035)


## ----Grid Plotting------------------------------------------------------------
# We need an XY matrix - each row is one species presence at a location
xy <- present %>% 
  # Split the sf_objects geometry into lat and lon
    mutate(lat = unlist(purrr::map(present$geometry, 1)),
         lon = unlist(purrr::map(present$geometry, 2))) %>% 
  # Turn into df to get rid of geometry and sf attributes
  as.data.frame() %>% 
  # Make datamatrix from lat and lon
  dplyr::select(lat, lon) %>% data.matrix()


# A vector of species names
spp_names <- present$SpeciesName



## -----------------------------------------------------------------------------
# the function from letsR that does this:
presence_raster <- lets.presab.points(xy, spp_names,
  # 50 Km resolution - a 1km resolution requires 60GB of RAM
  # Will offload to cluster when this is needed 
  resol = 50000,
  xmn = 476139.3, xmx = 5982555,
  ymn = 1600838, ymx = 7572415,
  crs = crs(mosquito_km)
)

## -----------------------------------------------------------------------------
# The presence absence object from letsR:
plot(presence_raster)

## -----------------------------------------------------------------------------
# Now plot many different resolutions of these rasters

## WARNING ## Available RAM may mean you can't run this section of the 
# notebook. 2km resolution requires 26GB of RAM

# Resolutions in km
resolutions <- c(50, 25, 15, 10, 5, 2, 1)

# Store in list
presence_raster = list()

for (i in seq_along(resolutions)) {
  
  # the function from letsR that does this:
  presence_raster[[i]] <- lets.presab.points(xy, spp_names,
                                        # 50 Km resolution - a 1km resolution requires 60GB of RAM
                                        # Will offload to cluster when this is needed 
                                        resol = resolutions[i] * 1000,
                                        # The extent of our point data
                                        xmn = 476139.3, xmx = 5982555,
                                        ymn = 1600838, ymx = 7572415,
                                        crs = crs(mosquito_km)
  )
  
  names(presence_raster[i]) = sprintf("presenceraster_%skm", resolutions[i])
  
  # Write to a file as well - named 
  writeRaster(presence_raster[[i]]$Richness_Raster, overwrite = TRUE,
              filename = sprintf("data/interim/presenceraster_%skm_EPSG3035.nc", resolutions[i]))
  
}


## -----------------------------------------------------------------------------
# Now plot both the raster layers and the world vector layer on top
# Both in EPSG:3035 LEA

# store plots here
plotlist <- list()
 
for (i in seq_along(presence_raster)) {
  # Raster as DF for ggplot2
  raster_df <- as.data.frame(presence_raster[[i]]$Richness_Raster, xy = TRUE)

  raster_extent <- extent(presence_raster[[i]]$Richness_Raster)

  plotlist[[i]] <- ggplot() +
    geom_sf(data = worldmap) +
    theme(panel.background = element_rect(fill = "aliceblue")) +
    geom_raster(data = raster_df, aes(x = x, y = y, fill = layer)) +
    scale_fill_gradientn(colours = c("#ffffff00", "#440154FF", "#FDE725FF"), values = c(0, 0.1, 1)) +
    
    # coord_sf(
    #   xlim = c(raster_extent[1], raster_extent[2]),
    #   ylim = c(raster_extent[3], raster_extent[4])
    # ) +
    
    # Adjusted zoom view for better plotting compared to above
    coord_sf(
         xlim = c(476139.3 + 2000000, 5982555),
         ylim = c(1600838, 7572415 - 2000000)
    ) +
    xlab(NULL) +
    ylab(NULL) +
    labs(fill = "Species Density") +
    ggtitle(sprintf("%skm Grid Size", resolutions[i]))
}



## -----------------------------------------------------------------------------
# Render all plot list at a good resolution?

# Save as good PNG First

for (i in seq_along(plotlist)) {
  
  ggsave(
    filename = sprintf(sprintf("%skm_Grid_Size.png", resolutions[i])), 
    path = "notebooks/grid-plotting/", dpi = 300, plot = plotlist[[i]], 
    height = 6, width = 6
  )
}



## -----------------------------------------------------------------------------
# One large plot?

A3_plot <- cowplot::plot_grid(plotlist = plotlist)

ggsave(plot = A3_plot, 
       filename = "Large_Plot.png", path = "notebooks/grid-plotting/", 
       height = 11.7, width = 16.5, dpi = 600, device = "png")



## -----------------------------------------------------------------------------
# What are the average values of each raster layer? Will this tell us the best
# layer or grid size to use? Balanced with ecology as well?

# Mean values of all rasters
means <- lapply(seq_along(resolutions), function(x) raster::cellStats(presence_raster[[x]]$Richness_Raster, stat = "mean"))
names(means) <- paste0(resolutions, "km")

# get the frequency of raster values (species present)
freqs <- lapply(seq_along(resolutions), function(x) raster::freq(presence_raster[[x]]$Richness_Raster))
names(freqs) <- paste0(resolutions, "km")


# Plot these?
freq_scale <- lapply(freqs, as.data.frame) %>% 
  bind_rows(.id = "Scale") %>% 
  pivot_wider(names_from = Scale, values_from = count)


# Write to a nice CSV
freq_scale %>% write_csv("notebooks/grid-plotting/scale_grid_size.csv")



## -----------------------------------------------------------------------------
# Make a bar chart or table
long_freq_scale <- freq_scale %>% 
  # remove 0 values - kinda pointless
  filter(value != 0) %>% 
  pivot_longer(!value, names_to = "scale", values_to = "count")

long_freq_scale %>% 
  ggplot(aes(x = value, y = count, fill = value)) +
  geom_col(position = "dodge") +
  facet_wrap("scale") +
  xlab("Species Density")

