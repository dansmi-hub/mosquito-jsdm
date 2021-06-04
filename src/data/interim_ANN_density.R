### Point Distance Histograms
library(tidyverse)
library(spatstat)
library(sf)
library(raster)


# load in shape files
worldmap <- st_read("data/interim/worldmap_3035.nc")
worldmap <- as.owin(worldmap)
worldmap_km <- rescale(worldmap, 1000)

# Load in point files and format
mosquito_points <- st_read("data/interim/mosquito_points.shp")
mosquito_points <- st_as_sf(mosquito_points)
msq_pp <- mosquito_points
msq_pp <- st_transform(msq_pp, 3035)
mosquito_ppp <- as.ppp(msq_pp)
marks(mosquito_ppp) <- NULL
# Cahnge dist to Km
mosquito_km <- rescale(mosquito_ppp, 1000, "km")


# st_inter
plot(worldmap)



# Mean nearest neighbour in km 
mean(nndist(mosquito_km, k=1))
# Mean second nearest neighbor in km
mean(nndist(mosquito_km, k=2))


hist(log(nndist(mosquito_km, k =1)))

K1 <- density(mosquito_km) # Using the default bandwidth
plot(K1, main=NULL, las=1)
contour(K1, add=TRUE)


Q <- quadratcount(mosquito_ppp, nx= 20, ny=20)
plot(Q)


K <- Kest(mosquito_km)
plot(K, main=NULL, las=1, legendargs=list(cex=0.8, xpd=TRUE))


# Clustering - Pair Correlation by KM
# g > 1 = clustering
# g < 1 = dispersion
g  <- pcf(mosquito_km)

pdf(file = "reports/figures/pcf_mosquito_points.pdf")
plot(g, main=NULL, las=1, legendargs=list(cex=0.8, xpd=TRUE))
title(main = "sptstat:pcf() output for mosquito points",
      sub = "g(r) < 1 = clustering, g(r) > 1 = dispersion")
dev.off()


# Kernal density Raster ---------------------------------------------------

K3 <- density(mosquito_km) # Using a 50km bandwidth
plot(K3, main=NULL, las=1)
contour(K3, add=TRUE)

