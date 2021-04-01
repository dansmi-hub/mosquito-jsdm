library(sf)
library(raster)
library(rworldmap)
library(rworldxtra)

mosquito_points
wc2.5 <- getData(name = "worldclim", res = 2.5, var = "bio")

bio1 <- raster::subset(wc2.5, 1)

plot(bio1)
plot(mosquito_points, add = T)

masked <- mask(bio1, mosquito_points)
plot(masked)

world <- getMap(resolution = "high")

crs(world)
crs(bio1)
crs(mosquito_points)


raster::intersect(mosquito_points, world)

inter <- sf::st_intersects(mosquito_points, world)
plot(inter)

names(world)

eu <- getData(name = "GADM", "GBR")

bio1

world <- st_as_sf(world)

europe <- filter(world, GLOCAF == "Europe")

# CLIP THIS WITH ST_CROP? CORINE uses EPSG:3035
paleartic <- world %>% filter(LAT >= 20 & LON >= -20 & LON <= 30)

palearctic_corine <- st_transform(paleartic, 3035)

st_crop(palearctic_corine, xmin = -20, xmax = 30, ymin = 20, ymax = 70)

ggplot() + 
  geom_sf(data = palearctic_corine)

grid <- sf::st_make_grid(world)

bbox <- sf::st_bbox(mosquito_points)

grid







