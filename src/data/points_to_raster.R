# Spatial points to gridded raster values

library(sf); library(raster); library(plotKML)

rast <- raster()
extent(raster) <- extent(mosquito_points)

# covnert points to sp
points <- as(mosquito_points, "Spatial")

example_raster <- raster(crs = crs(points), 
                         vals = 0, resolution = c(10, 10))


rasterize(x = example_raster, y = points)


plot(example_raster)

rast <- vect2rast(points)



uk <- getData("GADM", country = "GBR", level = 1)

plot(uk)

grid <- makegrid(uk, cellsize = .5)

grid <- SpatialPoints(grid, proj4string = CRS(proj4string(uk)))

plot(uk)
plot(grid, pch = ".", add = T)

