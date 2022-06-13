library(raster)
library(rgdal)


Btown <- raster("Data/BloomingtonIN.tif")
Btown
#r = raster(matrix(1:12,3,4))
#rasterToPoints(r)

plot(Btown)
