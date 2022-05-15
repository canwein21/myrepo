library(raster)
library(sp)
library(rgdal)

WD <- "C:/Users/User/OneDrive/Desktop/Research Assistant/SMAP_L3_SM_P_20200501_R18290_002.tif"
setwd("C:/Users/User/OneDrive/Desktop/Research Assistant/BtownRaster")

Btown <- raster("C:/Users/User/OneDrive/Desktop/Research Assistant/SMAP_L3_SM_P_20200501_R18290_002.tif") 
Btown

plot(Btown, main = "Bloomington, Indiana area generally")

col = terrain.colors(3)
brk <- c(250, 350, 380, 500)

par(xpd = FALSE, mar = c(5.1, 4.1, 4.1, 4.5))
plot(Btown, col = col, breaks = brk, main = "General Bloomington, IN area", legend = FALSE)
par(xpd = TRUE)
legend(par()$usr[2], 4713700, legend = c("I don't know why this is blank"), fill = rev(col))

crs(Btown)

myBtown <- crs(Btown)
myBtown

minValue(Btown)
maxValue(Btown)

RGB_stack <- stack("C:/Users/User/OneDrive/Desktop/Research Assistant/SMAP_L3_SM_P_20200501_R18290_002.tif")

par(col.axis = "white", col.lab = "white", tck = 0)
plotRGB(RGB_stack, r = 1, g = 2, b = 3, axes = TRUE, main = "Still trying to figure out why this is blank")
RGB_stackNEW <- stack("C:/Users/User/OneDrive/Desktop/Research Assistant/SMAP_L3_SM_P_20200501_R18290_002.tif")
RGB_stackNEW


func <- function(x) {
  x[rowSums(x == 0) == 3, ] <- NA
  x}

newRGBImage <- calc(RGB_stack, func)

par(col.axis = "white", col.lab = "white", tck = 0)
plotRGB(newRGBImage, r = 1, g = 2, b = 3, axes = TRUE, main = "= NA")

par(col.axis = "black", col.lab = "black", tck = 0)
hist(DSM_HARV, main = "", xlab = "", ylab = "", col = "wheat")

ncell(Btown)

hist(Btown, maxpixels = ncell(Btown), main = "", xlab = "", ylab = "", col = "blue")

nlayers(Btown)

GDALinfo("C:/Users/User/OneDrive/Desktop/Research Assistant/SMAP_L3_SM_P_20200501_R18290_002.tif")
