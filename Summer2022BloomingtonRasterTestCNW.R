library(raster)
library(sp)
library(rgdal)
library(maptools)
library(rgeos) #Error in library(rgeos) : there is no package called ‘rgeos’ ?

#Okay I guess I messed up the first time I tried to comment. Woops
#also this is how you comment in stuff! It can be really helpful later on so you can make notes to yourself and to others

#I don't see your data at all so please upload your data files to the folder I created here called "Data"
#then you can call your files directly from github like this:

#Btown <- raster("Data/SMAP_L3_SM_P_20200501_R18290_002.tif") 

#This way, other people can run your code and you wont have to work from your local drive anymore
#this can be really helpful if youre working from multiple computers
#KESONDRA & MALLORY - Can you see everything being pushed and pulled and committed? The only files currently in "myrepo" are the "BloomingtonIN.tif" and related files.

WD <- "C:/Users/User/OneDrive/Desktop/Research Assistant/BtownRaster/240789883/BloomingtonIN.tif"
setwd("C:/Users/User/OneDrive/Desktop/Research Assistant/BtownRaster/240789883/")

Btown <- raster("Data/BloomingtonIN.tif") #calling it from the data folder
#Btown <- raster("C:/Users/User/OneDrive/Desktop/Research Assistant/BtownRaster/240789883/BloomingtonIN.tif") 
Btown

#Defining Min/Max values
Btown <- setMinMax(Btown)
Btown
cellStats(Btown, min)
#Need to figure out why this is giving me "In min(x, na.rm = na.rm) : no non-missing arguments to min; returning Inf". For max, it's giving me "-Inf."
#Looking at other people's code on Stack Overflow, there shouldn't be an error.
cellStats(Btown, max)
cellStats(Btown, range)

#Coordinate reference system
Btown@crs
Btown@extent
str(Btown)
#
BTown <- stack("C:/Users/User/OneDrive/Desktop/Research Assistant/BtownRaster/240789883/BloomingtonIN.tif")
BTown
names(BTown) <- paste0("L", seq(1:nlayers(BTown)))
class(BTown)
#Computing sum by year
NewLayer <- calc(brick(BTown), fun=sum)
plot(NewLayer, main = "New Layer", col = blue)
#Working on the extent of the raster
extent(Btown)
newextent <- extent(0,900,0,350)
class(newextent)
Btown <- crop(x = Btown, y = newextent)
plot(Btown, main = "Manually cropped raster\nBloomington, Indiana")
plot(newextent, col="blue3", lwd = 4, add = TRUE)
plot(Btown, main = "Manually Cropped Raster of Bloomington, IN", add = TRUE)

#Extracting raster pixel values using vector polygons
nlayers(Btown)
#Looks like I can't summarize the data as shown by NEON's tutorial because the "Btown" raster is 1 layer
values(Btown)
as.data.frame(Btown, xy = TRUE)
BloomingtonIN <- brick("C:/Users/User/OneDrive/Desktop/Research Assistant/BtownRaster/240789883/BloomingtonIN.tif")
BloomingtonIN
#I used the S4 method for signature 'Raster,matrix.' Not sure if this was the right one to use, and not sure why all my values per entry all ready "NA."
extract(Btown, newextent, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE, fun=NULL, na.rm=TRUE, layer, nl, df=FALSE, factors=FALSE)
#Still trying to figure out and set the CRS here
crs(Btown) <- "+proj=merc +zone=0"
crs(Btown)
dim(Btown)
Btown[303,886]
#Running the Btown[303,886] or any other number within the given dimensions of 350x900 gives me the following output: [1] NA. Is this because it's a single-layer raster?
#Trying out more of the raster commands here
raster(Btown)

#Still trying to figure out where in the raster the soil moisture values are and how to extract them, Using the tutorial Kesondra sent me the other day to see if that helps at all.
find_smap(id = "SPL3SMAP", dates = "2021-01-01", version = 4)
#R isn't able to find the function "find_smap" but I can find it in the Help section of R. Moving on if I can.
extract("Btown", extent(0,900,0,350), method = 'simple', buffer = NULL, small = FALSE, cellnumbers = FALSE, fun = NULL, na.rm = TRUE, nl = 1, df = FALSE, factors = FALSE)
#Getting the following errors: 1) "Error in h(simpleError(msg, call)) : error in evaluating the argument 'y' in selecting a method for function 'extract': insufficient number of elements (should be 4)"
#2) Error in (function (classes, fdef, mtable)) : unable to find an inherited method for function 'extract' for signature '"character", "Extent"'
extent(Btown(0,900,0,350))

#Creating SPDF
Btown <- SpatialPointsDataFrame(Btown[,4:3], proj4string = Btown@crs, Btown)
sp <- SpatialPoints(v[,1:2, drop = FALSE], proj4string = crs(Btown))

plot(Btown, main = "Bloomington, Indiana area generally")

col = terrain.colors(3)
brk <- c(250, 350, 380, 500)

par(xpd = FALSE, mar = c(5.1, 4.1, 4.1, 4.5))
plot(Btown, col = col, breaks = brk, main = "General Bloomington, IN area", legend = FALSE)
par(xpd = TRUE)
legend(par()$usr[2], 4713700, legend = c("I don't know why this is blank"), fill = rev(col))

crs(Btown)
Btown@crs

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
func

newRGBImage <- calc(RGB_stack, func)

par(col.axis = "white", col.lab = "white", tck = 0)
plotRGB(newRGBImage, r = 1, g = 2, b = 3, axes = TRUE, main = "= NA")

par(col.axis = "black", col.lab = "black", tck = 0)

ncell(Btown)
ncol(Btown)
nrow(Btown)
set.seed(123)
Bloomington <- raster(ncol=960, nrow=400)
Bloomington_list <- list()
for(i in 1:960){
  Bloomington_list[[i]] <- setValues(Bloomington, Bloomingtonnorm(ncell(Btown), mean = 100, sd = 50))
}

#Having driver issues somewhere around here. Trying to figure that out.

hist(Btown, maxpixels = ncell(Btown), main = "Bloomington, IN distribution of values", col = "blue")

nlayers(Btown)

GDALinfo("C:/Users/User/OneDrive/Desktop/Research Assistant/BtownRaster/240789883/BloomingtonIN.tif")
GDALinfo
