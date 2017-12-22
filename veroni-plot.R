# install.packages("deldir", dependencies=TRUE)
# install.packages(c("sp", "maps", "maptools", "mapproj"), dependencies=TRUE)

library(deldir)
library(maps)
library(mapdata)

setwd("/Users/Evan/Dropbox/Code/chickens")

# import all the data
data <- read.csv("Chicken_Samples_Coordinates_OL_EIP.csv", header=T, strip.white=TRUE, stringsAsFactors=FALSE)

# extract the necessary info, and drop samples with any missing data
pts <- na.omit(data[c('Longtitude', 'Latitude', 'BP')])

# calculate tessellation and triangulation
vtess <- deldir(pts$Longtitude, pts$Latitude)

# convert to tile list
tiles <- tile.list(vtess)

par(mar=c(0,0,0,0))

# setup a plotting window, without any points
plot(pts$Longtitude, pts$Latitude, asp=1, type="n", bty="n", xlab="", ylab="", axes=FALSE)

# plot the Voronoi diagram from the tile list
plot(tiles, fillcol=pts[,'BP'], showpoints=FALSE, add=TRUE, close=FALSE)

# plot the map
map('world', interior=F, col='black', add=T)

# add the points
points(pts$Longtitude, pts$Latitude, pch=20, cex=1, col="white")
points(pts$Longtitude, pts$Latitude, pch=20, cex=0.5, col="red")

