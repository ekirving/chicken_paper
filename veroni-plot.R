# install.packages("deldir", dependencies=TRUE)
# install.packages(c("sp", "maps", "maptools", "mapproj"), dependencies=TRUE)

library(deldir)
library(rworldmap)

setwd("/Users/Evan/Dropbox/Code/chickens")

# import all the data
data <- read.csv("Chicken_Samples_Coordinates_OL_EIP.csv", header=T)

# extract the necessary info, and drop samples with any missing data
pts <- data.matrix(na.omit(data[c('Longtitude', 'Latitude', 'BP')]))

# calculate tessellation and triangulation
vtess <- deldir(pts[,'Longtitude'], pts[,'Latitude'])

# convert to tile list
tiles <- tile.list(vtess)

par(mar=c(0,0,0,0))

# setup a plotting window, without any points
plot(pts, asp=1, type="n", bty="n", xlab="", ylab="", axes=FALSE)

# plot the Voronoi diagram from the tile list
# plot(tiles, fillcol=pts[,'BP'], showpoints=FALSE, add=TRUE, close=FALSE)

# add the points
points(pts, pch=20, cex=0.5, col="black")

newmap <- getMap(resolution = "low")
plot(newmap, asp = 1, add=TRUE)


# -------------

pts <- pts[1:3,]

par(mar=c(0,0,0,0))
plot(newmap)
points(pts[,'Longtitude'], pts[,'Latitude'], cex=0.5, col="red")
# points(pts[,'Latitude'], pts[,'Longtitude'], cex=0.5, col="green")

