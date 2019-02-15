# install.packages("deldir", dependencies=TRUE)
# install.packages(c("sp", "maps", "maptools", "mapproj"), dependencies=TRUE)

library(deldir)
library(dplyr)
library(maps)
library(mapdata)
library(maptools)
library(mapproj)
library(rgeos)
library(sp)

setwd("/Users/Evan/Dropbox/Code/chickens")

# import all the data
data <- read.csv("Chicken_Samples_Coordinates_OL_EIP.csv", header=T, strip.white=TRUE, stringsAsFactors=FALSE)

# create an ID column to uniquely identify each record
data['ID'] <- rownames(data)

# remove low quality dates
data <- data[data$Confidence != 'No',]

# extract the necessary info, and drop samples with any missing data
pts <- na.omit(data[c('Longtitude', 'Latitude', 'BP')])

# remove duplicate points
pts <- pts %>% group_by(Longtitude, Latitude) %>% summarise(BP = max(BP))

# convert points into an SP df
pts.spdf <- SpatialPointsDataFrame(pts[c('Longtitude', 'Latitude')], pts, match.ID=TRUE)

# function to compute the veroni tessalation and return SpatialPolygons
SPointsDF_to_voronoi_SPolysDF <- function(sp) {

    # tile.list extracts the polygon data from the deldir computation
    vor_desc <- tile.list(deldir(sp@coords[,1], sp@coords[,2]))

    lapply(1:(length(vor_desc)), function(i) {

        # tile.list gets us the points for the polygons but we
        # still have to close them, hence the need for the rbind
        tmp <- cbind(vor_desc[[i]]$x, vor_desc[[i]]$y)
        tmp <- rbind(tmp, tmp[1,])

        # now we can make the Polygon(s)
        Polygons(list(Polygon(tmp)), ID=i)

    }) -> vor_polygons

    # hopefully the caller passed in good metadata!
    sp_dat <- as.data.frame(sp@data)

    # this way the IDs _should_ match up w/the data & voronoi polys
    rownames(sp_dat) <- sapply(slot(SpatialPolygons(vor_polygons), 'polygons'), slot, 'ID')

    SpatialPolygonsDataFrame(SpatialPolygons(vor_polygons), data=sp_dat)
}

# get the veroni tessalation for the points
vor.spdf <- SPointsDF_to_voronoi_SPolysDF(pts.spdf)

# fetch the map to use, and convert to SP
map.obj <- map('world', fill=TRUE, plot=FALSE)
map.sp <- map2SpatialPolygons(map.obj, IDs=map.obj$names)

# simplify the polgons a tad (tweak 0.00001 to your liking)
map.sp <- gSimplify(map.sp, tol = 0.00001)

# this is a well known R / GEOS hack (usually combined with the above) to
# deal with "bad" polygons
map.sp <- gBuffer(map.sp, byid=TRUE, width=0)

# merge all adjacent polys, so our map is just coastlines
map.outline <- gUnionCascaded(map.sp)

# intersect the two sets of polygons
map.masked <- raster::intersect(vor.spdf, map.outline)

# collapse the default margins
par(mar=c(0,0,0,0))

# display the map
spplot(map.masked, zcol="BP", col=NA, col.regions=heat.colors(21)[1:20],
       par.settings = list(panel.background=list(col="white")),
       sp.layout = list("sp.points", pts.spdf, pch = 16, cex = 0.5, col = "black"))
