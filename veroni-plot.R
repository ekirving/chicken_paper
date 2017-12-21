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

# raster::mask(interpol.stack, map.polygons)

# plot the map
map('world', interior=F, col='black', add=T)

# add the points
points(pts$Longtitude, pts$Latitude, pch=20, cex=1, col="white")
points(pts$Longtitude, pts$Latitude, pch=20, cex=0.5, col="red")

# ------

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
    sp_dat <- sp@data

    # this way the IDs _should_ match up w/the data & voronoi polys
    rownames(sp_dat) <- sapply(slot(SpatialPolygons(vor_polygons),
                                    'polygons'),
                               slot, 'ID')

    SpatialPolygonsDataFrame(SpatialPolygons(vor_polygons),
                             data=sp_dat)

}

require(dplyr)

# remove duplicate points
pts <- pts %>% group_by(Longtitude, Latitude) %>% summarise(BP = max(BP))

vor_pts <- SpatialPointsDataFrame(pts[c('Longtitude', 'Latitude')], pts, match.ID=TRUE)
vor <- SPointsDF_to_voronoi_SPolysDF(vor_pts)
vor_df <- fortify(vor)

tmp <-map('world', fill=TRUE, plot=FALSE)
tmp.sp <- map2SpatialPolygons(tmp, IDs=tmp$names)

plot(tmp.sp)

# nms <- as.data.frame(tmp$names)
# rownames(nms) <- tmp$names
# tmp.spd <- SpatialPolygonsDataFrame(, data=nms)
# raster::intersect(vor, tmp.spd)

intr <- raster::intersect(vor, tmp.sp)

plot(intr)

# ----------------

# simplify the polgons a tad (tweak 0.00001 to your liking)
tmp.sp2 <- gSimplify(tmp.sp, tol = 0.00001)

# this is a well known R / GEOS hack (usually combined with the above) to
# deal with "bad" polygons
tmp.sp2 <- gBuffer(tmp.sp2, byid=TRUE, width=0)

# merge the adjacent polys
regionOfInterest <- gUnionCascaded(tmp.sp2)

# plot(tmp.sp)
plot(regionOfInterest)

intr2 <- raster::intersect(vor, regionOfInterest)

plot(intr2)

# add the points
points(pts$Longtitude, pts$Latitude, pch=20, cex=1, col="white")
points(pts$Longtitude, pts$Latitude, pch=20, cex=0.5, col="red")

