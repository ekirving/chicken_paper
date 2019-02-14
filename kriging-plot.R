#!/usr/bin/env Rscript

# code from...
# https://rpubs.com/nabilabd/118172
# http://rstudio-pubs-static.s3.amazonaws.com/80464_9156596afb2e4dcda53e3650a68df82a.html
# https://stackoverflow.com/questions/43436466/create-grid-in-r-for-kriging-in-gstat/43444232#answer-45948442

library(sp)
library(dplyr)
library(ggplot2)
library(automap)
library(viridis)
library(maps)
library(maptools)

# ------------------------------------------------------------------------------
# import the chicken data and make the SpatialPointsDataFrame
# ------------------------------------------------------------------------------

setwd("/Users/Evan/Dropbox/Code/chickens")

# import all the data
pts <- read.csv("Chicken_Samples_Coordinates_OL_EIP.csv", header=T,
                strip.white=TRUE, stringsAsFactors=FALSE)

# remove low quality dates
# pts <- pts[pts$Confidence != 'No',]

# extract the necessary info, and drop samples with any missing data
pts <- na.omit(pts[c('Latitude', 'Longtitude', 'BP')])
colnames(pts) <- c('lat', 'long', 'BP')

# remove duplicate points
pts <- pts %>% group_by(lat, long) %>% summarise(BP = max(BP))

# Convert to SpatialPointsDataFrame
coordinates(pts) <- ~long + lat

# set the projection, use WGS84 long-lat projection
proj4string(pts) <- CRS("+init=epsg:4326")

# ------------------------------------------------------------------------------
# make the raster grid to interpolate over
# ------------------------------------------------------------------------------

map.obj <- map('world', fill=TRUE, plot=FALSE)
spdf <- map2SpatialPolygons(map.obj, IDs=map.obj$names)
proj4string(spdf) <- CRS("+init=epsg:4326")  # WGS84 long-lat projection

grd <- makegrid(spdf, n = 100000)  # TODO make bigger for smoother surface
colnames(grd) <- c('x','y')

grd_pts <- SpatialPoints(coords = grd, proj4string=CRS("+init=epsg:4326"))

# find all points in `grd_pts` that fall within `spdf`
pts.grid <- grd_pts[spdf, ]

# ------------------------------------------------------------------------------
# fix the projection
# ------------------------------------------------------------------------------

# Use EPSG: 3857 (Spherical Mercator)
pts_t <- spTransform(pts, CRSobj = CRS("+init=epsg:3857"))
grd_pts_in_t <- spTransform(pts.grid, CRSobj = CRS("+init=epsg:3857"))

# ------------------------------------------------------------------------------
# do the Kriging
# ------------------------------------------------------------------------------

# PbKrig <- autoKrige(BP~1, pts, grd_pts_in)
PbKrig <- autoKrige(BP~1, pts_t, grd_pts_in_t)


# TODO find best formula
# plot(autofitVariogram(BP~1, pts_t))

# class(pts)
# head(pts)
# plot(autofitVariogram(BP~sqrt(dist), pts_t))


# plot(PbKrig)
# plot(PbKrig$krige_output)

# ------------------------------------------------------------------------------
#
# ------------------------------------------------------------------------------

# convert back to lat/long
tmp <- spTransform(PbKrig$krige_output, CRSobj = CRS("+init=epsg:4326"))  # WGS84 long-lat projection

# TODO bound

ggplot() +
    # scale_fill_viridis(name = "Density", direction = -1) +
    geom_tile(data=as.data.frame(tmp), aes(x=x, y=y, fill=var1.pred)) +
    geom_point(data=as.data.frame(pts), aes(x=long, y=lat)) +
    # geom_text(data=as.data.frame(pts), aes(x=long, y=lat, label=BP),hjust=0, vjust=0) +
    xlim(-25, 150) +
    ylim(-50, 80) +
    coord_equal() +
    # scale_fill_viridis(name = "BP") +
    scale_fill_viridis(name = "BP", na.value = 'gainsboro', option='viridis',
                       # direction=-1,
        # rescale the color palette so the zero threshold is obvious
        # values=rescale(c(-1, 0-.Machine$double.eps, 0, 0+.Machine$double.eps,1))

        ) +

    theme_bw()

