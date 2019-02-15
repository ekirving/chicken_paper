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

# TODO remove low quality dates
# pts <- pts[pts$Confidence != 'No',]

# extract the necessary info, and drop samples with any missing data
pts <- na.omit(pts[c('Latitude', 'Longtitude', 'BP')])
colnames(pts) <- c('lat', 'long', 'BP')

# remove duplicate points
pts <- pts %>% group_by(lat, long) %>% summarise(BP = max(BP))

# convert to SpatialPointsDataFrame and add WGS84 long-lat projection
pts <- SpatialPointsDataFrame(coords = pts[c('long','lat')], data = pts, proj4string=CRS("+init=epsg:4326"))

# ------------------------------------------------------------------------------
# make the raster grid to interpolate over
# ------------------------------------------------------------------------------

map.obj <- map('world', fill=TRUE, plot=FALSE)
spdf <- map2SpatialPolygons(map.obj, IDs=map.obj$names)
proj4string(spdf) <- CRS("+init=epsg:4326")  # WGS84 long-lat projection

grd <- makegrid(spdf, n = 1e6)  # TODO make bigger for smoother surface
colnames(grd) <- c('long', 'lat')

grd_pts <- SpatialPointsDataFrame(coords = grd, data = grd, proj4string=CRS("+init=epsg:4326"))

# find all points in `grd_pts` that fall within `spdf`
pts.grid <- grd_pts[spdf, ]

# ------------------------------------------------------------------------------
# fix the projection
# ------------------------------------------------------------------------------

# head(pts)
# head(pts.grid)

# Use EPSG: 3857 (Spherical Mercator)
pts_t <- spTransform(pts, CRSobj = CRS("+init=epsg:3857"))
grd_pts_in_t <- spTransform(pts.grid, CRSobj = CRS("+init=epsg:3857"))

# ------------------------------------------------------------------------------
# do the Kriging
# ------------------------------------------------------------------------------

# PbKrig <- autoKrige(BP~1,              pts_t, grd_pts_in_t)
# PbKrig <- autoKrige(BP~sqrt(abs(lat)), pts_t, grd_pts_in_t)
# PbKrig <- autoKrige(BP~abs(lat),       pts_t, grd_pts_in_t)
# PbKrig <- autoKrige(BP~lat,            pts_t, grd_pts_in_t)
PbKrig <- autoKrige(BP~-abs(lat),            pts_t, grd_pts_in_t)

# # TODO find best formula
# plot(autofitVariogram(BP~1, pts_t))
# plot(autofitVariogram(BP~lat, pts_t))
# plot(autofitVariogram(BP~abs(lat), pts_t))
# plot(autofitVariogram(BP~-abs(lat), pts_t))
# plot(autofitVariogram(BP~sqrt(abs(lat)), pts_t))


# ------------------------------------------------------------------------------
# plot the model
# ------------------------------------------------------------------------------

# convert back to lat/long
tmp <- spTransform(PbKrig$krige_output, CRSobj = CRS("+init=epsg:4326"))  # WGS84 long-lat projection
# TODO bound var1.pred at 0

ggplot() +
    # scale_fill_viridis(name = "Density", direction = -1) +
    geom_tile(data=as.data.frame(tmp), aes(x=long, y=lat, fill=var1.pred)) +
    geom_point(data=as.data.frame(pts), aes(x=long, y=lat)) +
    # geom_text(data=as.data.frame(pts), aes(x=long, y=lat, label=BP),hjust=0, vjust=0) +
    xlim(-25, 150) +
    ylim(-50, 80) +
    coord_equal() +
    scale_fill_viridis(name = "BP", na.value = 'gainsboro', option='viridis',
                       # direction=-1,
        # rescale the color palette so the zero threshold is obvious
        # values=rescale(c(-1, 0-.Machine$double.eps, 0, 0+.Machine$double.eps,1))
        limits=c(0, 3600)
        ) +

    theme_bw()

