#!/usr/bin/env Rscript

# code from...
# https://rpubs.com/nabilabd/118172
# http://rstudio-pubs-static.s3.amazonaws.com/80464_9156596afb2e4dcda53e3650a68df82a.html
# https://stackoverflow.com/questions/43436466/create-grid-in-r-for-kriging-in-gstat/43444232#answer-45948442

library(sp)
library(dplyr)
library(ggplot2)
library(automap)
library(googlesheets)
library(viridis)
library(maps)
library(mapdata)
library(maptools)

# ------------------------------------------------------------------------------
# import the chicken data and make the SpatialPointsDataFrame
# ------------------------------------------------------------------------------

setwd("/Users/Evan/Dropbox/Code/chickens")

# grab the data file
gsheet <- gs_title("Chicken_Samples_Coordinates_OL_Jan2019")

# get the tab containing the known data
chickens <- gsheet %>% gs_read(ws = "Reviewed Jan 2019 (Good Chi)")

# extract the relevant columns
chickens <- chickens[c('Confidence', 'Lower Range BP', 'Upper Range BP', 'Latitude', 'Longtitude')]
colnames(chickens) <- c('confidence', 'BP_low', 'BP_high', 'lat', 'long')

# remove all the NA data
chickens <- na.omit(chickens)

# TODO remove low quality dates
# chickens <- chickens[chickens$Confidence != 'No',]

# cast the lat/long to numeric
chickens$Latitude <- as.numeric(chickens$Latitude)
chickens$Longtitude <- as.numeric(chickens$Longtitude)

# remove duplicate points
chickens <- chickens %>% group_by(lat, long) %>% summarise(BP_low = max(BP_low), BP_high = min(BP_high))

# convert to SpatialPointsDataFrame and add WGS84 long-lat projection
pts <- SpatialPointsDataFrame(coords = chickens[c('long','lat')], data = chickens, proj4string=CRS("+init=epsg:4326"))

# ------------------------------------------------------------------------------
# make the raster grid to interpolate over
# ------------------------------------------------------------------------------

map.obj <- map('world', fill=TRUE, plot=FALSE)
spdf <- map2SpatialPolygons(map.obj, IDs=map.obj$names)

map.hires <- map("worldHires", plot=FALSE, fill=TRUE)
spdf <- map2SpatialPolygons(map.hires, map.hires$names)
# map.polygon <- rworldmap::getMap(resolution = "high")

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
PbKrig <- autoKrige(BP_low~-abs(lat),            pts_t, grd_pts_in_t)

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
    geom_tile(data=as.data.frame(tmp), aes(x=long, y=lat, fill=var1.pred)) +
    geom_point(data=as.data.frame(pts), aes(x=long, y=lat), colour='red') +
    # geom_text(data=as.data.frame(pts), aes(x=long, y=lat, label=BP),hjust=0, vjust=0) +
    xlim(-25, 190) +
    ylim(-50, 80) +
    coord_equal() +
    scale_fill_viridis(name = "BP", na.value = 'gainsboro', option='viridis',
                       # direction=-1,
        # rescale the color palette so the zero threshold is obvious
        # values=rescale(c(-1, 0-.Machine$double.eps, 0, 0+.Machine$double.eps,1))
        limits=c(0, 3600)
        ) +

    theme_bw()

