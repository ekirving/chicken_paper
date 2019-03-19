#!/usr/bin/env Rscript

# code from...
# https://rpubs.com/nabilabd/118172
# http://rstudio-pubs-static.s3.amazonaws.com/80464_9156596afb2e4dcda53e3650a68df82a.html
# https://stackoverflow.com/questions/43436466/create-grid-in-r-for-kriging-in-gstat/43444232#answer-45948442

library(sp, quietly = TRUE)
library(dplyr, quietly = TRUE)
library(ggplot2, quietly = TRUE)
library(automap, quietly = TRUE)
library(googlesheets, quietly = TRUE)
library(viridis, quietly = TRUE)
library(maps, quietly = TRUE)
library(mapdata, quietly = TRUE)
library(maptools, quietly = TRUE)

# get the command line arguments
args <- commandArgs(trailingOnly = TRUE)
col <- args[1]
bio <- args[2]
stdev <- args[3]

# TODO remove when done testing
# setwd("/Users/Evan/Dropbox/Code/chickens")
# col <- 'BP_low'  # or 'BP_high'
# bio <- 'bio11'    # or 'bio6', 'bio11'
# stdev <- 425

# limits for min/max displayed longitude
xmin <- -20
xmax <- 235

# ------------------------------------------------------------------------------
# import the chicken data and make the SpatialPointsDataFrame
# ------------------------------------------------------------------------------

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

# remove duplicate points
chickens <- chickens %>%
    group_by(lat, long) %>%
    summarise(BP_low = max(BP_low), BP_high = min(BP_high))

# convert to SpatialPointsDataFrame and set standard WGS84 long-lat projection
pts <- SpatialPointsDataFrame(coords = chickens[c('long','lat')],
                              data = chickens[,-which(names(chickens) %in% c('long','lat'))],
                              proj4string=CRS("+init=epsg:4326"))

# ------------------------------------------------------------------------------
# make a raster grid to interpolate over
# ------------------------------------------------------------------------------

# # get a hires map of the world as spatial polygons
# spdf <- rworldmap::getMap(resolution = "high")
#
# grd <- makegrid(spdf, n = 1e6)  # make 'n' bigger for a smoother surface
# colnames(grd) <- c('long', 'lat')
#
# grd_pts <- SpatialPointsDataFrame(coords = grd, data = grd,
#                                   proj4string=CRS(" +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
#
# # find all points in `grd_pts` that fall within `spdf`
# pts.grid <- grd_pts[spdf, ]

# ------------------------------------------------------------------------------
# prepare a raster map of climate data
# ------------------------------------------------------------------------------

# resolution is measured in minutes of a degree (0.5, 2.5, 5, and 10)
climate <- raster::getData('worldclim', var='bio', res=10, path = 'raster')

# bio1  = Annual Mean Temperature (units °C * 10)
# bio6  = Min Temperature of Coldest Month
# bio11 = Mean Temperature of Coldest Quarter
# see https://www.worldclim.org/bioclim

# plot the climate map
png(file=paste0('png/', bio, '-map.png'), width=16, height=8, units='in', res=300)
plot(climate[[bio]])
dev.off()

# convert the raster to a SpatialPointsDataFrame
pts.grid <- raster::rasterToPoints(climate[[bio]], spatial=TRUE)

# assign climate values to the sample locations
pts[[bio]] <- raster::extract(climate[[bio]], pts)

# set any NA values to 0
pts[[bio]][is.na(pts[[bio]])] <- 0

# ------------------------------------------------------------------------------
# do the Kriging
# see http://desktop.arcgis.com/en/arcmap/10.3/tools/3d-analyst-toolbox/how-kriging-works.htm
# ------------------------------------------------------------------------------

# Use EPSG: 3857 (Spherical Mercator) projection
pts_t <- spTransform(pts, CRSobj = CRS("+init=epsg:3857"))
grd_pts_in_t <- spTransform(pts.grid, CRSobj = CRS("+init=epsg:3857"))

# compose the formula
krig.formula <- as.formula(paste(col, '~', bio))

# plot the fit
png(file=paste0('png/', col, '-', bio, '-variogram.png'), width=8, height=4, units='in', res=300)
plot(autofitVariogram(krig.formula, pts_t))
dev.off()

# perform the Krigging
chicken.krig <- autoKrige(krig.formula, pts_t, grd_pts_in_t)

# convert back to lat/long before plotting
krig.latlong <- spTransform(chicken.krig$krige_output, CRSobj = CRS("+init=epsg:4326"))

# set any negative BP values to 0
krig.latlong$var1.pred[krig.latlong$var1.pred < 0] <- 0

# mask high standard error regions
krig.latlong$var1.pred[krig.latlong$var1.stdev > stdev] <- NA

# shift the framing of the map, so we can see the Pacific
krig.latlong@coords[,'x'][krig.latlong$x < xmin] <- krig.latlong$x[krig.latlong$x < xmin] + 360
pts@coords[,'long'][pts$long < xmin] <- pts$long[pts$long < xmin] + 360

# ------------------------------------------------------------------------------
# plot the model
# ------------------------------------------------------------------------------

# png(file=paste0('png/', col, '-', bio, '-krige.png'), width=16, height=8, units='in', res=300)

# plot the map
ggplot() +

    # the Krige surface
    geom_tile(data=as.data.frame(krig.latlong), aes(x=x, y=y, fill=var1.pred)) +

    # the sample locations
    geom_point(data=as.data.frame(pts), aes(x=long, y=lat), colour='red') +

    # the dates of the sample locations
    # geom_text(data=as.data.frame(pts), aes(x=long, y=lat, label=BP),hjust=0, vjust=0) +

    # set the limits of the x scale
    scale_x_continuous(limits = c(xmin, xmax), expand = c(0, 0)) +

    # use a fixed aspect ratio
    coord_equal() +

    # set the colour palette for the Krige surface
    scale_fill_viridis(name = "BP", na.value = 'gainsboro', option='viridis') +

    # use minimal ggplot theme
    theme_bw() +

    # make the legend tall so there is better color definition
    theme(legend.key.height = unit(x = 3, units = 'cm'))

# dev.off()
