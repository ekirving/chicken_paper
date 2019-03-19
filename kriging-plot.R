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
# chickens$Latitude <- as.numeric(chickens$Latitude)
# chickens$Longtitude <- as.numeric(chickens$Longtitude)

# remove duplicate points
chickens <- chickens %>% group_by(lat, long) %>% summarise(BP_low = max(BP_low), BP_high = min(BP_high))

# convert to SpatialPointsDataFrame and add WGS84 long-lat projection
pts <- SpatialPointsDataFrame(coords = chickens[c('long','lat')], data = chickens, proj4string=CRS("+init=epsg:4326"))

# ------------------------------------------------------------------------------
# make the raster grid to interpolate over
# ------------------------------------------------------------------------------

# map.obj <- map('world', fill=TRUE, plot=FALSE)
# spdf <- map2SpatialPolygons(map.obj, IDs=map.obj$names)
# proj4string(spdf) <- CRS("+init=epsg:4326")  # WGS84 long-lat projection

# map.hires <- map("worldHires", plot=FALSE, fill=TRUE)
# spdf <- map2SpatialPolygons(map.hires, map.hires$names)
# proj4string(spdf) <- CRS("+init=epsg:4326")  # WGS84 long-lat projection

# get hires map of the world as spatial polygons
spdf <- rworldmap::getMap(resolution = "high")

grd <- makegrid(spdf, n = 1e6)  # TODO make bigger for smoother surface
colnames(grd) <- c('long', 'lat')

grd_pts <- SpatialPointsDataFrame(coords = grd, data = grd, proj4string=CRS(" +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))

# find all points in `grd_pts` that fall within `spdf`
pts.grid <- grd_pts[spdf, ]

# ------------------------------------------------------------------------------
# get the climate data
# see https://www.gis-blog.com/r-raster-data-acquisition/
# ------------------------------------------------------------------------------

# 2.5 = hires
# climate <- raster::getData('worldclim', var='bio', res=2.5, path = 'raster')
climate <- raster::getData('worldclim', var='bio', res=10, path = 'raster')

# pts.grid.old <- pts.grid

# pts.grid2 <- extract(climate$bio1, spdf)
pts.grid <- rasterToPoints(climate$bio1, spatial=TRUE)

pts$bio1 <- extract(climate$bio1, pts)

# temp hack to get rid of NA values
pts$bio1[is.na(pts$bio1)] <- 0

# # BIO1 = Annual Mean Temperature
# plot(climate$bio1, main="Annual Mean Temperature")
#
# library(stars)
#
# HS.stars <- st_as_stars(.x = climate$bio1)
#
# ggplot() +
#     geom_stars(data = HS.stars)

# ------------------------------------------------------------------------------
# standardise the projection
# ------------------------------------------------------------------------------

# TODO is this really necessary?
# Use EPSG: 3857 (Spherical Mercator)
pts_t <- spTransform(pts, CRSobj = CRS("+init=epsg:3857"))
grd_pts_in_t <- spTransform(pts.grid, CRSobj = CRS("+init=epsg:3857"))

# ------------------------------------------------------------------------------
# do the Kriging
# see http://desktop.arcgis.com/en/arcmap/10.3/tools/3d-analyst-toolbox/how-kriging-works.htm
# ------------------------------------------------------------------------------

PbKrig <- autoKrige(BP_low ~ bio1, pts_t, grd_pts_in_t)

# TODO find best formula
# plot(autofitVariogram(BP_low ~ abs(bio1), pts_t))

# ------------------------------------------------------------------------------
# plot the model
# ------------------------------------------------------------------------------

# convert back to lat/long
tmp <- spTransform(PbKrig$krige_output, CRSobj = CRS("+init=epsg:4326"))  # WGS84 long-lat projection
# TODO bound var1.pred at 0

# TODO shift the baseline
# tmp <- sp::recenter(tmp)

ggplot() +
    geom_tile(data=as.data.frame(tmp), aes(x=x, y=y, fill=var1.pred)) +
    geom_point(data=as.data.frame(pts), aes(x=long, y=lat), colour='red') +
    # geom_text(data=as.data.frame(pts), aes(x=long, y=lat, label=BP),hjust=0, vjust=0) +
    # xlim(-125, 190) +
    # ylim(-50, 80) +
    coord_equal() +
    scale_fill_viridis(name = "BP", na.value = 'gainsboro', option='viridis',
                       # direction=-1,
        # rescale the color palette so the zero threshold is obvious
        # values=rescale(c(-1, 0-.Machine$double.eps, 0, 0+.Machine$double.eps,1))
        limits=c(0, 3600)
        ) +

    # use minimal ggplot theme
    theme_bw() +

    # make the legend tall so there is better color definition
    theme(legend.key.height = unit(x = 3, units = 'cm'))

# ------------------------------------------------------------------------------

# getData('ISO3')$ISO3[1:10]
# c('AFG', 'PAK')

iso.list <- raster::getData('ISO3')$ISO3
# iso.list <- iso.list[!iso.list %in% c('XAD', 'AIA', 'ATA', 'BES', 'BVT', 'IOT', 'VGB', 'XCA', 'CXR', 'XCL', 'CCK')]
# raster.iso <- lapply(iso.list, function(iso) {
#     raster::getData(name = 'alt', country = iso, path = 'raster')
# })

# get all the available raster files (lots are missing so suppress any errors)
raster.iso <- lapply(iso.list, function(iso) {
    tryCatch(
        raster::getData(name = 'alt', country = iso, path = 'raster'),
        error=function(e) NULL
    )
})

# drop NULL values
raster.iso <- Filter(Negate(is.null), raster.iso)

# overwrite overlapping regions
raster.iso$overwrite <- TRUE

# perfomr the merge
# m <- do.call(merge, raster.iso)
m <- do.call(merge, unlist(raster.iso, recursive=FALSE))

writeRaster(m, filename = "all-countries.tif")

plot(m)

# load the saved raster file
r <- raster("all-countries.tif")
