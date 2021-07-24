#!/usr/bin/env Rscript

# Author:    Evan K. Irving-Pease
# Copyright: Copyright 2021, University of Copenhagen
# Email:     evan.irvingpease@gmail.com
# License:   MIT

# inspiration from...
# https://rpubs.com/nabilabd/118172
# http://rstudio-pubs-static.s3.amazonaws.com/80464_9156596afb2e4dcda53e3650a68df82a.html
# https://stackoverflow.com/questions/43436466/create-grid-in-r-for-kriging-in-gstat/43444232#answer-45948442

quiet <- function(x) {
    suppressMessages(suppressWarnings(x))
}
quiet(library(sp))
quiet(library(sf))
quiet(library(dplyr))
quiet(library(ggplot2))
quiet(library(automap))
quiet(library(readr))
quiet(library(viridis))
quiet(library(maps))
quiet(library(mapdata))
quiet(library(maptools))
quiet(library(ggrepel))
quiet(library(raster))
quiet(library(rgdal))
quiet(library(argparser))

# get the command line arguments
p <- arg_parser("Plot the distribution of ancient chickens maps")
p <- add_argument(p, "--data", default = "data/Chicken_Samples_Coordinates_OL_JorisManuscript_Reviewed_June_2021_Good_Chi.tsv", help = "Path to the spreadsheet to use")
p <- add_argument(p, "--column", default = "BP_high", help = "Column in the spreadsheet to use as the date (e.g., 'BP_low', 'BP_high')")
p <- add_argument(p, "--high-conf", flag = TRUE, help = "Only retain high confidence samples")
p <- add_argument(p, "--clusters", default = 100, help = "Number of clusters to use for thinning observations")
p <- add_argument(p, "--labels", default = 60, help = "Number of smaples to label")
p <- add_argument(p, "--bioclimate", default="bio11", help = "Bioclimate variable to use for interpolation (e.g., 'bio1', 'bio6', 'bio11')")
p <- add_argument(p, "--resolution", default=10, help = "Resolution (in minutes) of each raster tile  (e.g., 0.5, 2.5, 5, and 10)")
p <- add_argument(p, "--stderr", default=600, help = "Maximum standard error in the model to display")
p <- add_argument(p, "--palette", default="viridis", help = "Colour palette for the maps (e.g., 'viridis', 'magma', 'inferno', 'plasma', 'cividis')")

argv <- parse_args(p)

# min/max limits for the lat/long of the map
xmin <- -20
xmax <- 235
ymin <- -55
ymax <-  85

# ------------------------------------------------------------------------------
# import the sample data and make the SpatialPointsDataFrame
# ------------------------------------------------------------------------------

# grab the data file
samples <- read_tsv(argv$data)

# extract the relevant columns
samples <- samples[c('Confidence', 'Lower Range BP', 'Upper Range BP', 'Latitude', 'Longitude')]
colnames(samples) <- c('confidence', 'BP_low', 'BP_high', 'lat', 'long')

# remove all the NA data
samples <- na.omit(samples)

# find the midpoint of the dates
samples$BP_mid <- round(rowMeans(samples[c('BP_low', 'BP_high')]))

# remove low quality samples
if (argv$high_conf) {
    samples <- samples[samples$confidence != 'No',]
}

# find the oldest sample
max.age <- max(samples[,argv$column])

# remove duplicate points (keep the oldest)
samples <- samples %>%
    group_by(lat, long) %>%
    slice(which.max(.data[[argv$column]])) %>%
    ungroup()

# calculate the Euclidean between all sample points
dist_mat <- dist(samples[c('lat','long')], method = 'euclidean')

# perform hierarchical clustering of the samples, using the average of each cluster
hclust_avg <- hclust(dist_mat, method = 'average')

# extract the cluster labels for K clusters, and K/2 clusters
samples$cluster <- cutree(hclust_avg, k = argv$clusters)
samples$label <- cutree(hclust_avg, k = argv$labels)

# only retain the oldest sample from each cluster
samples.thin <- samples %>%
    group_by(cluster) %>%
    slice(which.max(.data[[argv$column]]))

# get all the dropped samples
samples.drop <- anti_join(samples, samples.thin)

# show labels for half of the retained samples used for the map
samples.label <- samples %>%
    group_by(label) %>%
    slice(which.max(.data[[argv$column]])) %>%
    ungroup() %>%
    dplyr::select(one_of('lat', 'long', argv$column))

# convert to SpatialPointsDataFrame and set standard WGS84 long-lat projection
pts <- SpatialPointsDataFrame(coords = samples.thin[c('long','lat')],
                              data = samples.thin[,-which(names(samples.thin) %in% c('long','lat'))],
                              proj4string=CRS("+init=epsg:4326"))

# ------------------------------------------------------------------------------
# fetch a raster map containing climate data to interpolate over
# ------------------------------------------------------------------------------

# make the output directories
dir.create('png/bio/', recursive = TRUE, showWarnings = FALSE)
dir.create('png/krige/', recursive = TRUE, showWarnings = FALSE)
dir.create('png/stderr/', recursive = TRUE, showWarnings = FALSE)
dir.create('png/vario/', recursive = TRUE, showWarnings = FALSE)
dir.create('raster/', showWarnings = FALSE)

# resolution is measured in minutes of a degree (0.5, 2.5, 5, and 10)
climate <- raster::getData('worldclim', var='bio', res=argv$resolution, path='raster')

# bio1  = Annual Mean Temperature (units °C * 10)
# bio6  = Min Temperature of Coldest Month
# bio11 = Mean Temperature of Coldest Quarter
# see https://www.worldclim.org/bioclim

# plot the climate map
map_file <- paste0('png/bio/', argv$bioclimate, '-res', argv$resolution, '-map.png')
if (!file.exists(map_file)) {
    png(file=map_file, width=16, height=8, units='in', res=300)
    plot(climate[[argv$bioclimate]])
    dev.off()
}

# convert the raster to a SpatialPointsDataFrame
pts.grid <- raster::rasterToPoints(climate[[argv$bioclimate]], spatial=TRUE)

# assign climate values to the sample locations
pts[[argv$bioclimate]] <- raster::extract(climate[[argv$bioclimate]], pts)

# set any NA values to 0
pts[[argv$bioclimate]][is.na(pts[[argv$bioclimate]])] <- 0

# ------------------------------------------------------------------------------
# perform the Kriging
# see http://desktop.arcgis.com/en/arcmap/10.3/tools/3d-analyst-toolbox/how-kriging-works.htm
# ------------------------------------------------------------------------------

# Use EPSG: 3857 (Spherical Mercator) projection
pts_t <- spTransform(pts, CRSobj = CRS("+init=epsg:3857"))
grd_pts_in_t <- spTransform(pts.grid, CRSobj = CRS("+init=epsg:3857"))

# compose the Kriging formula (e.g. date is dependent on climate variable)
krig.formula <- as.formula(paste(argv$column, '~', argv$bioclimate))

# plot the fit
png(file=paste0('png/vario/', argv$column, '-hiq', argv$high_conf, '-num', argv$clusters, '-', argv$bioclimate, '-res', argv$resolution, '-variogram.png'), width=8, height=4, units='in', res=300)
plot(autofitVariogram(krig.formula, pts_t))
dev.off()

# perform the Kriging
samples.krig <- autoKrige(krig.formula, pts_t, grd_pts_in_t)

# convert back to lat/long before plotting
krige.sp <- spTransform(samples.krig$krige_output, CRSobj = CRS("+init=epsg:4326"))

# set any negative values to 0
krige.sp$var1.pred[krige.sp$var1.pred < 0] <- 0

# and cap positive values at the oldeset observed date
krige.sp$var1.pred[krige.sp$var1.pred > max.age] <- max.age

# mask high standard error regions
krige.sp$var1.pred[krige.sp$var1.stdev > argv$stderr] <- NA

# shift the framing of the map, so we can see the Pacific
krige.sp@coords[,'x'][krige.sp$x < xmin] <- krige.sp$x[krige.sp$x < xmin] + 360
pts@coords[,'long'][pts$long < xmin] <- pts$long[pts$long < xmin] + 360
samples.drop$long[samples.drop$long < xmin] <- samples.drop$long[samples.drop$long < xmin] + 360
samples.label$long[samples.label$long < xmin] <- samples.label$long[samples.label$long < xmin] + 360

# ------------------------------------------------------------------------------
# fetch a better map with more precise oceans boundaries
# ------------------------------------------------------------------------------

# download the map data
if (!file.exists("ne_50m_ocean.zip")) {
    download.file("https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/physical/ne_50m_ocean.zip", "ne_50m_ocean.zip")
    unzip("ne_50m_ocean.zip", exdir="ne_50m_ocean")
}

# load the shape file
ocean_st <- st_read("ne_50m_ocean/ne_50m_ocean.shp")

if (!file.exists("ne_110m_lakes.zip")) {
    download.file("https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/physical/ne_110m_lakes.zip", "ne_110m_lakes.zip")
    unzip("ne_110m_lakes.zip", exdir="ne_110m_lakes")
}

# load the shape file
lakes_st <- st_read("ne_110m_lakes/ne_110m_lakes.shp")

# ------------------------------------------------------------------------------
# plot the model
# ------------------------------------------------------------------------------

png(file=paste0('png/krige/', argv$column, '-hiq', argv$high_conf, '-num', argv$clusters, '-', argv$bioclimate, '-res', argv$resolution, '-err', argv$stderr, '-', argv$palette, '-krige.png'), width=16, height=8, units='in', res=300)

# plot the map
ggplot() +

    # plot the Krige surface
    geom_tile(data=as.data.frame(krige.sp), aes(x=x, y=y, fill=var1.pred)) +

    # over-plot the ocean and lake boundaries
    geom_sf(data = ocean_st, fill="white", color="white") +
    geom_sf(data = lakes_st, fill="white", color="white") +

    # plot the unused samples first
    geom_point(data=samples.drop, aes(x=long, y=lat), shape = 21, colour = "red") +

    # plot the samples used for the Kriging
    geom_point(data=as.data.frame(pts), aes(x=long, y=lat), shape = 19, colour = "red") +

    # plot the dates of the samples used for Kriging
    geom_text_repel(data=samples.label, aes_string(x='long', y='lat', label=argv$column), min.segment.length = 0, box.padding = 0.5, na.rm = TRUE) +

    # set the limits of the scales
    scale_x_continuous(limits = c(xmin, xmax), expand = c(0, 0)) +
    scale_y_continuous(limits = c(ymin, ymax), expand = c(0, 0)) +

    # set the colour palette for the Krige surface
    scale_fill_viridis(name = "BP", na.value = 'gainsboro', option=argv$palette,
                       limits = c(0, max.age)) +

    # use minimal ggplot theme
    theme_bw() +

    # make the legend tall so there is better color definition
    theme(legend.key.height = unit(x = 3, units = 'cm'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

dev.off()

# ------------------------------------------------------------------------------
# plot the standard error
# ------------------------------------------------------------------------------

png(file=paste0('png/stderr/', argv$column, '-hiq', argv$high_conf, '-num', argv$clusters, '-', argv$bioclimate, '-res', argv$resolution, '-stderr.png'), width=16, height=8, units='in', res=300)

# plot the map
ggplot() +

    # plot the Krige surface
    geom_tile(data=as.data.frame(krige.sp), aes(x=x, y=y, fill=var1.stdev)) +

    # plot the samples used for the Kriging
    geom_point(data=as.data.frame(pts), aes(x=long, y=lat), colour = "red") +

    # set the limits of the scales
    scale_x_continuous(limits = c(xmin, xmax), expand = c(0, 0)) +
    scale_y_continuous(limits = c(ymin, ymax), expand = c(0, 0)) +

    # use a fixed aspect ratio
    coord_equal() +

    # set the colour palette for the Krige surface
    scale_fill_viridis(name = "stderr", na.value = 'gainsboro', option='plasma',
                       limits = c(0, 1200)) +

    # use minimal ggplot theme
    theme_bw() +

    # make the legend tall so there is better color definition
    theme(legend.key.height = unit(x = 3, units = 'cm'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

dev.off()
