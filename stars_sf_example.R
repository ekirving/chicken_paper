library(MASS)
library(tidyverse)
library(sf)
library(stars)
library(ggspatial)

# library(devtools)
# install_github("r-spatial/stars")

setwd("/Users/Evan/Dropbox/Code/chickens")

# code from https://github.com/brfitzpatrick/stars-sf-example/blob/master/stars_sf_example.md

# Load the Digital Elevation Model

# DEM.250.rast <- raster::raster(x =  '~/DEM_250.tif')
DEM.250.rast <- raster::getData(name = 'alt', country = 'CHE')

# Calculate a hill shaded terrain surface

Slope.rast <- raster::terrain(x = DEM.250.rast, opt = 'slope')
Aspect.rast <- raster::terrain(x = DEM.250.rast, opt = 'aspect')
HS.rast <- raster::hillShade(slope = Slope.rast, aspect = Aspect.rast, normalize = TRUE)
raster::plot(HS.rast, col=grey(0:100/100), legend=FALSE)

# Convert the hill shaded terrain surface from an object of class raster to an
# object of class stars
HS.stars <- st_as_stars(.x = HS.rast)

# Read in the cantonal boundaries as a simple features sf object
# canton.sf <- read_sf('gadm36_CHE_0_sf.rds')
canton.sf <- readRDS('gadm36_CHE_1_sf.rds')

# Check both objects use the same coordinate reference system
st_crs(HS.stars) == st_crs(canton.sf)

# Reproject the cantonal boundaries to the coordinate reference system of the hill shaded terrain surface
# canton.sf <- st_transform(x = canton.sf, crs = st_crs(HS.stars))

# Plot the hill shaded terrain surface with ggplot2
p1 <- ggplot() +
    geom_stars(data = HS.stars) +
    scale_fill_gradient(low = 'black', high = 'white', na.value = 'white') +
    coord_equal()

p1

# Add the cantonal boundaries to the plot of the hill shaded terrain surface
p2 <- p1 +
    geom_sf(data = canton.sf, fill = NA, size = 2, colour = 'white') +
    geom_sf(data = canton.sf, fill = NA, size = 1, colour = 'black')

p2

# Find the union of the cantonal boundary polygons
border.sf <- st_union(x = canton.sf)

# Add the union of the cantonal boundary polygons to the plot of the hill shaded terrain surface
p3 <- p1 +
    geom_sf(data = border.sf, fill = NA, size = 2, colour = 'white') +
    geom_sf(data = border.sf, fill = NA, size = 1, colour = 'black')

p3

# Crop the hill shaded terrain surface to the union of the cantonal boundary polygons
CH.HS.stars <- st_crop(x = HS.stars, y = border.sf)

p4 <- ggplot() +
    geom_stars(data = CH.HS.stars) +
    scale_fill_gradient(low = 'black', high = 'white', na.value = 'white') +
    coord_equal() +
    geom_sf(data = border.sf, fill = NA, size = 2, colour = 'white') +
    geom_sf(data = border.sf, fill = NA, size = 1, colour = 'black')

p4

# Now suppose we wanted to overlay some spatial points data, for this example I simulate some such data
mvn.pts.sf <- mvrnorm(n = 500, mu = c(7.4474, 46.9480), Sigma = diag(x = 0.2, nrow = 2)) %>%
    as_tibble() %>%
    rename(X = V1, Y = V2) %>%
    st_as_sf(x = .,
             coords = c('X', 'Y'),
             crs = 4326
    ) %>%
    st_transform(crs = st_crs(CH.HS.stars))

# Determine which canton each point intersects
points.in.cantons.sf <- st_intersection(x = mvn.pts.sf, y = canton.sf)

# Count the number of points within each canton
n.pts.tb <- group_by(points.in.cantons.sf, NAME_1) %>%
    count() %>%
    arrange(desc(n)) %>%
    as_tibble() %>%
    dplyr::select(NAME_1, n)

# Add these counts to a simple features object containing the cantonal boundaries
canton.npts.sf <- left_join(x = canton.sf, y = n.pts.tb, by = 'NAME_1') %>%
    mutate(n.pts = case_when( is.na(n) ~ as.integer(0),
                              !is.na(n) ~ n
    )
    )

# error in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : polygon edge not found
# https://stackoverflow.com/questions/10581440/error-in-grid-calll-textbounds-as-graphicsannotxlabel-xx-xy-polygon

# Produce a more complex plot...
ggplot() +
    # render the raster map
    geom_stars(data = CH.HS.stars, aes(x = x, y = y, alpha = -layer), fill = 'black') +

    # hide second legend for hillshading mapped to alpha channel
    guides(alpha = FALSE) +

    # make background transparent
    scale_alpha_continuous(na.value = 0) +

    # draw canton boundaries and set transparent fill colour based on sqrt of count of points
    geom_sf(data = canton.npts.sf, aes(fill = sqrt(n.pts)), alpha = 0.5, colour = 'white') +

    # draw large black circules with small white circles inside
    geom_sf(data = points.in.cantons.sf, size = 2) +
    geom_sf(data = points.in.cantons.sf, size = 1, colour = 'white') +

    # use the nice viridis colour scale
    scale_fill_viridis_c() +

    # add canton names to the plot
    geom_sf_label(data = canton.sf, aes(label = NAME_1), alpha = 0.5, fill = 'white') +

    # show Km scale in corner
    annotation_scale(width_hint = 0.3) +

    # label the legend
    labs(x = NULL, y = NULL, fill = expression(sqrt(paste('Number of Points')))) +

    # use minimal ggplot theme
    theme_bw() +

    # make the legend tall so there is better color definition
    theme(legend.key.height = unit(x = 3, units = 'cm'))


