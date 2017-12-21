# install.packages("dismo", dependencies=TRUE)

library(dismo);
library(rgeos)
library(deldir);
library(maptools)

#data
# stores <- c("Paris", "Lille", "Marseille", "Nice", "Nantes", "Lyon", "Strasbourg")
# lat <- c(48.85,50.62,43.29,43.71,47.21,45.76,48.57)
# lon <- c(2.35,3.05,5.36,7.26,-1.55,4.83,7.75)
# d <- data.regionsme(stores, lon, lat)

# -----
# import all the data
d <- read.csv("Chicken_Samples_Coordinates_OL_EIP.csv", header=T, strip.white=TRUE, stringsAsFactors=FALSE)
d <- na.omit(d[1:7,c('BP', 'Longtitude', 'Latitude')])
colnames(d) <- c('stores', 'lon', 'lat')
# -----

coordinates(d) <- c("lon", "lat")
proj4string(d) <- CRS("+proj=longlat +datum=WGS84")

data(wrld_simpl)
# regions <- wrld_simpl[wrld_simpl$ISO3 %in% c("regions", "DEU", "AUS"), ]
# regions <- wrld_simpl[wrld_simpl$REGION %in% c(0, 2, 9, 19, 142, 150), ]
regions <- wrld_simpl[wrld_simpl$REGION %in% c(2, 9, 19, 142, 150), ]

# levels(wrld_simpl$ISO3)

# transform to a planar coordinate reference system (as suggested by @Ege Rubak)
prj <- CRS("+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80  +units=m")
d <- spTransform(d, prj)
regions <- spTransform(regions, prj)

# voronoi function from 'dismo'
# note the 'ext' argument to spatially extend the diagram
vor <- dismo::voronoi(d, ext=extent(regions) + 10)

fix <- slot(regions, "polygons")
regions <- lapply(fix, checkPolygonsHoles)

# use intersect to maintain the attributes of the voronoi diagram
r <- intersect(vor, regions)

plot(r, col=rainbow(length(r)), lwd=1)
points(d, pch = 20, col = "black", cex = 1)

# or, to see the names of the areas
spplot(r, 'stores')

