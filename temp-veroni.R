install.packages("dismo", dependencies=TRUE)

library(dismo);
library(rgeos)
library(deldir);
library(maptools)

#data
stores <- c("Paris", "Lille", "Marseille", "Nice", "Nantes", "Lyon", "Strasbourg")
lat <- c(48.85,50.62,43.29,43.71,47.21,45.76,48.57)
lon <- c(2.35,3.05,5.36,7.26,-1.55,4.83,7.75)
d <- data.frame(stores, lon, lat)
coordinates(d) <- c("lon", "lat")
proj4string(d) <- CRS("+proj=longlat +datum=WGS84")

data(wrld_simpl)
fra <- wrld_simpl[wrld_simpl$ISO3 == 'FRA', ]

# transform to a planar coordinate reference system (as suggested by @Ege Rubak)
prj <- CRS("+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80  +units=m")
d <- spTransform(d, prj)
fra <- spTransform(fra, prj)


# voronoi function from 'dismo'
# note the 'ext' argument to spatially extend the diagram
vor <- dismo::voronoi(d, ext=extent(fra) + 10)

# use intersect to maintain the attributes of the voronoi diagram
r <- intersect(vor, fra)

plot(r, col=rainbow(length(r)), lwd=3)
points(d, pch = 20, col = "white", cex = 3)
points(d, pch = 20, col = "red", cex = 2)

# or, to see the names of the areas
spplot(r, 'stores')
