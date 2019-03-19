#!/usr/bin/env Rscript

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
