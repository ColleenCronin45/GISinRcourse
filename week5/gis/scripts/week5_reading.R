
#  create a SpatRaster object named my_rast

raster_filepath = system.file("raster/srtm.tif", package = "spDataLarge")

my_rast = terra::rast(raster_filepath)

class(my_rast)

terra::plot(my_rast)

# read in a raster file

single_raster_file = system.file("raster/srtm.tif", package = "spDataLarge")

single_rast = terra::rast(raster_filepath)

# create a new raster file:

new_raster = terra::rast(nrows = 6, ncols = 6, 
                  xmin = -1.5, xmax = 1.5, ymin = -1.5, ymax = 1.5,
                  vals = 1:36)

# multiple layers

multi_raster_file = system.file("raster/landsat.tif", package = "spDataLarge")

multi_rast = terra::rast(multi_raster_file)

multi_rast

# retrieve the number of layers stored in a SpatRaster object:

terra::nlyr(multi_rast)

# select layers:

multi_rast3 = terra::subset(multi_rast, 3)

multi_rast4 = terra::subset(multi_rast, "landsat_4")

# combining several SpatRaster objects into one:

multi_rast34 = c(multi_rast3, multi_rast4)
