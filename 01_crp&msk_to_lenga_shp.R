library(raster)
library(rgdal)

files <- list.files("~/ROCCO/PROJECTS/16b_trapananda_TS/00_MOD13_QA_testt/00_rawdata", pattern = glob2rx("*.tif"), full.names =T) 
r1 <- raster(files[1])

lenga <- readOGR(dsn = path.expand("~/ROCCO/PROJECTS/16b_trapananda_TS/shp"), layer = "Lenga_distribution_UTMWGS8419S")
cuenca <- readOGR(dsn = path.expand("~/ROCCO/PROJECTS/16b_trapananda_TS/shp"), layer = "subcuenca_manihuales")


prj <- projection(r1)
lengaprj <- spTransform(lenga, "+proj=utm +zone=19 +south +ellps=WGS84 +units=m +no_defs")
cuencaprj <- spTransform(cuenca, "+proj=utm +zone=19 +south +ellps=WGS84 +units=m +no_defs")
projection(r1)

names <- substr(files, 79, 94)
dir <- "~/ROCCO/PROJECTS/16b_trapananda_TS/00_MOD13_QA_testt/02_msk_lenga"

outnames <- paste(dir, "/", "SC_manihuales_", names, "_lenga.tif", sep = "")

for (i in 1:length(files)){
  r1 <- raster(files[i], band = 1)
  r2 <- raster(files[i], band = 2)
  r3 <- raster(files[i], band = 3)
  r4 <- raster(files[i], band = 4)
  
  crp1 <- crop(r1, cuencaprj)
  crp2 <- crop(r2, cuencaprj)
  crp3 <- crop(r3, cuencaprj)
  crp4 <- crop(r4, cuencaprj)
  
  crp1 <- mask(crp1, cuencaprj)
  crp2 <- mask(crp2, cuencaprj)
  crp3 <- mask(crp3, cuencaprj)
  crp4 <- mask(crp4, cuencaprj)
  
  msk1 <- mask(crp1, lengaprj)
  msk2 <- mask(crp2, lengaprj)
  msk3 <- mask(crp3, lengaprj)
  msk4 <- mask(crp4, lengaprj)
  
  st <- stack(msk1, msk2, msk3, msk4)
  
  writeRaster(st, outnames[i], overwrite = T)
}











