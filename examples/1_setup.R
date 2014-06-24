

## path to shared folder
datadir="/Users/adamw/GoogleDrive/Work/ZA_2014/workshop/Data/"

library(foreach)
library(doMC)
registerDoMC(3)

library(raster)
library(spgrass6)
gisbase="/Applications/GRASS-6.4.app/Contents/MacOS"
