##############################################################################
######## Code to resample 10m DEM for the Cape Peninsula to the 30m Landsat grid
######## Also trims 2009 and resamples Landcover for the Western Cape to the Landsat grid
##############################################################################
######## Compiled by Jasper Slingsby 2014
######## Last edited: 5 July 2014
##############################################################################
######## 
###Steps:
##############################################################################

#install.packages(c("Raster", "sp"))

library(raster,sp)

###Set working directory to local directory to get data - NOTE - you need to set wd to save data at the end too!!!
if (Sys.getenv("USER")=='jasper') setwd("/Users/jasper/Documents/GIS/CapePeninsula") #use "USER" for Mac
if (Sys.getenv("USERNAME")=='whoeveryouare') setwd ("C:/") #for PC

###Get data 10m digital elevation model (DEM) and Landsat tile covering the Cape Peninsula
d=raster("DEM10m_LO19/cct_10m",band=1) #10m DEM in LO19 projection
#d
lc=raster("landcover/landcover")
#lc; slotNames(lc); slotNames(lc@data); lc@data@attributes
lc_meta=lc@data@attributes #save metadata

#l=raster("LC81750842014115LGN00/LC81750842014115LGN00_B1.TIF",band=1) #Landsat tile in UTM - dropped in favour of using GEE output to set projections and extents
##l

l=stack("/Users/jasper/Dropbox/Shared/Postfire_workshop/data/clean/landsat/20140630_54ef46e7ea_LC8_L1T_ANNUAL_GREENEST_TOA__2013-2014-0000000000-0000000000.tif")[[1]]
l=crop(l, extent(250000, 270210, 6189390, 6247260))
values(l)=1:length(l) #replace Landsat band values with an index - needed for when tracking spatial location of data when in table format

###Crop Landsat tile to the Peninsula
#ext=extent(c(xmin=250000, xmax=270000, ymin=-3810000, ymax=-3750000)) #crop to just the region of interest to limit size of output shapefile
#l=crop(l,ext)
#values(l)=1:length(l) #replace Landsat band values with an index - needed for when tracking spatial location of data when in table format

###Resample and reproject 10m DEM to the 30m Landsat grid
d=projectRaster(d,l) #note that function uses bilinear interpolation by default. Can be a little slow...

###Resample and reproject landcover raster to the 30m Landsat grid
lc=projectRaster(lc,l, method="ngb") #note that we set method to nearest neighbour because the numbers (1:7) represent categories

###Set working directory to Dropbox to write data - PLEASE DO NOT WRITE FILES UNLESS ABSOLUTELY NECESSARY!!!
if (Sys.getenv("USER")=='jasper') setwd("/Users/jasper/Dropbox/Shared/Postfire_workshop/data/clean") #use "USER" for Mac
if (Sys.getenv("USERNAME")=='whoeveryouare') setwd ("C:/") #for PC

writeRaster(d, "dem_landsat_30m.grd") #you will get a warning that you need to add the code ", overwrite=TRUE" to the function
writeRaster(l, "indexgrid_landsat_30m.grd") #you will get a warning that you need to add the code ", overwrite=TRUE" to the function
writeRaster(lc, "landcover2009_landsat_30m.grd") #you will get a warning that you need to add the code ", overwrite=TRUE" to the function
write.csv(lc_meta, "landcover2009_classes.csv", row.names=FALSE)
