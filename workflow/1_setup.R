
########################################
### Set some parameters for your machine

### Working directory (should be the root of the repository)

if (Sys.getenv("USER")=='jasper') setwd("/Users/jasper/Dropbox/Shared/Postfire_workshop/data") #use "USER" for Mac
if (Sys.getenv("USER")=='adamw') setwd("/Users/adamw/repos/postfire") #use "USER" for Mac

if (Sys.getenv("USERNAME")=='whoeveryouare') setwd ("C:/") #for PC


## path to shared folder that has the source data
## Never write anythiing to this folder!
datadir="/Users/adamw/Dropbox/Postfire_workshop/Data/"

## temporary working directory
## this is where temporary files will be written
tmpdir="data/tmp"
if(!file.exists(tmpdir)) dir.create(tmpdir)


## Machine details
ncores=3   # the number of cores you want to use for parallel processing

## bath to GRASS executables
gisbase="/Applications/GRASS-6.4.app/Contents/MacOS"

########################################
### Load libraries
library(foreach)
library(doMC)
registerDoMC(ncores)
library(raster)
library(rasterVis)
library(rgdal)
library(reshape2)
library(sp)
library(knitr);library(rmarkdown)
library(animation)
## load grass
library(spgrass6)

## get current working directory as an object to feed knitr
cwd=getwd()


### Set raster options
rasterOptions(format="GTiff", overwrite=T)


########################################
### Define graphical parameters

## 16 bit NDVI color bar
cndvi=function(br=0.2,c1=c("darkgrey","burlywood4"),c2=c("burlywood4","darkgreen","green")){
  at=unique(c(seq(-1,0,len=32768),seq(0,1,len=32768)))
  bg=colorRampPalette(c1)
  gr=colorRampPalette(c2)
  return(list(at=at,col=c(bg(sum(at<br)),gr(sum(at>=br)))))
}
ndvi.colors=cndvi()$col


### import Peninsula polygon for nice plotting
