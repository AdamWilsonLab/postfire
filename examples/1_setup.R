
########################################
### Set some parameters for your machine

### Working directory (should be the root of the repository)
setwd('~/repos/postfire')

## path to shared folder that has the data
datadir="/Users/adamw/GoogleDrive/Work/ZA_2014/workshop/Data/"

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

## load grass
library(spgrass6)

## get current working directory as an object to feed knitr
cwd=getwd()


########################################
### Define graphical parameters
cndvi=function(br=0.2,c1=c("darkgrey","burlywood4"),c2=c("burlywood4","darkgreen","green")){
  at=c(seq(-1,0,len=50),seq(0.01,1,len=50))
  bg=colorRampPalette(c1)
  gr=colorRampPalette(c2)
  return(list(at=at,col=c(bg(sum(at<br)),gr(sum(at>=br)))))
}
