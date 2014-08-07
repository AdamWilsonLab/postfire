### Setup script:
##     1 loads necessary libraries
##     2 Sets parameters for your machine (working directory, path to dropbox, etc.)
##     3 Sets some graphical parameters for making plots

########################################
### Load libraries
libs=c(
  "doMC",
  "rasterVis",
  "rgdal",
  "reshape2",
  "sp",
  "knitr",
  "rmarkdown",
  "spgrass6",
  "ggplot2",
  "dplyr",
  "minpack.lm",
  "rjags")

lapply(libs, require, character.only=T)

########################################
### Set some parameters for your machine

### Working directory (should be the root of the repository)
# "USER" for Mac
Sys.getenv()
if (Sys.getenv("USER")=='adamw') {
  setwd("/Users/adamw/repos/postfire") 
  ## path to shared Dropbox folder that has the source data
  ## Never write anythiing to this folder!
  datadir="/Users/adamw/Dropbox/Postfire_workshop/Data/"
  ## bath to GRASS executables
  gisbase="/Applications/GRASS-6.4.app/Contents/MacOS"
  ## Machine details
  ncores=3   # the number of cores you want to use for parallel processing
}  
  
  # "USERNAME" for PC
if (Sys.getenv("USERNAME")=='whoeveryouare') setwd ("C:/") 


registerDoMC(ncores)

#########################################
## Package settings

## Set raster options
rasterOptions(format="GTiff", overwrite=T)

## knitr options - a function to add figure captions in HTML output 
## from http://stackoverflow.com/questions/15010732/caption-in-the-html-output-of-knitr
htmlcap = function(before, options, envir) {if(!before) {
  paste('<p class="caption">',options$htmlcap,"</p>",sep="")
}
}
opts_knit$set(root.dir=getwd(),cache=T,base.url = NULL,htmlcap=htmlcap,purl = hook_purl)

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


presentation_theme <- theme_gray()+
  theme(text = element_text(size = 18)) 
  
theme_set(presentation_theme)
