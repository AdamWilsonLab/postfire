#' NDVI Exploration
#' ========================================================
#' 
#' Let's explore the LANDSAT data.
#' 
## ----,setup,echo=F,cache=F,results='hide',message=FALSE------------------
##  First some set up
source("../1_setup.R")
opts_chunk$set(cache=T,root.dir="../..",upload.fun = imgur_upload, base.url = NULL)

#' 
#' 
#' 
#' # Data
#' 
#' ## Load LANDSAT data
## ------------------------------------------------------------------------
# LANDSAT 4
L4=stack(paste0(datadir,"ee_ZA_output/20140630_54ef46e7ea_LT4_L1T_ANNUAL_GREENEST_TOA__1982-1993-0000000000-0000000000.tif"))
NAvalue(L4)=0
gain(L4)=.01
names(L4)=paste0("Y",1982:1993)
L4=setZ(L4,1982:1993)

# LANDSAT 5
L5=stack(paste0(datadir,"ee_ZA_output/20140630_54ef46e7ea_LT5_L1T_ANNUAL_GREENEST_TOA__1984-2012-0000000000-0000000000.tif"))
NAvalue(L5)=0
gain(L5)=.01
names(L5)=paste0("Y",1984:2012)
L5=setZ(L5,1984:2012)

# LANDSAT 7
L7=stack(paste0(datadir,"ee_ZA_output/20140630_54ef46e7ea_LE7_L1T_ANNUAL_GREENEST_TOA__1999-2014-0000000000-0000000000.tif"))
NAvalue(L7)=0
gain(L7)=.01
names(L7)=paste0("Y",1999:2014)
L7=setZ(L7,1999:2014)

# LANDSAT 8
L8=stack(paste0(datadir,"ee_ZA_output/20140630_54ef46e7ea_LC8_L1T_ANNUAL_GREENEST_TOA__2013-2014-0000000000-0000000000.tif"))
gain(L8)=.01
NAvalue(L8)=0
names(L8)=paste0("Y",2013:2014)
L8=setZ(L8,2013:2014)

#' 
#' 
#' Let's check out one of the LANDSAT objects.  Raster provides a summary by just typing the object's name:
## ------------------------------------------------------------------------
L7

#' 
#' 
#' 
#' And a plot of the first year:
## ----fig.width=7, fig.height=6-------------------------------------------
levelplot(L7[[1]],col.regions=cndvi()$col,cuts=length(cndvi()$at),at=cndvi()$at,margin=F)

#' 
#' 
#' 
#' That's strange, EarthEngine has added some columns to the tif to the West of the peninsula (see all that white space?).  Let's crop it.  First define an 'extent' in lat-lon coordinates and project them to 
## ------------------------------------------------------------------------
peninsula=extent(250000,270210,6189390,6247260)
L7=crop(L7,peninsula)

#' 
#' 
#' Let's try the plot again:
## ----fig.width=7, fig.height=6-------------------------------------------
levelplot(L7[[1]],col.regions=cndvi()$col,cuts=length(cndvi()$at),at=cndvi()$at,margin=F)

#' 
#' Much better.  Now let's look at the last few years.
#' 
## ----fig.width=7, fig.height=6-------------------------------------------
levelplot(L7[[12:16]],col.regions=cndvi()$col,cuts=length(cndvi()$at),at=cndvi()$at,layout=c(5,1))

#' 
#' 
#' ## Change through time
#' 
#' It's still hard to see change, so let's pick a pixel and plot NDVI as a function of time.  But first we need to pick a pixel (or a few) to plot.  There are a few ways to do this.  First let's try extracting values from a single cell:
## ------------------------------------------------------------------------
plot(c(L7[199165])~getZ(L7),type="l",ylab="NDVI",xlab="Year")

#' 
#' 
#' Or, we can use the `click` function (in Raster) that allows you to pick points on the map.  First we need to plot the data using the plot() command.
#' 
## ----,eval=FALSE---------------------------------------------------------
## ## first plot the data
## plot(L7[[1]])
## ## specify how many points you want to select
## nclicks=5
## d=click(L7,xy=T,n=nclicks,cell=T,id=TRUE,type="l")
## ## now click on the map to select your points
## 
## ## reshape the data for easy plotting
## d$id=1:nrow(d)
## d2=melt(d,id.var=c("x","y","cell","id"));colnames(d2)=c("X","Y","Point","id","Year","NDVI")
## d2$Year=as.numeric(sub("Y","",d2$Year))
## p1=xyplot(NDVI~Year,groups=id,data=d2,type="l",auto.key=list(x=0,y=1))
## p2=levelplot(L7[[1]],col.regions=cndvi()$col,cuts=length(cndvi()$at),at=cndvi()$at,margin=F)+
##   layer(panel.text(d$x,d$y,d$id,col="red",cex=2))
## print(c(p1,p2,merge.legends=T))

#' 
#' ### Exercise: 
#' 1. Explore the map for areas you are familiar with. Do you see any patterns?
#' 2. Can you identify any fires looking at the NDVI profiles?
#' 
#' 
## ----,purl,echo=FALSE,results='hide',messages=F,error=FALSE,background=T----
## this chunk outputs a copy of this script converted to a 'normal' R file with comments
purl("ExploreNDVI.Rmd",documentation=2,output = "ExploreNDVI.R", quiet = TRUE) 

#' 
