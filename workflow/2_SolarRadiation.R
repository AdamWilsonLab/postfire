########################################################
### Calculate potential solar radiation using the r.sun 

## load the elevation dataset
dem=as(raster(paste0(datadir,"/Clean/dem_landsat_30m.gri")),"SpatialGridDataFrame")


## Initialize a temporary grass session in temporary dir.
initGRASS(gisbase,SG=dem,override=T,location="CapePoint",mapset="PERMANENT")
## by default this will create this in the tempdir(), you can change that by setting home and gisDbase

## update projection to use the grid of the dem
execGRASS("g.proj", flags="c",proj4=proj4string(dem))
# import the dem data into grass
writeRAST6(dem, "dem", zcol = 1, NODATA=NULL, ignore.stderr = NULL, useGDAL=NULL, overwrite=TRUE, flags=NULL)
## If the dem is in a format gdal can read directly, you can do this instead...
#execGRASS("r.in.gdal", flags="overwrite",input=,output="dem")

## set the region to match the elevation dataset
## if you want to only process a subset of the dem, specify it here
execGRASS("g.region", rast="dem")


## calculate slope and aspect
execGRASS("r.slope.aspect",flags="overwrite",elevation="dem",slope="slope",aspect="aspect")

#######################
### Radiation model

## year doesn't matter, only the day-month
startdate=as.Date("2001-01-01")
stopdate=as.Date("2001-12-31")

## specify the frequency of the radiation calculation. 
## "day" is best (daily estimates averaged by month), but "month" is much faster
by="month"

## build table of dates to process
dates=data.frame(date=(seq(startdate,stopdate,by=by)))
dates$doy=format(dates$date,"%j")
dates$month=as.numeric(format(dates$date,"%m"))
dates$season=ifelse(dates$month%in%c(12,1,2),"Summer",
                    ifelse(dates$month%in%c(3,4,5),"Fall",
                           ifelse(dates$month%in%c(6,7,8),"Winter","Spring")))

## confirm the dates table looks ok, we'll estimate the daily total solar radiation for each row in this table:
head(dates)

## loop through days and calculate radiation
foreach(i=1:nrow(dates)) %dopar% {
  day=dates$doy[i]
  print(paste0("Processing day ",day))
       execGRASS("r.sun",flags=c("s","overwrite"),
                 elevin="dem",aspin="aspect",slopein="slope",
                 day=as.numeric(day),lin=3.0,step=2,dist=1,
                 glob_rad=paste0("rad_tot.",day))
  return(day)
}


### summarize by month and write out a tiff to the data directory
foreach(m=unique(dates$month)) %dopar% {
    execGRASS("r.series",flags="overwrite",input=paste0("rad_tot.",dates$doy[dates$month==m],collapse=","),
          output=paste0("rad_month_",m),method="average")
    ## add a color bar - not necessary, but nice for easy plotting
    execGRASS("r.colors",map=paste0("rad_month_",m),color="bgyr",flags="e")
    ## write the data out to a tif
    execGRASS("r.out.gdal",input=paste0("rad_month_",m),output="/data/rad/rad_",m,".tif"),
              createopt="COMPRESS=LZW",createopt="zlevel=9", type="UInt16")
}


## remove grass working folder
system(paste0("rm -rf ",tempdir(),"/CapePoint"))
