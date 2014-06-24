########################################################
### Calculate potential solar radiation

dem=as(raster(paste0(datadir,"/Clean/dem_landsat_30m.gri")),"SpatialGridDataFrame")

initGRASS(gisbase,SG=dem,home="data/tmp",gisDbase="data/tmp",location="CapePoint",mapset="PERMANENT",override=T)
## update projection to use the landsat grid
execGRASS("g.proj", flags="c",proj4=proj4string(dem))
# import the dem data
writeRAST6(dem, "dem", zcol = 1, NODATA=NULL, ignore.stderr = NULL, useGDAL=NULL, overwrite=TRUE, flags=NULL)
execGRASS("g.region", rast="dem")

## import the data
#execGRASS("r.in.gdal", flags="overwrite",input=,output="dem")

execGRASS("r.slope.aspect",flags="overwrite",elevation="dem",slope="slope",aspect="aspect")


#######################
### Radiation model

## build table of dates to process
startdate=as.Date("2000-01-15")
stopdate=as.Date("2000-12-31")
tsteps=364

dates=data.frame(date=(seq(startdate,stopdate,len=tsteps)))
dates$doy=format(dates$date,"%j")
dates$month=as.numeric(format(dates$date,"%m"))
dates$season=ifelse(dates$month%in%c(12,1,2),"Summer",ifelse(dates$month%in%c(3,4,5),"Fall",ifelse(dates$month%in%c(6,7,8),"Winter","Spring")))
## loop through days and calculate radiation

foreach(i=1:nrow(dates)) %dopar% {
  day=dates$doy[i]
  print(paste0("Processing day ",day))
       execGRASS("r.sun",flags=c("s","overwrite"),
                 elevin="dem",aspin="aspect",slopein="slope",
                 day=as.numeric(day),lin=3.0,step=2,dist=1,
                 glob_rad=paste0("rad_tot.",day))
}

### summarize by month
foreach(m=unique(dates$month)) %dopar% {
    execGRASS("r.series",flags="overwrite",input=paste0("rad_tot.",dates$doy[dates$month==m],collapse=","),
          output=paste0("rad_month_",m),method="average")
    execGRASS("r.out.gdal",input=paste0("rad_month_",m),output=paste0(datadir,"/Clean/rad_",m,".tif"),
              createopt="COMPRESS=LZW",createopt="zlevel=9", type="UInt16")
}

