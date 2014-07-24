# DataPrep
Jasper Slingsby & Adam M. Wilson  
July 22, 2014  




This script assembles various environmental layers into a common 30m grid for the Cape Peninsula.  It also calculates veg age based on the fire data.

## Index raster
Create a raster of an index grid (`ig`) to spatially connect all the datasets.

```r
ig=raster(paste0(datadir,"clean/indexgrid_landsat_30m.grd")) 
```

## Vegetation 


```r
rv=readOGR(dsn=paste0(datadir,"raw/VegLayers/Vegetation_Indigenous_Remnants"), layer="Vegetation_Indigenous_Remnants") 
```

```
## OGR data source with driver: ESRI Shapefile 
## Source: "/Users/adamw/Dropbox/Postfire_workshop/Data/raw/VegLayers/Vegetation_Indigenous_Remnants", layer: "Vegetation_Indigenous_Remnants"
## with 3428 features and 7 fields
## Feature type: wkbPolygon with 2 dimensions
```

```r
#remnant veg layer - readOGR() reads shapefiles
#rv; summary(rv$National_); summary(rv$Subtype); summary(rv$Community); levels(rv@data$National_)
rv_meta=data.frame(1:length(levels(rv@data$National_)), levels(rv@data$National_)) #save VegType metadata
colnames(rv_meta)=c("ID", "code") #rename columns
write.csv(rv_meta, "data/vegtypecodes.csv", row.names=F)

# reproject to the CRS of the Landsat index grid (UTM 34S)
rv=spTransform(rv,CRS(proj4string(ig)))
```

Extract the national veg types from the veg layer into a 30m raster based on the index grid

```r
rvrfile="data/vegtypes_landsat_30m.tif"
if(!file.exists(rvrfile))
  rvr=rasterize(rv, ig, field=c("National_"), fun="max",file=rvrfile) #get national veg type for each cell
## read it back in and 'factorize' it
rvr=raster(rvrfile)
rvr=as.factor(rvr)
levels(rvr)=rv_meta[levels(rvr)[[1]]$ID,]
#levelplot(rvr,col.regions=rainbow(nrow(rv_meta),start=.3))
```

Count number of veg types for each cell (i.e. ID mixed cells)

```r
rvcfile="data/count_vegtypes_landsat_30m.tif"
if(!file.exists(rvcfile))
  rvc=rasterize(rv, ig, field=c("National_"), fun="count",file=rvcfile) 
rvc=raster(rvcfile)
```

Are there any mixed cells?


```r
table(values(rvc))
```

```
## 
##      1 
## 342151
```

## Fire data

```r
fi=readOGR(dsn=paste0(datadir,"raw/Fire"), layer="CapePenFires") #Cape Peninsula fires history layers 1962-2007
```

```
## OGR data source with driver: ESRI Shapefile 
## Source: "/Users/adamw/Dropbox/Postfire_workshop/Data/raw/Fire", layer: "CapePenFires"
## with 4578 features and 9 fields
## Feature type: wkbPolygon with 3 dimensions
```

```
## Warning: Z-dimension discarded
```

```r
fi=spTransform(fi,CRS(proj4string(ig)))

### Extract fire history data and convert to a 30m raster
fi$STARTDATE[which(fi$STARTDATE==196201001)]=19620101#fix an anomalous date...

#Raster showing total numbers of fires in each grid cell
## note the if(!file.exists)) first checks if the file already exists so you don't rerun this everytime you run the script.
ficfile="data/fires_number_1962to2007_landsat_30m.tif"
if(!file.exists(ficfile))
    fic=rasterize(fi, ig, field=c("STARTDATE"), fun="count",file=ficfile) 

fic=raster(ficfile)
```



### Rasterize fire data into annual fire maps 

```r
years=sort(unique(fi$YEAR)) #get the unique list of years in which fires occurred
years=1962:2014
#years=years[years>1981] #trim to 1982 onwards (our earliest reliable Landsat data)

## first check if file already exists, if not, run this
rfifile="data/fires_annual_landsat_30m.tif"
if(!file.exists(rfifile)) {
rfi=foreach(y=years,.combine=stack,.packages="raster") %dopar% {
 #loop through years making a raster of burnt cells (1/0) for each
  ## check if there were any fires that year, if not, return zeros
  if(sum(fi$YEAR==y)==0) 
      td= raster(extent(ig),res=res(ig),vals=0)
  ## if there is >0 fires, then rasterize it to the grid
  if(sum(fi$YEAR==y)>0) 
      td=rasterize(fi[which(fi$YEAR==y),],ig, field="YEAR", fun="count", background=0) 
  ## return the individual raster
  return(td)
  }
writeRaster(rfi,file=rfifile)#,options=c("COMPRESS=LZW","PREDICTOR=2"))
}

rfi=stack(rfifile)
## add year as name
names(rfi)=paste0("Fire_",years)
```


```r
levelplot(rfi[[30:40]],scales=list(draw=F),at=c(0,0.5,1),col.regions=c("transparent","red"),auto.key=F,maxpixels=1e4)
```

![plot of chunk fireplot](./DataPrep_files/figure-html/fireplot.png) 


### Calculate veg age from fire history
Now we have an object `rfi` (rasterized fires) with one band/layer for each year with 0s and 1s indicating whether that pixel burned in that year.  We can use that to calculate the time since fire by setting the year that burned to 0 and adding 1 for each subsequent year until the next fire.  

First let's look at one pixel's data:


```r
x=as.vector(rfi[551072])
x2=rbind(fire=x)
colnames(x2)=years
kable(x2)
```



|     | 1962| 1963| 1964| 1965| 1966| 1967| 1968| 1969| 1970| 1971| 1972| 1973| 1974| 1975| 1976| 1977| 1978| 1979| 1980| 1981| 1982| 1983| 1984| 1985| 1986| 1987| 1988| 1989| 1990| 1991| 1992| 1993| 1994| 1995| 1996| 1997| 1998| 1999| 2000| 2001| 2002| 2003| 2004| 2005| 2006| 2007| 2008| 2009| 2010| 2011| 2012| 2013| 2014|
|:----|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|
|fire |    0|    0|    0|    0|    0|    0|    0|    0|    0|    0|    1|    0|    0|    1|    0|    0|    0|    0|    0|    0|    0|    0|    0|    0|    0|    0|    0|    0|    0|    0|    1|    0|    0|    0|    0|    0|    0|    1|    0|    0|    0|    0|    0|    0|    0|    0|    0|    0|    0|    0|    0|    0|    0|


So we need a function that finds the fires and counts up each year.  We'll put in years before the first fire as negatives so we can identify them later.


```r
fage=function(x){
  ## if there are no fires, return all negative numbers
  if(sum(x)==0){return(-1:(-length(x)))}
  if(sum(x)>0){
    ## create empty vector of veg ages
    tage=rep(NA,length(years))
    ## years with fire
    fids=which(x>0)  
    tage[fids]=0
    ## fill in years before first fire
    tage[1:fids[1]]=-1:(-fids[1])
    ## Now loop through years and count up unless there was a fire
    for(i in (fids[1]+1):length(years))
    tage[i]=ifelse((i-1)%in%fids,0,tage[i-1]+1)
return(tage)
}}
```
Now let's try that: 

```r
x=as.vector(rfi[551072])
x2=rbind(fire=x,age=fage(x))
colnames(x2)=years
kable(x2)
```



|     | 1962| 1963| 1964| 1965| 1966| 1967| 1968| 1969| 1970| 1971| 1972| 1973| 1974| 1975| 1976| 1977| 1978| 1979| 1980| 1981| 1982| 1983| 1984| 1985| 1986| 1987| 1988| 1989| 1990| 1991| 1992| 1993| 1994| 1995| 1996| 1997| 1998| 1999| 2000| 2001| 2002| 2003| 2004| 2005| 2006| 2007| 2008| 2009| 2010| 2011| 2012| 2013| 2014|
|:----|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|
|fire |    0|    0|    0|    0|    0|    0|    0|    0|    0|    0|    1|    0|    0|    1|    0|    0|    0|    0|    0|    0|    0|    0|    0|    0|    0|    0|    0|    0|    0|    0|    1|    0|    0|    0|    0|    0|    0|    1|    0|    0|    0|    0|    0|    0|    0|    0|    0|    0|    0|    0|    0|    0|    0|
|age  |   -1|   -2|   -3|   -4|   -5|   -6|   -7|   -8|   -9|  -10|  -11|    0|    1|    2|    0|    1|    2|    3|    4|    5|    6|    7|    8|    9|   10|   11|   12|   13|   14|   15|   16|    0|    1|    2|    3|    4|    5|    6|    0|    1|    2|    3|    4|    5|    6|    7|    8|    9|   10|   11|   12|   13|   14|

Now use `calc` to apply that to the full stack.

```r
agefile="data/ages_annual_landsat_30m.tif"
if(!file.exists(agefile))
    age=calc(rfi,fage,file=agefile,progress='text',dataType="INT1S")
age=stack(agefile)
names(age)=paste0("age_",years)
age=setZ(age,years)
```


```r
levelplot(age[[30:40]],at=seq(0,53,len=100),col.regions=rainbow(100,start=.3),scales=list(draw=F),auto.key=F,
          main="Veld age through time",maxpixels=1e4)
```

![plot of chunk fireanim](./DataPrep_files/figure-html/fireanim.png) 


## NDVI Compositing


```r
getNDVI=function(file,years,prefix){
  ndvi=stack(paste0(datadir,"raw/NDVI/",file))
  NAvalue(ndvi)=0
offs(ndvi)=-2
gain(ndvi)=.001
names(ndvi)=paste0(prefix,years)
ndvi=setZ(ndvi,years)
}
```

Now use the function to read in the data and add the relevant metadata.

```r
l4=getNDVI(file="20140722_26dbab02_LT4_L1T_ANNUAL_GREENEST_TOA__1982-1993-0000000000-0000000000.tif",
           years=1982:1993,prefix="L4_")
l5=getNDVI(file="20140722_26dbab02_LT5_L1T_ANNUAL_GREENEST_TOA__1984-2012-0000000000-0000000000.tif",
           years=1984:2012,prefix="L5_")
l7=getNDVI(file="20140722_26dbab02_LE7_L1T_ANNUAL_GREENEST_TOA__1999-2014-0000000000-0000000000.tif",
           years=1999:2014,prefix="L7_")
l8=getNDVI(file="20140722_26dbab02_LC8_L1T_ANNUAL_GREENEST_TOA__2013-2014-0000000000-0000000000.tif",
           years=2013:2014,prefix="L8_")
```

Let's check out one of the LANDSAT objects.  Raster provides a summary by just typing the object's name:

```r
l7
```

```
## class       : RasterStack 
## dimensions  : 1929, 674, 1300146, 16  (nrow, ncol, ncell, nlayers)
## resolution  : 30, 30  (x, y)
## extent      : 249990, 270210, 6189390, 6247260  (xmin, xmax, ymin, ymax)
## coord. ref. : +proj=utm +zone=34 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 
## names       : L7_1999, L7_2000, L7_2001, L7_2002, L7_2003, L7_2004, L7_2005, L7_2006, L7_2007, L7_2008, L7_2009, L7_2010, L7_2011, L7_2012, L7_2013, ... 
## min values  :  -34.77,  -34.77,  -34.77,  -34.77,  -34.77,  -34.77,  -34.77,  -34.77,  -34.77,  -34.77,  -34.77,  -34.77,  -34.77,  -34.77,  -34.77, ... 
## max values  :   30.77,   30.77,   30.77,   30.77,   30.77,   30.77,   30.77,   30.77,   30.77,   30.77,   30.77,   30.77,   30.77,   30.77,   30.77, ... 
## time        : 1999 - 2014 (range)
```

And a plot of a few different years:


```r
tyears=2002:2005
yearind=which(getZ(l5)%in%tyears)
levelplot(l5[[yearind]],col.regions=cndvi()$col,cuts=length(cndvi()$at),at=cndvi()$at,layout=c(length(yearind),1),scales=list(draw=F),maxpixels=1e4)
```

![plot of chunk landsatplot](./DataPrep_files/figure-html/landsatplot.png) 


There is some temporal overlap between sensors, let's look at that:

```r
tl=melt(list(l4=getZ(l4),l5=getZ(l5),l7=getZ(l7),l8=getZ(l8)))
xyplot(as.factor(L1)~value,data=tl,type="l",groups=as.factor(L1),asp=.15,lwd=5,ylab="LANDSAT Satellite",xlab="Year")
```

![Timeline of LANDSAT data by sensor](./DataPrep_files/figure-html/landsateras.png) 

There are several ways these data could be combined.  The individual scenes could be assessed for quality (cloud contamination, etc.), sensors could be weighted by sensor quality (newer=better?).  Today, we'll simply take the maximum of available data for each year.  

      What are the limitations of this approach? 


```r
nyears=1984:2014

ndvifile="data/ndvi_annual_landsat_30m.tif"
if(!file.exists(ndvifile)){

  ndvi=foreach(y=nyears,.combine=stack,.packages="raster") %dopar% {
    # find which LANDSATs have data for the desired year
    w1=lapply(
      list(l4=getZ(l4),l5=getZ(l5),l7=getZ(l7),l8=getZ(l8)),
      function(x) ifelse(y%in%x,which(y==x),NA))
    # drop LANDSATs with no data for this year
    w2=w1[!is.na(w1)]
    # make a stack with the desired year for all sensors that have data
    tndvi=max(stack(lapply(1:length(w2),function(i) {
        print(i)
      td=get(names(w2[i]))
      return(td[[w2[i]]])
    })),na.rm=T)
    return(tndvi)
    }
  writeRaster(ndvi,file=ndvifile,overwrite=T)
}

ndvi=stack(ndvifile)
names(ndvi)=paste0("ndvi_",nyears)
ndvi=setZ(ndvi,nyears)
```



```r
tyears=1984:2014
yearind=which(getZ(ndvi)%in%tyears)

levelplot(ndvi[[yearind]],col.regions=cndvi()$col,cuts=length(cndvi()$at),
          at=cndvi()$at,margin=F,scales=list(draw=F),
          names.attr=getZ(ndvi)[yearind],maxpixels=1e4)
```

![Merged annual maximum LANDSAT NDVI](./DataPrep_files/figure-html/ndviplot.png) 

# Data Compilation

## Select domain of interest
Here we will define the subset of cells that we will explore further.  You can fiddle with these settings to include fewer (or more) cells.  If your computer is slow, you may want to subset this further.


```r
## load data for masking
cover=raster(paste0(datadir,"clean/landcover2009_landsat_30m.gri"))

maskfile="data/mask_landsat_30m.tif"
if(!file.exists(maskfile)){
    mask=overlay(cover,fic,fun=function(x,y) x==1&y>0,filename=maskfile)
}
mask=raster(maskfile)

## load additional covariate data
tmax=raster(paste0(datadir,"clean/Tmax_jan_mean.gri"))
tmin=raster(paste0(datadir,"clean/Tmin_jul_mean.gri"))
tpi=raster(paste0(datadir,"clean/tpi500.gri"))
dem=raster(paste0(datadir,"clean/dem_landsat_30m.gri"))

### Make a dataframe of all spatial data
## Beware, this approach will only work if your data are all in identical projection/grid/etc.
maskids=which(values(mask)==1)
              
sdat=data.frame(
  id=extract(ig, maskids),
  coordinates(ig)[maskids,],
  veg=extract(rvr, maskids),
  cover=extract(cover, maskids),
  tmax=extract(tmax, maskids),
  tmin=extract(tmin, maskids),
  dem=extract(dem, maskids),
  tpi=extract(tpi, maskids)
)

kable(head(sdat))
```



|    id|      x|       y| veg| cover|  tmax|   tmin|   dem|   tpi|
|-----:|------:|-------:|---:|-----:|-----:|------:|-----:|-----:|
| 83925| 260445| 6243525|  18|     1| 28.19|  9.413| 152.5| 7.934|
| 84598| 260415| 6243495|  18|     1| 28.40|  9.556| 150.5| 7.260|
| 84599| 260445| 6243495|  18|     1| 28.19|  9.459| 149.6| 6.183|
| 84600| 260475| 6243495|  18|     1| 28.52|  9.779| 146.9| 5.759|
| 84601| 260505| 6243495|  18|     1| 28.79| 10.022| 138.2| 5.770|
| 85271| 260385| 6243465|  18|     1| 28.50|  9.714| 139.8| 5.918|


## Temporally varying data

```r
## subset age to years with ndvi data
age=age[[which(getZ(age)%in%getZ(ndvi))]]

tdat=data.frame(
  id=extract(ig, maskids),
  extract(age, maskids),
  extract(ndvi,maskids)
  )
kable(tdat[1:10,1:10])
```



|    id| age_1984| age_1985| age_1986| age_1987| age_1988| age_1989| age_1990| age_1991| age_1992|
|-----:|--------:|--------:|--------:|--------:|--------:|--------:|--------:|--------:|--------:|
| 83925|      -23|      -24|      -25|      -26|      -27|      -28|      -29|      -30|      -31|
| 84598|      -23|      -24|      -25|      -26|      -27|      -28|      -29|      -30|      -31|
| 84599|      -23|      -24|      -25|      -26|      -27|      -28|      -29|      -30|      -31|
| 84600|      -23|      -24|      -25|      -26|      -27|      -28|      -29|      -30|      -31|
| 84601|      -23|      -24|      -25|      -26|      -27|      -28|      -29|      -30|      -31|
| 85271|      -23|      -24|      -25|        0|        1|        2|        3|        4|        5|
| 85272|      -23|      -24|      -25|      -26|      -27|      -28|      -29|      -30|      -31|
| 85273|      -23|      -24|      -25|      -26|      -27|      -28|      -29|      -30|      -31|
| 85274|      -23|      -24|      -25|      -26|      -27|      -28|      -29|      -30|      -31|
| 85275|      -23|      -24|      -25|      -26|      -27|      -28|      -29|      -30|      -31|

### Reshape temporal data
It's often easier to work with data in 'long' format where there is one row for each observation and another column indicating what the observation is.  Let's `melt` the data to 'long' format.

```r
tdatl=melt(tdat,id.var="id")
tdatln=cbind.data.frame(lab=levels(tdatl$variable),do.call(rbind,strsplit(as.character(levels(tdatl$variable)),"_")))
tdatl[,c("type","year")]=tdatln[match(tdatl$variable,tdatln$lab),2:3]
tdatl=dcast(tdatl,id+year~type,value.var="value")

kable(head(tdatl),row.names = F)
```



|    id|year | age|   ndvi|
|-----:|:----|---:|------:|
| 83925|1984 | -23| 0.2790|
| 83925|1985 | -24| 0.5820|
| 83925|1986 | -25| 0.4830|
| 83925|1987 | -26| 0.3160|
| 83925|1988 | -27| 0.3080|
| 83925|1989 | -28| 0.4155|

Save it as an R data object for later use.

```r
save(sdat,tdat,tdatl,file="data/modeldata.Rdata")
```



