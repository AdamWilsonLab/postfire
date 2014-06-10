#####################################
### Compilation of biomass accumulation data

setwd("/media/Data/Work/Regional/CFR/BiomassAccumulation/BiomassAccumulation_500m/analysis")

### Start GRASS
system("grass /media/Data/Work/grassdata/CFR/biomass_500m")
cd /media/Data/Work/Regional/CFR/BiomassAccumulation/BiomassAccumulation_500m/analysis
db.connect database=/media/Data/Work/grassdata/CFR/biomass_500m/sqlite.db driver=sqlite

### set resolution
gdalinfo /media/Data/Work/Regional/CFR/MODIS/MCD13Q1.nc
r.in.gdal -o band=1 input="NETCDF:/media/Data2/Data/MODIS/CFR/MOD13A1/2_NC/A2000049.nc:500m_16_days_NDVI" output=ndvi --o
#r.in.gdal -o band=1 input="NETCDF:/media/Data/Work/Regional/CFR/MODIS/MCD13A1.nc:500m_16_days_NDVI" output=ndvi --o
g.region -p rast=ndvi
r.colors -e map=ndvi color=ndvi

## Generate integer ID grid
r.info ndvi
r.mapcalc "ID=int((row()-1)*1660+col())"  #### update to number of columns in map!!!!
r.colors map=ID col=gyr

### Import fynbos biome
v.extract input=veg@PERMANENT output=fynbos type=boundary,centroid,area 'where=BIOME="Fynbos Biome" and GROUPNAME NOT in ("Estuarine Vegetation","Seashore Vegetation","Inland Saline Vegetation","Freshwater Wetlands","Waterbodies")' --o
#v.to.rast input=fynbos@biomass output=fynbos use=attr type=area column=GROUPID labelcolumn=GROUPNAME --o
v.to.rast input=fynbos output=fynbos use=val type=area value=1 --o

### Drop dams
v.to.rast input=dams@PERMANENT output=dams use=val type=area value=1 --o
r.buffer input=dams@biomass output=dams1km distances=1 units=kilometers --o

### Untransformed fynbos
r.resamp.stats -w input=transformed@PERMANENT output=transformed method=average --o
r.colors -e map=transformed color=grey1.0
r.mapcalc 'fynbos_untransformed=if(!isnull(dem@dem)&isnull(dams1km)&fynbos==1&transformed==0,1,null())'
r.out.gdal input=fynbos_untransformed format=GTiff output=fynbos_untransformed nodata=-3000
r.colors map=fynbos_untransformed color=ryb

### Identify large core patches of fynbos and drop the small ones
r.null map=fynbos_untransformed null=0
r.neighbors -c input=fynbos_untransformed output=patch method=minimum size=5 --o
r.null map=patch setnull=0 ; r.colors map=patch color=byg
r.to.vect input=patch output=patch feature=area --o
v.clean input=patch output=patch2 type=area tool=rmarea thresh=5000000 --o
g.rename vect=patch2,patch --o
v.to.rast input=patch output=patch2 col=cat
g.rename rast=patch2,patch --o

### resample dem data to 500m scale
r.resamp.stats -w input=dem@dem output=dem method=average --o
r.resamp.stats -w input=slope@dem output=slope method=average --o
r.resamp.stats -w input=aspect@dem output=aspect method=average --o
r.resamp.stats -w input=accumulationHa@dem output=accumulation method=average --o

### topography metrics
r.resamp.stats -w input=eastwest@dem output=eastwest method=average --o
r.resamp.stats -w input=northsouth@dem output=northsouth method=average --o
r.resamp.stats -w input=sinslope@dem output=sinslope method=average --o

## downsample climate data
r.resamp.interp input=tmean@climate output=tmean method=bicubic --o
r.resamp.interp input=map@climate output=map method=bicubic --o
r.resamp.interp input=tmin07@climate output=tmin07 method=bicubic --o
r.resamp.interp input=tmax01@climate output=tmax01 method=bicubic --o
r.resamp.interp input=HeatUnits@climate output=HeatUnits method=bicubic --o
r.resamp.interp input=frostdur@climate output=frostdur method=bicubic --o
r.resamp.interp input=pptconc@climate output=pptconc method=bicubic --o
r.resamp.interp input=mmp12@climate output=mmp12 method=bicubic --o
r.resamp.interp input=mmp01@climate output=mmp01 method=bicubic --o
r.resamp.interp input=mmp7@climate output=mmp7 method=bicubic --o
r.resamp.interp input=HeatWaves@climate output=heatwaves method=bicubic --o
r.resamp.interp input=apan@climate output=apan method=bicubic --o
r.resamp.interp input=apanevap_12@climate output=apan_12 method=bicubic --o

## Import radiation data
r.resamp.stats -w input=rad_ave.Fall@biomass output=rad_ave.Fall method=average --o
r.resamp.stats -w input=rad_ave.Spring@biomass output=rad_ave.Spring method=average --o
r.resamp.stats -w input=rad_ave.Summer@biomass output=rad_ave.Summer method=average --o
r.resamp.stats -w input=rad_ave.Winter@biomass output=rad_ave.Winter method=average --o
r.mapcalc "rad_ave=(rad_ave.Fall+rad_ave.Winter+rad_ave.Spring+rad_ave.Summer)/4"

## Rasterize and resample soils data from vector data
v.to.rast input=geol@PERMANENT output=soilfert use=attr type=area column=FERTILITY --o
v.to.rast input=geol@PERMANENT output=soiltext use=attr type=area column=TEXTURE  --o
v.to.rast input=geol@PERMANENT output=soilph use=attr type=area column=PH --o

r.null map=soilfert setnull=0  ; r.colors -e map=soilfert color=bgyr
r.null map=soiltext setnull=0  ; r.colors -e map=soiltext color=bgyr
r.null map=soilph setnull=0  ; r.colors -e map=soilph color=bgyr

### resample veg to 500m scale
r.resamp.stats -w input=veg@PERMANENT output=veg method=mode --o
echo "SELECT Distinct VEGTYPEID,GROUPID,NAME,GROUPNAME,BIOME FROM veg" | db.select fs="," database=/media/Data/Work/grassdata/CFR/PERMANENT/sqlite.db > vegtypes.csv


############################################################################################
############################################################################################
###  Write data to disk

### Add other explanatory characteristics for each pixel
### Climate, Elevation, aspect, radiation, slope, accumulation, soil, vegetation
R
library(spgrass6)

vars=c("accumulation","aspect","dem","rad_ave","slope","eastwest","northsouth","sinslope","soilfert","soiltext","soilph",
           "map","mmp12","mmp01","mmp7","tmax01","tmin07","apan","apan_12","heatwaves",
           "tmean","pptconc","transformed","veg","fynbos")

### write data to geotiffs for archiving
for(i in c("ID",vars)) system(paste("r.out.gdal input=",i,"@biomass_500m format=GTiff output=data/geotiffs/",i,".tif createopt=COMPRESS=LZW ",sep=""))

### Read in data as an R object
d=readRAST6("ID",plugin=T)
for(i in vars)  d@data[,i]=readRAST6(i,plugin=T)@data

d=as(d,"SpatialPixelsDataFrame") #15 minutes!

## add vegnames
vegtype=read.csv("data/vegtypes.csv",stringsAsFactors=F)
vegtype=rbind.data.frame(vegtype,c(0,0,"Other","Other","Other"))
## assign veg types
d$vegname=as.factor(vegtype$NAME[match(d$veg,vegtype$VEGTYPEID)])
d$veggroupname=as.factor(vegtype$GROUPNAME[match(d$veg,vegtype$VEGTYPEID)])

## combine small veg groups
smallveg=names(table(d$veg)[table(d$veg)<100])
d$veg[d$veg%in%smallveg]=00
d$veg=as.factor(d$veg)

### convert soils to factors
d$soilfert=factor(d$soilfert,ordered=F)
d$soilph=factor(d$soilph,ordered=F)
d$soiltext=factor(d$soiltext,ordered=F)

## bin soil data
d$soilhighfert=ifelse(d$soilfert==1,0,1) # high fertility soils (bin classes 2,3,4 together as "high")
d$soilfinetext=ifelse(d$soiltext==4,0,1) # coarse soils (bin classes 1,2,3 together as "fine")
d$soilacid=ifelse(d$soilph==1,1,0) # acidic soils (bin classes 2,3 together as "not acidic")

### Add coordinates to data 
d@data[,c("lon","lat")]=coordinates(d)

### Convert fynbos to logical
d$fynbos=!is.na(d$fynbos)

### add indicator for CFR
cfr= readOGR("/media/Data/Work/Regional/CFR/BaseGISData/CFR.shp","CFR")
d$cfr=!is.na(overlay(d,cfr))

### drop cells outside fynbos
d=d[d$fynbos,]  

### Save it
save(d,file="data/covariates.Rdata")

