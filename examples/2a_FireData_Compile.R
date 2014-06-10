#####################################
### Compilation of biomass accumulation data

setwd("/media/Data/Work/Regional/CFR/BiomassAccumulation/BiomassAccumulation_500m/analysis")
### Run R in GRASS if desired to enable pushing maps to GIS
system("grass -text /media/Data/Work/grassdata/CFR/biomass_500m")
db.connect database=/media/Data/Work/grassdata/CFR/biomass_500m/sqlite.db driver=sqlite

## Set region to NDVI layer
g.region -p rast=ndvi

######################################################################
### Convert MODIS MOD45A1 product to shapefiles to combine with field data below
R
library(spgrass6)
library(raster)

### First download the geotiffs for Southern Africa (Window 13)
ftpsite="ftp://ba1.geog.umd.edu/TIFF/Win13/"
tiffolder="/media/Data2/Data/MODIS/CFR/MCD45A1/1_GeoTiff"

## download 
system(paste("wget -q --user=user --password=burnt_data -S --recursive --no-parent --no-directories -N -P",tiffolder," ",ftpsite," &")) 
## unzip it
system(paste("gunzip -r ",tiffolder,sep=""))

## Make table of metadata
files=data.frame(bfile=list.files(tiffolder,pattern="*burndate.tif$"),stringsAsFactors=F)
files$qfile=sub("burndate","ba_qa",files$bfile)
files$dateid=do.call("rbind",strsplit(files$bfile,"[.]"))[,2]
files$year=format(as.Date(substr(files$dateid,2,8),"%Y%j"),"%Y")
files$month=format(as.Date(substr(files$dateid,2,8),"%Y%j"),"%m")

for(i in 1:nrow(files)){
  ## this loop reads in one month's data at a time, subsets only 'high confidence' burned
  ## area pixels and generates a shapefile with some updated date metadata.
  ## read in burned area data and quality from nc file
#system("gdalinfo data/geotiffs/ID.tif")  #get coords for ID map to align geotifs
system(paste("gdalwarp -te 17.7475130 -35.1036696 26.0043592 -30.9975115 -tr 0.004974003750502 -0.004977040138384 -of GTiff -t_srs EPSG:4326 ",
               tiffolder,"/",files$bfile[i]," ",tempdir(),"/tmp_burndate.tif -q -overwrite",sep=""))
  system(paste("gdalwarp -te 17.7475130 -35.1036696 26.0043592 -30.9975115 -tr 0.004974003750502 -0.004977040138384 -of GTiff -t_srs EPSG:4326 ",
               tiffolder,"/",files$qfile[i]," ",tempdir(),"/tmp_ba_qa.tif -q -overwrite",sep=""))
## Read them into GRASS
system(paste("r.in.gdal input=",tempdir(),"/tmp_burndate.tif output=burndate --o",sep=""))
  system(paste("r.null map=burndate@biomass_500m setnull=900,9998,9999,10000,0 "))
  system(paste("r.in.gdal input=",tempdir(),"/tmp_ba_qa.tif output=ba_qa --o",sep=""))
  system(paste("r.null map=ba_qa setnull=0 "))
## create new binary burned/notburned map
system("r.mapcalc 'burned=if(burndate>0)'")
## identify clumps of burned pixels
system("r.clump burned output=burnclump --o")
## write the sizes of each clump to disk
system("r.le.patch map=burnclump siz=s1 out=patchsize --quiet")
## read in the sizes
ps=read.table("r.le.out/patchsize",header=T)
dropsmall=5  #drop patches with fewer than this number of pixels
dropid=ps$att[ps$size>dropsmall]
## drop small paches from the burndate
if(length(dropid)>0) system(paste("r.mapcalc burndate2='if(",paste("burnclump==",dropid,collapse="|",sep=""),",burndate,null())'",sep=""))
if(length(dropid)==0) {
  system("g.remove rast=burndate,ba_qa,burndate2 vect=burndate")
  print(paste("%%%%%%%%%%%%%%%%%%%%% Element ",i," has no big fires, skipping %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"))
  next
}
## now read into R for some additional calculations
bd=readRAST6("burndate2",plugin=T)
bd$patch=readRAST6("burnclump",plugin=T)@data[,1]
bd$qa=readRAST6("ba_qa",plugin=T)@data[,1]
#fullgrid(bd)=F
#bd=bd[!is.na(bd$burndate2),]
#library(lattice)
#bwplot(burndate2~patch,data=bd@data,horizontal=F)
bd_med=do.call("rbind",tapply(bd$burndate2,bd$patch,function(x) c(FireDateMedian=round(median(x,na.rm=T)),FireDateSD=sd(x,na.rm=T))))
bd@data[,c("FireDateMedian","FireDateSD")]=bd_med[match(bd$patch,rownames(bd_med)),]
bd$keep=ifelse(abs(bd$burndate2-bd$FireDateMedian)>10,0,1)  #ID cells with >10 days difference from median date
bd$FireDateMedian[bd$keep==0]=NA #drop pixels with strange dates
writeRAST6(bd,zcol="FireDateMedian","burndate3",overwrite=T)
## Now export vector version 
   system("r.to.vect --quiet input=burndate3 output=burndate feature=area --o")
   ## update metadata
   tjday=sprintf("%03d",as.numeric(system(
     "db.select -c table=burndate database=/media/Data/Work/grassdata/CFR/biomass_500m/sqlite.db driver=sqlite 'sql=select value from burndate'",
     intern=T)))
   tdate=as.Date(paste(files$year[i],tjday,sep=""),"%Y%j")
   tday=format(tdate,"%d")
   tmonth=format(tdate,"%m")
   ## Extract Year
   system("v.db.addcol burndate column=\"YEAR int\"")
   system(paste("v.db.update map=burndate layer=1 column=YEAR value=",files$year[i],sep=""))
   ## Fire Date as julian day
   system("v.db.addcol burndate column=\"JULIANDAY varchar\"")
   system(paste("echo 'update burndate set JULIANDAY=julianday(date(YEAR||\"-01-01\"))' | db.execute",sep="")) 
   ## Fire Date as Date
   system("v.db.addcol burndate column=\"FIREDATE varchar\"")
   system(paste("echo 'update burndate set FIREDATE=date(strftime(\"%J\",JULIANDAY+value-1))' | db.execute",sep=""))
   ## Fire Date as Month
   system("v.db.addcol burndate column=\"MONTH int\"")
   system(paste("echo 'update burndate set MONTH=strftime(\"%m\",FIREDATE)' | db.execute",sep=""))
   ## FireID
   system("v.db.addcol burndate column=\"FIREID varchar\"")
   system(paste("echo 'update burndate set FIREID=(\"MCD45A1_\"||YEAR||\"_\"||MONTH||\"_\"||ROWID)' | db.execute",sep=""))
   ## Clean up
   system("v.db.dropcol map=burndate layer=1 column=JULIANDAY")
   system("v.db.dropcol map=burndate layer=1 column=value")
  system(paste("v.out.ogr -c -e input=burndate type=area dsn=data/firedata/MCD45A1 olayer=MCD45A1_",
               files$year[i],files$month[i]," layer=1 format=ESRI_Shapefile",sep=""))
  ## Clean up
  system("g.remove rast=burndate,ba_qa,burndate2 vect=burndate")
closeAllConnections()
print(paste("%%%%%%%%%%%%%%%%%%%%% Finished ",i," out of ",nrow(files)," %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"))
}

### Now merge them all to a single shapefile with ogr2ogr
shps=list.files("data/firedata/MCD45A1",pattern="*.shp",full=T)
for(i in 1:length(shps)){
  if(i==1) system(paste("ogr2ogr -overwrite -nln MCD45A1 data/firedata ",shps[i]))
  if(i>1)  system(paste("ogr2ogr -append  -nln MCD45A1 data/firedata ",shps[i]))
}


######################################################################
### Compile fire data
### Subset only fires post 1989 that are larger than 2km
system("ogr2ogr -overwrite -nln FireData -sql 'SELECT FIRE_CODE as FIREID,YEAR,MONTH,STARTDATE,\"CN\" AS SOURCE FROM all_fires_08_09 WHERE YEAR>=1950' data/firedata /media/Data/Work/Regional/CFR/FireAnalysis/FireDataJuly2009/all_fires_08_09.shp")
## add peninsula fires to shapefile
system("ogr2ogr -append  -nln FireData -sql 'SELECT FIREID,YEAR,STARTDATE,\"TMNP\" AS SOURCE FROM CapePenFires WHERE YEAR>=1950' data/firedata /media/Data/Work/Regional/CFR/FireAnalysis/Cape_Pen_fires/WGS84/CapePenFires.shp")
## add MODIS data to shapefile
system("ogr2ogr -append  -nln FireData -sql 'SELECT FIREID,YEAR,FIREDATE AS STARTDATE,\"MCD45A1\" AS SOURCE FROM MCD45A1' data/firedata data/firedata/MCD45A1.shp")

### Now open DBF and fill in any missing (NULL) fire ids with missing_1, etc.
library(foreign)
fd=read.dbf("data/firedata/FireData.dbf")
fd$FIREID=as.character(fd$FIREID)
fd$FIREID[is.na(fd$FIREID)]=paste("Missing",1:sum(is.na(fd$FIREID)),sep="_")
write.dbf(fd,"data/firedata/FireData.dbf")

## Quit R to do a loop in GRASS
q("no")

FIRES=`db.select -c table=FireData database=data/firedata/ driver=dbf 'sql=select FIREID from FireData' | sort | uniq`  #uniqe and sorted fireIDs
N=`echo $FIRES | awk '{print NF}'`
Ns=`seq 1 1 $N`

rm data/firedata/FireData.csv
g.remove vect=tfire,tfire2,tID,tfiregrid rast=tfire,tfire2

for n in $Ns; do
    i=`echo $FIRES | awk '{print $'$n'}'`  # get fire number for this iteration
    ## Read in one fire at a time
    v.in.ogr -c dsn=data/firedata/FireData.shp output=tfire where="FIREID IN ('$i')" --o --quiet
    v.db.addcol tfire column="fireareakm"
    v.to.db map=tfire type=boundary option=area units=k column=fireareakm  --quiet # add land area field
    ## Rasterize it to get every 500m MODIS pixel that touches the fire
    g.region vect=tfire res=0.0002  #resolution of rasterized fire
    v.to.rast input=tfire output=tfire use=attr column=cat type=area --o --quiet #rasterize
    r.null map=tfire null=0  #convert null to 0 for r.neighbors
    g.region vect=tfire align=ndvi@biomass_500m  #resolution of rasterized fire
    ## keep every 500m pixel that touches fire
    r.resamp.stats -w input=tfire output=tfire2 method=maximum --o  --quiet
    r.null map=tfire2 setnull=0  #convert 0 back to null
    ## Vectorize the grid
    r.to.vect input=tfire2 output=tfire2 feature="point" --o --quiet #convert to point for each pixel
    v.db.addcol tfire2 col="ID integer"
    v.what.rast vector=tfire2 raster=ID@biomass_500m column=ID  --quiet #get burned IDs from ID raster
    ## make vector grid of IDs
    r.to.vect input=ID output=tID feature=area --o --quiet
    v.db.addcol tID column="gridareakm double, dcoastm double"
    v.to.db map=tID type=boundary option=area units=k column=gridareakm --quiet # add grid area field
    ## if near ocean, overlay with land area to drop ocean/water -- too slow do it for all at once instead
    v.distance from=tID to=coastline@PERMANENT from_type=centroid to_type=line upload=dist column=dcoastm --quiet
    cdist=`echo 'select round(min(dcoastm),0) from tID ' | db.select -c`  #get minimum distance
    if [ $cdist -lt 1000 ]; then  #if less than 1km, overlay them
        echo "%%%%% Fire $i is near coast ($cdist m), performing coastal overlay %%%%%%"
        v.overlay ainput=tID atype=area binput=CFR@PERMANENT output=tfiregrid operator=and --o --quiet
        v.db.renamecol tfiregrid col="a_gridareakm,gridareakm"
        v.db.renamecol tfiregrid col="a_dcoastm,dcoastm"
        v.db.renamecol tfiregrid col="a_value,ID"
        v.db.addcol tfiregrid column="landareakm double"
        v.to.db map=tfiregrid type=boundary option=area units=k column=landareakm  --quiet # add land area field
        for col in `v.info -c tfiregrid | awk -F"|" '{print $2}' | grep "_"` ; do echo $col; v.db.dropcol tfiregrid col=$col --quiet ; done
        g.remove vect=tID
        g.rename vect=tfiregrid,tID --o
    else #otherwise create a new one
        echo "%%%%% Fire $i is not near coast ($cdist m), skipping coastal overlay %%%%%"
        v.db.renamecol tID col="value,ID"
        v.db.addcol tID column="landareakm double"
        v.db.update map=tID layer=1 column=landareakm qcolumn=gridareakm 
    fi
    ## Add burned area to each pixel
    echo "%%%%% Overlaying with fire to get burned area %%%%%%"
    v.overlay ainput=tID binput=tfire output=tfiregrid operator=and --o --quiet
    ## drop any areas <1 hectare
    v.clean input=tfiregrid output=tfiregrid2  tool=rmarea thresh=10000 --o #--quiet  #drop areas <.1 hectares
    g.remove vect=tfiregrid
    g.rename vect=tfiregrid2,tfiregrid --o
    ## Rasterize it to get every 500m MODIS pixel that touches the fire
    g.region vect=tfiregrid res=0:00:0.05  #resolution of rasterized fire
    v.to.rast input=tfiregrid output=tfiregrid use=attr column=a_id type=area --o --quiet #rasterize
    r.to.vect input=tfiregrid output=tfiregrid_clean feature=area  --quiet --overwrite
    v.clean input=tfiregrid_clean output=tfiregrid_clean2  tool=rmarea thresh=10000 --o --quiet
    g.remove vect=tfiregrid_clean
    g.rename vect=tfiregrid_clean2,tfiregrid_clean --o
    g.region vect=tfire align=ndvi@biomass_500m  #return to ndvi resolution
    v.db.addcol tfiregrid_clean column="burnareakm double"
    v.to.db map=tfiregrid_clean type=boundary option=area units=k column=burnareakm --quiet # add land area field
    v.db.addcol tfiregrid column="burnareakm double"
    echo "UPDATE tfiregrid SET burnareakm=(SELECT burnareakm FROM tfiregrid_clean WHERE tfiregrid.a_id=tfiregrid_clean.value);" | db.execute
    ## Clean up column names
    v.db.renamecol tfiregrid col="a_ID,ID"
    v.db.renamecol tfiregrid col="a_landareakm,landareakm"
    v.db.renamecol tfiregrid col="a_gridareakm,gridareakm"
    v.db.renamecol tfiregrid col="a_dcoastm,dcoastm"
    v.db.renamecol tfiregrid col="b_FIREID,FIREID"
    v.db.renamecol tfiregrid col="b_fireareakm,fireareakm"
    v.db.renamecol tfiregrid col="b_YEAR,YEAR"
    v.db.renamecol tfiregrid col="b_MONTH,MONTH"
    v.db.renamecol tfiregrid col="b_STARTDATE,STARTDATE"
    v.db.renamecol tfiregrid col="b_SOURCE,SOURCE"
for col in `v.info -c tfiregrid | awk -F"|" '{print $2}' | grep "_"` ; do echo $col; v.db.dropcol tfiregrid col=$col --quiet ; done  #drop other fields
    ## Get distances of each pixel to fire edge and coastline
    v.db.addcol tfiregrid column="dfireboundm double, lat double, lon double"
    v.distance from=tfiregrid to=tfire from_type=centroid to_type=boundary upload=dist column=dfireboundm --quiet
    v.to.db map=tfiregrid type=centroid option=coor column="lon,lat" --quiet
    echo "%%%%%%% Writing data to disk %%%%%%"
    if [ -f data/firedata/FireData.csv ]; then  #if file exists, just add to it
       v.db.select -c map=tfiregrid layer=1 column=* fs=, >> data/firedata/FireData.csv
    else #otherwise create a new one
       v.db.select  map=tfiregrid layer=1 column=* fs=, > data/firedata/FireData.csv
    fi
    echo "%%%%%%%%%%%%   Finished fire $n  out of $N      %%%%%%%%%%%%%"
    g.remove vect=tfire,tfire2,tID,tfiregrid,tfiregrid2,tfiregrid_clean rast=tfire,tfire2 --quiet
done

##############################################################################################
##############################################################################################
### Start here to work in R

R
setwd("/media/Data/Work/Regional/CFR/BiomassAccumulation_500m/")
library(spgrass6)
library(sp);library(rgdal);library(lattice)
library(RNetCDF)
library(reshape);library(multicore)
library(heR.Misc) # for dapply
source("/media/Data/Work/Stat/Scripts/Functions.r")
library(cluster) #to cluster burned cells for extracting from NDVI database

### Connect to MODIS NDVI NetCDF file
modfile="/media/Data/Work/Regional/CFR/MODIS/MCD13A1.nc"
modis=open.nc(modfile,write=F)  # Opens connection with netcdf file
system(paste("ncdump -h ",modfile))
date=as.Date("2000-02-18")+var.get.nc(modis,"time")/24             #values of time
decyear=as.numeric(format(date,"%Y"))+as.numeric(format(date,"%j"),1,1,sep="-")/365
lat=var.get.nc(modis,"lat")             #values of time
lon=var.get.nc(modis,"lon")             #values of time

#### Read in fire grid summary
fd=read.csv("data/firedata/FireData.csv",stringsAsFactors=F)
fd=fd[,-grep("cat",colnames(fd))]
colnames(fd)=tolower(colnames(fd))

### convert dates
fd$month=as.integer(fd$month)
fd$year=as.integer(fd$year)
fd$startdate=as.character(fd$startdate)
fd$firedate=as.Date(NA)
fd$firedate[grepl("/",fd$startdate)]=as.Date(fd$startdate[grepl("/",fd$startdate)],format="%d/%m/%Y")
fd$firedate[!grepl("/",fd$startdate)]=as.Date(fd$startdate[!grepl("/",fd$startdate)],format="%Y%m%d")
fd$firedate[grepl("-",fd$startdate)]=as.Date(fd$startdate[grepl("-",fd$startdate)],format="%Y-%m-%d") #MODIS dates
nodate=is.na(fd$firedate)  #entries with no date
nomonth=is.na(fd$firedate)&(is.na(fd$month)|fd$month==0)  #entries with no date and no month recorded
fd$firedate[nodate&!nomonth]=as.Date(paste(fd$year[nodate&!nomonth],fd$month[nodate&!nomonth],15,sep="-"))  #combine fields to make new date
fd=fd[,-grep("year|month|startdate",colnames(fd))]  #drop extra columns

## drop fires with missing dates
fd=fd[!is.na(fd$firedate),]

## drop null burnedareas - these are so small they had virtually no area
fd=fd[!is.na(fd$burnareakm),]

### clean up cell areas
sID=unique(fd$id[(fd$burnareakm/fd$gridareakm)>1.05])  # get ID's for a few strange values where burned area>grid area with 5% forgiveness
sID2=unique(fd$id[(fd$burnareakm/fd$landareakm)<.95])  # get ID's that were ever burned < 95% to avoid partial burns
sID=unique(c(sID,sID2))  
fd=fd[!fd$id%in%sID,]  #drop these problem cells

## find cells with >1 fires on the same day with different fireid's - overlapping fire polygons on same day
dupefires=unique(fd[,c("id","fireid","firedate")]);dupefires$idfiredate=paste(dupefires$id,dupefires$firedate,sep="_") 
sID4a=names(table(dupefires$idfiredate)[table(dupefires$idfiredate)>1])
sID4b=do.call(rbind,strsplit(sID4a,"_"));sID4b=data.frame(id=as.integer(sID4b[,1]),firedate=sID4b[,2],stringsAsFactors=F) #get IDs with duplicate fires
sID4=c()
for(i in 1:nrow(sID4b)){  #create list of rownames to drop
  print(paste(i," out of ",nrow(sID4b)))
  fdt=fd[fd$id==sID4b$id[i]&fd$firedate==sID4b$firedate[i],]
  sID4=c(sID4, as.integer(rownames(fdt)[!rownames(fdt)%in%rownames(fdt)[which.min(fdt$dfireboundm)]]))
}
fd=fd[!rownames(fd)%in%sID4,]  #drop these problem cells

## drop cells that were split into two polygons (same id, same fireid, same firedate, but different areas)
dupefires2=paste(fd$id,fd$firedate,sep="_")
sID5=as.integer(do.call(rbind,strsplit(names(table(dupefires2)[table(dupefires2)>1]),"_"))[,1])
fd=fd[!fd$id%in%sID5,]  #drop these problem cells

### get unique ID's
uID=unique(fd$id)
n=length(uID)

### Drop all but most recent pre 2000 fire
## Add number of fires
rID=as.data.frame(table(fd$id));colnames(rID)=c("id","nfire") #number of burns/pixel
fd=merge(fd,rID,all.x=T,sort=F)
  ## find the next firedate for each pixel for each time to get pre-fire ndvi
fdp=fd[fd$firedate<date[1],] #subset only pre-modis fires
lastpfire=tapply(as.integer(fdp$firedate),fdp$id,max)
lastpfire=melt(lastpfire);colnames(lastpfire)=c("id","lastfiredate")
lastpfire$lastfiredate=as.Date(lastpfire$lastfiredate,origin=as.Date("1970-01-01"))
fd=merge(fd,lastpfire,all.x=T)
fd=fd[fd$firedate>=date[1]|fd$firedate==fd$lastfiredate,]  #drop the earlier fires
fd=fd[,!grepl("lastfiredate",colnames(fd))] #drop that column


#####################################################################
### get region to extract
k=30  #number of clusters to use, use more if ram is limited
fd$cluster=as.integer(clara(cbind(fd$lon,fd$lat),k)$clustering)  #bin IDs into clusters for extracting NDVI data
s=sample(1:nrow(fd),10000);plot(lat~lon,col=fd$cluster[s],data=fd[s,])
st=data.frame(lon=tapply(fd[s,]$lon,fd[s,]$cluster,mean),lat=tapply(fd[s,]$lat,fd[s,]$cluster,mean))
text(rownames(st),x=st$lon,y=st$lat)  #show clusters in a plot
save(fd,file="data/FireDataClean.Rdata")

#####################################################################
#####################################################################
#####################################################################
### Extract NDVI
load("data/FireDataClean.Rdata")

#print.nc(modis)

#########################################
### check ndvi pixel by pixel to identify any missed fires using strucchange package's breakpoints
library(strucchange)
###  load NDVI
  ndvi=var.get.nc(modis,"500m_16_days_NDVI")
  ndvi[ndvi==-3000]=NA  #drop null values
  ndvi=ndvi*0.0001  #convert to NDVI
  ndvi[ndvi<(-1)]=-1; ndvi[ndvi>1]=1  #drop unreasonable values
### load EVI for comparison
  evi=var.get.nc(modis,"500m_16_days_EVI")
  evi[evi==-3000]=NA  #drop null values
  evi=evi*0.0001  #convert to NDVI
  evi[evi<(-1)]=-1; evi[evi>1]=1  #drop unreasonable values
### load the ID matrix and transform it to link with ndvi
  id=as.matrix(readGDAL("data/geotiffs/ID.tif"))  #read in ID raster for region
  dimnames(id)=list(lonid=1:length(lon),latid=rev(1:length(lat)))  #reverse latitude to make it line up with everything else!
  id=melt.array(id);colnames(id)[3]="id"
  id=id[id$id%in%unique(fd$id),]  #keep only cells with fire data
  id$lat=lat[id$latid]
  id$lon=lat[id$lonid]

### Define a function that runs breakpoints() on the ndvi vector for each pixel in fid
checkfire<-function(x,graphic=T) {
  time=1:dim(ndvi)[3]  #the time vector
  tndvi=ndvi[id$lonid[x],id$latid[x],] # get ndvi for that pixel
  tevi=evi[id$lonid[x],id$latid[x],] #get evi for that pixel
  if(sum(!is.na(tndvi))<50) return(NULL)  #if there are <50 non-na values, quit
  bs=breakpoints(tndvi~time,breaks=5)  # estimate any breakpoints
  if(any(is.na(bs$breakpoints))) return(NULL)  #if no breakpoints, quit
  ## build the output table for each breakpoint
  ## check for drop in ndvi
  if(!any(is.na(bs$breakpoints))){  
    bps=data.frame(id=id$id[x],breaktime=bs$breakpoints,conf.low=confint(bs)$confint[,1],conf.high=confint(bs)$confint[,3],
      rss=NA,bic=NA,lowtime=NA,change=NA)
    for(bi in 1:length(bs$breakpoints)){
      bps$change[bi]=      mean(tndvi[(bps$breaktime[bi]+1):min(length(tndvi),bps$breaktime[bi]+23)],na.rm=T)-
        mean(tndvi[max(1,bps$breaktime[bi]-23):bps$breaktime[bi]],na.rm=T)  #get change in mean from year before to after
      ## extract rss and bic
      bps$rss[bi]=summary(bs)$RSS[1,bi]-summary(bs)$RSS[1,bi+1]
      bps$bic[bi]=summary(bs)$RSS[2,bi]-summary(bs)$RSS[2,bi+1]
      ## get time of lowest ndvi within 1 year of breakpoint
      lowrange=max(1,(bps$breaktime[bi]-23)):min(bps$breaktime[bi]+23,length(time))
      bps$lowtime[bi]=time[lowrange][which.min(tndvi[lowrange])]
       }
    ## keep only negative changes (where ndvi drops after breakpoint)
    bps=bps[bps$change<0,]
  }
  ### Draw a graphic to see what's going on, if desired
  if(graphic){
    png(paste("breakoutput/id_",x,".png",sep=""),width=1000,height=800)
    plot(tndvi~date,type="l",ylim=c(0,1),las=1,ylab="NDVI",main=paste("Pixel ID:",id$id[x]),xlab="Date")
    lines(tevi~date,col="darkgreen")
    if(length(bps$breaktime)>0){
      abline(v=date[bps$breaktime],col="red",lwd=2)
      abline(v=date[bps$lowtime],col="yellow",lwd=2)
      abline(v=date[c(bps$conf.high,bps$conf.low)],lwd=.75,col="red",lty="dashed")
      text(date[bps$breaktime],0.8,paste("change:",round(bps$change,2),"\n rss=",round(bps$rss,2),"\n bic=",round(bps$bic,2)),col="red")}
    abline(v=fd$firedate[fd$id==id$id[x]],lwd=2,col="purple",lty="dotted")
    legend("topleft",legend=c("NDVI","EVI","Breakpoint","Firedata"),
           col=c("black","darkgreen","red","purple"),lty=c("solid","solid","solid","dotted"),bty="n")
    dev.off()
  }
  print(paste(x," out of ",nrow(id)))
  return(bps)
}

dev.off()  #close any open graphics so multicore won't kill R
system.time(breaks<<-do.call(rbind,mclapply(1:nrow(id),checkfire,graphic=F,mc.cores=3))) #18 hours!
save(breaks,file="data/breaks.Rdata")  #save for later
gc()

##########################################################
##########################################################
### Extract the MODIS data

### function to extract NDVI for each pixel cluster
getndvi<-function(i){
  tfd=fd[fd$cluster==i,]
  tid=unique(tfd$id)
  coordinates(tfd)=c("lon","lat")
  b=bbox(tfd)
  lonid=which(lon>=b[1,1]&lon<=b[1,2])
  latid=which(lat>=b[2,1]&lat<=b[2,2])
  tlon=lon[lon>=b[1,1]&lon<=b[1,2]]
  tlat=lat[lat>=b[2,1]&lat<=b[2,2]]
  print("Extracting data from netcdf file")
  ## import ndvi
  ndvi=var.get.nc(modis,"500m_16_days_NDVI",
    start=c(min(lonid),min(latid),1),count=c(length(lonid),length(latid),NA)) #read in NDVI
  ndvi[ndvi==-3000]=NA  #drop null values
  ndvi=ndvi*0.0001  #convert to NDVI
  ndvi[ndvi<(-1)]=-1  #drop unreasonable values
  ndvi[ndvi>1]=1  #drop null values
  dimnames(ndvi)=list(lon=lonid,lat=latid,time=1:dim(ndvi)[3])
  ## import evi
  evi=var.get.nc(modis,"500m_16_days_EVI",
    start=c(min(lonid),min(latid),1),count=c(length(lonid),length(latid),NA)) #read in NDVI
  evi[evi==-3000]=NA  #drop null values
  evi=evi*0.0001  #convert to NDVI
  dimnames(evi)=list(lon=lonid,lat=latid,time=1:dim(ndvi)[3])
  ## import index quality
  viq=var.get.nc(modis,"500m_16_days_pixel_reliability",
    start=c(min(lonid),min(latid),1),count=c(length(lonid),length(latid),NA)) #read in NDVI
  viq[viq==-1]=NA  #drop null values
  dimnames(viq)=list(lon=lonid,lat=latid,time=1:dim(ndvi)[3])
  ## import ndvi dates
  ndvidate=var.get.nc(modis,"500m_16_days_composite_day_of_the_year",
    start=c(min(lonid),min(latid),1),count=c(length(lonid),length(latid),NA)) #read in NDVI
  dimnames(ndvidate)=list(lon=lonid,lat=latid,time=1:dim(ndvidate)[3])
  ndvidate[ndvidate<=0|ndvidate>=366]=NA
  ## Import IDs
  id=as.matrix(readGDAL("data/geotiffs/ID.tif",  #read in ID raster for region
    offset=c(length(lat)-max(latid),min(lonid)-1),
    region.dim=c(length(latid),length(lonid)),silent=T)) #read in ID
  dimnames(id)=list(lon=lonid,lat=rev(latid))  #reverse latitude to make it line up with everything else!
  ### Melt the arrays
  print("Melting Array")
  ## ID
  id2=melt.array(id);colnames(id2)[3]="id"
  id2$lid=paste(id2$lon,id2$lat,sep="_")
  id2=id2[id2$id%in%tid,c("lid","id")]
  ## NDVI
  ndvi2=melt.array(ndvi)
  ndvi2$lid=paste(ndvi2$lon,ndvi2$lat,sep="_")
  ndvi2=ndvi2[ndvi2$lid%in%id2$lid,c("lid","time","value")]
  colnames(ndvi2)[3]="ndvi"
  ## EVI
  evi2=melt.array(evi)
  evi2$lid=paste(evi2$lon,evi2$lat,sep="_")
  evi2=evi2[evi2$lid%in%id2$lid,c("lid","time","value")]
  colnames(evi2)[3]="evi"
  ## Index quality
  viq2=melt.array(viq)
  viq2$lid=paste(viq2$lon,viq2$lat,sep="_")
  viq2=viq2[viq2$lid%in%id2$lid,c("lid","time","value")]
  colnames(viq2)[3]="quality"
  ## NDVI date
  ndvidate2=melt.array(ndvidate)
  ndvidate2$lid=paste(ndvidate2$lon,ndvidate2$lat,sep="_")
  ndvidate2=ndvidate2[ndvidate2$lid%in%id2$lid,c("lid","time","value")]
  ndvidate2$ndvidate=as.Date(paste(format(date[ndvidate2$time],"%Y"),ndvidate2$value,sep="-"),"%Y-%j")
  ndvidate2$ndvidate[is.na(ndvidate2$value)]=date[ndvidate2$time[is.na(ndvidate2$value)]]
  ndvidate2=ndvidate2[,!grepl("value",colnames(ndvidate2))]
  ## Put them together
  print("Merging data and ID's")
  ndvi3=merge(id2,ndvi2)
  ndvi3=merge(ndvi3,ndvidate2)
  ndvi3=merge(ndvi3,evi2)
  ndvi3=merge(ndvi3,viq2)
  ndvi3$id=as.integer(ndvi3$id)
#  ndvi3=ndvi3[order(ndvi3$id,ndvi3$ndvidate),]  #for testing
#  xyplot(ndvi~ndvidate,groups=id,data=ndvi3[ndvi3$id%in%sample(unique(ndvi3$id),5),],type="l",ylim=c(-.5,1))  #sample plot for testing
  print(paste("%%%%%  Finished with cluster ",i," out of ",k," %%%%%%%"))
  gc()
#  return(ndvi5)
    return(ndvi3[order(ndvi3$id,ndvi3$time),c("id","time","ndvidate","ndvi","evi","quality")])
}

### Extract the data
k=max(fd$cluster);k
ndvi1=lapply(1:k, getndvi)  #21 minutes for 30, 37 minutes for 20, 19.6  minutes for 10 clusters
save(ndvi1,file="data/justndvi.Rdata")  # this includes id,time,ndvi for each pixel in fd in k clusters
gc()

########################################################################################
### Now calculate Ages
load("data/justndvi.Rdata")
load("data/FireDataClean.Rdata")
### drop some to make it run faster
#    fd=fd[sample(1:nrow(fd),nrow(fd)*.25),]

getages <- function(bdt,verbose=F){
  tfd=fd[fd$id%in%unique(bdt$id),]  #subset pixel data
  cl=tfd$cluster[1]
  ## get first fire for each id
  mindate=data.frame(mindate=do.call(c,tapply(tfd$firedate,tfd$id,min,simplify=F)));mindate$id=rownames(mindate)
  ## add firetime to bdt data
  if(verbose) print(paste("Getting firetimes for each pixel in cluster ",cl))
  bdt$ndviscenedate=date[bdt$time]
  bdt2=merge(bdt,tfd,all.x=T) #combine pixel info, makes separate row for each firedate-ndvidate combo
  bdt3=merge(bdt2,mindate,all.x=T)  #add minimum date for each pixel
  ## find the next firedate for each pixel for each time to get pre-fire ndvi
  if(verbose)   print(paste("Getting 'nextfire' dates for each pixel for each time for cluster ",cl))
  bdt_next=bdt3[bdt3$firedate>bdt3$ndvidate,]
  if(nrow(bdt_next)>0){  #if any nextfiredates, get them
    nextdate=tapply(as.integer(bdt_next$firedate),list(bdt_next$id,bdt_next$time),min)
    nextdate=melt(nextdate);colnames(nextdate)=c("id","time","nextfiredate")
    nextdate$nextfiredate=as.Date(nextdate$nextfiredate,origin=as.Date("1970-01-01"))
    bdt4=merge(bdt3,nextdate,all.x=T)
    bdt4=bdt4[bdt4$firedate<=bdt4$ndvidate|bdt4$mindate==bdt4$firedate,]
  }
  if(nrow(bdt_next)==0) {bdt4=bdt3; bdt4$nextfiredate=NA}  # otherwise they are missing
  ## find the maximum firedate for each pixel for each time
  if(verbose)   print(paste("Getting previous firedate for each pixel for each time",cl))
  maxdate=tapply(bdt4$firedate,list(bdt4$id,bdt4$time),max,simplify=T)
  maxdate=melt(maxdate);colnames(maxdate)=c("id","time","maxdate")
  maxdate$maxdate=as.Date(maxdate$maxdate,origin=as.Date("1970-01-01"))
  bdt5=merge(bdt4,maxdate,all.x=T)
  bdt5=bdt5[bdt5$firedate==bdt5$maxdate,]  #drop prior fires
  if(nrow(bdt)!=nrow(bdt5)) print(paste("####################################           Mismatch between row numbers in cluster",cl))
  ## get pixel age
  if(verbose)   print(paste("Getting pixel age for ",cl))
  bdt=bdt5
  bdt=bdt[order(bdt$id,bdt$ndvidate),]
  bdt$age=as.numeric(NA)
  bdt$age[bdt$firedate<=bdt$ndvidate]=as.numeric(bdt$ndvidate[bdt$firedate<=bdt$ndvidate]-bdt$firedate[bdt$firedate<=bdt$ndvidate])/365
  ## set pre-fire time
  bdt$prefireage=as.numeric(NA)
  if(sum(!is.na(bdt$nextfiredate))>0){
    bdt$prefireage[!is.na(bdt$nextfiredate)]=-as.numeric(bdt$nextfiredate[!is.na(bdt$nextfiredate)]-bdt$ndvidate[!is.na(bdt$nextfiredate)])/365}
  bdt2=bdt[order(bdt$id,bdt$ndvidate),][bdt$firedate>as.Date("2000-01-01"),]  #for testing
  print(paste("%%%% Finished cluster ",cl," %%%%"))
  gc()
  return(bdt)
}

### add the fire data and ages
ndvi=mclapply(ndvi1,getages,mc.cores=4,mc.preschedule=F) #  minutes  - more than 2 maxes out ram with full dataset
## check for nulls
nulls=which(unlist(lapply(ndvi,function(x) is.null(x)))); print("these clusters are null:"); nulls
errors=which(unlist(lapply(ndvi,function(x) ifelse(sum(grepl("Error",x))>0,T,F)))); print("these clusters have errors:"); errors
if(length(unique(c(nulls,errors)))>0) ndvi=ndvi[-unique(c(nulls,errors))]  #drop ones that are null if any

## add possible breaks
breaks$breakfire=T
ndvi=lapply(ndvi,function(x) {
  x2=merge(x,breaks[,c("id","breaktime","breakfire")],by.x=c("id","time"),by.y=c("id","breaktime"),all.x=T)
  x2$breakfire[is.na(x2$breakfire)]=F
  return(x2)
})  

### save it
save(ndvi,file="data/ndvi.Rdata")
rm(ndvi1); gc()



