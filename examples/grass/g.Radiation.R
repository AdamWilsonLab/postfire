########################################################
########################################################
### Calculate potential solar radiation for CFR

system("grass64 /media/Data/Work/grassdata/CFR/radiation")
db.connect database=/media/Data/Work/grassdata/CFR/radiation/sqlite.db driver=sqlite


#######################
### Radiation model

   ## loop through days and calculate radiation -slooooow
    for DAY in `seq 1 365` ; do
       DAY_STR=`echo $DAY | awk '{printf("%.03d", $1)}'`
       echo "Processing day $DAY_STR at `date` ..."
       r.sun -s elevin=dem@biomass aspin=aspect@biomass slopein=slope@biomass day=$DAY lin=3.0 step=0.5 dist=1 glob_rad=rad_tot.$DAY_STR --o
       r.colors -e rad_tot.$DAY_STR col=bgyr 
   done

## Loop through modis scene dates and take average radiance for each modis scene
   R
   library(RNetCDF);library(spgrass6);library(multicore)
   bins=rep(1:23,each=16)[1:365];
   rad=system("g.mlist type=rast pattern=rad_tot*",intern=T)
   radiance=readRAST6(rad[1],plugin=T);gc()
   for(b in unique(bins)){
     inc=rad[bins%in%b]
     radtemp=readRAST6(inc[1],plugin=T);gc()
     for(i in inc[-1]) {radtemp@data[,i]=readRAST6(i,plugin=F)@data ; gc();print(i)}
     radiance@data=data.frame(apply(radtemp@data,1,mean,na.rm=T));gc() #5 minutes
     writeRAST6(radiance,paste("rad_ave.",strsplit(inc[1],".",fixed=T)[[1]][2],sep=""),over=T)
   }


## get MODIS dates from NetCDF
   modis=open.nc("/media/Data/Work/Regional/CFR/MODIS/MODIS.nc",write=F)  # Opens connection with netcdf file
   date=as.Date(paste(var.get.nc(modis,"year"),var.get.nc(modis,"month"),var.get.nc(modis,"day"),sep="/"),format="%Y/%m/%d")      #values of lat
   month=var.get.nc(modis,"month")
   jday=as.numeric(date-as.Date(paste(format(date,"%Y"),1,1,sep="-")))
   jday=data.frame(unique(cbind(month,jday)));jday=jday[order(jday$jday),] #drop duplicates & sort
   jday$seas=ifelse(jday$month%in%c(12,1,2),"Summer",ifelse(jday$month%in%c(3,4,5),"Fall",ifelse(jday$month%in%c(6,7,8),"Winter","Spring")))
## Get seasonal and annual means
   rada=system("g.mlist type=rast pattern=rad_ave*",intern=T)
   rada=data.frame(rada=rada,jday=as.numeric(do.call(rbind,strsplit(rada,".",fixed=T))[,2]))
   jday=unique(merge(jday[,c("jday","seas")],rada))
   radiance=readRAST6(rad[1],plugin=T);gc()
## Loop through bins and take average radiance for each season
   for(s in unique(jday$seas)){
     inc=as.character(jday$rada[jday$seas%in%s])
     radtemp=readRAST6(inc[1],plugin=T);gc()
     for(i in inc[-1]) {radtemp@data[,i]=readRAST6(i,plugin=F)@data ; gc();print(i)}
     radiance@data=data.frame(apply(radtemp@data,1,mean,na.rm=T));gc() #5 minutes
     writeRAST6(radiance,paste("rad_ave.",s,sep=""),over=T)
   }
## Set colors
   radiance=readRAST6("rad_ave.Summer",plugin=T);gc()
   for(s in unique(jday$seas)[-1]){radiance@data[,s]=readRAST6(paste("rad_ave.",s,sep=""),plugin=F)@data ; gc();print(i)} 
   cols=data.frame(quantile(as.vector(radiance@data),na.rm=T),c("blue","green","yellow","orange","red"))
   write.table(cols,file="radiancecolors.txt",row.names=F,col.names=F,quote=F)
   for(s in unique(jday$seas)){ system(paste("r.colors map=rad_ave.",s," rules=radiancecolors.txt",sep=""))}

### these were originally written the biomass mapset and moved here with the following command.

R
library(spgrass6)
r=system("g.list -f rast | grep \"rad_tot\"",intern=T)
r=sub("[[:space:]]*$","",r)
for(i in r) {system(paste("g.copy ",i,"@biomass,",i,sep="")) ; print(i)}


### Write out GEOTiff of each day's data
mkdir radiation
g.region zoom=rad_tot.001 align=rad_tot.001
for DAY in `seq 1 365` ; do
       DAY_STR=`echo $DAY | awk '{printf("%.03d", $1)}'`
       echo "Processing day $DAY_STR at `date` ..."
r.out.gdal input=rad_tot.$DAY_STR output=radiation/rad_tot.$DAY_STR.tiff createopt="COMPRESS=LZW" createopt="zlevel=9" type=UInt16
   done
