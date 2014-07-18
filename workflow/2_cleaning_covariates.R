##############################################################################
######## Code to get and resample GIS data for the Cape Peninsula to the 30m Landsat grid
######## Also draws data into table format and calculates veg age based on fire data
##############################################################################
######## Compiled by Jasper Slingsby 2014
######## Last edited: 5 July 2014
##############################################################################
######## 
###Steps:
##############################################################################

library(raster,sp)

###Set working directory 
if (Sys.getenv("USER")=='jasper') setwd("/Users/jasper/Dropbox/Shared/Postfire_workshop/data") #use "USER" for Mac
if (Sys.getenv("USERNAME")=='whoeveryouare') setwd ("C:/") #for PC

###Get data
ig=raster("clean/indexgrid_landsat_30m.grd") #get index grid - raster() reads grids
#ig; slotNames(ig)

rv=readOGR(dsn="raw/VegLayers/Vegetation_Indigenous_Remnants", layer="Vegetation_Indigenous_Remnants") #remnant veg layer - readOGR() reads shapefiles
#rv; summary(rv$National_); summary(rv$Subtype); summary(rv$Community); levels(rv@data$National_)
rv_meta=data.frame(1:length(levels(rv@data$National_)), levels(rv@data$National_)) #save VegType metadata
colnames(rv_meta)=c("ID", "VegType") #rename columns

fi=readOGR(dsn="raw/Fire", layer="CapePenFires") #Cape Peninsula fires history layers 1962-2007
#fi

###Set all CRS to that of the Landsat index grid (UTM 34S)
rv=spTransform(rv,CRS(proj4string(ig)))
fi=spTransform(fi,CRS(proj4string(ig)))

###Plot data to see what we have
plot(rv)
plot(fi, add=T, col="red")

###Extract the national veg types from the veg layer into a 30m raster based on the index grid
rvr=rasterize(rv, ig, field=c("National_"), fun="max") #get national veg type for each cell
rvc=rasterize(rv, ig, field=c("National_"), fun="count") #count number of veg types for each cell (i.e. ID mixed cells)

###Extract fire history data and convert to a 30m raster
fi$STARTDATE[which(fi$STARTDATE==196201001)]=19620101#fix an anomalous date...
fic=rasterize(fi, ig, field=c("STARTDATE"), fun="count") #Raster showing numbers of fires

###Write out objects and dump from memory
writeRaster(fic, "clean/fires_number_1962to2007_landsat_30m.grd")#, overwrite=TRUE)
writeRaster(rvr, "clean/vegtypes_landsat_30m.grd")#, overwrite=TRUE)
write.csv(rv_meta, "clean/vegtypecodes.csv", row.names=F)
rm(list=c("fic", "rvr", "rv", "rvc", "rv_meta"))

###Extract fire history data and convert to a 30m raster for each year   ### ?calc, ?stackApply, ?overlay
years=sort(unique(fi$YEAR)) #get the unique list of years in which fires occurred
#years=years[years>1981] #trim to 1982 onwards (our earliest reliable Landsat data)

x=list(length=length(years)) #make a "list" object to store our annual rasters
x[[1]]=ig
for (i in 2:length(years)) #loop through years making a raster of burnt cells (1/0) for each
  {
  x[[i]]=rasterize(fi[which(fi$YEAR==years[i]),],ig, field="YEAR", fun="count", background=0) #field=1,fun=max, background=0
  }
names(x)=c("Index", years[-1]) #name the rasters by year
xs=stack(x) #make a "stack" of the annual rasters

###Calculate a table of veg age from fire history
xt=as.data.frame(xs)

for(i in 2:ncol(xt))
{
  xt[,i]=(xt[,i]*-1)+1
}

xr=cbind(xt[,1],matrix(0, nrow=nrow(xt), ncol=ncol(xt)-1)) #Make a matrix for output "ages"

xr[which(xt[,2]==1),2]<-50 #set starting "age" at 50 years (just so we can spot them)

for(i in 3:ncol(xr))
{
xr[,i]=(xr[,i-1]+1)*xt[,i] #set each cells age as the value in the last year + 1 and multiply by corresponding cel in xt (1=no burn, 0=burn) 
}

colnames(xr)=c("Index", 1964:2007)

write.csv(xr, "clean/vegage.csv", row.names=F)

############

##try reducing file size as follows for each year? Or when writing the file out?
#xs[[1]]@data@values[which(xs[[1]]@data@values==1983)]=1

#writeRaster(xb, "clean/fires_annual_landsat_30m.grd")
