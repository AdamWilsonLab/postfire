#' ---
#' title: "DataPrep - Working for Water plots"
#' author: "Jasper Slingsby, Adam M. Wilson and Glenn R. Moncrieff"
#' output: pdf_document
#' ---
#' 
#' 
## ----setup1,echo=F,results='hide', warning=FALSE, message=FALSE, cache = F----
##  Source the setup file

source("../setup.R")

#' 
#' This script assembles various environmental layers into a common 30m grid for the Cape Peninsula.  It also calculates veg age based on the fire data.
#' 
#' ## Index raster
#' Import raster of an index grid (`ig`) to spatially connect all the datasets.
## ----index, warning=FALSE, message=FALSE, cache = F----------------------
ig=raster(paste0(datadir,"clean/indexgrid_landsat_30m.grd")) 

#' 
#' ## Vegetation 
#' 
## ----veg, warning=FALSE, message=FALSE, cache = F------------------------
rv=readOGR(dsn=paste0(datadir,"raw/VegLayers/Vegetation_Indigenous_Remnants"), layer="Vegetation_Indigenous_Remnants") 
#remnant veg layer - readOGR() reads shapefiles
#rv; summary(rv$National_); summary(rv$Subtype); summary(rv$Community); levels(rv@data$National_)
rv_meta=data.frame(1:length(levels(rv@data$National_)), levels(rv@data$National_)) #save VegType metadata
colnames(rv_meta)=c("ID", "code") #rename columns
write.csv(rv_meta, "data/vegtypecodes.csv", row.names=F)

# reproject to the CRS of the Landsat index grid (UTM 34S)
rv=spTransform(rv,CRS(proj4string(ig)))


#' 
#' Extract the national veg types from the veg layer into a 30m raster based on the index grid
## ----veg2,htmlcap="Land cover types aggregated to the 30m grid", warning=FALSE, message=FALSE, cache = F----
rvrfile="data/vegtypes_landsat_30m.tif"
## note the if(!file.exists)) first checks if the file already exists so you don't rerun this everytime you run the script... 
if(!file.exists(rvrfile))
  rvr=rasterize(rv, ig, field=c("National_"), fun="max",file=rvrfile) #get national veg type for each cell
## read it back in and 'factorize' it
rvr=raster(rvrfile)
rvr=as.factor(rvr)
rv_meta$code=as.character(rv_meta$code)
levels(rvr)=rv_meta[levels(rvr)[[1]]$ID,]
levelplot(rvr,col.regions=rainbow(nrow(rv_meta),start=.3))

#' 
#' Count number of veg types for each cell (i.e. ID mixed cells)
## ----vegc, warning=FALSE, message=FALSE, cache = F-----------------------
rvcfile="data/count_vegtypes_landsat_30m.tif"
if(!file.exists(rvcfile))
  rvc=rasterize(rv, ig, field=c("National_"), fun="count",file=rvcfile) 
rvc=raster(rvcfile)

#' 
#' Are there any mixed cells?
#' 
## ---- warning=FALSE, message=FALSE, cache = F----------------------------
table(values(rvc))

#' 
#' 
#' ## load WfW data
## ---- warning=FALSE, message=FALSE, cache = F----------------------------
alien_plots<-read.csv(paste0(datadir,"clean/alienplots.csv"),stringsAsFactors = FALSE)
alien_xy<-cbind(as.numeric(alien_plots$x),as.numeric(alien_plots$y))
alien_xy<-SpatialPoints(alien_xy,crs(ig))
alien_data<-SpatialPointsDataFrame(alien_xy,alien_plots[,4:8])


##load plot clearing data
alien_n <- readOGR(dsn=paste0(datadir,"raw/AlienPlots/Nbal Analysis_TMNP_20160314"),"Nbal Analysis_TMNP_North_20160314")
alien_c <- readOGR(dsn=paste0(datadir,"raw/AlienPlots/Nbal Analysis_TMNP_20160314"),"Nbal Analysis_TMNP_Central_20160314")
alien_s <- readOGR(dsn=paste0(datadir,"raw/AlienPlots/Nbal Analysis_TMNP_20160314"),"Nbal Analysis_TMNP_South_20160314")

alien_n<-spTransform(alien_n,CRS(proj4string(ig)))
alien_c<-spTransform(alien_c,CRS(proj4string(ig)))
alien_s<-spTransform(alien_s,CRS(proj4string(ig)))

nn<-seq(1,nrow(alien_n@data),by=1)
nc<-seq(max(nn)+1,max(nn)+nrow(alien_c@data),by=1)
ns<-seq(max(nc)+1,max(nc)+nrow(alien_s@data),by=1)

nn1 <- spChFIDs(alien_n, as.character(nn))
nc1 <- spChFIDs(alien_c, as.character(nc))
ns1 <- spChFIDs(alien_s, as.character(ns))

temp<-spRbind(nn1,nc1)
vegclear<-spRbind(temp,ns1)

#' 
#' 
#' ## Fire data
## ----fire1, warning=FALSE, message=FALSE, cache = F----------------------
fi=readOGR(dsn=paste0(datadir,"raw/Fire"), layer="CapePenFires") #Cape Peninsula fires history layers 1962-2007
fi=spTransform(fi,CRS(proj4string(ig)))

### Extract fire history data and convert to a 30m raster
fi$STARTDATE[which(fi$STARTDATE==196201001)]=19620101#fix an anomalous date...

#Raster showing total numbers of fires in each grid cell
ficfile="data/fires_number_1962to2007_landsat_30m.tif"
if(!file.exists(ficfile))
    fic=rasterize(fi, ig, field=c("YEAR"), fun="count",file=ficfile) 

fic=raster(ficfile)

#' 
#' 
#' ## Rasterize fire data into annual fire maps 
## ----fire2, warning=FALSE, message=FALSE, cache = F----------------------
## Set up a "Days since 1 January 1960" column on the fire polygons
sdate=fi$STARTDATE #get the unique list of years in which fires occurred
sdate[which(substr(sdate, 5, 8)=="0000")]=sdate[which(substr(sdate, 5, 8)=="0000")]+1231 #set those fires to occur on the 31st Dec - need to check these and fix

ddate=as.Date(as.character(sdate), format="%Y%m%d")
sdate=as.numeric(ddate-as.Date("19600101", format="%Y%m%d"))
fi$Day=sdate
fi$DayDate=ddate
sdate=sort(unique(sdate))

#' 
#' ## NDVI Compositing
#' 
## ----fgetndvi, warning=FALSE, message=FALSE, cache = F-------------------
getNDVI=function(file,datefile,prefix){
  ndvi=stack(paste0(datadir,"raw/NDVI/",file))
  NAvalue(ndvi)=0
offs(ndvi)=-2
gain(ndvi)=.001
dates=as.Date(read.csv(paste0(datadir,"raw/NDVI/",datefile),header=F)[,1])
names(ndvi)=paste0(prefix,sub("-","",dates))
ndvi=setZ(ndvi,dates)
}

#' 
#' Now use the function to read in the data and add the relevant metadata.
## ----loadLandsat, warning=FALSE, message=FALSE, cache = F----------------
l4=getNDVI(file="2016045_v1g_LT4_L1T_TOA_daily__1988-03-08-1992-11-14.tif",
           datefile="LT4_L1T_TOA2.csv",prefix="L4_")
l5=getNDVI(file="2016045_v1g_LT5_L1T_TOA_daily__1984-06-09-2011-04-17.tif",
           datefile="LT5_L1T_TOA2.csv",prefix="L5_")
l7=getNDVI(file="2016045_v1g_LE7_L1T_TOA_daily__1999-08-30-2016-03-21.tif",
           datefile="LE7_L1T_TOA2.csv",prefix="L7_")
l8=getNDVI(file="2016045_v1g_LC8_L1T_TOA_daily__2013-05-24-2016-03-13.tif",
           datefile="LC8_L1T_TOA2.csv",prefix="L8_")


#' 
#' Let's check out one of the LANDSAT objects.  Raster provides a summary by just typing the object's name:
## ---- warning=FALSE, message=FALSE, cache = F----------------------------
l7

#' 
#' And a plot of a few different dates:
#' 
## ----landsatplot, fig.width=7, fig.height=6, warning=FALSE, message=FALSE, cache = F----
yearind=which(getZ(l7)%in%getZ(l7)[1:5])
levelplot(l7[[yearind]],col.regions=cndvi()$col,cuts=length(cndvi()$at),at=cndvi()$at,layout=c(length(yearind),1),scales=list(draw=F),maxpixels=1e5)

#' 
#' 
#' There is some temporal overlap between sensors, let's look at that:
## ----landsateras,fig.cap="Timeline of LANDSAT data by sensor",fig.height=3, warning=FALSE, message=FALSE, cache = F----
tl=melt(list(l4=getZ(l4),l5=getZ(l5),l7=getZ(l7),l8=getZ(l8)))
xyplot(as.factor(L1)~value,data=tl,pch=16,groups=as.factor(L1),asp=.15,lwd=5,ylab="LANDSAT Satellite",xlab="Date")

#' 
#' There are several ways these data could be combined.  
#' The individual scenes could be assessed for quality (cloud contamination, etc.), 
#' sensors could be weighted by sensor quality (newer=better?).  
#' Today, we'll simply combine (stack) all the available observations for each pixel.  
#' 
## ----ndviprocess, warning=FALSE, message=FALSE, cache = F----------------
ndates=unique(tl$value)
## write that to disk for later use
write.table(ndates,file="data/ndates.csv",row.names=F,col.names=F,sep=",")

### concatenate all sensors to a single raster stack (this just links the filenames)
undvi=stack(l4,l5,l7,l8)
undvi=setZ(undvi,c(getZ(l4),getZ(l5),getZ(l7),getZ(l8)))
### Sort them by date (z)
ndvi=undvi[[order(getZ(undvi))]]

ndvifile="ndvi_wfw.Rdata"
if(!file.exists(ndvifile)){
save(ndvi,file="ndvi_wfw.Rdata")
# ## create a new ndvi layer
# ndvifile="data/ndvi_landsat_30m.tif"
# if(!file.exists(ndvifile)){
# writeRaster(ndvi,filename=ndvifile)
# }
# ## load it
# ndvi=raster(ndvifile)
  
}
load(ndvifile)

#' 
## ----ndviplot,fig.cap="Merged annual maximum LANDSAT NDVI", warning=FALSE, message=FALSE, cache = F----
yearind=which(getZ(ndvi)%in%getZ(l7)[1:5])

levelplot(ndvi[[yearind]],col.regions=cndvi()$col,cuts=length(cndvi()$at),
          at=cndvi()$at,margin=F,scales=list(draw=F),
          names.attr=getZ(ndvi)[yearind],maxpixels=1e4)

#' 
#' # Data Compilation
#' 
#' ## Select domain of interest
#' Here we will define the subset of cells that we will explore further.  In this case it is the WfW plots. You can fiddle with these settings to include fewer (or more) cells.  If your computer is slow, you may want to subset this further.
#' 
#' ## load spatial data masking
## ----sdat,results='asis', warning=FALSE, message=FALSE, cache = F--------

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
janrad=raster(paste0(datadir,"clean/janrad.gri"))
julrad=raster(paste0(datadir,"clean/julrad.gri"))
aspect=raster(paste0(datadir,"clean/aspect.gri"))

### Make a dataframe of all spatial data
## Beware, this approach will only work if your data are all in identical projection/grid/etc.
maskids=extract(mask,alien_xy,cellnumbers=TRUE)
maskids=maskids[,1]

cleardat=data.frame(
  id=extract(ig, maskids),
  coordinates(ig)[maskids,],
  extract(vegclear,alien_xy) #maskids wont work here becasue vegclear is not a raster
)
              
sdat=data.frame(
  id=extract(ig, maskids),
  coordinates(ig)[maskids,],
  veg=extract(rvr, maskids),
  cover=extract(cover, maskids),
  tmax=extract(tmax, maskids),
  tmin=extract(tmin, maskids),
  janrad=extract(janrad, maskids),
  julrad=extract(julrad, maskids),
  aspect=extract(aspect, maskids),
  dem=extract(dem, maskids),
  tpi=extract(tpi, maskids)
 # firecount=extract(fic, maskids)
)

sdat<-cbind(sdat,alien_data@data)
kable(head(sdat))

#' 
#' ## Reshape temporal data
#' It's often easier to work with data in 'long' format where there is one row for each observation and another column indicating what the observation is.  Let's `melt` the data to 'long' format.
## ----tdatl,results='asis', warning=FALSE, message=FALSE, cache = F-------
#extract ndvi data
ftdatw="data/tdatw.Rdata"	
if(!file.exists(ftdatw)){	
  	
tdatw=data.frame(	
  id=extract(ig, maskids),	
#  extract(age, maskids),	
  extract(ndvi,maskids)	
  )	
save(tdatw,file=ftdatw)	
}	
	
load(ftdatw)	
kable(tdatw[1:10,1:10])	

tdatl=melt(tdatw,id.var="id")
tdatln=cbind.data.frame(lab=levels(tdatl$variable),
                        do.call(rbind,strsplit(as.character(levels(tdatl$variable)),"_")))
tdatln$type=ifelse(tdatln[,"1"]%in%c("L4","L5","L7","L8"),"ndvi","age")

tdatl[,c("type","date")]=tdatln[match(tdatl$variable,tdatln$lab),4:3]
tdatl$miss=paste(substr(as.character(tdatl$variable), 1, 2))

tdat=dcast(tdatl,id+date+miss~type,value.var="value")
## convert date to proper format
n = 5 #where to insert separator

tdat$date=paste(substr(as.character(tdat$date), 1, 5-1), ".", substr(as.character(tdat$date), n, nchar(as.character(tdat$date))), sep = "")
## convert year from a factor to numeric
tdat$date=as.Date(as.character(tdat$date),"%Y.%m.%d")

kable(head(tdat))


#' 
#' ### Extract fire data for WfW plots and calculates age in days
## ----fire3, warning=FALSE, message=FALSE, cache = F----------------------

#extract fire data for each wfw plot
fi_plot <- extract(fi,alien_xy)
#create a seq of date from earliest NDVI to most recent
alldate <- seq.Date(min(tdat$date),max(tdat$date),by="day")

firefile="fire.age_wfw.Rdata"
if(!file.exists(firefile)){

fire.age <- matrix(,length(alien_xy),length(alldate))

#loop through plots
for (i in 1:length(alien_xy)){
  # get fire dates for each pixel
  dates.b <- filter(fi_plot, point.ID == i) %>% select(DayDate)
  
  #loop through days - ouch
  for (j in 1:length(alldate)){
    if(any(!is.na(dates.b)) && ((alldate[j]-min(dates.b[,1],na.rm=T))>=0)){
    fire.diff <- alldate[j] - dates.b[,1]
    fire.diff.pos <- fire.diff[which(fire.diff>=0)]
    fire.age[i,j] <- min(fire.diff.pos)
    }
  }
}
fire.age<-as.data.frame(fire.age)
names(fire.age) <- alldate
save(fire.age,file=firefile)
}

load(firefile)


#create data frame
fi.plot.dat <- data.frame(
              id=extract(ig, alien_xy),
              fire.age)
names(fi.plot.dat) <- c("id",as.character(alldate))

fi.age<-melt(fi.plot.dat,id.vars="id")
fi.age$variable<-as.Date(fi.age$variable)


#' 
#' ##join temporal datasets
## ----join, warning=FALSE, message=FALSE, cache = F-----------------------
#join NDVI and age data
tdat.all<-left_join(tdat, fi.age, by = c("id" = "id", "date" = "variable"))
tdat.all.com<-tdat.all[complete.cases(tdat.all),]
## check it out
kable(head(tdat.all),row.names = F)

#join clearing data
clearsmall <- select(cleardat, id,point.ID,First_Date, First_End_, Second_End, Third_End_, Fourth_End, Fifth_End_)
clearsmall$First_Date <- as.Date(clearsmall$First_Date, format="%Y%m%d")
clearsmall$First_End_ <- as.Date(clearsmall$First_End_, format="%Y%m%d")
clearsmall$Second_End <- as.Date(clearsmall$Second_End, format="%Y%m%d")
clearsmall$Third_End_ <- as.Date(clearsmall$Third_End_, format="%Y%m%d")
clearsmall$Fourth_End <- as.Date(clearsmall$Fourth_End, format="%Y%m%d")
clearsmall$Fifth_End_ <- as.Date(clearsmall$Fifth_End_, format="%Y%m%d")

#matrix with a row for every plot and columns up to max fires
fmat<-matrix(,nrow=length(unique(fi_plot$point.ID)),ncol= max(count(fi_plot,point.ID)[,2]))
fnum<-numeric(length(unique(fi_plot$point.ID)))

for(i in  1:length(unique(fi_plot$point.ID))){
  temp <- filter(fi_plot, point.ID == i) %>% select(DayDate)
  temp <- as.character(temp[,1])
  fmat[i,1:length(temp)]<-temp
  fnum[i] <- i
}

fmat<-as.data.frame(fmat)
fmat <- cbind(fnum,fmat)
names(fmat) <- c("point.ID","fire1","fire2","fire3","fire4")

fmat$fire1 <- as.Date(fmat$fire1, format="%Y-%m-%d")
fmat$fire2 <- as.Date(fmat$fire2, format="%Y-%m-%d")
fmat$fire3 <- as.Date(fmat$fire3, format="%Y-%m-%d")
fmat$fire4 <- as.Date(fmat$fire4, format="%Y-%m-%d")


clear.all.dat <- left_join(clearsmall,fmat,by="point.ID")

#' 
#' ## Create some plots
#' 
## ----plots1, echo=FALSE,warning=FALSE,message=FALSE, fig.width = 16, fig.height=12----
#playing with plots

#data restrict to dates which we have fire ages
plotdat <- left_join(tdat.all.com,sdat,by="id") %>%  left_join(clear.all.dat,by="id") %>% mutate(plot.label = paste(Vegetation,Aliens,Site)) %>% filter(ndvi > -2)


#filter for dates or missions
temp1<-filter(plotdat,date < "2007-05-14") %>% filter(miss == "L5")
temp3<-filter(plotdat, date >= "2007-05-14") %>% filter(miss == "L5")

#' 
#' ### Time series by dates
#' 
## ----plots2, echo=FALSE,warning=FALSE,message=FALSE, fig.width = 16, fig.height=12----
#plot recovery for each pixel by date
#pdf("nolines_dates.pdf",width=16,height=12)
P <- ggplot(plotdat,aes(x=date, y=ndvi, group = miss, colour = miss)) +
geom_line() +
coord_cartesian(ylim=c(-0.4,0.8)) +
theme(axis.text = element_text(size = 5), strip.text.x = element_text(size = 6)) +
facet_wrap( ~ plot.label, ncol=9)
suppressWarnings(print(P))
#dev.off()

#' 
#' ### Time series with fires and clearing
#' read vertical lines show clearing, purple vertical lines show fires
## ----plots3, echo=FALSE,warning=FALSE,message=FALSE, fig.width = 16, fig.height=12----
#with vlines
#pdf("lines_dates.pdf",width=16,height=12)
P <- ggplot(plotdat,aes(x=as.numeric(date), y=ndvi, group = miss, colour = miss)) +
geom_line() +
  
geom_vline(aes(xintercept = as.numeric(fire1),col="red")) +
geom_vline(aes(xintercept = as.numeric(fire2),col="red")) +
geom_vline(aes(xintercept = as.numeric(fire3),col="red")) +
geom_vline(aes(xintercept = as.numeric(fire4),col="red")) +
geom_vline(aes(xintercept = as.numeric(as.Date("2015/02/28")),col="red")) +
  
geom_vline(aes(xintercept = as.numeric(First_Date),col="blue")) +
geom_vline(aes(xintercept = as.numeric(First_End_),col="blue")) +
geom_vline(aes(xintercept = as.numeric(Second_End),col="blue")) +
geom_vline(aes(xintercept = as.numeric(Third_End_),col="blue")) +
geom_vline(aes(xintercept = as.numeric(Fourth_End),col="blue")) +
geom_vline(aes(xintercept = as.numeric(Fifth_End_),col="blue")) +
  
coord_cartesian(xlim=c(10000,17000),ylim=c(-0.4,0.8)) +
theme(axis.text = element_text(size = 10)) +
facet_wrap( ~ plot.label, ncol=9) 
suppressWarnings(print(P))
#dev.off()

#' 
#' ### Time series zoomed to clearing data
#' read vertical lines show clearing, purple vertical lines show fires
## ----plots4, echo=FALSE,warning=FALSE,message=FALSE, fig.width = 16, fig.height=12----
#with vlines time zoom
#pdf("lines_dates_zoom.pdf",width=16,height=12)
P <- ggplot(plotdat,aes(x=as.numeric(date), y=ndvi, group = miss, colour = miss)) +
geom_line() +
  
geom_vline(aes(xintercept = as.numeric(fire1),col="red")) +
geom_vline(aes(xintercept = as.numeric(fire2),col="red")) +
geom_vline(aes(xintercept = as.numeric(fire3),col="red")) +
geom_vline(aes(xintercept = as.numeric(fire4),col="red")) +
geom_vline(aes(xintercept = as.numeric(as.Date("2015/02/28")),col="red")) +
  
geom_vline(aes(xintercept = as.numeric(First_Date),col="blue")) +
geom_vline(aes(xintercept = as.numeric(First_End_),col="blue")) +
geom_vline(aes(xintercept = as.numeric(Second_End),col="blue")) +
geom_vline(aes(xintercept = as.numeric(Third_End_),col="blue")) +
geom_vline(aes(xintercept = as.numeric(Fourth_End),col="blue")) +
geom_vline(aes(xintercept = as.numeric(Fifth_End_),col="blue")) +
  
coord_cartesian(xlim=c(14000,17000),ylim=c(-0.4,0.8)) +
scale_x_continuous(limits= c(14000,17000)) +
theme(axis.text = element_text(size = 10)) +
facet_wrap( ~ plot.label, ncol=9) 
suppressWarnings(print(P))
#dev.off()

#' 
#' ### Recovery for all plots
#' showing only L5
## ----plots5, echo=FALSE,warning=FALSE,message=FALSE, fig.width = 16, fig.height=12----
#plot recovery for each pixel by age
#pdf("Veg_age.pdf",width=16,height=12)
P <- ggplot(temp1,aes(x=value, y=ndvi)) +
geom_point() +
geom_smooth(alpha=0.5) +
coord_cartesian(xlim=c(0,8000),ylim=c(-0.4,0.8)) +
theme(axis.text = element_text(size = 15)) +
facet_wrap( ~ plot.label, ncol=9)
suppressWarnings(print(P))
#dev.off()

#' 
#' ### Recovery by vegetation type
#' showing only L5
## ----plots6, echo=FALSE,warning=FALSE,message=FALSE, fig.width = 16, fig.height=12----
# or by veg type
#pdf("Veg_summary.pdf",width=16,height=12)
P<-ggplot(temp1,aes(x=value, y=ndvi)) +
geom_point() +
geom_smooth(alpha=0.5) +
coord_cartesian(xlim=c(0,8000),ylim=c(-0.4,0.8)) +
theme(axis.text = element_text(size = 15)) +
facet_grid(Aliens ~ Vegetation)
suppressWarnings(print(P))
#dev.off()

#' 
#' ### Fynbos invaded plots
#' showing only L5
## ----plots7, echo=FALSE,warning=FALSE,message=FALSE, fig.width = 16, fig.height=12----
test<-filter(temp1,Vegetation == "Fynbos" & Aliens == "Invaded")
#plot recovery for each pixel by age
#pdf("F_I.pdf",width=16,height=12)
P<-ggplot(test,aes(x=value, y=ndvi)) +
geom_point() +
geom_smooth(alpha=0.5) +
coord_cartesian(xlim=c(0,8000),ylim=c(-0.4,0.8)) +
theme(axis.text = element_text(size = 15)) +
facet_wrap( ~ plot.label, ncol=3) 
suppressWarnings(print(P))
#dev.off()

#' 
#' ### Fynbos plots with no aliens
#' showing only L5
## ----plots8, echo=FALSE,warning=FALSE,message=FALSE, fig.width = 16, fig.height=12----
test<-filter(temp1,Vegetation == "Fynbos" & Aliens == "No Aliens")
#plot recovery for each pixel by age
#pdf("F_NA.pdf",width=16,height=12)
P<-ggplot(test,aes(x=value, y=ndvi)) +
geom_point() +
geom_smooth(alpha=0.5) +
coord_cartesian(xlim=c(0,8000),ylim=c(-0.4,0.8)) +
theme(axis.text = element_text(size = 15)) +
facet_wrap( ~ plot.label, ncol=3)
suppressWarnings(print(P))
#dev.off()



#' 
#' ### Save R data object for later use.
## ----save----------------------------------------------------------------
## drop the 'wide' version
save(clear.all.dat,tdat.all,file="data/modeldata_nofire_wfw.Rdata")

#' 
## ----echo=FALSE,eval=FALSE,results='hide',message=FALSE,error=FALSE------
## ## this chunk outputs a copy of this script converted to a 'normal' R file with all the text and chunk information commented out
## purl("workflow/7_WorkingforWater/DataPrepFullRecord_WfW.Rmd",documentation=2,output="workflow/7_WorkingforWater/DataPrepFullRecord_WfW.R", quiet = TRUE)

#' 
