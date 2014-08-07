#############################################################################
######## Code to play with interpolation of TM microclimate data to the 
######## Lansat 30m UTM 34S grid for the Cape Peninsula
##############################################################################
######## Compiled by Jasper Slingsby 2014
######## Last edited: 16 July 2014
##############################################################################
######## 
###Steps:
##############################################################################

###Get libraries
library(raster)
library(gstat)
library(rgdal)
library(automap)

###Set working directory
if (Sys.getenv("USER")=='jasper') setwd("/Users/jasper/Dropbox/Shared/Postfire_workshop/data")
###SET YOUR USERNAME AND WORKING DIRECTORY - REMEMBER TO COPY THE FILES TO YOUR COMPUTER AND DON'T NOT TO RUN THIS DIRECTLY FROM DROPBOX!!!
if (Sys.getenv("USER")=='macpunk') setwd("/Dropbox/Shared/Postfire_workshop/data")
if (Sys.getenv("USERNAME")=='windowspunk') setwd("/Dropbox/Shared/Postfire_workshop/data")

########################
###Fetch raster data
########################
#Fetch, rename etc
datnames=list.files("clean", pattern=".grd", full.names=T)
rdat=lapply(datnames, FUN="raster")
rdat=stack(rdat)
nms=substr(list.files("clean", pattern=".grd", full.names=F), 1, 3)
names(rdat)=nms

#Make a "SpatialPixelsDataFrame"
allD<-as(rdat,"SpatialPixelsDataFrame")

########################
###Fetch logger data
########################
#Load jan and july microclimate stats
jan<-read.csv("raw/Microclimate/jan.csv", row.names = 1, header = T, stringsAsFactors = F)
july<-read.csv("raw/Microclimate/july.csv", row.names = 1, header = T, stringsAsFactors = F)

#Make spatial data
jan<-SpatialPointsDataFrame(cbind(jan$UTM.east,jan$UTM.north), jan, proj4string=CRS(proj4string(allD)))
july<-SpatialPointsDataFrame(cbind(july$UTM.east,july$UTM.north), july, proj4string=CRS(proj4string(allD)))

#Rename variables in jan and july to match allD
nms=nms[-which(nms%in%c("fir", "lan", "oce", "veg", "ind"))]
nmsj=c("asp30m", "dem30m", "slp30m", "tpi500.30m")
for(i in 1:length(nms)){names(jan)[which(names(jan)%in%nmsj[i])]=nms[i]}
for(i in 1:length(nms)){names(july)[which(names(july)%in%nmsj[i])]=nms[i]}

#################################
###Spatial prediction
#################################
 #fits the variogram automatically by estimating starting parameters from the data and then running fit.variogram()
jankru <- autoKrige(Tmax.mean ~ dem + asp + slo + tpi, input_data=jan, allD, na.action=na.pass)
julykru <- autoKrige(Tmin.mean ~ dem + asp + slo + tpi, input_data=july, allD, na.action=na.pass)

#####################
###Plot results
par(mfrow=c(1,2))
image(raster(jankru$krige_output), col=rev(heat.colors(18)), main="Average January Tmax")
points(jan, cex=.25) #datalogger locations
image(raster(julykru$krige_output), col=rev(heat.colors(18)), main="Average July Tmin")
points(july, cex=.25) #datalogger locations

pdf("/Users/jasper/Dropbox/SAEON/Conferences/Biodiversity SA 2013/Analyses/TM_Jan_TMax.mean.pdf", height=7, width=4)
image(raster(jankru$krige_output), col=rev(heat.colors(18)), main="Average January Tmax")
points(july, cex=.5, pch=19) #datalogger locations
dev.off()

pdf("/Users/jasper/Dropbox/SAEON/Conferences/Biodiversity SA 2013/Analyses/TM_Jul_TMin.mean.pdf", height=7, width=4)
image(raster(julykru$krige_output), col=rev(heat.colors(18)), main="Average July Tmin")
points(july, cex=.5, pch=19) #datalogger locations
dev.off()

#################################
###Cross validation
#################################
#"leave-one-out CV for universal kriging with 4 vars
jankru.cv <- autoKrige.cv(Tmax.mean ~ dem + asp + slo + tpi, input_data=jan) #, allD, na.action=na.pass)
summary(jankru.cv)

julykru.cv <- autoKrige.cv(Tmin.mean ~ dem + asp + slo + tpi, input_data=july) #, allD, na.action=na.pass)
summary(julykru.cv)

par(mfrow=c(1,2))
bubble(jankru.cv$krige.cv_output, "residual", main = "Tmax.mean CV residuals")
bubble(julykru.cv$krige.cv_output, "residual", main = "Tmin.mean CV residuals")


################################
###Play with velocities
################################
janR<-raster(jankru$krige_output)
julR<-raster(julykru$krige_output)

climslopeX<-terrain(janR,'slope')
climslopeN<-terrain(julR,'slope')

pdf("/Users/jasper/Dropbox/SAEON/Conferences/Biodiversity SA 2013/Analyses/Tmax.slope.pdf", height=7, width=4)
image(climslopeX, col=heat.colors(18), main="Tmax Slope")
dev.off()

image(log(1/climslopeX), col=heat.colors(18))

image(climslopeN)

spplot(log(1/climslopeX))


climaspX<-terrain(janR,'aspect')
climaspN<-terrain(julR,'aspect')

image(climaspX)
image(climaspN)

image(climslopeX*climaspX)