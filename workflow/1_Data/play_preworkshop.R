#############################################################################
######## Code to play with landsat and GIS data for the Cape Peninsula
##############################################################################
######## Compiled by Jasper Slingsby 2014
######## Last edited: 8 July 2014
##############################################################################
######## 
###Steps:
##############################################################################

library(raster,sp)

###Set working directory 
if (Sys.getenv("USER")=='jasper') setwd("/Users/jasper/Dropbox/Shared/Postfire_workshop/data") #use "USER" for Mac
if (Sys.getenv("USERNAME")=='whoeveryouare') setwd ("C:/") #for PC

###Get landsat data
l4=stack("clean/landsat/20140630_54ef46e7ea_LT4_L1T_ANNUAL_GREENEST_TOA__1982-1993-0000000000-0000000000.tif"); setZ(l4, 1982:1993, name='time'); names(l4)=paste("L4", 1982:1993, sep="_"); gain(l4)=0.01
l5=stack("clean/landsat/20140630_54ef46e7ea_LT5_L1T_ANNUAL_GREENEST_TOA__1984-2012-0000000000-0000000000.tif"); setZ(l5, 1984:2012, name='time'); names(l5)=paste("L5", 1984:2012, sep="_"); gain(l5)=0.01
l7=stack("clean/landsat/20140630_54ef46e7ea_LE7_L1T_ANNUAL_GREENEST_TOA__1999-2014-0000000000-0000000000.tif"); setZ(l7, 1999:2014, name='time'); names(l7)=paste("L7", 1999:2014, sep="_"); gain(l7)=0.01
l8=stack("clean/landsat/20140630_54ef46e7ea_LC8_L1T_ANNUAL_GREENEST_TOA__2013-2014-0000000000-0000000000.tif"); setZ(l8, 2013:2014, name='time'); names(l8)=paste("L8", 2013:2014, sep="_"); gain(l8)=0.01

###Trim extents
peninsula = extent(250000, 270210, 6189390, 6247260)
l4=crop(l4, peninsula)
l5=crop(l5, peninsula)
l7=crop(l7, peninsula)
l8=crop(l8, peninsula)

###Get covariates
index=raster(paste0(datadir,"clean/indexgrid_landsat_30m.grd"))
veg=raster(paste0(datadir,"clean/vegtypes_landsat_30m.grd"))
cover=raster(paste0(datadir,"clean/landcover2009_landsat_30m.grd"))
dem=raster(paste0(datadir,"clean/dem_landsat_30m.grd"))
age=read.csv(paste0(datadir,"clean/vegage.csv"))

###Make all data one big dataframe
dat=cbind(as.data.frame(index), as.data.frame(veg), as.data.frame(cover), as.data.frame(dem), age, as.data.frame(l4), as.data.frame(l5), as.data.frame(l7), as.data.frame(l8))
names(dat)[1:4]=c("Index", "Vegetation_type", "Landcover", "Elevation")

###Free up some memory
dat=na.omit(dat) #remove rows with NA data
rm(list=c("l4","l5","l7","l8","age","cover","veg","index","dem", "peninsula")) #remove unwanted objects from memory
dat=dat[which(dat$Landcover==1),] #trim to "natural" veg only

###Have a first squiz at the data by comparing among missions
plot(dat$L8_2013 ~ dat$L7_2013, ylab="Landsat 8", xlab="Landsat 7")
summary(lm(dat$L7_2013 ~ dat$L8_2013))

summary(lm(dat$L4_1990 ~ dat$L5_1990))
summary(lm(dat$L5_1999 ~ dat$L7_1999))

###Play with one mission - Landsat7
##First remove cells that have never burnt
dat_age=dat[-which(dat$X2007==93),]

##NDVI ~ Age across all cells
plot(dat_age$L7_2007 ~ dat_age$X2007)

##Fit a linear model
fit1=lm(dat_age$L7_2007 ~ dat_age$X2007)
summary(fit1)
abline(fit1, col="red")

##Fit a Loess spline
#fit2=loess(dat_age$L7_2007 ~ dat_age$X2007)
#abline(fit2, col="blue")

##Explore max NDVI by veg type
L7=dat[,91:106]
L7_max=apply(L7, MARGIN=1, FUN="max")
boxplot(L7_max~dat$Vegetation_type)

###Try NDVI~age and veg type?
fit2=lm(dat_age$L7_2007 ~ dat_age$X2007 + dat_age$Vegetation_type)
summary(fit2)

fit3=lm(dat_age$L7_2007 ~ dat_age$X2007 + dat_age$Vegetation_type + dat_age$Elevation)
summary(fit3)

###Quick look at a few cells
GF=dat_age[which(dat_age$Vegetation_type==15),] #Granite F

plot(unlist(GF[1,])[91:106], type="both")

SF=dat_age[which(dat_age$Vegetation_type==16),] #Sandstone F

plot(unlist(SF[1,])[91:106], type="both")
