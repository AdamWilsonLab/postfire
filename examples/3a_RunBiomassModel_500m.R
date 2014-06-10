#####################################
### Biomass accumulation model preparation

system("ssh -X protea")

setwd("/media/Data/Work/Regional/CFR/BiomassAccumulation/BiomassAccumulation_500m/")
### Run R in GRASS if desired to enable pushing maps to GIS
#system("grass63 -text /media/Data/Work/grassdata/CFR/biomass_500m")
#db.connect database=/media/Data/Work/grassdata/CFR/biomass_500m/sqlite.db driver=sqlite


#R
setwd("/media/Data/Work/Regional/CFR/BiomassAccumulation/BiomassAccumulation_500m/")
#library(spgrass6)
library(sp)#;library(rgdal);library(lattice)
library(reshape);library(multicore)
library(multicore)
source("/media/Data/Work/Stat/Scripts/Functions.r")


## Convert ndvi list to long format and save it - takes a long time (~30minutes) is very memory intensive
load(file="analysis/data/ndvi.Rdata")  #only load if ndvi timeseries needed
ndvi=rbind.fill(ndvi);gc() #6 minutes
save(ndvi,file="data/ndvilong.Rdata")
#load("data/ndvilong.Rdata")
## Do some QC on the NDVI data
ndvi=ndvi[!is.na(ndvi$age)&!is.na(ndvi$ndvi),] #drop obs of unknown age or ndvi
ndvi=ndvi[ndvi$dfireboundm>=500&ndvi$dcoastm>=1000,]  # drop obs near fire boundaries and near coast
ndvi$agemonth=round(as.numeric(ndvi$ndvidate-ndvi$firedate)/30)  #add monthly age term (to aggregate to monthly later)
ndvi=ndvi[!ndvi$id%in%names(tapply(ndvi$agemonth,ndvi$id,function(x) max(x)<36))[tapply(ndvi$agemonth,ndvi$id,function(x) max(x)<36)],] #drop records with no data less than 3 years old
ndvi=ndvi[!ndvi$id%in%names(table(ndvi$id))[table(ndvi$id)<2*12*3],] #drop records with less than 3 years of data
ndvi=ndvi[order(ndvi$id,ndvi$time),]  #sort it for convenience later
ndvi$firemonth=as.integer(format(ndvi$firedate,"%m"))  #add fire month
save(ndvi,file="analysis/data/ndvilong_subset.Rdata")
gc()

################################################################
#####  Start here if the above ndvi processing has already been done

### Load the full data set
load(file="analysis/data/ndvilong_subset.Rdata")
load(file="analysis/data/FireDataClean.Rdata")
load("analysis/data/covariates.Rdata") #loads d_full

## drop transformed pixels (so no predictions will be made in transformed areas
d=d[d$transformed==0,]
#d=d[d$cfr,]

## drop non-fynbos pixels
ndvi=ndvi[ndvi$id%in%d$ID,]  
### Drop uneeded columns
ndvi=ndvi[,c("id","time","ndvi","ndvidate","fireid","lat","lon","age","prefireage","agemonth","firemonth")];gc()

########################
#### Set model name for naming objects below
mname="20111127_75p"
if(!file.exists(paste("analysis/modeloutput/",mname,sep=""))) dir.create(paste("analysis/modeloutput/",mname,sep=""),recursive=T)

### subset dataset
holdout=0.25  #percent to hold out for validation
s=sort(sample(unique(ndvi$id),round(length(unique(ndvi$id))*(1-holdout)))); length(s)
write.csv(s,paste("analysis/modeloutput/",mname,"/",mname,"_subset.csv",sep=""),row.names=F)
#s=read.csv(paste("analysis/modeloutput/",mname,"/",mname,"_subset.csv",sep=""))[,1]
d$subset=factor(ifelse(d$ID%in%unique(ndvi$id),ifelse(d$ID%in%s,"Model Fitting","Validation"),"Prediction"),levels=c("Model Fitting","Validation","Prediction"),ordered=T)

## Select and scale environmental data
#envars=c("dem","eastwest","northsouth","slope","rad_ave","map","mmp01","mmp7","tmax01","tmin07","pptconc","apan","apan_12","heatwaves","soilhighfert","soilfinetext","soilacid")
envars=c("dem","eastwest","slope","rad_ave","map","mmp01","tmax01","tmin07","pptconc","soilhighfert","soilfinetext","soilacid")
#envars=c("dem","eastwest","slope","rad_ave","soilhighfert","soilfinetext","soilacid") #noclimate

scaled=scale(as.matrix(d@data[match(unique(ndvi$id),d$ID),envars[!grepl("soil",envars)]]))
env_full=cbind(intercept=1,scaled,
  as.matrix(d@data[match(unique(ndvi$id),d$ID),envars[grepl("soil",envars)]]))
env_full[is.nan(env_full)]=NA

cor(env_full,na.rm=T)>.75

### Save the scaling parameters to convert fitted coefficients back to metric units
beta.mu=c(intercept=0,attr(scaled,"scaled:center"),soilhighfert=0,soilfinetext=0,soilacid=0)
beta.sd=c(intercept=1,attr(scaled,"scaled:scale"),soilhighfert=1,soilfinetext=1,soilacid=1)
rm(scaled)  #drop the scaled data

######################################################################################
### Set up the data for modeling
ndvi_full=ndvi
ndvi=ndvi[ndvi$id%in%s,]; gc() 

## create two env frames for fitting and prediction
env=env_full[rownames(env_full)%in%s,]
  
### Drop missing values
omit=unique(ndvi$id)[as.numeric(which(is.na(apply(env,1,sum))))]; omit
if(length(omit)>0){
  env=env[!rownames(env)%in%omit,]
  ndvi=ndvi[!ndvi$id%in%omit,]
}
ndvi$id2=as.integer(as.factor(ndvi$id)); gc()

## Get counts
nGrid=length(unique(ndvi$id))            ;nGrid
nTime=length(unique(ndvi$time))          ;nTime
nBeta=ncol(env)                          ;nBeta

## Write data object
data=list(
  age=ndvi$age,
  ndvi=ndvi$ndvi,
  id=ndvi$id2,
  firemonth=ndvi$firemonth,
  nObs=nrow(ndvi),
  env=env,
  nGrid=nGrid,
  nBeta=nBeta
  )

## Function to generate initial values
gen.inits=function(nGrid,nBeta) { list(
  ## spatial terms
  alpha=runif(nGrid,0.1,0.5),
  gamma=runif(nGrid,0.1,.9),
  lambda=runif(nGrid,0.2,1),
 # k=runif(nGrid,0.4,.9),
  A=runif(nGrid,0,1),
  ## spatial means
  alpha.mu=runif(1,0.1,0.2),
  ## priors  
  gamma.beta=runif(nBeta,0,1),
  gamma.tau=runif(1,1,5),
  alpha.tau=runif(1,1,5),
  lambda.beta=runif(nBeta,0,2),
  lambda.tau=runif(1,0,2),
 # k.beta=rep(0,nBeta),
 # k.tau=runif(1,0,2),
  A.beta=runif(nBeta,0,0.05),
  A.tau=runif(1,0,2),
  phi=runif(1,0.1,1),
  tau=runif(1,0,2)
  )
}

## list of parameters to monitor later
params=c("gamma.beta","gamma.sigma","alpha","alpha.mu","alpha.sigma","lambda.beta","lambda.sigma","A.beta","A.sigma","sigma","phi")

### Save all data into Rdata object for model fitting
save(data,gen.inits,s,d,beta.mu,beta.sd,envars,env_full,ndvi_full,file=paste("analysis/modeloutput/",mname,"/",mname,"_inputdata.Rdata",sep=""))  #save it

### Now go to RunCluster, scp the data, and run the model there.

################################################################
################################################################
### JAGS

library(rjags)
library(snow)
source("analysis/multicore.R")

write.table(paste("Starting model ",mname," on ",date()),"ModelStart.txt")

## test compilation
system.time(m <<- jags.model(file="/media/Data/Work/projects/dissertation/5_Biomass/analysis/3b_BiomassModel_500m_jags.R",data=data,inits=gen.inits(),n.chains=1,n.adapt=100))
params=c("gamma.beta","gamma.sigma","k","k.beta","k.sigma","alpha","lambda.beta","lambda.sigma","A.beta","A.sigma","sigma","phi")
system.time(mc <<- coda.samples(m,params,n.iter=100))

## original 45minute compile, 36 minutes sample

## Parallel version
nChain=2
cl <- makeCluster(nChain, type = "SOCK")
clusterExport2(cl,".eval",.eval)
t1=system.time(m <<- cluster.jags.model(cl,file="/media/Data/Work/projects/dissertation/5_Biomass/analysis/3b_BiomassModel_500m_jags.R",data=data,inits=gen.inits,n.adapt=500));t1;gc()
t2=system.time(mc <<- cluster.coda.samples(cl,m,params,n.iter=500,thin=1));t2;gc()

stopCluster(cl)
## Get DIC
#dic=cluster.jags.dic(cl,m, n.iter=10, thin = 1, type="pD")  #doesn't seem to work on parallel models!

## Save output
save(m,mc,s,d,t1,t2,beta.mu,beta.sd,file=paste("analysis/modeloutput/",mname,"/",mname,".Rdata",sep=""))  #save it

