######################################################################
######################################################################
### NDVI stats - fit sine-exponential curve to ndvi data
setwd("/media/Data/Work/Regional/CFR/BiomassAccumulation_500m/")

load("data/ndvidata.Rdata")  #loads ndvi object (with ages and fire info) needed for fitcurve
load("data/FireDataClean.Rdata")
source("biomass_functions.R")
library(multicore); library(reshape);library(heR.Misc) # for dapply




## a few random plots
ndvi2=do.call(rbind,ndvi)
#ndvi_cn=ndvi2[ndvi2$source=="CN",]
#ndvi_mod=ndvi2[ndvi2$source=="MCD45A1",]

xyplot(evi~age|id,type="l",data=ndvi2[ndvi2$id%in%sample(ndvi2$id,1000),],layout=c(2,1))

minage=as.numeric(names(which(tapply(ndvi2$age,ndvi2$id,min,na.rm=T)<1)))
xyplot(evi~age,groups=id,type="l",data=ndvi2[ndvi2$id%in%minage[1:1000],])


st=data.frame(ndvi=tapply(ndvi2$ndvi,round(ndvi2$age,1),quantile,c(0.025,.25,.5,.75,.975),na.rm=T)); st$age=as.numeric(rownames(st))
plot(ndvi~age,data=st,type="l",xlim=c(0,10))

#############################
### Explore the fitted breaks

table(ndvi2$breakfire)










#############################################################################
### Function to fit the curve
fitcurve<-function(bdt){  ## break cluster into separate dataframes for each pixel
##  dt=dapply(bdt,by=list(id=bdt$id,fireid=bdt$fireid),function(bdtt) {  #bdtt will be a dataframe for each pixel & Fire separately
  dt=dapply(bdt,by=list(id=bdt$id),function(bdtt) {  #bdtt will be a dataframe for each pixel separately - combine all fires
    if(sum(!is.na(bdtt$ndvi))==0) return(data.frame(mindate=NA,minndvi=NA,maxage=NA,minage=NA,firedate=NA, #if all NA, skip it
                      gamma=NA,alpha=NA,beta=NA,s1=NA,s2=NA,s3=NA,ss=NA,niter=NA,curvemin=NA,rtime=NA,
                      miss=1,miss.pre=NA,miss.post=NA,from=NA,to=NA,stringsAsFactors=F))
    bf=bdtt$age<0
    ## Get some summaries
    mindate=as.character(bdtt$ndvidate[which.min(bdtt$ndvi)])
    minndvi=ifelse(sum(!is.na(bdtt$ndvi))>=10,min(bdtt$ndvi,na.rm=T),NA)
    preb=ifelse(bdtt$maxdate[1]>min(date),mean(bdtt$ndvi[bdtt$prefireage>-1&bdtt$prefireage<0],na.rm=T),NA)  #mean ndvi of the previous year
    miss=sum(is.na(bdtt$ndvi))/nrow(bdtt)
    miss.pre=ifelse(sum(bf,na.rm=T)>0,sum(is.na(bdtt$ndvi[bf]))/nrow(bdtt[bf,]),NA)
    miss.post=ifelse(sum(!bf,na.rm=T)>0,sum(is.na(bdtt$ndvi[!bf]))/nrow(bdtt[!bf,]),NA)
    from=as.character(min(bdtt$ndvidate[!bf],na.rm=T))
    to=as.character(max(bdtt$ndvidate[!bf],na.rm=T))
    maxage=ifelse(sum(!is.na(bdtt$age))==0,NA,max(bdtt$age,na.rm=T))
    minage=ifelse(sum(!is.na(bdtt$age))==0,NA,min(bdtt$age,na.rm=T))
    ## Fit the curve if greater than 10 measurements post-fire
    if(sum(!is.na(bdtt$ndvi[!bf]))>=10){ ## if more than 10 non-na ndvis after fire,  fit the curve
      dt=na.omit(data.frame(x=bdtt$age[!bf],y=bdtt$ndvi[!bf]))  #drop missing data
      try1 <-try(minimize.fn(minimize.fn(parms,x=dt$x,y=dt$y)$parms,x=dt$x,y=dt$y)) #try function
      if(class(try1)=="try-error") { #Report error and return NAs - seems to be occasional reaction to starting parms, add loop here to jitter parms?
        ## Only 40ish have errors
#        assign("errorIDs",c(errorIDs,bdtt$id[1]),envir=baseenv())
        write.table(bdtt$id[1],"errors.txt",append=T,col.names=F,row.names=F)
        print(paste("fireid=",bdtt$fireid[1],"  ID=",bdtt$id[1],"   NA's=",sum(is.na(dt$y)),"    parms=",paste(parms,collapse=",")))
        return(data.frame(mindate,minndvi,maxage=maxage,minage=minage,firedate=as.character(bdtt$firedate[1]),
                          gamma=NA,alpha=NA,beta=NA,s1=NA,s2=NA,s3=NA,ss=NA,niter=NA,curvemin=NA,rtime=NA,
                          miss=miss,miss.pre=miss.pre,miss.post=miss.post,from=from,to=to,stringsAsFactors=F))
      }
      r<-minimize.fn(minimize.fn(parms,x=dt$x,y=dt$y)$parms,x=dt$x,y=dt$y) #run optim function
#      r<-minimize.fn(parms,x=dt$x,y=dt$y) #run optim function
      rtime=seq(0,50,.01)[abs(r$parms["gamma"]-exp(r$parms["alpha"]+r$parms["beta"]*seq(0,50,.01))-r$parms["gamma"])<=0.001][1] #recovery time
      return(data.frame(mindate,minndvi,maxage=maxage,minage=minage,firedate=as.character(bdtt$firedate[1]),
                        rbind(unlist(r$parms)),ss=r$SS,niter=r$count,curvemin=min(r$fit),rtime=rtime,
                        miss=miss,miss.pre=miss.pre,miss.post=miss.post,from=from,to=to,stringsAsFactors=F))
    }
    ## Otherwise just return NAs for parameters
    return(data.frame(mindate,minndvi,maxage=maxage,minage=minage,firedate=as.character(bdtt$firedate[1]),
                      gamma=NA,alpha=NA,beta=NA,s1=NA,s2=NA,s3=NA,ss=NA,niter=NA,curvemin=NA,rtime=NA,
                      miss=miss,miss.pre=miss.pre,miss.post=miss.post,from=from,to=to,stringsAsFactors=F))
  })
  print(paste("%%%%%%%%%%%%%%%%%%%%%%%%%%%%   Finished cluster ",bdt$cluster[1]," %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"))
  gc()
  return(dt)
} 

### test it on small version
 file.remove("errors.txt")  #remove file that will hold the pixels that don't converge
 ndvit=lapply(ndvi,function(x) x[1:10000,])  #make small version for testing
 ndvit=ndvit[5:10]
 ndvit=ndvit[which(unlist(lapply(ndvit,function(x) !is.null(x))))]  #drop ones that are null
 curves=mclapply(ndvit,fitcurve,mc.cores=2)
# curves=fitcurve(ndvi[[2]][1:10000,])
# head(curves[[1]])
# rm(ndvit)
# curves=lapply(ndvi[1],fitcurve)
rm(ndvit)

### Run it
file.remove("errors.txt")  #remove file that will hold the pixels that don't converge
system.time(curves<<-mclapply(ndvi,fitcurve,mc.cores=2,mc.preschedule=F)) #4.7 hours

errors=read.table("errors.txt")
nrow(errors)

## check for nulls
errors=which(unlist(lapply(curves,function(x) ifelse(sum(grepl("Error",x))>0,T,F)))); print("these clusters have errors:"); errors
if(length(unique(c(errors)))>0) curves=curves[-unique(errors)]  #drop ones that have errors if any

### collapse and clean up
curveslist=curves
curves=do.call(rbind.data.frame,curveslist)
curves$id=as.integer(as.character(curves$id))
for(i in c("mindate","firedate","from","to")) curves[,i]=as.Date(curves[,i])

### Save it
save(curves,file="data/curves.Rdata")

