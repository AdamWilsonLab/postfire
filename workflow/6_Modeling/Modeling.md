# Modeling PostFireTrajectories
Adam M. Wilson  
`r format(Sys.time(), "%B %d, %Y")`  




# Data

Load the model data we made in [DataPrep.R](../1_Data/DataPrep.R)

```r
load("data/modeldata.Rdata")
rv_meta=read.csv("data/vegtypecodes.csv")
sdat$vegn=rv_meta$code[match(sdat$veg,rv_meta$ID)]
rownames(sdat)=sdat$id

## clip tdat to include only 1984:2006 due to missing recent fire data
tdat=tdat[tdat$year%in%1984:2006,]
```

We know have two data frames we'll use in the modeling. The first is the spatial data:

```r
kable(head(sdat),row.names=F)
```



|    id|      x|       y| veg| cover|  tmax|   tmin|   dem|   tpi|vegn                         |
|-----:|------:|-------:|---:|-----:|-----:|------:|-----:|-----:|:----------------------------|
| 83925| 260445| 6243525|  18|     1| 28.19|  9.413| 152.5| 7.934|Peninsula Shale Renosterveld |
| 84598| 260415| 6243495|  18|     1| 28.40|  9.556| 150.5| 7.260|Peninsula Shale Renosterveld |
| 84599| 260445| 6243495|  18|     1| 28.19|  9.459| 149.6| 6.183|Peninsula Shale Renosterveld |
| 84600| 260475| 6243495|  18|     1| 28.52|  9.779| 146.9| 5.759|Peninsula Shale Renosterveld |
| 84601| 260505| 6243495|  18|     1| 28.79| 10.022| 138.2| 5.770|Peninsula Shale Renosterveld |
| 85271| 260385| 6243465|  18|     1| 28.50|  9.714| 139.8| 5.918|Peninsula Shale Renosterveld |

And the second is the temporal data:

```r
kable(head(tdat),row.names=F)
```



|    id| year| age|   ndvi|
|-----:|----:|---:|------:|
| 83925| 1984| -23| 0.2790|
| 83925| 1985| -24| 0.5820|
| 83925| 1986| -25| 0.4830|
| 83925| 1987| -26| 0.3160|
| 83925| 1988| -27| 0.3080|
| 83925| 1989| -28| 0.4155|


```r
## subset to only two fynbos veg types
n=500
sid=c(
  sample(which(sdat$vegn=="Peninsula Sandstone Fynbos"),n),
  sample(which(sdat$vegn=="Peninsula Granite Fynbos - North"),n),
  sample(which(sdat$vegn=="Hangklip Sand Fynbos"),n)
)
sdat=sdat[sid,]

#drop obs of unknown age or ndvi or with no data in sdat
tdat=tdat[!is.na(tdat$age)&
          !is.na(tdat$ndvi)&
          tdat$id%in%sdat$id
          ,] 

## drop negative ages (time before first fire) for now
tdat=tdat[tdat$age>=0,]
```

## Subsample Data

```r
#### Set model name for naming objects below and create directory to hold output
mname="v1"
if(!file.exists(paste("output/",mname,sep=""))) dir.create(paste("output/",mname,sep=""),recursive=T)

### subset dataset
holdout=0.50  #percent to hold out for validation
s=sort(sample(unique(sdat$id),round(length(unique(sdat$id))*(1-holdout)))); length(s)
```

```
## [1] 750
```

```r
write.csv(s,paste("output/",mname,"/",mname,"_subset.csv",sep=""),row.names=F)
sdat$subset=factor(ifelse(sdat$id%in%unique(tdat$id),ifelse(sdat$id%in%s,"Model Fitting","Validation"),"Prediction"),levels=c("Model Fitting","Validation","Prediction"),ordered=T)
```

## Create dummy variables for vegetation (and any other factors)

```r
sdat$veg=as.factor(sdat$veg)
lm1=lm(dem~veg,data=sdat)
tveg=model.matrix(lm1)[,-1]

kable(head(tveg))
```



|        | veg14| veg16|
|:-------|-----:|-----:|
|929831  |     0|     1|
|286018  |     0|     1|
|832818  |     0|     1|
|714090  |     0|     1|
|907728  |     0|     1|
|1088969 |     0|     1|


```r
## Select and scale environmental data
envars=c("dem","tpi","tmax","tmin")

scaled=scale(as.matrix(sdat[,envars]))
env_full=cbind(intercept=1,scaled,tveg)

### Save the scaling parameters to convert fitted coefficients back to metric units later
beta.mu=c(intercept=0,attr(scaled,"scaled:center"),rep(0,ncol(tveg)))
beta.sd=c(intercept=1,attr(scaled,"scaled:scale"),rep(1,ncol(tveg)))
rm(scaled)  #drop the scaled data
```


## Create model data

```r
tdat_full=tdat
tdat=tdat[tdat$id%in%s,]; gc() 
```

```
##           used (Mb) gc trigger  (Mb) max used  (Mb)
## Ncells 1250365 66.8    2251281 120.3  2100519 112.2
## Vcells 1631992 12.5   62999968 480.7 78190494 596.6
```

```r
## create two env frames for fitting and prediction
env=env_full[rownames(env_full)%in%s,]
  
### Drop missing values
omit=unique(tdat$id)[as.numeric(which(is.na(apply(env,1,sum))))]; omit
```

```
## numeric(0)
```

```r
if(length(omit)>0){
  env=env[!rownames(env)%in%omit,]
  tdat=tdat[!tdat$id%in%omit,]
}

## create new id that goes from 1 to nGrid
tdat$id2=as.integer(as.factor(tdat$id)); gc()
```

```
##           used (Mb) gc trigger  (Mb) max used  (Mb)
## Ncells 1250413 66.8    2251281 120.3  2100519 112.2
## Vcells 1643594 12.6   50399974 384.6 78190494 596.6
```

```r
## Get counts
nGrid=length(unique(tdat$id))            ;nGrid
```

```
## [1] 725
```

```r
nTime=length(unique(tdat$year))          ;nTime
```

```
## [1] 23
```

```r
nBeta=ncol(env)                          ;nBeta
```

```
## [1] 7
```

```r
## Write data object
data=list(
  age=tdat$age,
  ndvi=tdat$ndvi,
  id=tdat$id2,
  nObs=nrow(tdat),
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
  ## spatial means
  alpha.mu=runif(1,0.1,0.2),
  ## priors  
  gamma.beta=runif(nBeta,0,1),
  gamma.tau=runif(1,1,5),
  alpha.tau=runif(1,1,5),
  lambda.beta=runif(nBeta,0,2),
  lambda.tau=runif(1,0,2),
  tau=runif(1,0,2)
  )
}

## list of parameters to monitor (save)
params=c("gamma.beta","gamma.sigma","alpha",
         "alpha.mu","alpha.sigma","lambda.beta","lambda.sigma")

### Save all data into Rdata object for model fitting
save(data,gen.inits,s,sdat,beta.mu,beta.sd,envars,env_full,tdat_full,
     file=paste("output/",mname,"/",mname,"_inputdata.Rdata",sep="")) 
```


# JAGS

```r
foutput=paste0("output/",mname,"/",mname,"_modeloutput.Rdata")

if(!file.exists(foutput)){

  write.table(paste("Starting model ",mname," on ",date()),
            paste0("output/",mname,"ModelStart.txt"))

  ## test compilation
  t1=system.time(m <<- jags.model(file="workflow/6_Modeling/Model.R",
                             data=data,
                             inits=gen.inits(data$nGrid,data$nBeta),
                             n.chains=3,n.adapt=10000))
  t2=system.time(mc <<- coda.samples(m,params,n.iter=10000))

  save(m,mc,beta.mu,beta.sd,file=foutput)  
}
```

Model Summaries

```r
if(!exists("mc")) load(foutput)

## Potentially thin the data
mc2=window(mc,thin=1,start=1)
```

```
## Warning: start value not changed
## Warning: start value not changed
## Warning: start value not changed
```

```r
### Extract regression coefficients
mc_reg=mc2[,grep("gamma[.]|lambda[.]",colnames(mc[[1]]))]

xyplot(mc_reg)
```

![plot of chunk msummary](./Modeling_files/figure-html/msummary1.png) 

```r
densityplot(mc_reg)
```

![plot of chunk msummary](./Modeling_files/figure-html/msummary2.png) 

