# PostFireTrajectories
Adam M. Wilson  
`r format(Sys.time(), "%B %d, %Y")`  




# Data

Load the model data we made in [DataPrep.R](../3_DataAnnual/DataPrep.Rmd)

```r
load("data/modeldata_annual.Rdata")
rv_meta=read.csv("data/vegtypecodes.csv")
sdat$vegn=rv_meta$code[match(sdat$veg,rv_meta$ID)]

## clip tdat to include only 1984:2006 due to missing recent fire data
tdat=tdat[tdat$year%in%1984:2006,]

## now create a single monster table with all the data
dat=cbind.data.frame(tdat,sdat[match(tdat$id,sdat$id),])
## drop negative ages (time before first fire) for now
dat=dat[dat$age>=0,]
## look at the table
kable(head(dat),row.names=F)
```



|    id| year| age|   ndvi|    id|      x|       y| veg| cover|  tmax|  tmin| janrad| julrad| aspect|   dem|   tpi| firecount|vegn                         |
|-----:|----:|---:|------:|-----:|------:|-------:|---:|-----:|-----:|-----:|------:|------:|------:|-----:|-----:|---------:|:----------------------------|
| 83925| 2000|   0| 0.3845| 83925| 260445| 6243525|  18|     1| 28.19| 9.413|   8902|   3029|  2.135| 152.5| 7.934|         1|Peninsula Shale Renosterveld |
| 83925| 2001|   1| 0.5050| 83925| 260445| 6243525|  18|     1| 28.19| 9.413|   8902|   3029|  2.135| 152.5| 7.934|         1|Peninsula Shale Renosterveld |
| 83925| 2002|   2| 0.6160| 83925| 260445| 6243525|  18|     1| 28.19| 9.413|   8902|   3029|  2.135| 152.5| 7.934|         1|Peninsula Shale Renosterveld |
| 83925| 2003|   3| 0.4260| 83925| 260445| 6243525|  18|     1| 28.19| 9.413|   8902|   3029|  2.135| 152.5| 7.934|         1|Peninsula Shale Renosterveld |
| 83925| 2004|   4| 0.6460| 83925| 260445| 6243525|  18|     1| 28.19| 9.413|   8902|   3029|  2.135| 152.5| 7.934|         1|Peninsula Shale Renosterveld |
| 83925| 2005|   5| 0.6435| 83925| 260445| 6243525|  18|     1| 28.19| 9.413|   8902|   3029|  2.135| 152.5| 7.934|         1|Peninsula Shale Renosterveld |


## Change through time

It's still hard to see change while looking at the full peninsula, so let's:

1. pick a few pixels and plot NDVI as a function of time.  
2. zoom in on a smaller region

Let's load the NDVI and age data again.

```r
## load the NDVI data
ndvifile="data/ndvi_annual_landsat_30m.tif"
years=1984:2014
ndvi=stack(ndvifile)
names(ndvi)=paste0("ndvi_",years)
ndvi=setZ(ndvi,years)
ndvi=ndvi[[which(getZ(ndvi)%in%1984:2006)]]
```

And age...

```r
agefile="data/ages_annual_landsat_30m.tif"
age=stack(agefile)
names(age)=paste0("age_",1962:2014)
age=setZ(age,1962:2014)
age=age[[which(getZ(age)%in%getZ(ndvi))]]
```

But first we need to pick a pixel (or a few) to plot.  There are a few ways to do this.  First let's try extracting values from a few cells:

```r
## first plot the data
plot(ndvi[[1]])
## select a few points by row number
d=data.frame(cell=c(1095098,1070102,1006689))
points(sdat[match(d$cell,sdat$id),c("x","y")],pch=16)
text(sdat[match(d$cell,sdat$id),c("x","y")],labels=d$cell,pos=4)
```

![plot of chunk pickpoints](./PostFireTrajectories_files/figure-html/pickpoints.png) 

To select points for plotting, we can use the `click` function (in Raster) that allows you to pick points on the map.  First we need to plot the data using the plot() command. Then run the `click` command using the `ig` grid so the cell id's are returned. 

```r
## first plot the data
plot(ndvi[[2]])
## specify how many points you want to select
nclicks=5
## run the click function then click on the map (to the right).  This may not work in GUI's other than RStudio...
d=click(ig,n=nclicks,cell=T,id=T)
```


Now plot that as a function of time (year).

```r
ggplot(dat[dat$id%in%d$cell,],
       aes(x=year,y=ndvi,group=id))+
  geom_line(aes(color = factor(id)),size=1,linetype=id)
```

![plot of chunk unnamed-chunk-1](./PostFireTrajectories_files/figure-html/unnamed-chunk-1.png) 

And as a function of age

```r
ggplot(dat[dat$id%in%d$cell,],
       aes(x=age,y=ndvi,group=id))+
  geom_line(aes(color = factor(id)),size=1)+
  stat_smooth(fill = "grey50",aes(group = 1),col="red")
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

```
## Warning: Removed 1 rows containing missing values (stat_smooth).
```

![plot of chunk unnamed-chunk-2](./PostFireTrajectories_files/figure-html/unnamed-chunk-2.png) 

  Explore the NDVI data for various pixels by changing `nclicks` above and selecting new points on the map.  Remember that we subsetted the dat dataframe to include only 'natural vegetation' so you may select a point with no data.  

### Regional plot

Alternatively, we can aggregate the data from a larger region.  First import a shapefile of the reserves on the peninsula and the fire data.

```r
## load the reserve shapefile
reserves=readOGR(paste0(datadir,"raw/reserves/"),"reserves")
reserves=spTransform(reserves,CRS(proj4string(ig)))

## or pick a fire polygon
fi=readOGR(dsn=paste0(datadir,"raw/Fire"), layer="CapePenFires") #Cape Peninsula fires history layers 1962-2007
```

```
## Warning: Z-dimension discarded
```

```r
## transform to working projection
fi=spTransform(fi,CRS(proj4string(ig)))
```

Now select a region to explore.  You could do a single fire, a single reserve, or any combination of regions using the code below.  

```r
## select a reserve
resname="SILVERMINE"
reg1=reserves[which(reserves$MASTERNAME==resname),]

## or pick a fire
#reg1=fi[which(fi$FIREID==2000103),]
## get cell numbers in that region

## Extract the data for that polygon
rd=extract(ig,reg1)[[1]]


ggplot(sdat[sdat$id%in%rd,], aes(x=x,y=y))+
   geom_tile(aes(fill=dem))
```

![plot of chunk subset](./PostFireTrajectories_files/figure-html/subset.png) 

Let's look at all those pixels through time:

```r
ggplot(dat[dat$id%in%rd,],aes(x=as.numeric(year),y=ndvi,group=id))+
  geom_line(size=.2,alpha=.1)+
  stat_smooth(fill = "grey50",aes(group = 1),col="red")+
  coord_fixed(ratio = 80)
```

```
## geom_smooth: method="auto" and size of largest group is >=1000, so using gam with formula: y ~ s(x, bs = "cs"). Use 'method = x' to change the smoothing method.
```

![plot of chunk regextract](./PostFireTrajectories_files/figure-html/regextract.png) 

And vs. cell age

```r
ggplot(dat[dat$age>=0&dat$id%in%rd,],aes(x=age,y=ndvi,group=id))+
  geom_line(size=.2,alpha=.1)+facet_wrap(~vegn,nrow=1)+
  stat_smooth(fill = "grey50",aes(group = 1),col="red")#+xlim(0, 30)
```

```
## geom_smooth: method="auto" and size of largest group is >=1000, so using gam with formula: y ~ s(x, bs = "cs"). Use 'method = x' to change the smoothing method.
## geom_smooth: method="auto" and size of largest group is >=1000, so using gam with formula: y ~ s(x, bs = "cs"). Use 'method = x' to change the smoothing method.
## geom_smooth: method="auto" and size of largest group is >=1000, so using gam with formula: y ~ s(x, bs = "cs"). Use 'method = x' to change the smoothing method.
## geom_smooth: method="auto" and size of largest group is >=1000, so using gam with formula: y ~ s(x, bs = "cs"). Use 'method = x' to change the smoothing method.
```

![plot of chunk unnamed-chunk-3](./PostFireTrajectories_files/figure-html/unnamed-chunk-3.png) 

# Non-linear model fitting

The full model I've been using (minus the seasonal component) says that the expected NDVI at some location $i$ in time $t$ comes from a normal distribution as follows:

$\text{NDVI}_{i,t}\sim\mathcal{N}(\mu_{i,t},\sigma)$ 

where the mean ($\mu$) is a nonlinear function including the post-fire NDVI value ($\alpha$), the potential increase in NDVI ($\gamma$), and the post-fire recovery rate ($\lambda$) as follows:

$\mu_{i,t}=\alpha_i+\gamma_i\Big(1-e^{-\frac{age_{i,t}}{\lambda_i}}\Big)$

## Create a subset
To make things run faster while we're experimenting, let's subset the data,  For example, drop ages before the first fire (which are censored and uncertain), keep only fires in SILVERMINE, and select only "Peninsula Sandstone Fynbos"


Let's look at that timeseries for all points in the subsetted region.  

```r
dats=dat[dat$vegn=="Peninsula Sandstone Fynbos"&dat$id%in%rd,] 
dats=dats[!is.na(dats$id),]

ggplot(dats,aes(x=age,y=ndvi,group=id))+
  geom_line(size=.1,alpha=.1)
```

![plot of chunk p1](./PostFireTrajectories_files/figure-html/p1.png) 

Woah, that's messy.  Could there be any information there?  Let's see what happens when we fit all pixels at once.


```r
## Assign starting values for all parameters.  The search has to start somewhere...
start=list(alpha=0.2,gamma=0.4,lambda=4)
## define lower and upper bounds for the parameters to limit the search
lower=c(0,0,0)
upper=c(1,1,10)
## other nls control settings
ctl=nls.control(maxiter = 150,minFactor=1e-10)
## Assign ages at which to evaluate the model
x=seq(0,30,len=100)


sform=as.formula(ndvi~alpha+gamma*(1-exp(-age/lambda)))
m <- nlsLM(sform, data =dats, start = start, trace = T,control=ctl,lower=lower,upper=upper)
summary(m)
```

Plot it:


```r
## make a new dataframe of predictions from the model
dpred=cbind.data.frame(ndvi=predict(m,newdata=data.frame(age=x)),age=x,id=1)

ggplot(dats,aes(x=age,y=ndvi,group=id))+
  geom_line(size=.2,alpha=.2)+
  geom_line(data=dpred,aes(y=ndvi,x=age),colour="red",size=2)
```

![plot of chunk fitmodel2](./PostFireTrajectories_files/figure-html/fitmodel2.png) 

Now look at just a few pixels.

```r
ids=unique(dats$id)[1:15]
y=dats$ndvi[dats$id%in%ids]
x=dats$age[dats$id%in%ids]

ggplot(data.frame(x,y))+
  geom_point(aes(x=x,y=y))
```

![plot of chunk unnamed-chunk-4](./PostFireTrajectories_files/figure-html/unnamed-chunk-4.png) 
Note the misalignment of fire data likely caused the low NDVI values at the high ages. Otherwise there is a relatively clear signal of increasing NDVI with age. 

# Fit curves binned by category
First write a function to use with `calc`.

```r
tparms=list(
  sform=as.formula(ndvi~alpha+gamma*(1-exp(-age/lambda))),
  start=list(alpha=0.1,gamma=0.4,lambda=6),
  lower=c(0,0,0),
  upper=c(1,1,20),
  ctl=nls.control(maxiter = 150,minFactor=1e-10)
)


## Define a function to process curves by chunk
crvfit=function(x,parms=tparms){
  ttry=try(coef(nlsLM(parms$sform,data=x, start = parms$start, trace = F,
           control=parms$ctl,lower=parms$lower,upper=parms$upper)),silent=T)
if(class(ttry)=="try-error") return(c(alpha=NA,gamma=NA,lambda=NA))
return(ttry)
}

## now bin by veg type and fit curve across all pixels in each type
tdg=by(dat,dat$veg,FUN=crvfit,simplify=T)
```

```
## Warning: lmdif: info = 0. Improper input parameters.
```

```r
tdg=do.call(rbind,tdg)
rownames(tdg)=rv_meta$code[as.numeric(rownames(tdg))]
tdg
```

```
##                                         alpha    gamma  lambda
## Beach                                  0.3851 0.078185  0.6605
## Cape Flats Dune Strandveld - False Bay 0.4161 0.008381 20.0000
## Cape Flats Sand Fynbos                 0.1000 0.400000  6.0000
## Cape Lowland Freshwater Wetlands       0.3178 0.055126  1.0062
## Hangklip Sand Fynbos                       NA       NA      NA
## Peninsula Granite Fynbos - North       0.3678 0.086941  0.4017
## Peninsula Granite Fynbos - South       0.4595 0.125753 20.0000
## Peninsula Sandstone Fynbos             0.3517 0.077353 20.0000
## Peninsula Shale Fynbos                 0.4602 0.066549 20.0000
## Peninsula Shale Renosterveld           0.3560 0.192459  1.3739
## Southern Afrotemperate Forest              NA       NA      NA
```



