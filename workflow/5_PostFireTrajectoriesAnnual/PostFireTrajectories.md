---
title: "PostFireTrajectories"
author: "Adam M. Wilson"
date: 'August 10, 2014'
output:
  html_document:
    keep_md: yes
    number_sections: yes
    theme: cerulean
    toc: yes
  pdf_document:
    toc: yes
---


```
## Warning: cannot open file '../1_setup.R': No such file or directory
```

```
## Error: cannot open the connection
```

```
## Error: could not find function "raster"
```


# Data

Load the model data we made in [DataPrep.R](../1_Data/DataPrep.R)

```r
load("data/modeldata.Rdata")
```

```
## Warning: cannot open compressed file 'data/modeldata.Rdata', probable
## reason 'No such file or directory'
```

```
## Error: cannot open the connection
```

```r
rv_meta=read.csv("data/vegtypecodes.csv")
```

```
## Warning: cannot open file 'data/vegtypecodes.csv': No such file or
## directory
```

```
## Error: cannot open the connection
```

```r
sdat$vegn=rv_meta$code[match(sdat$veg,rv_meta$ID)]
```

```
## Error: object 'rv_meta' not found
```

```r
## now create a single monster table with all the data
dat=cbind.data.frame(tdatl,sdat[match(tdatl$id,sdat$id),])
```

```
## Error: object 'tdatl' not found
```

```r
## drop negative ages (time before first fire) for now
dat=dat[dat$age>=0,]
```

```
## Error: object 'dat' not found
```

```r
## look at the table
kable(head(dat),row.names=F)
```

```
## Error: object 'dat' not found
```


## Change through time

It's still hard to see change while looking at the full peninsula, so let's:

1. pick a few pixels and plot NDVI as a function of time.  
2. zoom in on a smaller region

Let's load the NDVI data again.

```r
## load the NDVI data
ndvifile="data/ndvi_landsat_30m.tif"
years=1984:2014
ndvi=stack(ndvifile)
```

```
## Error: invalid 'times' value
```

```r
names(ndvi)=paste0("ndvi_",years)
```

```
## Error: object 'ndvi' not found
```

```r
ndvi=setZ(ndvi,years)
```

```
## Error: could not find function "setZ"
```

But first we need to pick a pixel (or a few) to plot.  There are a few ways to do this.  First let's try extracting values from a few cells:

```r
## first plot the data
plot(ndvi[[1]])
```

```
## Error: object 'ndvi' not found
```

```r
## select a few points by row number
d=data.frame(cell=c(1095098,1070102,1006689))
points(sdat[match(d$cell,sdat$id),c("x","y")],pch=16)
```

```
## Error: object 'sdat' not found
```

```r
text(sdat[match(d$cell,sdat$id),c("x","y")],labels=d$cell,pos=4)
```

```
## Error: object 'sdat' not found
```

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

```
## Error: could not find function "ggplot"
```

And as a function of age

```r
ggplot(dat[dat$id%in%d$cell,],
       aes(x=age,y=ndvi,group=id))+
  geom_line(aes(color = factor(id)),size=1)+
  stat_smooth(fill = "grey50",aes(group = 1),col="red")
```

```
## Error: could not find function "ggplot"
```

  Explore the NDVI data for various pixels by changing `nclicks` above and selecting new points on the map.  Remember that we subsetted the dat dataframe to include only 'natural vegetation' so you may select a point with no data.  

### Regional plot

Alternatively, we can aggregate the data from a larger region.  First import a shapefile of the reserves on the peninsula and the fire data.

```r
## load the reserve shapefile
reserves=readOGR(paste0(datadir,"raw/reserves/"),"reserves")
```

```
## Error: could not find function "readOGR"
```

```r
reserves=spTransform(reserves,CRS(proj4string(ig)))
```

```
## Error: could not find function "spTransform"
```

```r
## or pick a fire polygon
fi=readOGR(dsn=paste0(datadir,"raw/Fire"), layer="CapePenFires") #Cape Peninsula fires history layers 1962-2007
```

```
## Error: could not find function "readOGR"
```

```r
## transform to working projection
fi=spTransform(fi,CRS(proj4string(ig)))
```

```
## Error: could not find function "spTransform"
```

Now select a region to explore.  You could do a single fire, a single reserve, or any combination of regions using the code below.  

```r
## select a reserve
resname="SILVERMINE"
reg1=reserves[which(reserves$MASTERNAME==resname),]
```

```
## Error: object 'reserves' not found
```

```r
## or pick a fire
#reg1=fi[which(fi$FIREID==2000103),]
## get cell numbers in that region

## Extract the data for that polygon
rd=extract(ig,reg1)[[1]]
```

```
## Error: could not find function "extract"
```

```r
ggplot(sdat[sdat$id%in%rd,], aes(x=x,y=y))+
   geom_tile(aes(fill=dem))
```

```
## Error: could not find function "ggplot"
```

Let's look at all those pixels through time:

```r
ggplot(dat[dat$id%in%rd,],aes(x=as.numeric(year),y=ndvi,group=id))+
  geom_line(size=.2,alpha=.1)+
  stat_smooth(fill = "grey50",aes(group = 1),col="red")+
  coord_fixed(ratio = 80)
```

```
## Error: could not find function "ggplot"
```

And vs. cell age

```r
ggplot(dat[dat$age>=0&dat$id%in%rd,],aes(x=age,y=ndvi,group=id))+
  geom_line(size=.2,alpha=.1)+facet_wrap(~vegn,nrow=1)+
  stat_smooth(fill = "grey50",aes(group = 1),col="red")#+xlim(0, 30)
```

```
## Error: could not find function "ggplot"
```

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
```

```
## Error: object 'dat' not found
```

```r
dats=dats[!is.na(dats$id),]
```

```
## Error: object 'dats' not found
```

```r
ggplot(dats,aes(x=age,y=ndvi,group=id))+
  geom_line(size=.1,alpha=.1)
```

```
## Error: could not find function "ggplot"
```

Woah, that's messy.  Could there be any information there?  Let's see what happens when we fit all pixels at once.


```r
sform=as.formula(ndvi~alpha+gamma*(1-exp(-age/lambda)))
m <- nlsLM(sform, data =dats, start = start, trace = T,control=ctl,lower=lower,upper=upper)
```

```
## Error: could not find function "nlsLM"
```

```r
summary(m)
```

```
## Error: object 'm' not found
```

Plot it:


```r
## make a new dataframe of predictions from the model
dpred=cbind.data.frame(ndvi=predict(m,newdata=data.frame(age=x)),age=x,id=1)
```

```
## Error: object 'm' not found
```

```r
ggplot(dats,aes(x=age,y=ndvi,group=id))+
  geom_line(size=.2,alpha=.2)+
  geom_line(data=dpred,aes(y=ndvi,x=age),colour="red",size=2)
```

```
## Error: could not find function "ggplot"
```

Do you believe it?  Useful?  How to improve upon this approach?  What other factors are important?

# Process pixel by pixel



