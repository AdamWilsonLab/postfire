NDVI Exploration
========================================================

Let's explore the LANDSAT data.





# Data

## Load LANDSAT data

```r
# LANDSAT 4
L4 = stack(paste0(datadir, "ee_ZA_output/20140630_54ef46e7ea_LT4_L1T_ANNUAL_GREENEST_TOA__1982-1993-0000000000-0000000000.tif"))
NAvalue(L4) = 0
gain(L4) = 0.01
names(L4) = paste0("Y", 1982:1993)
L4 = setZ(L4, 1982:1993)

# LANDSAT 5
L5 = stack(paste0(datadir, "ee_ZA_output/20140630_54ef46e7ea_LT5_L1T_ANNUAL_GREENEST_TOA__1984-2012-0000000000-0000000000.tif"))
NAvalue(L5) = 0
gain(L5) = 0.01
names(L5) = paste0("Y", 1984:2012)
L5 = setZ(L5, 1984:2012)

# LANDSAT 7
L7 = stack(paste0(datadir, "ee_ZA_output/20140630_54ef46e7ea_LE7_L1T_ANNUAL_GREENEST_TOA__1999-2014-0000000000-0000000000.tif"))
NAvalue(L7) = 0
gain(L7) = 0.01
names(L7) = paste0("Y", 1999:2014)
L7 = setZ(L7, 1999:2014)

# LANDSAT 8
L8 = stack(paste0(datadir, "ee_ZA_output/20140630_54ef46e7ea_LC8_L1T_ANNUAL_GREENEST_TOA__2013-2014-0000000000-0000000000.tif"))
gain(L8) = 0.01
NAvalue(L8) = 0
names(L8) = paste0("Y", 2013:2014)
L8 = setZ(L8, 2013:2014)
```


Let's check out one of the LANDSAT objects.  Raster provides a summary by just typing the object's name:

```r
L7
```

```
## class       : RasterStack 
## dimensions  : 1929, 1589, 3065181, 16  (nrow, ncol, ncell, nlayers)
## resolution  : 30, 30  (x, y)
## extent      : 222540, 270210, 6189390, 6247260  (xmin, xmax, ymin, ymax)
## coord. ref. : +proj=utm +zone=34 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 
## names       :  Y1999,  Y2000,  Y2001,  Y2002,  Y2003,  Y2004,  Y2005,  Y2006,  Y2007,  Y2008,  Y2009,  Y2010,  Y2011,  Y2012,  Y2013, ... 
## min values  : -327.7, -327.7, -327.7, -327.7, -327.7, -327.7, -327.7, -327.7, -327.7, -327.7, -327.7, -327.7, -327.7, -327.7, -327.7, ... 
## max values  :  327.7,  327.7,  327.7,  327.7,  327.7,  327.7,  327.7,  327.7,  327.7,  327.7,  327.7,  327.7,  327.7,  327.7,  327.7, ... 
## time        : 1999 - 2014 (range)
```



And a plot of the first year:

```r
levelplot(L7[[1]], col.regions = cndvi()$col, cuts = length(cndvi()$at), at = cndvi()$at, 
    margin = F)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 



That's strange, EarthEngine has added some columns to the tif to the West of the peninsula (see all that white space?).  Let's crop it.  First define an 'extent' in lat-lon coordinates and project them to 

```r
peninsula = extent(250000, 270210, 6189390, 6247260)
L7 = crop(L7, peninsula)
```


Let's try the plot again:

```r
levelplot(L7[[1]], col.regions = cndvi()$col, cuts = length(cndvi()$at), at = cndvi()$at, 
    margin = F)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

Much better.  Now let's look at the last few years.


```r
levelplot(L7[[12:16]], col.regions = cndvi()$col, cuts = length(cndvi()$at), 
    at = cndvi()$at, layout = c(5, 1))
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 


## Change through time

It's still hard to see change, so let's pick a pixel and plot NDVI as a function of time.  But first we need to pick a pixel (or a few) to plot.  There are a few ways to do this.  First let's try extracting values from a single cell:

```r
plot(c(L7[199165]) ~ getZ(L7), type = "l", ylab = "NDVI", xlab = "Year")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 


Or, we can use the `click` function (in Raster) that allows you to pick points on the map.  First we need to plot the data using the plot() command.


```r
## first plot the data
plot(L7[[1]])
## specify how many points you want to select
nclicks = 5
d = click(L7, xy = T, n = nclicks, cell = T, id = TRUE, type = "l")
## now click on the map to select your points

## reshape the data for easy plotting
d$id = 1:nrow(d)
d2 = melt(d, id.var = c("x", "y", "cell", "id"))
colnames(d2) = c("X", "Y", "Point", "id", "Year", "NDVI")
d2$Year = as.numeric(sub("Y", "", d2$Year))
p1 = xyplot(NDVI ~ Year, groups = id, data = d2, type = "l", auto.key = list(x = 0, 
    y = 1))
p2 = levelplot(L7[[1]], col.regions = cndvi()$col, cuts = length(cndvi()$at), 
    at = cndvi()$at, margin = F) + layer(panel.text(d$x, d$y, d$id, col = "red", 
    cex = 2))
print(c(p1, p2, merge.legends = T))
```

### Exercise: 
1. Explore the map for areas you are familiar with. Do you see any patterns?
2. Can you identify any fires looking at the NDVI profiles?




