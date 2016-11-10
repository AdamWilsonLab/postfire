# Modeling PostFireTrajectories
Adam M. Wilson  
`r format(Sys.time(), "%B %d, %Y")`  




# Data

Load the model data we made in [DataPrep.R](../1_Data/DataPrep.R) and filter out unwated/duplicate

```r
load("data/modeldata_dim_fix.Rdata")

plotsites <- c("CP","LB1","LB2","CB","BK")
sdat=sdat[sdat$Site.y%in%plotsites,]
sdat <- sdat[complete.cases(sdat),]

#drop obs of unknown age or ndvi or with no data in sdat
tdat=tdat[!is.na(tdat$DA)&
          !is.na(tdat$ND)&
          tdat$QA==0&
          tdat$maskid%in%sdat$id
          ,] 

#we have a problem of duplicate pixels..ie plots that are so close they fall in the same pixel and hence have the same data

#find these plots:
idplot=data.frame(plot=sdat$plot,maskid=sdat$id)
idcount=as.data.frame(table(sdat$id))
idup=filter(idcount,Freq>1)
pdup=filter(idplot,maskid%in%idup$Var1) #df with plots and ids of duplicates
dplot=pdup$plot
did=unique(pdup$maskid)

#create new df with average plot conditions
nplot <- sdat[0,]
for (i in 1:length(did)){
 stemp <- filter(sdat,id==did[i])
 nplot[i,] <- stemp[1,]
 nplot[i,]$plot=stemp$plot[1]*1000
 nplot[i,][1,33:72] = colMeans(stemp[,33:72],na.rm=T)
}

#remove duplicates from sdata and replace with average plots
sdat <- filter(sdat,!(id %in% did))
sdat <- rbind(sdat,nplot)
#remove nas
sdat <- sdat[complete.cases(sdat),]
rownames(sdat)=sdat$id

#drop duplicates of id date
tdat <- tdat[!duplicated(data.frame(tdat$maskid,tdat$date)),]
```

We know have two data frames we'll use in the modeling. The first is the spatial data:

```r
kable(head(sdat),row.names=F)
```

      id        x         y     aspect         dem       slope   soilfert   soiltext     soilph   transformed   fynbos   soilhighfert   soilfinetext    soilacid        map       mmp1        mmp7        pcv       apan     tmax01     tmin07   radtot_21dec   radtot_21jun  Site.x   PLOT.x   plot_smpl          lat        lon  region   plot_num    plot   X.x   seasonally_apparent   deciduous   resprout_postfire   serotinous   max_height_cm        herb    geophyte   graminoid   low_shrub   mid_shrub   tall_shrub   succulent        tree   BranchingOrder   Height_cm   LeafLength_cm   AvgLeafWidth_cm   LeafThickness_mm        SLA         LMA        FWC   LeafSucculence    Leaf_DMC        LWR   CanopyArea   percent_N   percent_C   C_to_N_ratio   std_cor_d_13C_12C   IRIDACEAE   THYMELAEACEAE    ROSACEAE    RUTACEAE     POACEAE   CYPERACEAE   PROTEACEAE    FABACEAE   ERICACEAE   RESTIONACEAE   ASTERACEAE   high_flam    low_flam    med_flam   culm_stem_leaf   leaf_leaf   mixed_leaf   none_leaf   longevity0   longevity1   longevity2   longevity4  Site.y    PLOT.y    Latitude   Longitude   X.y      avgcov
--------  -------  --------  ---------  ----------  ----------  ---------  ---------  ---------  ------------  -------  -------------  -------------  ----------  ---------  ---------  ----------  ---------  ---------  ---------  ---------  -------------  -------------  -------  -------  ----------  ----------  ---------  -------  ---------  -----  ----  --------------------  ----------  ------------------  -----------  --------------  ----------  ----------  ----------  ----------  ----------  -----------  ----------  ----------  ---------------  ----------  --------------  ----------------  -----------------  ---------  ----------  ---------  ---------------  ----------  ---------  -----------  ----------  ----------  -------------  ------------------  ----------  --------------  ----------  ----------  ----------  -----------  -----------  ----------  ----------  -------------  -----------  ----------  ----------  ----------  ---------------  ----------  -----------  ----------  -----------  -----------  -----------  -----------  -------  -------  ----------  ----------  ----  ----------
 2253410   259125   6211875   149.8309    33.46078    5.694791   1.000000   4.000000   1.000000     0.2665240        1      0.0000000      0.0000000   1.0000000   682.3060   14.12535    99.72899   441.7835   1898.903   23.86345   8.382595       9401.356       3325.712  CP       2        CP_2         -34.20683   18.38435  CP       2              2   225             0.0017948   0.0017948           0.4257214    0.0135993        93.14027   0.0452161   0.0096645   0.4014221   0.3341157   0.1069999    0.1024437   0.1029270   0.0000000         2.020287    50.02745       13.232826         0.3398653          0.8471172   52.19043   0.0319156   2.001749        0.0440623   0.4025955   73.13953    1792.1339   0.8361731    46.00725       70.90005           -26.81245   0.0059368       0.0082839   0.0000000   0.0041419   0.0272677    0.0621290    0.0135993   0.1266740   0.0176032      0.3120254    0.2032997   0.7744719   0.1259837   0.0994063        0.2008836   0.6119011    0.1870772           0    0.0020710    0.1874223    0.7541074    0.0562612  CP             2   -34.20683    18.38435     2   0.1126405
 2272306   259125   6209875   216.5702    96.90564   12.339615   1.268010   4.000000   1.536020     0.0000137        1      0.2680099      0.0000000   0.7319901   707.2510   15.09460   101.06982   438.2695   1901.996   23.18549   8.302604       9338.130       2268.344  CP       8        CP_8         -34.22349   18.38426  CP       8              5   267             0.0013968   0.0013968           0.2853757    0.0052381        56.42103   0.0551765   0.0000000   0.3243461   0.4863808   0.0171113    0.0422544   0.0009778   0.0076826         2.479396    35.53327        9.447905         0.3172471          0.5323542   47.18855   0.0264830   1.521083        0.0360275   0.4603783   46.35832     966.4535   0.6951657    48.62377       79.34908           -27.52742   0.0084523       0.0230478   0.0000000   0.0118731   0.0104763    0.0469338    0.0516831   0.0034223   0.2400468      0.2669360    0.0909356   0.7457040   0.0856262   0.1016215        0.2208403   0.6026691    0.1094424           0    0.0099176    0.1676207    0.6942319    0.0611816  CP             8   -34.22349    18.38426     5   0.1290389
 2272309   259875   6209875   176.4447   129.37647    9.269324   1.000000   4.000000   1.000000     0.0000000        1      0.0000000      0.0000000   1.0000000   726.8206   15.55605   103.19497   437.5802   1912.385   23.33031   8.352744       9413.488       2504.192  CP       9        CP_9         -34.22359   18.39424  CP       9              6   271             0.0000000   0.0000000           0.2666613    0.1936421        94.56188   0.0041956   0.0000000   0.4220591   0.3784896   0.0016137    0.1936421   0.0033887   0.0000000         2.443406    50.25981       18.818882         0.4536117          0.8334896   40.40500   0.0371600   1.276537        0.0395464   0.4777708   72.29968    2324.8527   0.5492869    49.03962      100.90267           -28.00966   0.0004034       0.0000000   0.0000000   0.0169437   0.0137163    0.0186381    0.2396321   0.0032274   0.2794094      0.3897047    0.0292884   0.9728094   0.0065354   0.0206552        0.2735194   0.5916573    0.1348233           0    0.0000000    0.0214620    0.9584476    0.0200904  CP             9   -34.22359    18.39424     6   0.1175626
 2272313   260875   6209875   127.9813   102.18384    7.243772   1.000000   3.963345   1.000000     0.0002413        1      0.0000000      0.0366554   1.0000000   737.0247   16.08575   103.38804   436.4362   1902.198   23.41469   8.287785       9423.643       3265.308  CP       10       CP_10        -34.22369   18.40384  CP       10             7   215             0.0000000   0.0000000           0.3035622    0.0242643        62.65875   0.0087765   0.0263294   0.2297367   0.6453278   0.0655653    0.0211668   0.0005163   0.0030976         2.341865    29.86966       10.241095         0.2060266          0.5352911   42.66456   0.0279233   1.044648        0.0282340   0.4661105   37.12874    1119.1886   0.6290645    45.73953       77.43049           -25.57580   0.0005163       0.0583376   0.0206505   0.0108415   0.0077439    0.0557563    0.1316469   0.0887971   0.2617450      0.1662364    0.1130614   0.8203407   0.0356221   0.1440372        0.1342282   0.7774910    0.0882808           0    0.0000000    0.1078988    0.7232834    0.1688178  CP            10   -34.22369    18.40384     7   0.1256532
 2279392   259125   6209125   212.3685    13.65982    4.195307   1.869836   4.000000   2.739672     0.0091765        1      0.8698360      0.0000000   0.1301640   706.2279   15.15624    99.90845   437.1956   1931.071   23.14123   8.382407       9410.034       2928.404  CP       11       CP_11        -34.23152   18.38435  CP       11             8   216             0.0019900   0.0000000           0.2031841    0.0000000        59.34527   0.0069652   0.0023881   0.4348259   0.5180100   0.0218905    0.0159204   0.0224876   0.0000000         2.497292    34.00227       12.981659         0.3604468          0.6922387   46.51909   0.0276679   1.571899        0.0392250   0.3981216   42.63155    1551.8792   0.8425095    42.04531       71.41392           -26.40212   0.0011940       0.0000000   0.0000000   0.0437811   0.0000000    0.0348259    0.1233831   0.0378109   0.0312438      0.4000000    0.2620896   0.9068657   0.0350249   0.0581095        0.3910448   0.5631841    0.0457711           0    0.0000000    0.0875622    0.8447761    0.0676617  CP            11   -34.23152    18.38435     8   0.1515420
 2279395   259875   6209125   240.6257    37.29056    8.024838   1.617154   4.000000   2.234308     0.0066084        1      0.6171539      0.0000000   0.3828461   731.7316   16.01322   102.62701   435.6623   1926.494   23.28620   8.432534       9403.029       2625.095  CP       12       CP_12        -34.23183   18.39344  CP       12             9   217             0.0000000   0.0000000           0.2364816    0.0099135        95.90573   0.0447008   0.0045061   0.3375991   0.4372747   0.0823720    0.0935472   0.0255948   0.0000000         2.552637    53.96454       15.114584         0.2858171          0.8423472   46.51247   0.0342391   1.513559        0.0418590   0.4501750   48.64116    1949.4060   0.7270849    47.45858       75.61713           -27.47276   0.0018025       0.0099135   0.0081110   0.0200072   0.0000000    0.0434391    0.0387527   0.0639870   0.1582552      0.2941601    0.2173756   0.8404831   0.0906633   0.0688536        0.2577505   0.6330209    0.1092286           0    0.0063086    0.1998919    0.7777578    0.0160418  CP            12   -34.23183    18.39344     9   0.1004275

And the second is the temporal data:

```r
kable(head(tdat),row.names=F)
```



 plot  date           DA       EV       ND   QA  site   plot_num   plot_long     maskid
-----  -----------  ----  -------  -------  ---  -----  ---------  ----------  --------
    2  2000-02-18     33   0.1547   0.2247    0  CP     2          CP_2         2253410
    2  2000-03-05     49   0.1230   0.2305    0  CP     2          CP_2         2253410
    2  2000-03-21     65   0.0000   0.0000    0  CP     2          CP_2         2253410
    2  2000-04-06     81   0.1310   0.2056    0  CP     2          CP_2         2253410
    2  2000-04-22     97   0.1233   0.1885    0  CP     2          CP_2         2253410
    2  2000-05-08    113   0.0000   0.0000    0  CP     2          CP_2         2253410

## Subsample Data

```r
#### Set model name for naming objects below and create directory to hold output
mname=substr(
  system(" git log --pretty=format:%H | head -n 1",intern=T),
  1,8)

mname="v1"

if(!file.exists(paste("output/",mname,sep=""))) dir.create(paste("output/",mname,sep=""),recursive=T)

### subset dataset
holdout=0.05#percent to hold out for validation
s=sort(sample(unique(sdat$id),round(length(unique(sdat$id))*(1-holdout)))); length(s)
```

```
## [1] 446
```

```r
write.csv(s,paste("output/",mname,"/",mname,"_subset.csv",sep=""),row.names=F)
sdat$subset=factor(ifelse(sdat$id%in%unique(tdat$maskid),ifelse(sdat$id%in%s,"Model Fitting","Validation"),"Prediction"),levels=c("Model Fitting","Validation","Prediction"),ordered=T)
```

## scale data


```r
## Select and scale environmental data
envars=c("map","graminoid")

scaled=scale(as.matrix(sdat[,envars]))
env_full=cbind(intercept=1,scaled)

### Save the scaling parameters to convert fitted coefficients back to metric units later
beta.mu=c(intercept=0,attr(scaled,"scaled:center"))
beta.sd=c(intercept=1,attr(scaled,"scaled:scale"))
rm(scaled)  #drop the scaled data
```


## Create model data

```r
tdat_full=tdat
tdat=tdat[tdat$maskid%in%s,]; gc() 
```

```
##           used (Mb) gc trigger  (Mb) max used  (Mb)
## Ncells 1365491   73    2637877 140.9  2164898 115.7
## Vcells 4194034   32    9804484  74.9  9781288  74.7
```

```r
## create two env frames for fitting and prediction
env=env_full[rownames(env_full)%in%s,]
  
### Drop missing values
omit=unique(tdat$maskid)[as.numeric(which(is.na(apply(env,1,sum))))]; omit
```

```
## numeric(0)
```

```r
if(length(omit)>0){
  env=env[!rownames(env)%in%omit,]
  tdat=tdat[!tdat$maskid%in%omit,]
}

## create new id that goes from 1 to nGrid
tdat$id2=as.integer(as.factor(tdat$maskid)); gc()
```

```
##           used (Mb) gc trigger  (Mb) max used  (Mb)
## Ncells 1365613 73.0    2637877 140.9  2164898 115.7
## Vcells 4259172 32.5    9804484  74.9  9781288  74.7
```

```r
## Get counts
nGrid=length(unique(tdat$maskid))        ;nGrid
```

```
## [1] 401
```

```r
nTime=length(unique(tdat$year))          ;nTime
```

```
## [1] 0
```

```r
nBeta=ncol(env)                          ;nBeta
```

```
## [1] 3
```

```r
## Write data object
data=list(
  age=tdat$DA,
  ndvi=tdat$ND,
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
  t1=system.time(m <<- jags.model(file="workflow/8_TraitsPlots/Model.R",
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
mc2=window(mc,thin=20,start=1)

### Extract regression coefficients
mc_reg=mc2[,grep("gamma[.]|lambda[.]",colnames(mc[[1]]))]

xyplot(mc_reg)
densityplot(mc_reg)
```


```r
### Calculate convergence metrics
## Autocorrelation
ac=melt(autocorr.diag(mc_reg,lags=seq(0,200,5))); 
ac$Var1=as.numeric(do.call(rbind,strsplit(as.character(ac$Var1)," "))[,2])
colnames(ac)=c("Lag","Parameter","value")
```

Summarize parameter values:

```r
kable(summary(mc_reg)[[2]])
names(beta.mu)
```
