#source the setup file and load index raster so you can link plot id's in data frame to location
#source("/projects/academic/adamw/regional/cfr/postfire_plots/postfire/workflow/setup_horae.R")
source("workflow/setup.R")
ig=raster(paste0(datadir,"clean/indexgrid_modis_250m.grd"))  


########## plotting results
   #set wd - three options 
   # 1. clim - climate only model
   # 2. clim_traits - climate + traits model
   # 3. traits - traits only model

#the varialbes used as predictors in each model:
# clim <-  c("wdef","pcv","tmin07","radtot_21jun","slope"))
# clim_trait <-  c("wdef","pcv","tmin07","radtot_21jun","slope","LMA","resprout_postfire","max_height_cm"))
# trait <-  c("LMA","resprout_postfire","max_height_cm"))


    #setwd("~/all_plots/clim_traits")
    foutput= paste0(glenndir, "clim_traits/clim_traits_allplots_out.Rdata")
    finput= paste0(glenndir, "clim_traits/clim_traits_allplots_in.Rdata")
    
     
    load(foutput)
    #relevant objects are:
    # mc - output from mcmc. for all these analyses the only parameters monitored are plot-level lambda and gamma
    
    load(finput)
    #relevant objects are:
    #sdat - all spatial plot data
    #tdat - all temporal plot data
    #data - data sent to jags: data$env is the full matrix of environmental preidcotrs used in the model

    ## Potentially thin the data
    mc2=window(mc,thin=5,start=1)

    ### Extract regression coefficients
    mc_reg=mc2[,grep("gamma|lambda",colnames(mc[[1]]))]

    ## run if you want to look at fitted chains
    #xyplot(mc_reg)
    #densityplot(mc_reg)


    #'
    #' Summarize parameter values:
    confint <- as.data.frame(summary(mc_reg)[[2]])
    names(confint) <- c("min","lower","mid","upper","max")
    confint$names <- rownames(confint)
    confint.g <- confint[1:398,] #plot-level-gamma
    confint.l <- confint[399:796,] #plot-level-lambda
      
      envdat <- as.data.frame(data$env) #all environemtal data
      envdat$id <- as.numeric(rownames(envdat))
      allenv <- left_join(envdat,sdat,by="id") #create moster spatial data to look up plot details (eg lat, lon, original name)
    
    #select a variable
      plotx <- envdat$LMA
    
    #lambda
   #####
    #create lambda data frame
      lscatt_dat <- data.frame(confint.l,xvar=plotx,site=allenv$Site.x)
    #plot lambda
    Pl_scatt <- ggplot(data=lscatt_dat,aes(x=xvar,y=mid,color=site)) +
      geom_point() +
      geom_errorbar(aes(ymin=lower,ymax=upper)) +
      theme_bw() +
      labs(title =" plot level parameters", x = "Predictor", y = "Mean lambda")

   Pl_scatt
    
   #gamma
   #####
   #create gamma data frame
   gscatt_dat <- data.frame(confint.g,xvar=plotx,site=allenv$Site.x)
   #plot gamma
    Pg_scatt <- ggplot(data=gscatt_dat,aes(x=xvar,y=mid,color=site)) +
      geom_point() +
      geom_errorbar(aes(ymin=lower,ymax=upper)) +
      theme_bw() +
      labs(title =" plot level parameters", x = "Predictor", y = "Mean gamma")
    
    Pg_scatt
    
    
###Explore relationships among traits and climate vars
    
covs <- allenv[,which(names(allenv) %in% c("Site.y", "wdef.y", "pcv.y", "tmin07.y", "radtot_21jun.y", "slope.y", "LMA.y", "resprout_postfire.y", "max_height_cm.y"))]

covs$max_height_cm.y <- log(covs$max_height_cm.y)

colnames(covs) <- c("slope", "pcv", "tmin07", "rad_jun", "resprout", "ln_height", "LMA", "Site", "wdef")

covs <- covs[,c("wdef", "tmin07", "slope", "rad_jun", "pcv", "resprout", "ln_height", "LMA", "Site")]

covs[,1:8] <- sapply(covs[,1:8], function(x){(x-mean(x))/var(x)})

#GGally::ggpairs(covs[,1:8])
GGally::ggpairs(covs, ggplot2::aes(colour=Site))

summary(lm(LMA ~ wdef + tmin07 + slope + rad_jun + pcv, data = covs))
summary(lm(resprout ~ wdef + tmin07 + slope + rad_jun + pcv, data = covs))
summary(lm(ln_height ~ wdef + tmin07 + slope + rad_jun + pcv, data = covs))


all <- cbind(confint.g$mid, confint.l$mid,covs)
colnames(all)[1:2] <- c("gamma", "lambda")

GGally::ggpairs(all, ggplot2::aes(colour=Site))

summary(lm(lambda ~ wdef + tmin07 + slope + rad_jun + pcv + ln_height + LMA + resprout, data = all))

summary(lm(gamma ~ wdef + tmin07 + slope + rad_jun + pcv + ln_height + LMA + resprout, data = all))

picante::cor.table(all[,1:10])

spall <- cbind(all, allenv$Latitude, allenv$Longitude)

coordinates(spall) <- ~ allenv$Longitude + allenv$Latitude

plot(spall[which(spall$Site=="CP"),], col = rainbow(20), pch=20)

####
###Mapping
####

###Prep DEM
dem <- raster("/Users/jasper/Documents/GIS/SRTM/dem_SRTM90.tif")
dem <- crop(dem, extent(spall)+c(-.5,1,-1,.5))
dem7 <- focal(dem, w=matrix(1, 7, 7), mean)

###Calculate hillshade
slp <- terrain(dem7, "slope")
asp <- terrain(dem7, "aspect")
hs <- hillShade(slp, asp)

###
image(dem7, col=terrain.colors(2500), xlab="", ylab="")
plot(hs, col=grey(seq(0,1,1/60), alpha=.5), add=T, legend=F)
points(spall[1], col="white", bg=rainbow(20), pch=21, cex=.75, lwd=.1)
