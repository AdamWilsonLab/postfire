# #script to fix omission/commission in modis fire data
# library(dplyr)
# library(tidyr)
# library(lubridate)
# 
# #load temporal and spatial ndvi and site data
# # load("~/Documents/Projects/postfire/postfire/data/modeldata_dim.Rdata")

#load data on missing fires
fdat1=read.csv(paste0(datadir,"raw/Traits_plots/fix_fires_final.csv"),stringsAsFactors = F)

#create empty column for new dates
tdat2 <- tdat
blank <- as.numeric(matrix(data = NA, nrow = nrow(tdat2), ncol = 1))
tdat2$DA2 <- blank

#create plot name and obs id column
tdat2$obsid <- seq(1:nrow(tdat2))
tdat2$plot_long=as.character(tdat2$plot_long)

####case 1
#aa - remove all fires
f_aa <- filter(fdat1,type=="aa")
plots_name <- unique(f_aa$full_name)

#loop through plots
for (i in 1:length(plots_name)){
  
  #get focal plot
  pn <- plots_name[i]
  temp_dat <- tdat2[which(tdat2$plot_long==pn),]
  
  #get age on day 1
  first_date = min(temp_dat$date)
  first_age = temp_dat[which(temp_dat$date==first_date),] %>% dplyr::select(DA)
  
  #recalculate age assuming no fires
  temp_dat$DA2 = as.numeric(temp_dat$date-min(temp_dat$date)) + as.numeric(first_age)
  
  #replace data
  tdat2$DA = replace(tdat2$DA,temp_dat$obsid,temp_dat$DA2)
}


####case 2
#a - remove specific fires
#each line in fdat1 is a single plot - fire combo
f_a <- filter(fdat1,type=="a")
plots_name <- unique(f_a$full_name)

#loop through plots
for (i in 1:length(plots_name)){
  
  #get focal plot
  pn <- plots_name[i]
  temp_dat <- tdat2[which(tdat2$plot_long==pn),]
  
  #find years in which fires to be removed occurs
  fyear <- f_a[which(f_a$full_name==pn),5] %>% unique()
  
  #loop through days
  for (j in 1:nrow(temp_dat)){
    
    #on day one age is the same
    if(j==1){
      temp_dat$DA2[j] = temp_dat$DA[j]
      DAflag = 0
    }
    
    #otherwise if not day 1
    else {
      #first we determine if there has been a fire on this day
      #DAflag is negative if a fire has occured, positive if not
      #NA handling
      if(all(is.na(c(temp_dat$DA[j],temp_dat$DA[j-1])))) {
      DAflag = 0
      } else if (is.na(temp_dat$DA[j-1]) & !(is.na(temp_dat$DA[j]))){
      DAflag = -1 
      } else {
      DAflag = temp_dat$DA[j]-temp_dat$DA[j-1]}
      
      #check if year is same as fire to be deleted
      year_j <- year(temp_dat$date[j])
      
      #algorythm is: age of plot increases unless there is a fire in a year in which we are looking for a fire to remove
      if ((!(year_j %in% fyear)) & (DAflag<0)){
        temp_dat$DA2[j]=temp_dat$DA[j]
      }
      else {
      #age just increases
        temp_dat$DA2[j]=temp_dat$DA2[j-1]+as.numeric(temp_dat$date[j]-temp_dat$date[j-1])
      }
      
    }
  }
  #replace data
  tdat2$DA = replace(tdat2$DA,temp_dat$obsid,temp_dat$DA2)
}

####case 3
#m - add a single fire by copying another site
#each line in fdat1 is a plot - replacement plot combo
f_m <- filter(fdat1,type=="m")
plots_name <- unique(f_m$full_name)
rep_name <-  unique(f_m$rep_name)

#loop through sites
for (i in 1:length(f_m$full_name)){
    
    #get focal plot
    pn <- f_m$full_name[i]
    rn <- f_m$rep_name[i]
    temp_dat <- tdat2[which(tdat2$plot_long==pn),]
    rep_dat <- tdat2[which(tdat2$plot_long==rn),]
    temp_dat$DA2 <- temp_dat$DA
    
    #loop through days in replacement plot to find fires
    for (j in 1:nrow(rep_dat)){
      
      rdate = rep_dat$date[j]
      rage  = rep_dat$DA[j]
      
      #on day one age is the same
      if(j==1){
        DAflag = 0
      } else {
        #NA handling
        if(all(is.na(c(rep_dat$DA[j],rep_dat$DA[j-1])))) {
          DAflag = 0
        } else if (is.na(rep_dat$DA[j-1]) & !(is.na(rep_dat$DA[j]))){
          DAflag = -1 
        } else {
          #DAflag is negative if a fire has occured, positive if not
          DAflag = rep_dat$DA[j]-rep_dat$DA[j-1]}
      }
      
      #if there is a fire on this day in the plot from which we are taking replacements, 
      # then add this to the actual plot
      if(DAflag<0)
        
        #loop through days in focal plot
        for (k in 1:nrow(temp_dat)) {
          #if there is a fire but not the year we are looking for a fire to remove
          #then we keep original age
          
            if (rdate > temp_dat$date[k]){
              temp_dat$DA2[k]=temp_dat$DA2[k]
            }  else if (rdate == temp_dat$date[k]) {
              temp_dat$DA2[k]=rage
            } else {
              temp_dat$DA2[k]=temp_dat$DA2[k-1]+as.numeric(temp_dat$date[k]-temp_dat$date[k-1])
            }
        }
    }
    #replace data
    tdat2$DA = replace(tdat2$DA,temp_dat$obsid,temp_dat$DA2)
  }

####case 4
#ma - add a single fire at a known date
f_ma <- filter(fdat1,type=="ma")

for (i in 1:length(f_ma$full_name)){

  #get focal plot
  pn <- f_ma$full_name[i]
  rdate = as.Date(f_ma$actual[i],format="%d/%m/%Y")
  temp_dat <- tdat2[which(tdat2$plot_long==pn),]
  temp_dat$DA2 <- temp_dat$DA
    
  #loop through days
  for (k in 1:nrow(temp_dat)) {
    
    #if we are past the date of the fire to be added we calculate a new age
    #that becomes the plot age unless the old plot age is younger
    #choose the youngest
    if (rdate > temp_dat$date[k]){
      temp_dat$DA2[k]=temp_dat$DA2[k]
    }  else if (rdate <= temp_dat$date[k]) {
      temp=as.numeric(temp_dat$date[k]-rdate)
      if(!(is.na(temp_dat$DA2[k]))){
        if(temp<temp_dat$DA2[k]){
          temp_dat$DA2[k]=temp
        }
      } else if ((is.na(temp_dat$DA2[k])) & !(is.na(temp))){
        temp_dat$DA2[k]=temp
      }
    } else {
      temp_dat$DA2[k]=temp_dat$DA2[k]
    }
  }
  #replace data
  tdat2$DA = replace(tdat2$DA,temp_dat$obsid,temp_dat$DA2)
}
  
####case 5
#cm - remove all fires, then add a single fire at a known date
f_cm <- filter(fdat1,type=="cm")

plots_name <- unique(f_cm$full_name)

#loop through plots
for (i in 1:length(plots_name)){
  
  #get focal plot
  pn <- plots_name[i]
  temp_dat <- tdat2[which(tdat2$plot_long==pn),]
  
  #get age on day 1
  first_date = min(temp_dat$date)
  first_age = temp_dat[which(temp_dat$date==first_date),] %>% dplyr::select(DA)
  
  #recalculate age assuming no fires
  temp_dat$DA2 = as.numeric(temp_dat$date-min(temp_dat$date)) + as.numeric(first_age)
  
  #replace data
  tdat2$DA = replace(tdat2$DA,temp_dat$obsid,temp_dat$DA2)
}

#now add fires
for (i in 1:length(f_cm$full_name)){
  
  #get focal plot
  pn <- f_cm$full_name[i]
  rdate = as.Date(f_cm$actual[i],format="%d/%m/%Y")
  temp_dat <- tdat2[which(tdat2$plot_long==pn),]
  temp_dat$DA2 <- temp_dat$DA2
  
  #loop through days
  for (k in 1:nrow(temp_dat)) {
    
    #if we are past the date of the fire to be added we calculate a new age
    #that becomes the plot age unless the old plot age is younger
    #choose the youngest
    if (rdate > temp_dat$date[k]){
      temp_dat$DA2[k]=temp_dat$DA2[k]
    }  else if (rdate <= temp_dat$date[k]) {
          temp=as.numeric(temp_dat$date[k]-rdate)
          if(!(is.na(temp_dat$DA2[k]))){
              if(temp<temp_dat$DA2[k]){
                temp_dat$DA2[k]=temp
              }
          } else if ((is.na(temp_dat$DA2[k])) & !(is.na(temp))){
            temp_dat$DA2[k]=temp
          }
    } else {
      temp_dat$DA2[k]=temp_dat$DA2[k]
    }
  }
  #replace data
  tdat2$DA = replace(tdat2$DA,temp_dat$obsid,temp_dat$DA2)
}
  
#replace original temporal data
tdat <- tdat2 %>% dplyr::select(-DA2)