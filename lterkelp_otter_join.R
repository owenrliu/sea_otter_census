### This script joins data from kelp Landsat observations (SBC LTER)
### to otter census tracts (WERC Otter Count)

## Required packages ###
library(tidyverse)
library(ncdf4)
library(RANN)

#### Extract NetCDF data ####
extract_ncdf <- function() {
  # New kelp Landsat data is in NetCDF #
  nc <- nc_open("data/LandsatKelpBiomass.nc")
  
  ## We grab each variable from the NetCDF file as we need them
  
  # latitude and longitude of each Landsat pixel
  latlon <- tibble(kelp.y=ncvar_get(nc,varid="lat"),kelp.x=ncvar_get(nc,varid="lon"))
  
  # collapse date into one identifier ("year_quarter")
  quart <- tibble(year=ncvar_get(nc,varid="year"),quarter=ncvar_get(nc,varid="quarter")) %>% 
    unite(date,year,quarter)
  # estimated biomass for each pixel, joined to its lat/lon
  biomass <- as.data.frame(ncvar_get(nc,varid="biomass"))
  names(biomass) <-quart$date
  biomass <- latlon %>% 
    bind_cols(biomass)%>% 
    gather(date,biomass,-kelp.y,-kelp.x)
  # estimated SE of biomass for each pixel, joined to its lat/lon
  biomass_se <- as.data.frame(ncvar_get(nc,varid="biomass_se"))
  names(biomass_se) <-quart$date
  biomass_se <- latlon %>% 
    bind_cols(biomass_se) %>%
    gather(date,biomass_se,-kelp.y,-kelp.x)
  
  # Join long-form biomass and biomass_se data
  biomass$biomass_se <- biomass_se$biomass_se
  
  # Separate date column again
  dat_all <- biomass %>% 
    separate(date,c("year","quarter"),sep="_")
  dat_all$year <- as.numeric(dat_all$year)
  dat_all$quarter <- as.numeric(dat_all$quarter)
  
  return(dat_all)
}

#### Otter centroids ####
get_centroids <- function() {
  # This represents the maximum extent in the data of the otter population
  # and polygons are consistent, so it can be used to match to all years of kelp data
  centroids <- read.csv("data/census_centroids_WGS84.csv",stringsAsFactors = F)
  centroids <- centroids %>% 
    select(y,x,POLY_ID) %>%
    mutate(idx=row_number())
  return(centroids)
}

get_otter_data <- function() {
  dat <- read.csv("data/otter_census_w_PISCO.csv",stringsAsFactors = F)%>% 
    rename(year=Year) %>%
    select(year:trend5yr) %>%
    distinct(POLY_ID,year,.keep_all=T)
}

#### Join Landsat to otter census centroids ####
# function that takes a year of Landsat kelp data, the polygon centroid data, 
# and a maximum distance cutoff (in degrees), and returns the joined data
join_to_centroids <- function(kelpdat,centroids,maxdist=0.04) {
  
  # crop Landsat data to extent of centroids
  maxlat <- max(centroids$y)
  minlat <- min(centroids$y)
  maxlon <- max(centroids$x)
  minlon <- min(centroids$x)
  kelpdatcrop <- kelpdat %>% filter(kelp.y<=maxlat,kelp.y>=minlat,kelp.x<=maxlon,kelp.x>=minlon)
  
  #nearest neighbors
  nn <- nn2(data=centroids[,1:2],query=kelpdatcrop[,1:2],k=1)
  
  # assign an otter census tract and its data to each Landsat pixel
  kelpdatcrop$idx <- as.numeric(nn$nn.idx)
  kelpdatcrop$dist <- as.numeric(nn$nn.dists)
  
  # remove matches that are too far away
  kelpdatcrop <- kelpdatcrop %>% filter(dist<=maxdist)
  
  # join the data
  datjoin <- kelpdatcrop %>% left_join(centroids,by="idx")
  
  return(datjoin)
}

## Extract the data and do the join ##
kelpdat <- extract_ncdf()
centroids <- get_centroids()
otterdata <- get_otter_data()
kelp_ott_join <- join_to_centroids(kelpdat,centroids,maxdist=0.04)
rm(kelpdat,centroids)

#### Summary Stats ####
## for each census tract's data, calculate some statistics ##
na_to_zero <- function(vec) {
  vec[is.na(vec)] <- 0
  return(vec)
}
calc_stats <- function(dat,otterdata) {
  
  datsumm <- dat %>% 
    group_by(POLY_ID,year,quarter) %>%
    
    #presence/absence
    mutate(presence=ifelse(biomass>0,1,0))%>%
    
    # propna is the number of NA pixels (cloud cover);
    # totbio is the total summed biomass in each census tract;
    # totpixels is the total number of nonzero pixels (presence of kelp)
    summarise(propna=sum(is.na(biomass))/n(),
              totbio=sum(biomass,na.rm=T),
              totpixels=sum(presence,na.rm=T))%>%
    
    #remove quarterly observations with >10% NAs
    filter(propna<0.1)%>%
    
    # find the maximum biomass across the timeseries for each tract
    # in order to normalize each tract's time series to its total available habitat area (proportional biomass)
    # tothab is the cumulative sum of total presence (i.e., is habitat area growing over time?)
    group_by(POLY_ID) %>%
    mutate(maxbio=max(totbio),
           propbio=totbio/maxbio,
           zbio=(totbio-mean(totbio))/sd(totbio),
           tothab=cummax(totpixels),
           proptothab=totpixels/max(tothab),
           tothablag1=lag(tothab,1),
           newpixels=tothab-tothablag1,
           #fix years
           quart2=quarter/4,
           date=year+quart2)%>%
    select(-tothablag1,-quart2)
    
  #join otter population data
  out <- left_join(datsumm, otterdata,by=c("POLY_ID","year"))
  
  # filter and fill in zeroes. Otter census started in 1985, and no otter census in 2011
  # ALL OTHER NA's IN OTTER DATA SHOULD BE TRUE ZEROES
  out <- out %>% 
    filter(year!=2011,year>1984) %>%
    mutate_at(vars(dens_sm:trend5yr),funs(w_na=na_to_zero))
  
  return(out)
}
stats<-calc_stats(kelp_ott_join,otterdata)
