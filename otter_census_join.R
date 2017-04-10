## Otter census join
library(tidyverse)

# Import all otter data files
census_dat <- list()
for(i in 1985:2010) {
  temp<-read.csv(paste0("data/census_",i,".csv"),stringsAsFactors = F)
  name <- paste0("dat_",i)
  census_dat[[name]] <- temp
}

for(i in 2012:2016) {
  temp<-read.csv(paste0("data/census_",i,".csv"),stringsAsFactors = F)
  name <- paste0("dat_",i)
  census_dat[[name]] <- temp
}

# all data in one frame
census_dat<- bind_rows(census_dat) %>% select(Year:Sect_ID)

# Match to PISCO sites
matchkey <- read.csv("data/census_2016_match.csv",stringsAsFactors = F)
matchkey <- matchkey %>% select(sitename,PISCO_CODE,POLY_ID)

# All otter data plus any PISCO sites within 5k
census_dat <- census_dat %>% left_join(matchkey,by="POLY_ID")

# PISCO site otter timeseries
site_pop <- census_dat %>% 
  # Only polygons near PISCO sites
  filter(!is.na(sitename)) %>%
  group_by(sitename,Year) %>%
  summarise(mean_dens= mean(dens_sm))

# PISCO site long/lat
site_locs <- read.csv("data/pisco_site_locs.csv",stringsAsFactors = F)
site_pop <- site_pop %>% left_join(site_locs)


## population over time
site_pop <- site_pop %>% arrange(desc(lat))
# average change


pop_growth <- site_pop %>% 
  group_by(sitename,lat) %>%
  do(mod = lm(mean_dens ~ Year, data = .)) %>%
  mutate(roc = coef(mod)[2]) %>%
  select(-mod)
  
ggplot(pop_growth,aes(x=roc,y=lat)) + geom_point()

## dN/Ndt?
pop_growth2 <- site_pop %>%
  #lag year and density by 1...
  mutate(N_1=lag(mean_dens),Y_1=lag(Year)) %>%
  #...to calculate per capita growth rate
  mutate(dnNdt=(mean_dens-N_1)/(mean_dens*(Year-Y_1))) %>%
  group_by(sitename,PISCO_CODE,lat,lon) %>%
  summarise(mean_r=mean(dnNdt,na.rm=T),sd_r=sd(dnNdt,na.rm=T)) %>%
  ungroup()

ggplot(pop_growth2,aes(y=mean_r,x=lat,ymin=mean_r-sd_r,ymax=mean_r+sd_r)) + 
  geom_pointrange() + 
  coord_flip()+
  geom_hline(yintercept=0,linetype=2)+
  ylim(-0.2,0.2)+
  theme_minimal()

## What about for all census dat?
popgrowth3 <- census_dat %>%
  ungroup() %>%
  group_by(POLY_ID,Year) %>%
  #lag year and density by 1...
  mutate(N_1=lag(lin_dens),Y_1=lag(Year)) %>%
  #...to calculate per capita growth rate
  mutate(dnNdt=(lin_dens-N_1)/(lin_dens*(Year-Y_1))) %>%
  summarise(mean_r=mean(dnNdt,na.rm=T),sd_r=sd(dnNdt,na.rm=T)) %>%
  ungroup()

## Export the two datasets
write.csv(census_dat,file="data/otter_census_w_PISCO.csv",row.names = F)
write.csv(site_pop,file="data/PISCO_sites_otter_pop.csv",row.names=F)
