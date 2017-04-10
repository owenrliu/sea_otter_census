#### PISCO data plus otter population ####

## REQUIRES OTTER POPULATION DATA MATCHED TO PISCO SITES FROM 'otter_census_join'
## and includes all assumptions therein (e.g., 5km buffer from PISCO sites)

# Otter population data spatially matched to relevant PISCO sites
otterpop <- read.csv("data/PISCO_sites_otter_pop.csv",stringsAsFactors = F)
# rename a couple of variables to avoid confusion
otterpop <- otterpop %>% rename(year=Year,otterdens=mean_dens)

#### PISCO data import and cleaning ####
# read in benthic dataset
ps.dat <- read.csv("data/pisco_subtidal.161.2.csv",stringsAsFactors = F)
# removing unneeded columns and adding density column
ps.dat <- ps.dat %>% select(-notes,-observer,-campus,-method)%>% mutate(density=count/60)

# site key
ps.site.key <- read.csv("data/pisco_subtidal.181.1.csv",stringsAsFactors = F)
ps.site.key <- ps.site.key %>% filter(method=="SBTL_SWATH")# just swath sampling

# species key
ps.spp.key <- read.csv("data/pisco_subtidal.180.1.csv",stringsAsFactors = F)
ps.spp.key <- ps.spp.key %>% 
  filter(sample_type=="SWATH",!(pisco_classcode=="NO_ORG"|pisco_classcode=="ASTSPP"|pisco_classcode=="CRYDEC"|pisco_classcode=="GORGAD"|pisco_classcode=="UNIDSP")) %>%
  select(-sample_type,-sample_subtype) %>% 
  mutate(dat="PISCO")
# just swath sampling and removing "non-organisms"

# For the PISCO data, we are defining a "site" as a site/area combination, within which there is normally 4-6 transects (see
# http://www.piscoweb.org/research/science-by-discipline/ecosystem-monitoring/kelp-forest-monitoring/subtidal-sampling-protoco)

ps.dat2 <- ps.dat %>%
  
  # create the new variables
  mutate(dataset="PISCO",species=classcode,year=year) %>%
  unite(site,site,side) %>%

  # calculate n, mean, se, and var by grouping data by year/site/species and counting swaths. calculating mean depth here also,
  # for later
  group_by(dataset,year,site,species) %>%
  summarise(depth=mean(depth),n=n(),mean.dens=mean(density),var.dens=var(density),se.dens=sqrt(var.dens/n)) %>%
  ungroup() %>%
  arrange(species,site,year) %>%
  filter(!is.na(var.dens)) %>%

  # join the overall species identifer
  left_join(ps.spp.key,by=c("species"="pisco_classcode"))
# ************************

#### Join otter data ####
dat_all <- ps.dat2 %>% left_join(otterpop,by=c("year"="year","site"="sitename"))

## exploratory plots- macrocystis adults
mac <- dat_all %>% filter(species=="MACPYRAD")
ggplot(mac,aes(x=otterdens,y=mean.dens,col=year))+
  geom_point()+
  theme_minimal()
mac_lm <- lm(mean.dens~year+otterdens+lat,data = mac)
summary(mac_lm)

## exploratory plot- red urchin
mesf <- dat_all %>% filter(species=="STRFRAAD")
ggplot(mesf,aes(x=otterdens,y=mean.dens,col=year))+
  geom_point()+
  ylim(0,1)+
  theme_minimal()
mesf_lm <- lm(mean.dens~year+otterdens+lat,data = mesf)
summary(mesf_lm)

## exploratory plot- purple urchin
strp <- dat_all %>% filter(species=="STRPURAD")
ggplot(strp,aes(x=otterdens,y=mean.dens,col=year))+
  geom_point()+
  ylim(0,1)+
  theme_minimal()
strp_lm <- lm(mean.dens~year+otterdens+lat,data = strp)
summary(strp_lm)

## What about temporal stability?? ##
stability <- dat_all %>% 
  group_by(site,species) %>%
  summarise(stab=mean(mean.dens)/sd(mean.dens),meanott=mean(otterdens))

macstab <- stability %>% filter(species=="MACPYRAD")
ggplot(macstab,aes(x=meanott,y=stab))+
  geom_point()+
  ylim(0,100)+
  theme_minimal()
