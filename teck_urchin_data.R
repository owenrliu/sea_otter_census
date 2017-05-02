### Urchin fishing data import
## from Sarah Teck and CDFW
library(tidyverse)

for(i in seq(1978,2010,by=2)) {
  file <- paste0(i,"_landings.csv") #file name
  datname <- paste0("land",i) # name for R variable
  inp <- read.csv(file=paste0("data/teck urchin data/",file),stringsAsFactors = F) # import the .csv
  
  # fix a few variable names
  names(inp)[1] <- "serial"
  names(inp)[11:12] <- c("land.month","land.day")
  
  assign(datname,inp) # store data in a dataframe in R
  rm(inp)
}

# 2011 data is a slightly different format (different variable names)
land2011 <- read.csv(file=paste0("data/teck urchin data/2011_landings.csv"),stringsAsFactors = F)
land2011 <- land2011[,-c(7,10,12:14,16)] # remove unmatched columns
names(land2011) <- names(land1978) # change variable names to match other data

# Join all the data into a master
landings.all <- bind_rows(land1978,land1980,land1982,land1984,land1986,land1988,land1990,land1992,land1994,land1996,land1998,
                          land2000,land2002,land2004,land2006,land2008,land2010,land2011)
head(landings.all)
rm(land1978,land1980,land1982,land1984,land1986,land1988,land1990,land1992,land1994,land1996,land1998,
   land2000,land2002,land2004,land2006,land2008,land2010,land2011)

# Landings, converted to metric tons, by origin block, year, and month
land.by.month <- landings.all %>% group_by(origin.code,landing.year,land.month) %>%
  summarise(catch.mt = sum(reported.catch.lbs)/2204.62)

# Number of months' data for each fishing block
blocks.months.count <- land.by.month %>% group_by(origin.code) %>% 
  summarise(num.months=n()) %>% 
  arrange(desc(num.months))

# Landings, converted to metric tons, by origin block and year
land.by.year <- landings.all %>% group_by(origin.code,landing.year) %>%
  summarise(catch.mt = sum(reported.catch.lbs)/2204.62)

# Number of years' data for each fishing block
blocks.years.count <- land.by.year %>% group_by(origin.code) %>% 
  summarise(num.yrs=n()) %>% 
  arrange(desc(num.yrs))

# combined count
blocks.count <- left_join(blocks.months.count,blocks.years.count)

### for JUST southern CA fishing blocks that have been subject to otter expansion
south.CA.blocks <- c(631,632,637,638,644,643,653:658)
bl.count.sCA <- blocks.count %>%
  filter(origin.code %in% south.CA.blocks)

# sample data for block 657 (just south of Point Conception)
block.657 <- land.by.month %>%
  mutate(month=month.abb[land.month],
         day="01") %>%
  unite(date,landing.year,month,day,sep="-") %>%
  mutate(date=as.Date(date,"%Y-%b-%d")) %>%
  filter(origin.code==657)
ggplot(block.657,aes(x=date,y=catch.mt))+
  geom_line()

# all relevant blocks
sCA.blocks.dat <- land.by.month %>%
  mutate(month=month.abb[land.month],
         day="01") %>%
  unite(date,landing.year,month,day,sep="-") %>%
  mutate(date=as.Date(date,"%Y-%b-%d")) %>%
  filter(origin.code %in% south.CA.blocks)
ggplot(sCA.blocks.dat,aes(x=date,y=catch.mt,col=factor(origin.code)))+
  geom_line()


### for JUST northern central CA fishing blocks that have been subject to otter expansion
north.CA.blocks <- c(501,502,503,478,479,472,473)
bl.count.nCA <- blocks.count %>%
  filter(origin.code %in% north.CA.blocks)

# all relevant blocks
nCA.blocks.dat <- land.by.month %>%
  mutate(month=month.abb[land.month],
         day="01") %>%
  unite(date,landing.year,month,day,sep="-") %>%
  mutate(date=as.Date(date,"%Y-%b-%d")) %>%
  filter(origin.code %in% north.CA.blocks)
ggplot(nCA.blocks.dat,aes(x=date,y=catch.mt,col=factor(origin.code)))+
  geom_line()