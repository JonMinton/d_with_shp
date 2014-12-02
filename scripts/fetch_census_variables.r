rm(list=ls())

# To do: 

# take 2001 and 2011 census tables identified previously and
# link to datazones
# standardise codes
# allow people to select n and N

# build UI to select 


# install.packages(
#   c(
#   "plyr",
#   "repmis",
#   "dplyr",
#   "ggplot2"
#     )
#   )
# 

require(plyr)
require(dplyr)
require(repmis)
require(ggplot2)
require(tidyr)


###############

# Load tables

# 2001

coo_2001 <- source_DropboxData(
  file="Census__2001__KS005__Country_Of_Origin.csv",
  key="erj713wnp535q20"
  )

eg_2001 <- source_DropboxData(
  file="Census__2001__KS006__Ethnic_Group_And_Language.csv",
  key="kfgj930jel9qep0"
  )

rel_2001 <- source_DropboxData(
  file="Census__2001__KS007__Religion.csv",
  key="zfnhe8kwxnpupwa"
  )


# links 
area_links_pt1 <- source_DropboxData(
  file="OUTPUT_AREA_2001_LOOKUP.csv",
  key="39wszvlpxy4qvpf"
  )

area_links_pt2 <- source_DropboxData(
  file="Census_2011_Lookup__OA_TO_HIGHER_AREAS.csv",
  key="95x5ozuw0c6xgxk"
  )



#############

area_links_pt1 <- tbl_df(area_links_pt1)
area_links_pt2 <- tbl_df(area_links_pt2)
area_links <- join(area_links_pt2, area_links_pt1)
area_links <- tbl_df(area_links)
area_links <- select(
  area_links,
  OutputArea2011Code,
  NRSoldOutputArea2001Code,
  OutputArea2001Code,
  Datazone2001Code,
  Easting,
  Northing
                     )

rm(area_links_pt1, area_links_pt2)


coo_2001 <- tbl_df(coo_2001)
eg_2001 <- tbl_df(eg_2001)
rel_2001 <- tbl_df(rel_2001)

coo_2001 <- rename(coo_2001, zonecode = Zone.Code)
eg_2001 <- rename(eg_2001, zonecode=Zone.Code)
rel_2001 <- rename(rel_2001, zonecode=Zone.Code)

area_links <- rename(
  area_links,
  zonecode=NRSoldOutputArea2001Code,
  datazone=Datazone2001Code
  )

coo_2001 <- join(coo_2001, area_links)
eg_2001 <- join(eg_2001, area_links)
rel_2001 <- join(rel_2001, area_links)

coo_2001 <- tbl_df(coo_2001)
eg_2001 <- tbl_df(eg_2001)
rel_2001 <- tbl_df(rel_2001)

coo_2001 <- arrange(coo_2001, datazone)
eg_2001 <- arrange(eg_2001, datazone)
rel_2001 <- arrange(rel_2001, datazone)

names(coo_2001) <- tolower(names(coo_2001))
names(eg_2001) <- tolower(names(eg_2001))
names(rel_2001) <- tolower(names(rel_2001))

coo_2001 <- coo_2001 %>% 
  select(
  -zonecode, -outputarea2011code,
  -outputarea2001code, -easting, -northing
  ) %>%
  gather(
    type,
    count,
    -datazone
    ) %>% 
  mutate(year=2001) %>%
  arrange(datazone, year)

eg_2001 <- eg_2001 %>%
  select(
    -zonecode, -outputarea2001code,
    -outputarea2011code, -easting, -northing
    ) %>%
  gather(
    type, count,
    -datazone
    ) %>% 
  mutate(year=2001) %>% 
  arrange(datazone, year)
  
rel_2001 <- rel_2001 %>%
  select(
    -zonecode, -outputarea2001code,
    -outputarea2011code, -easting, -northing
    ) %>%
  gather(
    type, count,
    -datazone
    ) %>%
  mutate(year=2001) %>%
  arrange(datazone, year)


rel_2001 <- rel_2001 %>% group_by(datazone, year, type) %>% 
  summarise(count=sum(count))

coo_2001 <- coo_2001 %>% group_by(datazone, year, type) %>%
  summarise(count = sum(count))

eg_2001 <- eg_2001 %>% group_by(datazone, year, type) %>%
  summarise(count = sum(count))


######################################################################
# 2011


# populations


# ethnic group  2a    ks201sc
eg_2011 <- source_DropboxData(
  file="KS201SC.csv",
  key="adxgd6edmvgyk82"
  ) %>% tbl_df()


# country of origin  2a ks 204sc
coo_2011 <- source_DropboxData(
  file="KS204SC.csv",
  key="xrs0mxq9alumojs"
  ) %>% tbl_df()


# religion  2a 209scb

rel_2011 <- source_DropboxData(
  file="KS209SCb.csv",
  key="aejzg3hbu443pxl"
) %>% tbl_df()


names(eg_2011) <- names(eg_2011) %>% tolower()
names(coo_2011) <- names(coo_2011) %>% tolower()
names(rel_2011) <- names(rel_2011) %>% tolower()

# filter away to total (the first x)

eg_2011 <- eg_2011 %>% rename(datazone=x) %>% 
  filter( datazone!="S92000003") %>% 
  gather(
    type,
    count,
    -datazone
    ) %>% mutate(year=2011)
coo_2011 <- coo_2011 %>% rename(datazone=x) %>%
  filter(datazone!="S92000003") %>% 
  gather(
    type,
    count,
    -datazone
  ) %>% mutate(year=2011)
rel_2011 <- rel_2011 %>% rename(datazone=x) %>%
  filter(datazone!="S92000003") %>% 
  gather(
    type,
    count,
    -datazone
  ) %>% mutate(year=2011)



###############

write.csv(
  coo_2001,
  "data/coo_2001.csv"
  )

write.csv(
  rel_2001,
  "data/rel_2001.csv"
)

write.csv(
  eg_2001,
  "data/eg_2001.csv"
)


write.csv(
  coo_2011,
  "data/coo_2011.csv"
)

write.csv(
  rel_2011,
  "data/rel_2011.csv"
)

write.csv(
  eg_2011,
  "data/eg_2011.csv"
)
