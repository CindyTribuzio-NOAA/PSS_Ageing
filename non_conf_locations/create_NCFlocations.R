# Any NORPAC observer data----
# Created by C. Tribuzio
# Updated 2/22/2024

# Libraries ----
libs <- c("tidyverse","sp","sf", "rnaturalearth", "rnaturalearthdata","spatialEco", "janitor")
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)

# Spatial Joins for making non-confidential ----

AKncf_grid <- st_read(dsn = paste(getwd(),"/confidential_locations/data/shapefiles/20kmhexagon_clip",sep=""),
                      layer = "20kmhexagon_clip")

# Bring in haul data ----
datfile <- "confidential_Shark_db_haul.csv"
filedir <- paste(getwd(), "/confidential_locations/", datfile, sep = "")
set_sm <- read_csv(filedir) %>% 
  clean_names() %>% 
  filter(!is.na(haul_latitude_decimal_degrees)) %>% #filter out hauls with no location info anyway   #filter out hauls which already have non-confidential locations
  select(haul_id, haul_latitude_decimal_degrees, haul_longitude_decimal_degrees) %>%  #select columns of interest
  rename(id = haul_id,
         lat = haul_latitude_decimal_degrees,
         lon = haul_longitude_decimal_degrees) %>% 
  data.frame

# Make non-confidential ----
# Setting up to match AKncf
OBSsp <- st_as_sf(set_sm,
                  coords = c("lon", "lat"),
                  remove = F,
                  crs = "+proj=longlat +datum=NAD83 +towgs84=0,0,0")
OBSsp <- st_transform(OBSsp, 3857)


# view grid and data together
ggplot()+
  geom_sf(data=AKncf_grid, color="red", fill=NA)+
  #geom_point(data = set_sm, aes(x = lat, y = lon))
  geom_sf(data=OBSsp)+
  theme_void()

#OBSsp <- SpatialPointsDataFrame(coords = set_sm[, c(2, 3)], 
#                                data = set_sm,
#                                proj4string = CRS("+proj=longlat"))
#OBSsp <- st_transform(OBSsp)
#OBSsp <- spTransform(OBSsp, CRS(proj4string(AKncf_grid)))

# merging layers
# hack to get around deprecated functions
#OBSsp_sf <- st_as_sf(set_sm)
#AKncf_grid_sf <- st_as_sf(AKncf_grid)

OBSsp_crs <- st_transform(OBSsp, crs = st_crs(AKncf_grid))


OBSsp2 <- st_join(AKncf_grid, OBSsp_crs) #automatically filters out US West Coast samples, do them separately below

OBSpts <- OBSsp2 %>% 
  filter(!is.na(lat)) %>% 
  as.data.frame() %>% 
  select(id, lat, lon, Latitude, Longitude) %>% 
  rename(ncf_lat = Latitude,
         ncf_lon = Longitude)
write_csv(OBSpts, paste(getwd(), "/confidential_locations/confidential_PSSAKhauls_ncf.csv", sep = ""))

# NOTE: there are recoveries outside of AK waters, which returned NA for ncf lat/long
# US West Coast ----
WCncf_grid <- st_read(dsn = paste(getwd(),"/confidential_locations/data/shapefiles/NMFS_WC",sep=""),
                      layer = "WC_all_poly")
WCncf_cent <- st_read(dsn = paste(getwd(),"/confidential_locations/data/shapefiles/NMFS_WC",sep=""),
                      layer = "WC_all_cent")

# filter set_sm for US WC data
WCset <- set_sm %>% 
  filter(lat < 50)
WCOBS <- st_as_sf(WCset,
                  coords = c("lon", "lat"),
                  remove = F,
                  crs = "+proj=longlat +datum=NAD83 +towgs84=0,0,0")
WCOBS <- st_transform(WCOBS, 3857)

# view grid and data together
ggplot()+
  geom_sf(data=WCncf_grid, color="red", fill=NA)+
  #geom_point(data = set_sm, aes(x = lat, y = lon))
  geom_sf(data=WCOBS)+
  theme_void()

WCOBS_crs <- st_transform(WCOBS, crs = st_crs(WCncf_grid))
WCcent <- st_transform(WCncf_cent, crs = st_crs(WCncf_grid))

WCncf <- st_join(WCncf_grid, WCcent)
WCOBS2 <- st_join(WCncf, WCOBS_crs)

WCOBSpts <- WCOBS2 %>% 
  filter(!is.na(lat)) %>% 
  as.data.frame() %>% 
  select(id, lat, lon, x, y) %>% 
  rename(ncf_lat = y,
         ncf_lon = x)
write_csv(WCOBSpts, paste(getwd(), "/confidential_locations/confidential_PSSWChauls_ncf.csv", sep = ""))



# Area Matches ----
libs <- c("tidyverse","akmarineareas2","sf")
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)

# Centroids of mgmt areas-----
#Use below to make selectable layers for hand filling in gaps
# NMFS ----
# create NMFS centroid
nmfs_cent <- st_centroid(nmfs)

# for checking, leave as formatted

nmfs_cent <- nmfs_cent %>% 
  mutate(long = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2])
ggplot()+
  geom_sf(data=nmfs, color="red", fill=NA)+
  geom_sf(data=ak)+
  geom_point(data=nmfs_cent, aes(x = long, y = lat))+
  theme_void()

# convert to dd
nmfs_cent_dd <- st_transform(nmfs_cent, crs = "+proj=longlat +datum=WGS84") %>% 
  mutate(long_dd = st_coordinates(.)[,1],
         lat_dd = st_coordinates(.)[,2]) %>% 
  as.data.frame() %>% 
  select(Area_Type, AreaID, long_dd, lat_dd)

nmfs_cent_dd[nmfs_cent_dd$AreaID == 517,]

# ADFG ----
# create NMFS centroid
adfg_cent <- st_centroid(adfg)

# for checking, leave as formatted

adfg_cent <- adfg_cent %>% 
  mutate(long = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2])
ggplot()+
  geom_sf(data=adfg, color="red", fill=NA)+
  geom_sf(data=ak)+
  geom_point(data=adfg_cent, aes(x = long, y = lat))+
  theme_void()

# convert to dd
adfg_cent_dd <- st_transform(adfg_cent, crs = "+proj=longlat +datum=NAD83") %>% 
  mutate(long_dd = st_coordinates(.)[,1],
         lat_dd = st_coordinates(.)[,2]) %>% 
  as.data.frame() %>% 
  select(Area_Type, AreaID, long_dd, lat_dd)

adfg_cent_dd[adfg_cent_dd$AreaID %in% c(645501),]





