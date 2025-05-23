# Any NORPAC observer data----
# Created by C. Tribuzio
# Updated 5/37/2022

# Libraries ----
libs <- c("tidyverse","sp","sf", "rnaturalearth", "rgdal", "rnaturalearthdata","spatialEco", "janitor")
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)

# Spatial Joins for making non-confidential ----

AKncf_grid <- readOGR(dsn = paste(getwd(),"/data/shapefiles/20kmhexagon_clip",sep=""),
                      layer = "20kmhexagon_clip")
# view grid
plot(AKncf_grid)

# Tag recovery data example ----
datfile <- "DDLLtag_summary.csv"
filedir <- paste(getwd(), "/data/tags/", datfile, sep = "")
tag_dat <- read_csv(filedir) %>% 
  clean_names() %>% 
  filter(!is.na(species))

# Create smaller set dataframe without extra cols
set_sm <- tag_dat %>% 
  filter(recovery_confidential == "Yes") %>% 
  select(lon = recovery_long, 
         lat = recovery_lat, 
         id) %>% 
  data.frame

# Make non-confidential ----
# Setting existing coordinate as lat-long system
OBSsp <- SpatialPointsDataFrame(coords = set_sm[, c(1, 2)], 
                                data = set_sm,
                                proj4string = CRS("+proj=longlat"))
OBSsp <- spTransform(OBSsp, CRS(proj4string(AKncf_grid)))

# merging layers
OBSsp2<-point.in.poly(OBSsp,AKncf_grid)

# extracting data
OBS_dat2<-as.data.frame(OBSsp2@data) %>% 
  select(c(id, Latitude, Longitude)) %>% 
  rename(ncf_lat = Latitude,
         ncf_lon = Longitude)

# Summary ----
# NOTE: there are recoveries outside of AK waters, which returned NA for ncf lat/long

OBS_out <- left_join(tag_dat, OBS_dat2) %>% 
  mutate(ncf_lat = ifelse(recovery_confidential == "No", recovery_lat, ncf_lat),
         ncf_lon = ifelse(recovery_confidential == "No", recovery_long, ncf_lon))

nrow(tag_dat) == nrow(OBS_out)

write_csv(OBS_out, paste(getwd(), "/Output/DDLL_tag_summary_ncf.csv", sep = ""))
