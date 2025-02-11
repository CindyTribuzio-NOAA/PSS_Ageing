# Updated Dec 17, 2024 by C. Tribuzio
# combines sample, specimen, haul and layer data to a user friendly wide format output csv
# all NPRB 2301 data in one place

# To Do ----


# Setup ----
libs <- c("tidyverse", "janitor", "googlesheets4")
#, "Hmisc", "RColorBrewer", "gridExtra", "gtable", 
#          "grid", "flextable", "officer", "lubridate", "RODBC", "DBI", "gtable", "patchwork")
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)
'%nin%'<-Negate('%in%') #this is a handy function
round_any = function(x, accuracy, f=round){f(x/ accuracy) * accuracy}

# Bring in data from google sheets----
samp_dat <- read_sheet('1pbSRX_9vj3Xe3_vqK_psvamk18oGSH3Kb-R6NeVSQkc') %>% clean_names() %>% 
  filter(sample_type %in% c('Embryo 1', 'Eye', 'Embryo 2', 'Eye L', 'Eye R', 'Eye B', 'Eye A', 'Candle', 'Spine P'))
samp_spec_join <- read_sheet('1pbSRX_9vj3Xe3_vqK_psvamk18oGSH3Kb-R6NeVSQkc', sheet = 'Sample_Join') %>% clean_names()
spec_dat <- read_sheet('1pbSRX_9vj3Xe3_vqK_psvamk18oGSH3Kb-R6NeVSQkc', sheet = 'Specimen_Info') %>% clean_names() %>% 
  select(-c(alternate_specimen_id, weight_kg, weight_type, original_weight_units))
haul_dat <- read_sheet('1pbSRX_9vj3Xe3_vqK_psvamk18oGSH3Kb-R6NeVSQkc', sheet = 'Haul_Info') %>% clean_names() %>% 
  select(haul_id, large_marine_ecosystem, haul_date_akt, noncon_lat, noncon_long, haul_year, nmfs_area, fmp_subarea)
haul_spec_join <- read_sheet('1pbSRX_9vj3Xe3_vqK_psvamk18oGSH3Kb-R6NeVSQkc', sheet = 'Haul_Join') %>% clean_names()
rank_loc <- read_sheet('1pbSRX_9vj3Xe3_vqK_psvamk18oGSH3Kb-R6NeVSQkc', sheet = 'Sample_Tracking') %>% clean_names()
layer_dat <- read_sheet('1j-TdYjQsN56HmBb07apldWv-TvOh1S_9T3oP6LuKoHU') %>% clean_names()

# Combining information ----
#clean up lists and weird stuff----
# reads in mixed format columns as lists, convert to character then unnest them
spec_dat$maturity <- as.character(spec_dat$maturity)
spec_dat <- spec_dat %>% 
  unnest(maturity)

#########################start here
# clean up sample_type following these rules:
# 1) All Eye A become Eye L, Eye B become Eye R, Eye by itself is Eye L
# 2) Embryos are Embryo 1, Embryo 2......(done in db)
# 3) Spine becomes Spine P (done in db)

#length conversion
# layer data, calc dry weight, drying duration, 14C outputs


# join things together
samp_dat2 <- samp_dat %>% 
  left_join(samp_spec_join) %>% 
  left_join(spec_dat) %>% 
  left_join(rank_loc) %>% 
  left_join(haul_spec_join) %>% 
  left_join(haul_dat) %>% 
  select(c(specimen_id, species_common_name, length_cm, length_type, sex, maturity, rank, large_marine_ecosystem,
           haul_date_akt, noncon_lat, noncon_long, nmfs_area, sample_type, sample_id)) %>% 
  pivot_wider(names_from = sample_type, values_from = sample_id)

test <-samp_dat2 %>% 
  summarise(n = dplyr::n(), .by = c(specimen_id, species_common_name, length_cm, length_type, sex, maturity, rank,
                                           large_marine_ecosystem, haul_date_akt, noncon_lat, noncon_long, nmfs_area, sample_type)) %>% 
  filter(n > 1L)

  dplyr::filter(n > 1L) 
