# Updated Nov 6, 2024 by C. Tribuzio
# brings in dogfish sample data and creates a summary

# To Do ----
# make into an automated quarto report

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
samp_dat <- read_sheet('1pbSRX_9vj3Xe3_vqK_psvamk18oGSH3Kb-R6NeVSQkc') %>% clean_names()
samp_spec_join <- read_sheet('1pbSRX_9vj3Xe3_vqK_psvamk18oGSH3Kb-R6NeVSQkc', sheet = 'Sample_Join') %>% clean_names()
spec_dat <- read_sheet('1pbSRX_9vj3Xe3_vqK_psvamk18oGSH3Kb-R6NeVSQkc', sheet = 'Specimen_Info') %>% clean_names()
haul_dat <- read_sheet('1pbSRX_9vj3Xe3_vqK_psvamk18oGSH3Kb-R6NeVSQkc', sheet = 'Haul_Info') %>% clean_names()
haul_spec_join <- read_sheet('1pbSRX_9vj3Xe3_vqK_psvamk18oGSH3Kb-R6NeVSQkc', sheet = 'Haul_Join') %>% clean_names()
rank_loc <- read_sheet('1pbSRX_9vj3Xe3_vqK_psvamk18oGSH3Kb-R6NeVSQkc', sheet = 'Sample_Tracking') %>% clean_names()

#clean up lists and weird stuff----
spec_dat$maturity <- as.character(spec_dat$maturity)
spec_dat$alternate_specimen_id <- as.character(spec_dat$alternate_specimen_id)
spec_dat <- spec_dat %>% 
  unnest(c(maturity, alternate_specimen_id))

haul_dat$nmfs_area <- as.character

# Summarize df samples----
samp_dat2 <- samp_dat %>% 
  left_join(samp_spec_join) %>% 
  left_join(spec_dat) %>% 
  left_join(rank_loc) %>% 
  left_join(haul_spec_join) %>% 
  left_join(haul_dat) %>% 
  filter(sample_type %in% c('Spine', 'Spine A', 'Spine P'),
         greater_sample_location == "TSMRI",
         status == 'Frozen') %>% 
  select(c('sample_id', 'specimen_id', 'alternate_specimen_id', 'length_cm', 'length_type', 'weight_kg', 'sex', 'maturity', 'rank', 'large_marine_ecosystem',
           'haul_date_akt', 'noncon_lat','noncon_long', 'nmfs_area'))
write_csv(samp_dat2, paste0(getwd(),'/Sample_Data/Summary/dogfish/NPRB2301spiny_dogfish_spines.csv'))
