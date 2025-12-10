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
# sample data (e.g., eyes, embryos)
samp_dat <- read_sheet('1pbSRX_9vj3Xe3_vqK_psvamk18oGSH3Kb-R6NeVSQkc') %>% clean_names() %>% 
  filter(sample_type %in% c('Embryo_1', 'Eye', 'Embryo_2', 'Eye_L', 'Eye_R', 'Eye_B', 'Eye_A', 'Candle', 'Spine_P')) %>% 
  select(-notes_some_got_out_of_order)
# lookup table for joining samples and specimens
samp_spec_join <- read_sheet('1pbSRX_9vj3Xe3_vqK_psvamk18oGSH3Kb-R6NeVSQkc', sheet = 'Sample_Join') %>% clean_names()
# specimen data (i.e., animal that the samples came from)
spec_dat <- read_sheet('1pbSRX_9vj3Xe3_vqK_psvamk18oGSH3Kb-R6NeVSQkc', sheet = 'Specimen_Info') %>% clean_names() %>% 
  select(-c(alternate_specimen_id, weight_kg, weight_type, original_weight_units, original_length_units)) %>% 
  filter(species_common_name %in% c("Spiny Dogfish", "Pacific Sleeper Shark"))
# haul data for each animal
haul_dat <- read_sheet('1pbSRX_9vj3Xe3_vqK_psvamk18oGSH3Kb-R6NeVSQkc', sheet = 'Haul_Info') %>% clean_names() %>% 
  select(haul_id, large_marine_ecosystem, haul_date_akt, noncon_lat, noncon_long, haul_year, nmfs_area, fmp_subarea, source)
# look up table for joining animals to hauls
haul_spec_join <- read_sheet('1pbSRX_9vj3Xe3_vqK_psvamk18oGSH3Kb-R6NeVSQkc', sheet = 'Haul_Join') %>% clean_names()
# sample quality ranks and status of a sample (e.g., frozen, shipped, delaminated, etc.) and where the sample can be found
rank_dat <- read_sheet('1i5Q1b6F8m9RK7l_L5-5fmbTkJq1hisUn8w2dwogeTPs') %>% clean_names()
# eye layer data
layer_dat <- read_sheet('1j-TdYjQsN56HmBb07apldWv-TvOh1S_9T3oP6LuKoHU') %>% clean_names() %>% 
  select(sample_id, ams_id, layer_type, layer_order, methods, vial_mt_g, vial_dry_samp_g, layer_diam_mm, image_comments,
         wt_to_nosams_mg, f_modern, fm_err, wt_to_sia_mg, wt_p_n, wt_p_c, d15n, d13c, cn_ratio, wt_to_csiaa)

# Cleaning up data weirdos ----
# reads in mixed format columns as lists, convert to character then unnest them
spec_dat$maturity <- as.character(spec_dat$maturity)
spec_dat <- spec_dat %>% 
  unnest(maturity)

#########################start here
# clean up sample_type following these rules:
# 1) All Eye A become Eye L, Eye B become Eye R, Eye by itself is Eye L
# 2) Embryos are Embryo 1, Embryo 2......(done in db)
# 3) Spine becomes Spine P (done in db)

samp_dat <- samp_dat %>% 
  mutate(sample_type = if_else(sample_type == "Eye" | sample_type == "Eye_A", "Eye_L", 
                               if_else(sample_type == "Eye_B", "Eye_R", sample_type)))

#length conversion
# dogfish length conversions from Tribuzio and Kruse 2012
dfa_pcl <- 3.48859
dfb_pcl <- 1.203964
dfa_fl <- 1.224333
dfb_fl <- 1.067497
pssa <- 17.78
pssb <- 1.1

spec_dat2 <- spec_dat %>% 
  mutate(length2 = if_else(length_type == "Precaudal Length" & species_common_name == "Spiny Dogfish", length_cm * dfb_pcl + dfa_pcl,
                           if_else(length_type == "Fork Length" & species_common_name == "Spiny Dogfish", length_cm * dfb_fl + dfa_fl,
                                   if_else(length_type == "Precaudal Length" & species_common_name == "Pacific Sleeper Shark", 
                                           length_cm * pssb + pssa, length_cm))),
         lt2 = if_else(length_cm != length2, "Total Length", length_type)) %>% 
  select(!c(length_cm, length_type, notes)) %>% 
  rename(length_cm = length2,
         length_type = lt2)

# make layer summary table----
samp_dat3 <- samp_dat %>% 
  left_join(samp_spec_join) %>% 
  left_join(spec_dat2) %>% 
  left_join(haul_spec_join) %>% 
  left_join(haul_dat) %>% 
  mutate(loc_complete = if_else(!is.na(large_marine_ecosystem), "Y","N"),
         bio_complete = if_else(is.na(length_type) | length_type != "Total Length", "N", "Y"),
         data_complete = if_else(loc_complete == "Y" & bio_complete == "Y", "Y", "N")) %>% 
  left_join(rank_dat) %>% 
  select(sample_id, specimen_id, species_common_name, sex, length_cm, length_type, haul_year) %>% 
  left_join(layer_dat) %>% 
  filter(methods %in% c('M11', 'M12', 'M13', 'M14', 'M15'))

# calc layers per eye----
nlayerdat <- samp_dat3 %>% 
  group_by(species_common_name, specimen_id, sample_id, length_cm, length_type) %>% 
  summarise(nlayers = length(ams_id)) %>% 
  group_by(species_common_name, specimen_id, length_cm, length_type) %>% 
  summarise(maxlayers = max(nlayers)) %>% 
  filter(!is.na(length_cm),
         species_common_name == 'Pacific Sleeper Shark')

ggplot(nlayerdat, aes(x = length_cm, y = maxlayers))+
  geom_point(size = 4, color = 'blue')+
  labs(x = "Length (cm)", y = 'Number of layers')+
  theme_bw()
