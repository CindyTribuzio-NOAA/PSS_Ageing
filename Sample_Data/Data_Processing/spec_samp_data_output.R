# Updated 6 Jan 2025 by C. Tribuzio
# combines sample, specimen, and haul data to a user friendly wide format output csv
# all NPRB 2301 ANIMAL data in one place
# this is a comprehensive list of all samples available for study
# can be updated as data are added, this code will overwrite NPRB2301_specimen_data and NPRB2301_layer_results


# To Do ----
# fix lengths
# add method to layer results so can filter out non useful samples
# build in ranks

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
  select(-notes)
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
  select(sample_id, ams_id, layer_type, layer_order, methods, vial_mt_g, vial_dry_samp_g, layer_diam_mm,
         wt_to_nosams_mg, f_modern, fm_err)

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


# make nice specimen summary table----
samp_dat2 <- samp_dat %>% 
  left_join(samp_spec_join) %>% 
  left_join(spec_dat2) %>% 
  #left_join(rank_loc) %>% 
  left_join(haul_spec_join) %>% 
  left_join(haul_dat) %>% 
  select(c(specimen_id, species_common_name, length_cm, length_type, sex, large_marine_ecosystem,
           haul_date_akt, noncon_lat, noncon_long, nmfs_area, sample_type, sample_id, source)) %>% 
  group_by(specimen_id, species_common_name, length_cm, length_type, sex, large_marine_ecosystem, haul_date_akt, noncon_lat, noncon_long,
           nmfs_area, sample_type, source) %>% 
  summarise(n_samp = length(sample_id)) %>% 
  pivot_wider(names_from = sample_type, values_from = n_samp) %>% 
  mutate(samp_test = sum(Eye_L, Eye_R, na.rm = T)) %>% 
  filter(samp_test > 0,
         !is.na(species_common_name)) %>% 
  select(!samp_test) %>% 
  mutate(loc_complete = if_else(!is.na(large_marine_ecosystem), "Y","N"),
         bio_complete = if_else(is.na(length_type) | length_type != "Total Length", "N", "Y"),
         data_complete = if_else(loc_complete == "Y" & bio_complete == "Y", "Y", "N")) %>% 
  left_join(rank_dat) %>% 
  select(!c(rcode, description, sample_quality, loc_complete, bio_complete, data_complete))

glink <- "https://docs.google.com/spreadsheets/d/1J3IKrdptj7eS3VZj_qbxXTXorgktHXnQ_gUv1mVaxwk/edit?gid=0#gid=0"
samp_dat2 %>% write_sheet(ss = glink, sheet = "specimen_data")

# make layer summary table----
samp_dat3 <- samp_dat %>% 
  left_join(samp_spec_join) %>% 
  left_join(spec_dat2) %>% 
  left_join(rank_loc) %>% 
  left_join(haul_spec_join) %>% 
  left_join(haul_dat) %>% 
  select(sample_id, specimen_id, sex, length_cm, length_type, haul_year)

layer_dat2 <- layer_dat %>% 
  left_join(samp_dat3) %>% 
  #mutate(haul_year = year(haul_date_akt)) %>% 
  #select(!haul_date_akt) %>% 
  mutate(layer_wt_mg = (vial_dry_samp_g - vial_mt_g)*1000,
         D14C = 1000 * (f_modern -1),
         D14C_err = 1000 * fm_err,
         delta14C = 1000 * (f_modern * exp((1950 - haul_year)/8276)-1)) %>% 
  select(!c(vial_mt_g, vial_dry_samp_g)) %>% 
  filter(f_modern >= 0)

layer_link <- "https://docs.google.com/spreadsheets/d/1xeHWScrJwWkeN_YV-C6euG_7G4w3u7nHjjew34BoSP0/edit?gid=0#gid=0"
layer_dat2 %>% write_sheet(ss = layer_link, sheet = "layer_results")

write_csv(layer_dat2, paste0(getwd(), "/Obj_1_14C/NPRB2301_layer_results"))

#samples with lengths----
layer_dat3 <- layer_dat %>% 
  left_join(samp_dat3) %>% 
  filter(is.na(wt_to_nosams_mg),
         methods != "M9/M10",
         length_cm > 0) %>% 
  mutate(layer_wt_mg = (vial_dry_samp_g - vial_mt_g)*1000) %>% 
  select(!c(vial_mt_g, vial_dry_samp_g, methods, layer_diam_mm, f_modern, fm_err, haul_date_akt,
            wt_to_nosams_mg))

write_csv(layer_dat3, paste0(getwd(), "/Sample_Data/Data_Processing/layers_to_NOSAMS.csv"))
