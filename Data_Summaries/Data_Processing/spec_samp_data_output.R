# Updated 6 Jan 2025 by C. Tribuzio
# combines sample, specimen, and haul data to a user friendly wide format output csv
# all NPRB 2301 ANIMAL data in one place
# this is a comprehensive list of all samples available for study
# can be updated as data are added, this code will overwrite NPRB2301_specimen_data and NPRB2301_layer_results


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
  select(sample_id, sample_desc, ams_id, std_lyr_id, layer_type, new_layer_type, protein_type, layer_order, methods, vial_mt_g, vial_dry_samp_g,
         grid_pixels, grid_mm, diameter_pixels, comments2,
         wt_to_nosams_mg, f_modern, fm_err, wt_to_sia_mg, wt_p_n, wt_p_c, d15n, d13c, cn_ratio, wt_to_csiaa, ala,
         ala_stdv, gly,	gly_stdv,	thr,	thr_stdv,	ser,	ser_stdv,	val,	val_stdv,	leu,	leu_stdv,	ile,	ile_stdv,	nle,	nle_stdv,
         pro,	pro_stdv,	asp,	asp_stdv,	glu,	glu_stdv,	phe,	phe_stdv,	tyr,	tyr_stdv,	lys,	lys_stdv,	csiaa_notes)

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

#length conversion ----
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

# Eye lens diameter estimates ----
layer_dat <- layer_dat %>% 
  mutate(layer_diam_mm = (diameter_pixels*grid_mm)/grid_pixels) %>% 
  rename(image_comments = comments2)

# make nice specimen summary table----
samp_dat2 <- samp_dat %>% 
  left_join(samp_spec_join) %>% 
  left_join(spec_dat2) %>% 
  #left_join(rank_loc) %>% 
  left_join(haul_spec_join) %>% 
  left_join(haul_dat) %>% 
  select(c(specimen_id, species_common_name, length_cm, length_type, sex, large_marine_ecosystem,
           haul_date_akt, haul_year, noncon_lat, noncon_long, nmfs_area, sample_type, sample_id, source)) %>% 
  group_by(specimen_id, species_common_name, length_cm, length_type, sex, large_marine_ecosystem, haul_date_akt, noncon_lat, noncon_long,
           haul_year, nmfs_area, sample_type, source) %>% 
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
  left_join(haul_spec_join) %>% 
  left_join(haul_dat) %>% 
  mutate(loc_complete = if_else(!is.na(large_marine_ecosystem), "Y","N"),
         bio_complete = if_else(is.na(length_type) | length_type != "Total Length", "N", "Y"),
         data_complete = if_else(loc_complete == "Y" & bio_complete == "Y", "Y", "N")) %>% 
  left_join(rank_dat) %>% 
  select(sample_id, specimen_id, species_common_name, sex, length_cm, length_type, haul_year, large_marine_ecosystem, noncon_lat, noncon_long) %>% 
  left_join(layer_dat)

layer_dat2 <- samp_dat3 %>% 
  #mutate(haul_year = year(haul_date_akt)) %>% 
  #select(!haul_date_akt) %>% 
  mutate(#layer_wt_mg = (vial_dry_samp_g - vial_mt_g)*1000, #this is a kinda meaningless parameter, given the poor scale
         D14C = 1000 * (f_modern -1),
         D14C_err = 1000 * fm_err,
         delta14C = 1000 * (f_modern * exp((1950 - haul_year)/8276)-1)) %>% 
  select(!c(vial_mt_g, vial_dry_samp_g, layer_type)) %>% 
  filter(f_modern >= 0 | cn_ratio >= 0 |wt_to_csiaa >= 0)

layer_link <- "https://docs.google.com/spreadsheets/d/1xeHWScrJwWkeN_YV-C6euG_7G4w3u7nHjjew34BoSP0/edit?gid=0#gid=0"
layer_dat2 %>% write_sheet(ss = layer_link, sheet = "combined_layer_results")

#write_csv(layer_dat2, paste0(getwd(), "/Obj_1_14C/NPRB2301_layer_results"))

###

AMS_dat <- samp_dat3 %>% 
  #mutate(haul_year = year(haul_date_akt)) %>% 
  #select(!haul_date_akt) %>% 
  mutate(#layer_wt_mg = (vial_dry_samp_g - vial_mt_g)*1000, #this is a kinda meaningless parameter, given the poor scale
    D14C = 1000 * (f_modern -1),
    D14C_err = 1000 * fm_err,
    delta14C = 1000 * (f_modern * exp((1950 - haul_year)/8276)-1)) %>% 
  select(!c(vial_mt_g, vial_dry_samp_g)) %>% 
  filter(f_modern >= 0) %>% 
  select(c(sample_id, specimen_id, species_common_name, sex, length_cm, length_type, 
           haul_year, large_marine_ecosystem, noncon_lat, noncon_long, sample_desc, ams_id, std_lyr_id, new_layer_type,
           protein_type, layer_order, methods, layer_diam_mm, image_comments, wt_to_nosams_mg, f_modern, fm_err, D14C, D14C_err, delta14C))

layer_link <- "https://docs.google.com/spreadsheets/d/1xeHWScrJwWkeN_YV-C6euG_7G4w3u7nHjjew34BoSP0/edit?gid=0#gid=0"
AMS_dat %>% write_sheet(ss = layer_link, sheet = "14C_layer_results")

SIA_dat <- samp_dat3 %>% 
  select(!c(vial_mt_g, vial_dry_samp_g)) %>% 
  filter(cn_ratio >= 0) %>% 
  select(c(sample_id, specimen_id, species_common_name, sex, length_cm, length_type, 
           haul_year, large_marine_ecosystem, noncon_lat, noncon_long, sample_desc, ams_id, std_lyr_id, new_layer_type,
           protein_type, layer_order, methods, layer_diam_mm, image_comments, wt_to_sia_mg, wt_p_n, wt_p_c, d15n, d13c, cn_ratio))

layer_link <- "https://docs.google.com/spreadsheets/d/1xeHWScrJwWkeN_YV-C6euG_7G4w3u7nHjjew34BoSP0/edit?gid=0#gid=0"
SIA_dat %>% write_sheet(ss = layer_link, sheet = "SIA_layer_results")

CSI_dat <- samp_dat3 %>% 
  select(!c(vial_mt_g, vial_dry_samp_g)) %>% 
  filter(ala >= 0) %>% 
  select(c(sample_id, specimen_id, species_common_name, sex, length_cm, length_type, 
           haul_year, large_marine_ecosystem, noncon_lat, noncon_long, sample_desc, ams_id, std_lyr_id, new_layer_type,
           protein_type, 
           layer_order, methods, layer_diam_mm, image_comments, wt_to_csiaa, ala, ala_stdv, gly, gly_stdv, thr, thr_stdv,
           ser, ser_stdv, val, val_stdv, leu, leu_stdv, ile, ile_stdv, nle, nle_stdv, pro, pro_stdv, asp, asp_stdv, 
           glu, glu_stdv, phe, phe_stdv, tyr, tyr_stdv, lys, lys_stdv, csiaa_notes))

layer_link <- "https://docs.google.com/spreadsheets/d/1xeHWScrJwWkeN_YV-C6euG_7G4w3u7nHjjew34BoSP0/edit?gid=0#gid=0"
CSI_dat %>% write_sheet(ss = layer_link, sheet = "CSIAA_layer_results")

diam_dat <- samp_dat3 %>% 
  select(!c(vial_mt_g, vial_dry_samp_g)) %>% 
  filter(layer_diam_mm >= 0) %>% 
  select(c(sample_id, specimen_id, species_common_name, sex, length_cm, length_type, 
           haul_year, large_marine_ecosystem, noncon_lat, noncon_long, sample_desc, ams_id, std_lyr_id, new_layer_type,
           new_layer_type, protein_type, layer_order, methods, layer_diam_mm, image_comments))

layer_link <- "https://docs.google.com/spreadsheets/d/1xeHWScrJwWkeN_YV-C6euG_7G4w3u7nHjjew34BoSP0/edit?gid=0#gid=0"
diam_dat %>% write_sheet(ss = layer_link, sheet = "layer_diameter_results")

#specimen sample quick reference----
#makes a quick look up sheet for which animals we have which output data for
pup_list <- layer_dat2 %>% 
  select(specimen_id, sample_desc) %>% 
  filter(sample_desc == 'SD embryo eye') %>% 
  mutate(n_embryo = 1) %>% 
  select(!sample_desc)
  
spec_dat3 <- samp_dat3 %>% 
  filter(sample_desc != 'SD embryo eye') %>% 
  filter(!is.na(cn_ratio) | !is.na(f_modern) | !is.na(ala)) %>% 
  group_by(specimen_id, species_common_name, sex, length_cm, length_type, haul_year, large_marine_ecosystem, noncon_lat, noncon_long) %>% 
  summarise(n_eyes = length(unique(sample_id)),
            n_layers = length(ams_id),
            n_diameters = sum(!is.na(layer_diam_mm)),
            n_14C = sum(!is.na(f_modern)),
            n_SIA = sum(!is.na(d15n)),
            n_CSIAA = sum(!is.na(ala))) %>% 
  left_join(pup_list) %>% 
  mutate(n_embryo = if_else(is.na(n_embryo), 0, n_embryo))

specp_link <- "https://docs.google.com/spreadsheets/d/1Yr6yGoU-_5JCO69TGdjaRJYhw2AL2YMG6kYeCGMQKyw/edit?gid=0#gid=0"
spec_dat3 %>% write_sheet(ss = specp_link, sheet = "Processed_specimens")

#samples with lengths----
#layer_dat3 <- layer_dat %>% 
#  left_join(samp_dat3) %>% 
#  filter(is.na(wt_to_nosams_mg),
#         methods != "M9/M10",
#         length_cm > 0) %>% 
#  mutate(layer_wt_mg = (vial_dry_samp_g - vial_mt_g)*1000) %>% 
#  select(!c(vial_mt_g, vial_dry_samp_g, methods, layer_diam_mm, f_modern, fm_err, haul_date_akt,
#            wt_to_nosams_mg))

#write_csv(layer_dat3, paste0(getwd(), "/Sample_Data/Data_Processing/layers_to_NOSAMS.csv"))

