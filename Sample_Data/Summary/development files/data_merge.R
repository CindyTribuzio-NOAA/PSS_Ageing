# Updated Aug 28 2023 by C. Tribuzio
# merges sample database csvs for NPRB ageing study

# To Do ----
# Once db is established, will pull data differently

# Setup ----
libs <- c("tidyverse", "janitor", "Hmisc", "RColorBrewer", "gridExtra", "gtable", 
          "grid", "flextable", "officer", "lubridate", "RODBC", "DBI", "gtable", "patchwork")
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)
'%nin%'<-Negate('%in%') #this is a handy function
round_any = function(x, accuracy, f=round){f(x/ accuracy) * accuracy}

# Bring in Data ----
# current version are csv out of:
# C:\Users\cindy.Tribuzio\Work\Sharks Misc\Sample Database\Tribuzio Shark Sample Data Entry Format_CAT6_5_2023.xlsx
# This will eventually be in shark sample database

# Sample to specimen joins
id_join <- read_csv(paste0(getwd(),"/Sample_Data/Summary/Shark_db_sampspecjoin5.csv"),lazy = F) %>% 
  clean_names() %>% 
  select(sample_id, specimen_id)

# Sample Information
samp_dat <- read_csv(paste0(getwd(),"/Sample_Data/Summary/Shark_db_sample5.csv"),lazy = F) %>% 
  clean_names() %>% 
  filter(sample_type %in% c("Eye L", "Eye R", "Eye A", "Eye B", "eye L", 
                            "Eye", "Embryo", "Embryo 1", "Embryo 2", "Embryo A",
                            "Embryo B", "Candle")) %>% 
  select(sample_id, sample_type, alternate_sample_id) %>% 
  left_join(id_join) %>%
  filter(!is.na(specimen_id)) %>% 
  mutate(specimen_id = as.numeric(specimen_id))

# Specimen information
spec_dat <- read_csv(paste0(getwd(),"/Sample_Data/Summary/Shark_db_specimen5.csv"),lazy = F) %>% 
  clean_names() %>% 
  filter(species_common_name %in% c("Pacific Sleeper Shark", "Spiny Dogfish"))

# Specimen to haul joins
haul_join <- read_csv(paste0(getwd(),"/Sample_Data/Summary/Shark_db_spechauljoin5.csv"),lazy = F) %>% 
  clean_names() %>% 
  select(haul_id, specimen_id)

# Haul information
haul_dat <- read_csv(paste0(getwd(),"/Sample_Data/Summary/confidential_Shark_db_haul5.csv"),lazy = F) %>% 
  clean_names() %>% 
  left_join(haul_join)

# Sample and specimen status information with ranking
status_dat <- read_csv(paste0(getwd(),"/Sample_Data/Summary/Shark_sample_status.csv"),lazy = F) %>% 
  clean_names()

rank_dat <- read_csv(paste0(getwd(),"/Sample_Data/Summary/Shark_specimen_ranks.csv"),lazy = F) %>% 
  clean_names()

# Join specimen and haul data ----
spec_hauls <- spec_dat %>% 
  left_join(haul_dat)

# summarize sample sizes per specimen ----
eye_pairs <- samp_dat %>% 
  filter(sample_type %in% c("Eye L", "Eye R", "Eye A", "Eye B", "eye L", 
                            "Eye")) %>% 
  group_by(specimen_id) %>% 
  summarise(n_eyes = length(sample_type)) %>% 
  ungroup()

df_prego <- samp_dat %>% 
  filter(sample_type %in% c("Embryo", "Embryo 1", "Embryo 2", "Embryo A",
                            "Embryo B", "Candle")) %>% 
  group_by(specimen_id) %>% 
  summarise(pregnant = if_else(length(sample_type)>0, "Y", "N")) %>% 
  ungroup()

#eye_cols <- samp_dat %>% 
#  filter(sample_type %in% c("Eye L", "Eye R", "Eye A", "Eye B", "eye L", #for ease, convert all unknown eye locations to a presumed L/R, can still trace back to orignial data
#                            "Eye")) %>% 
#  mutate(sample_type = if_else(sample_type == "Eye A", "Eye L", 
#                 if_else(sample_type == "Eye B", "Eye R", sample_type))) %>% 
#  pivot_wider(id_cols = specimen_id, names_from = sample_type, values_from = sample_id) %>% 
#  rename(EyeL_id = 'Eye L',
#         EyeR_id = 'Eye R')

allspec_dat <- spec_hauls %>% 
  left_join(eye_pairs) %>% 
#  left_join(eye_cols) %>% 
  left_join(df_prego) %>% 
  left_join(rank_dat) %>% 
  filter(!is.na(n_eyes)) %>% 
  select(specimen_id, species_common_name, species_scientific_name, length_cm, length_type, weight_kg, weight_type, sex, maturity,
         large_marine_ecosystem,haul_date_akt, capture_depth_m, noncon_lat, noncon_long, gear_type,
         haul_year, nmfs_area, fmp_subarea, n_eyes, pregnant, sample_quality, data_quality, rank)

# check for new specimen ids without status yet
s_update <- allspec_dat %>% 
  filter(is.na(rank))
s_update

write_csv(allspec_dat, paste0(getwd(),"/Sample_Data/Summary/confidential_specimen_data5.csv"))

# Sample data file ----
samp_dat2 <- samp_dat %>% 
  left_join(spec_dat) %>% 
  select(!c(original_length_units, original_weight_units, alternate_specimen_id, weight_kg, weight_type,
            maturity, maturity_info, species_scientific_name, alternate_sample_id)) %>% 
  mutate(sample_type = if_else(sample_type == "Eye A", "Eye L", 
             if_else(sample_type == "Eye B", "Eye R", sample_type)),
         sample_type = if_else(sample_type == "Embryo", "Embryo 1",
                               if_else(sample_type == "Embryo A", "Embryo 1",
                                       if_else(sample_type == "Embryo B", "Embryo 2",
                                               if_else(sample_type == "Candle", "Embryo 1", sample_type)))),
         lbin = round_any(length_cm, 10, floor)) %>% 
  left_join(rank_dat) %>% 
  left_join(status_dat) %>% 
  select(!c(sample_quality, data_quality))

unique(samp_dat2$rank)

write_csv(samp_dat2, paste0(getwd(),"/Sample_Data/Summary/confidential_sample_data4.csv"))
