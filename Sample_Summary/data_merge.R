# Updated Aug 28 2023 by C. Tribuzio
# Summary of sleeper shark eye samples available for NPRB ageing study

# To Do ----
# Once db is established, will pull data differently

# Setup ----
libs <- c("tidyverse", "janitor", "Hmisc", "RColorBrewer", "gridExtra", "gtable", 
          "grid", "flextable", "officer", "lubridate", "RODBC", "DBI", "gtable", "patchwork")
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)
'%nin%'<-Negate('%in%') #this is a handy function

# Bring in Data ----
# current version are csv out of:
# C:\Users\cindy.Tribuzio\Work\Sharks Misc\Sample Database\Tribuzio Shark Sample Data Entry Format_CAT6_5_2023.xlsx
# This will eventually be in shark sample database

# Sample to specimen joins
id_join <- read_csv(paste0(getwd(),"/Sample_Summary/Shark_db_sampspecjoin.csv")) %>% 
  clean_names() %>% 
  select(sample_id, specimen_id)

# Sample Information
samp_dat <- read_csv(paste0(getwd(),"/Sample_Summary/Shark_db_sample.csv")) %>% 
  clean_names() %>% 
  filter(sample_type %in% c("Eye L", "Eye R", "Eye A", "Eye B", "eye L", 
                            "Eye", "Embryo", "Embryo 1", "Embryo 2", "Embryo A",
                            "Embryo B", "Candle")) %>% 
  select(sample_id, sample_type, alternate_sample_id) %>% 
  left_join(id_join)

# Specimen information
spec_dat <- read_csv(paste0(getwd(),"/Sample_Summary/Shark_db_specimen.csv")) %>% 
  clean_names() %>% 
  filter(species_common_name %in% c("Pacific Sleeper Shark", "Spiny Dogfish"))

# Specimen to haul joins
haul_join <- read_csv(paste0(getwd(),"/Sample_Summary/Shark_db_spechauljoin.csv")) %>% 
  clean_names() %>% 
  select(haul_id, specimen_id)

# Haul information
haul_dat <- read_csv(paste0(getwd(),"/Sample_Summary/confidential_Shark_db_haul.csv")) %>% 
  clean_names() %>% 
  left_join(haul_join)

# Join specimen and haul data ----
spec_hauls <- spec_dat %>% 
  left_join(haul_dat)

# summarize sample sizes per specimen ----
eye_pairs <- samp_dat %>% 
  filter(sample_type %in% c("Eye L", "Eye R", "Eye A", "Eye B", "eye L", 
                            "Eye")) %>% 
  group_by(specimen_id) %>% 
  summarise(n_eyes = length(sample_type))

df_prego <- samp_dat %>% 
  filter(sample_type %in% c("Embryo", "Embryo 1", "Embryo 2", "Embryo A",
                            "Embryo B", "Candle")) %>% 
  group_by(specimen_id) %>% 
  summarise(pregnant = if_else(length(sample_type)>0, "Y", "N"))

allspec_dat <- spec_hauls %>% 
  left_join(eye_pairs) %>% 
  left_join(df_prego) %>% 
  filter(!is.na(n_eyes))

write_csv(allspec_dat, paste0(getwd(),"/Sample_Summary/confidential_specimen_data.csv"))
