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
id_join <- read_csv(paste0(getwd(),"/Sample_Data/Summary/Shark_db_sampspecjoin.csv")) %>% 
  clean_names() %>% 
  select(sample_id, specimen_id)

# Sample Information
samp_dat <- read_csv(paste0(getwd(),"/Sample_Data/Summary/Shark_db_sample.csv")) %>% 
  clean_names() %>% 
  filter(sample_type %in% c("Head")) %>% 
  select(sample_id, sample_type, alternate_sample_id) %>% 
  left_join(id_join) %>% 
  mutate(specimen_id = as.numeric(specimen_id))

# Specimen information
spec_dat <- read_csv(paste0(getwd(),"/Sample_Data/Summary/Shark_db_specimen.csv")) %>% 
  clean_names() %>% 
  filter(species_common_name %in% c("Pacific Sleeper Shark", "Spiny Dogfish"))

# Specimen to haul joins
haul_join <- read_csv(paste0(getwd(),"/Sample_Data/Summary/Shark_db_spechauljoin.csv")) %>% 
  clean_names() %>% 
  select(haul_id, specimen_id)

# Haul information
haul_dat <- read_csv(paste0(getwd(),"/Sample_Data/Summary/confidential_Shark_db_haul.csv")) %>% 
  clean_names() %>% 
  left_join(haul_join)

# Join specimen and haul data ----
spec_hauls <- spec_dat %>% 
  left_join(haul_dat)

# join with selected samples ----
samp_dat2 <- samp_dat %>% 
  left_join(spec_hauls)

write_csv(samp_dat2, paste0(getwd(),"/Sample_Data/Wilga_AFSC_heads.csv"))
