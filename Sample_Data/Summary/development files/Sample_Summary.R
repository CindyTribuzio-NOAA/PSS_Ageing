# PSS Sample Data Summary  ----
# Updated Aug 28 2023 by C. Tribuzio
# Summary of sleeper shark eye samples available for NPRB ageing study
# Do data_merge.R first to merge various db sheets into one useful file as below

# To Do ----


# Setup ----
libs <- c("tidyverse", "janitor", "Hmisc", "RColorBrewer", "gridExtra", "gtable", 
          "grid", "flextable", "officer", "lubridate", "RODBC", "DBI", "gtable", "patchwork")
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)
'%nin%'<-Negate('%in%') #this is a handy function

# Bring in Data ----
# last updated specimen ranks
spec_dat <- read_csv(paste0(getwd(),"/Sample_Data/Summary/confidential_specimen_data_ranked.csv"))

# PSS samples ----
#need to add in quality
PSSeye_qual <- spec_dat %>% 
  filter(species_common_name == "Pacific Sleeper Shark",
         n_eyes == 2) %>% 
  group_by(fmp_subarea) %>% 
  summarise(PSS_samples = length(specimen_id)) %>% 
  mutate(type = "pair")

PSSeye_qual1 <- spec_dat %>% 
  filter(species_common_name == "Pacific Sleeper Shark",
         n_eyes == 1) %>% 
  group_by(fmp_subarea) %>% 
  summarise(PSS_samples = length(specimen_id)) %>% 
  mutate(type = "single")

PSSsamples <- PSSeye_qual %>% 
  bind_rows(PSSeye_qual1)

# Length Dist Figs ----
# PSS only
PSS_Ldat <- spec_dat %>% 
  filter(species_common_name == "Pacific Sleeper Shark") %>% 
  filter(!is.na(PCL_final_cm)) %>% 
  mutate(Lbin = floor(PCL_final_cm/5)*5) %>% 
  group_by(sex, Rank, Lbin) %>% 
  summarise(n_lengths = length(PCL_final_cm))

ggplot(PSS_Ldat, aes(x = Lbin, y = n_lengths, fill = Rank))+
  geom_bar(stat = "identity")+
  facet_grid(sex~.)

sum(PSS_Ldat$n_lengths)





length_dat <- PSSeyes %>% 
  group_by(specimen_id, length_type) %>% 
  summarise(leng = mean(length_cm)) %>% 
  filter(!is.na(leng)) %>% 
  mutate(Lbin = floor(leng/5)*5) %>% 
  group_by(length_type, Lbin) %>% 
  summarise(n_lengths = length(leng))

# graphics ----
ggplot(length_dat, aes(x = Lbin, y = n_lengths, fill = length_type))+
  geom_bar(stat = "identity")+
  facet_grid(length_type~.)
