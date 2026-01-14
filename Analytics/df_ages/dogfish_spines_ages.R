# Updated 7 January 2026 by C. Tribuzio
# combines sample, specimen, and spine data for spiny dogfish

# Setup ----
libs <- c("tidyverse", "janitor", "googlesheets4")
#, "Hmisc", "RColorBrewer", "gridExtra", "gtable", 
#          "grid", "flextable", "officer", "lubridate", "RODBC", "DBI", "gtable", "patchwork")
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)
'%nin%'<-Negate('%in%') #this is a handy function
round_any = function(x, accuracy, f=round){f(x/ accuracy) * accuracy}

# Bring in data ----
# sample data (e.g., eyes, embryos)
#samp_dat <- read_sheet('1pbSRX_9vj3Xe3_vqK_psvamk18oGSH3Kb-R6NeVSQkc') %>% clean_names() %>% 
#  filter(sample_type %in% c('Spine_P')) %>% 
#  select(-notes_some_got_out_of_order)
# lookup table for joining samples and specimens
samp_spec_join <- read_sheet('1pbSRX_9vj3Xe3_vqK_psvamk18oGSH3Kb-R6NeVSQkc', sheet = 'Sample_Join') %>% clean_names()
# specimen data (i.e., animal that the samples came from)
spec_dat <- read_sheet('1J3IKrdptj7eS3VZj_qbxXTXorgktHXnQ_gUv1mVaxwk') %>% clean_names() %>% 
  filter(species_common_name %in% c("Spiny Dogfish"))
# spine data
spine_dat <- read_sheet('1m4smKQZMS8J7fq_oNQXpZp5_J6K5qaMiJn8W_ULUK_I') %>% clean_names()

# join specimen and spine data ----
df_age <- spec_dat %>% 
  left_join(samp_spec_join) %>% 
  left_join(spine_dat) %>% 
  filter(!is.na(reader)) %>% 
  select(-c(read_order, date_read, candle, spine_p, embryo_1, embryo_2, eye_l, eye_r, rank))

# Define if spine is worn ----
# based on McFarlane and King 2009 embryo EBD at birth = 2.45 mm

df_age <- df_age %>% 
  mutate(worn = if_else(lrp_mm < 2.45, 1, 0))

# estimate worn annuli ----
# using Tribuzio et al. 2010 equation 3 WOLS parameterization
# may change as WDFW will be final age authority

b0 <- 0.212
b1 <- 2.856
WA <- function(x) {floor(b0*(x ^ b1))} 

df_age <- df_age %>% 
  mutate(est_lost_ann = if_else(worn == 1, 0,
                                WA(lrp_mm)),
         est_age = if_else(worn == 1, annuli_ct - 2,
                           annuli_ct + est_lost_ann - 2))

# take median age of three readers ----
summ_age <- df_age %>% 
  group_by(sample_id, specimen_id, species_common_name, sex, length_cm, length_type, large_marine_ecosystem, haul_date_akt, noncon_lat, 
           noncon_long, haul_year, nmfs_area) %>% 
  summarise(med_age = median(est_age, na.rm = T))

# write to google drive ----
glink <- "https://docs.google.com/spreadsheets/d/1vSwK6RotLo52VmTlqGsSaYjwvUTYKpq989liY06V87g/edit?gid=0#gid=0"
summ_age %>% write_sheet(ss = glink, sheet = "dogfish_spine_ages")

# compare median to WDFW authority values----
Wage <- df_age %>% 
  filter(reader == "WDFW_final") %>% 
  select(sample_id, est_age)

Mage <- summ_age %>% 
  ungroup() %>% 
  select(sample_id, med_age)

WMcomp <- Wage %>% 
  left_join(Mage) %>% 
  mutate(age_diff = est_age - med_age)

ggplot(WMcomp, aes(x = est_age, y = med_age))+
  geom_point()+
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed")
