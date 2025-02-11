# Updated 6 Jan 2025 by C. Tribuzio


# To Do ----
# add in layer order

# Setup ----
libs <- c("tidyverse", "janitor", "googlesheets4")
#, "Hmisc", "RColorBrewer", "gridExtra", "gtable", 
#          "grid", "flextable", "officer", "lubridate", "RODBC", "DBI", "gtable", "patchwork")
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)
'%nin%'<-Negate('%in%') #this is a handy function
round_any = function(x, accuracy, f=round){f(x/ accuracy) * accuracy}

# bring in data----
layer_dat <- read_sheet('1xeHWScrJwWkeN_YV-C6euG_7G4w3u7nHjjew34BoSP0') %>% clean_names()

# plot nucleus results----
nuc_dat <- layer_dat %>% 
  filter(layer_type == "nucleus")

ggplot(nuc_dat, aes(x = tl_cm, y = delta14c))+
  geom_point()

# plot AMS by layer for each animal----
ggplot(layer_dat, aes(x = layer_order, y = delta14c))+
  geom_point()+
  facet_grid(sample_id~., scales = "free")

s420_dat <- layer_dat %>% 
  filter(sample_id == 420)
ggplot(s420_dat, aes(x = layer_order, y = delta14c))+
  geom_point()+
  #geom_smooth()+
  facet_grid(sample_id~., scales = "free")

# paired eyes----
# sample data (e.g., eyes, embryos)
samp_dat <- read_sheet('1pbSRX_9vj3Xe3_vqK_psvamk18oGSH3Kb-R6NeVSQkc') %>% clean_names() %>% 
  filter(sample_type %in% c('Embryo_1', 'Eye', 'Embryo_2', 'Eye_L', 'Eye_R', 'Eye_B', 'Eye_A', 'Candle', 'Spine_P')) %>% 
  select(-notes) %>% 
  mutate(sample_type = if_else(sample_type == "Eye" | sample_type == "Eye_A", "Eye_L", 
                               if_else(sample_type == "Eye_B", "Eye_R", sample_type)))

pair_list <- layer_dat %>% 
  left_join(samp_dat) %>% 
  group_by(specimen_id, sample_type) %>% 
  summarise(neyes = length(ams_id)) %>% 
  pivot_wider(names_from = sample_type, values_from = neyes) %>% 
  filter(!is.na(Eye_L),
         !is.na(Eye_R))

layer_list <- layer_dat %>% 
  left_join(samp_dat) %>% 
  filter(specimen_id %in% pair_list$specimen_id) %>% 
  select(sample_id, ams_id, layer_order, specimen_id, delta14c, sample_type) %>% 
  group_by(specimen_id, sample_id) %>% 
  summarise(nucleus = min(layer_order), capsule = max(layer_order)) %>% 
  pivot_longer(!c(specimen_id, sample_id), names_to = "layer_type", values_to = "layer_order")

pair_dat <- layer_dat %>% 
  left_join(samp_dat) %>% 
  filter(specimen_id %in% layer_list$specimen_id,
         layer_type %in% c("nucleus", "Capsule")) %>% 
  select(specimen_id, sample_type, layer_type, delta14c) %>% 
  pivot_wider(names_from = sample_type, values_from = delta14c) %>% 
  filter(!is.na(Eye_L))

ggplot(pair_dat, aes(x = Eye_L, y = Eye_R))+
  geom_point()

t.test(formula = score ~ time,
       alternative = "greater",
       mu = 0, 
       paired = TRUE,   
       var.equal = TRUE,
       conf.level = 0.95)
 
#   Paired t-test
# 
t.test(pair_dat$Eye_L, pair_dat$Eye_R,
       alternative = "two.sided",
       mu = 0, paired = TRUE,
       conf.level = 0.95)

#data:  pair_dat$Eye_L and pair_dat$Eye_R
#t = 1.073, df = 10, p-value = 0.3085
#alternative hypothesis: true mean difference is not equal to 0
#95 percent confidence interval:
#  -6.781162 19.378576
#sample estimates:
#  mean difference 
#6.298707 

# nucleus only
nuc_dat2 <- pair_dat %>% 
  filter(layer_type == "nucleus")

t.test(nuc_dat2$Eye_L, nuc_dat2$Eye_R,
       alternative = "two.sided",
       mu = 0, paired = TRUE,
       conf.level = 0.95)
