# Updated 26 Jan 2026 by C. Tribuzio


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

# NOT USED: eye diameter relative to size----
maxlayer <- layer_dat %>% 
  filter(length_cm >= 0) %>% 
  group_by(sample_id) %>% 
  summarise(maxl = max(layer_order))

maxdiam <- layer_dat %>% 
  filter(length_cm >= 0) %>% 
  left_join(maxlayer) %>% 
  filter(layer_order == maxl,
         layer_diam_mm >= 0) %>% 
  select(sample_id, length_cm, length_type, layer_order, layer_diam_mm, maxl)

diam_reg <- lm(layer_diam_mm ~ length_cm, data = maxdiam)
summary(diam_reg)
diam_reg$coefficients

ggplot(maxdiam, aes(x = length_cm, y = layer_diam_mm, color = length_type))+
  geom_point(size = 4)+
  geom_abline(slope = diam_reg$coefficients[2], intercept = diam_reg$coefficients[1])+
  labs(x = 'Length (cm)', y = 'Lens Diameter (mm)')+
  theme_bw()+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# NOT USED: plot nucleus results----
nuc_dat <- layer_dat %>% 
  filter(new_layer_type == "nucleus",
         !is.na(length_type))

ggplot(nuc_dat, aes(x = length_cm, y = delta14c, color = length_type))+
  geom_point()+
  theme_bw()

# plot AMS by layer for each animal----
ggplot(layer_dat, aes(x = layer_diam_mm, y = delta14c, color = as.factor(sample_id)))+
  geom_line(show.legend = F)+
  scale_color_viridis_d(option = "viridis", end = 0.85)+
  facet_grid(species_common_name~.)+
  theme_bw()

# plot AMS by layer for sample_id 420----
s420_dat <- layer_dat %>% 
  filter(sample_id == 420)
ggplot(s420_dat, aes(x = layer_diam_mm, y = delta14c))+
  geom_point(size = 4)+
  #geom_smooth()+
  #facet_grid(sample_id~., scales = "free")+
  labs(x = 'Layer Diameter (mm)', y = 'D14C')+
  theme_bw()

# sample_id 420 with reference curve----
refcurve <- read_csv(here::here(getwd(), 'Presentations', 'ABL_2025_FFS', '14C_reference_data.csv')) %>% 
  clean_names() %>% 
  filter(species != "PSS_420")
PSS_curve <- read_csv(here::here(getwd(), 'Presentations', 'ABL_2025_FFS', '14C_reference_data.csv')) %>% 
  clean_names() %>% 
  filter(species == "PSS_420")
ggplot(refcurve, aes(x = year_class, y = d14c, color = species))+
  geom_line()+
  geom_point(data = PSS_curve, aes(x = year_class, y = d14c), size = 4)+
  #scale_color_manual(values = cb3)+
  scale_color_viridis_d(option = "viridis", end = 0.85)+
  labs(x = 'Formation Year', y = 'D14C', color = "Reference")+
  theme_bw()


# NOT USED: paired eyes----
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
  select(sample_id, ams_id, layer_order, specimen_id, delta14c, sample_type, methods) %>% 
  group_by(specimen_id, sample_id, methods) %>% 
  summarise(nucleus = min(layer_order), capsule = max(layer_order)) %>% 
  pivot_longer(!c(specimen_id, sample_id, methods), names_to = "layer_type", values_to = "layer_order")

pair_dat <- layer_dat %>% 
  left_join(samp_dat) %>% 
  filter(specimen_id %in% layer_list$specimen_id,
         layer_type %in% c("nucleus", "Capsule")) %>% 
  select(specimen_id, sample_type, layer_type, delta14c) %>% 
  pivot_wider(names_from = sample_type, values_from = delta14c) %>% 
  filter(!is.na(Eye_L)) %>% 
  mutate(C14_diff = Eye_L - Eye_R)

methods_dat <- layer_dat %>% 
  left_join(samp_dat) %>% 
  filter(specimen_id %in% layer_list$specimen_id,
         layer_type %in% c("nucleus", "Capsule")) %>% 
  select(specimen_id, sample_type, layer_type, methods) %>% 
  pivot_wider(names_from = sample_type, values_from = methods) %>% 
  filter(!is.na(Eye_L)) %>% 
  mutate(method = if_else(Eye_L == Eye_R, "Same", "Diff")) %>% 
  select(specimen_id, layer_type, method) %>% 
  left_join(pair_dat)

ggplot(methods_dat, aes(x = Eye_L, y = Eye_R, color = method))+
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

# 14C and SIA together----
PSS_dyr <- PSS_curve %>% 
  select(!c(species, layer_diam_mm, d14c)) %>% 
  ungroup() %>% 
  mutate(row = row_number())
s420_dat <- layer_dat %>% 
  filter(sample_id == 420) %>% 
  select(layer_diam_mm, d14c, d15n, d13c) %>% 
  arrange(layer_diam_mm) %>% 
  mutate(row = row_number()) %>% 
  left_join(PSS_dyr, by = "row") %>% 
  select(!c(layer_diam_mm, row)) %>% 
  pivot_longer(!year_class, names_to = 'metric', values_to = 'value')


ggplot(s420_dat, aes(x = year_class, y = value, color = metric))+
  geom_point(show.legend = F , size = 4)+
  #geom_smooth()+
  facet_grid(metric~., scales = "free")+
  labs(y = "", x = "Formation Year")+
  scale_color_viridis_d(option = "viridis", end = 0.85)+
  theme_bw()+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# Overall figs----
# summary
layer_dat %>% 
  group_by(specimen_id, species_common_name) %>% 
  summarise(n_layers = length(std_lyr_id)) %>% 
  ungroup() %>% 
  group_by(species_common_name) %>% 
  summarise(n_sharks= length(n_layers), tot_layers = sum(n_layers))

# 14C (already above)
# SIA
N15_fig_dat <- SIA_dat %>% 
  select(sample_id, species_common_name, layer_diam_mm, d15n) %>% 
  filter(!is.na(layer_diam_mm)) 

ggplot(N15_fig_dat, aes(x = layer_diam_mm, y = d15n, color = as.factor(sample_id)))+
  geom_line(show.legend = F)+
  scale_color_viridis_d(option = "viridis", end = 0.85)+
  facet_grid(species_common_name~., scales = "free")+
  theme_bw()

C13_fig_dat <- SIA_dat %>% 
  select(sample_id, species_common_name, layer_diam_mm, d13c) %>% 
  filter(!is.na(layer_diam_mm)) 

ggplot(C13_fig_dat, aes(x = layer_diam_mm, y = d13c, color = as.factor(sample_id)))+
  geom_line(show.legend = F)+
  scale_color_viridis_d(option = "viridis", end = 0.85)+
  facet_grid(species_common_name~., scales = "free")+
  theme_bw()

# proteins
pro_dat <- layer_dat %>% 
  filter(protein_type != "whole layer") %>%
  select(sample_id, species_common_name, std_lyr_id, delta14c, protein_type) %>% 
  mutate(std_lyr_id = str_remove_all(std_lyr_id, "I"),
         std_lyr_id = str_remove_all(std_lyr_id, "S")) %>% 
  pivot_wider(names_from = protein_type, values_from = delta14c )

ggplot(pro_dat, aes(x = Insoluble, y = Soluble, color = as.factor(sample_id)))+
  geom_line(show.legend = F)+
  scale_color_viridis_d(option = "viridis", end = 0.85)+
  facet_grid(species_common_name~., scales = "free")+
  theme_bw()
  
# csiaa-aa
csi_dat <- layer_dat %>% 
  filter(!is.na(glu),
         !is.na(layer_diam_mm)) %>% 
  select(sample_id, species_common_name, layer_diam_mm, ala, gly, thr, ser, val, leu, pro, asp, glu, phe, tyr, lys) %>% 
  pivot_longer(!c(sample_id, species_common_name, layer_diam_mm), names_to = 'Amino', values_to = 'value')

ggplot(csi_dat, aes(x = layer_diam_mm, y = value, color = as.factor(sample_id)))+
  geom_line(show.legend = F)+
  scale_color_viridis_d(option = "viridis", end = 0.85)+
  facet_grid(species_common_name~Amino, scales = "free")+
  theme_bw()

# maternal/offspring?
mom_dat <- layer_dat %>% 
  filter(sample_desc != 'PSS eye',
         protein_type != 'Soluble') %>% 
  select(sample_id, specimen_id, sample_desc, new_layer_type, delta14c, d13c, d15n) %>% 
  mutate(relationship = if_else(sample_desc == 'SD eye', "Parent", "Offspring"),
         keep = if_else(sample_desc == 'SD eye' & new_layer_type == 'sequence layer', 'N',
                        if_else(sample_desc == 'SD eye' & new_layer_type == 'nucleus', 'N', 
                                if_else(sample_desc == 'SD eye' & new_layer_type == 'capsule', 'N', 'Y')))) %>% 
  filter(keep == 'Y') %>% 
  select(specimen_id, relationship, delta14c, d13c, d15n) %>% 
  pivot_longer(!c(specimen_id, relationship), names_to = 'Isotope', values_to = 'value') %>% 
  filter(!is.na(value),
         specimen_id != 18) %>% # not sure what's up with this sample, but we don't have an offspring anyway
  pivot_wider(names_from = relationship, values_from = value)

ggplot(mom_dat, aes(x = Parent, y = Offspring, color = as.factor(specimen_id)))+
  geom_point(show.legend = F)+
  scale_color_viridis_d(option = "viridis", end = 0.85)+
  facet_grid(.~Isotope, scales = "free")+
  theme_bw()
