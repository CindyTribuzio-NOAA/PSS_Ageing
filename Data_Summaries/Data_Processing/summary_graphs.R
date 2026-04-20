# Compare the 14C levels in soluble and insoluble proteins of the eye layers
# developed by C Tribuzio, AFSC
# Nov 6, 2025, cindy.tribuzio@noaa.gov

#setup----
libs <- c("tidyverse", "janitor", "googlesheets4", 'DiagrammeR', 'patchwork', 'gtable', 'grid', 'gridExtra')
#, "Hmisc", "RColorBrewer", "gridExtra", "gtable", 
#          "grid", "flextable", "officer", "lubridate", "RODBC", "DBI", "gtable", "patchwork")
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)
'%nin%'<-Negate('%in%') #this is a handy function
round_any = function(x, accuracy, f=floor){f(x/ accuracy) * accuracy} #note that this is specific to rounding down

roundUpNice <- function(x, nice=c(1,2,4,5,6,8,10)) { #rounds up to handy bin
  if(length(x) != 1) stop("'x' must be of length 1")
  10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
}

# bring in data----
layer_dat <- read_sheet('1xeHWScrJwWkeN_YV-C6euG_7G4w3u7nHjjew34BoSP0', sheet = 'combined_layer_results') %>% clean_names()

#simplify layer_type
layer_dat <- layer_dat %>% 
  mutate(layer_type = if_else(new_layer_type == 'candle yolk', 'candle yolk',
                              if_else(new_layer_type == 'capsule', 'capsule',
                                      if_else(new_layer_type == 'preapoptotic_2'|new_layer_type == 'preapoptotic', 'preapoptotic', 'postapoptotic'))))

# get min/max x-axis values
xvals <- layer_dat %>% 
  group_by(species_common_name) %>% 
  summarize(minx = min(layer_diam_mm, na.rm = T), maxx = max(layer_diam_mm, na.rm = T))

# make summary graphics for each animal----
spec_list <- unique(layer_dat$specimen_id)

# using the layer diameter, all graphs on the same scale
for (i in 1:length(spec_list)){
  loop_dat <- layer_dat %>% 
    filter(specimen_id == spec_list[i])
  loop_title <- paste('specimen_id =', loop_dat$specimen_id[1], loop_dat$species_common_name[1], round(loop_dat$length_cm[1], 0),
                      loop_dat$length_type[1], loop_dat$sex[1])
  #14C graph
  fig_14C <- ggplot(loop_dat, aes(x = layer_diam_mm, y = f_modern, shape = protein_type, color = layer_type,
                                  size = protein_type))+
    geom_point()+
    labs(title = loop_title, x = "Layer Diameter (mm)", y = '14C F Modern')+
    scale_size_manual(values = c('whole layer' = 2, "Soluble" = 5, "Insoluble" = 5))+
    scale_x_continuous(limits = c(0, 20))+
    scale_color_viridis_d(option = "mako", end = 0.85)+
    facet_grid(sample_id~.)+
    theme_bw()+
    theme(strip.text = element_blank())
  
  #13C graph
  fig_13C <- ggplot(loop_dat, aes(x = layer_diam_mm, y = d13c, shape = protein_type, color = layer_type,
                                  size = protein_type))+
    geom_point()+
    labs(x = "Layer Diameter (mm)", y = 'Delta 13C')+
    scale_color_viridis_d(option = "mako", end = 0.85)+
    scale_size_manual(values = c('whole layer' = 2, "Soluble" = 5, "Insoluble" = 5))+
    scale_x_continuous(limits = c(0, 20))+
    facet_grid(sample_id~.)+
    theme_bw()+
    theme(strip.text = element_blank())
  
  #15N graph
  fig_15N <- ggplot(loop_dat, aes(x = layer_diam_mm, y = d15n, 
                                  shape = protein_type, color = layer_type, 
                                  size = protein_type))+
    geom_point()+
    labs(x = "Layer Diameter (mm)", y = 'Delta 15N')+
    scale_color_viridis_d(option = "mako", end = 0.85)+
    scale_size_manual(values = c('whole layer' = 2, "Soluble" = 5, "Insoluble" = 5))+
    scale_x_continuous(limits = c(0, 20))+
    facet_grid(sample_desc + sample_id~., labeller = labeller(sample_id = function(x) paste("sample_id", x)))+
    theme_bw()
  
  #CSIAA graph
  
  #combined graph
  loop_fig <- fig_14C + fig_13C + fig_15N + plot_layout(axes = "collect",
                                                     guides = "collect")
  
  ggsave(plot = loop_fig, paste0(getwd(), '/Data_Summaries/Summary/summary_figs/x_fixed_diam/specimen_id', spec_list[i], '.png'))
  
  
}

#using the layer diameter, dynamic x-axis
for (i in 1:length(spec_list)){
  loop_dat <- layer_dat %>% 
    filter(specimen_id == spec_list[i])
  loop_title <- paste('specimen_id =', loop_dat$specimen_id[1], loop_dat$species_common_name[1], round(loop_dat$length_cm[1], 0),
                      loop_dat$length_type[1], loop_dat$sex[1])
  #14C graph
  fig_14C <- ggplot(loop_dat, aes(x = layer_diam_mm, y = f_modern, shape = protein_type, color = layer_type,
                                  size = protein_type))+
    geom_point()+
    labs(title = loop_title, x = "Layer Diameter (mm)", y = '14C F Modern')+
    scale_size_manual(values = c('whole layer' = 2, "Soluble" = 5, "Insoluble" = 5))+
    scale_x_continuous(limits = c(0, 20))+    scale_color_viridis_d(option = "mako", end = 0.85)+
    facet_grid(sample_id~.)+
    theme_bw()+
    theme(strip.text = element_blank())
  
  #13C graph
  fig_13C <- ggplot(loop_dat, aes(x = layer_diam_mm, y = d13c, shape = protein_type, color = layer_type,
                                  size = protein_type))+
    geom_point()+
    labs(x = "Layer Diameter (mm)", y = 'Delta 13C')+
    scale_color_viridis_d(option = "mako", end = 0.85)+
    scale_size_manual(values = c('whole layer' = 2, "Soluble" = 5, "Insoluble" = 5))+
    facet_grid(sample_id~.)+
    theme_bw()+
    theme(strip.text = element_blank())
  
  #15N graph
  fig_15N <- ggplot(loop_dat, aes(x = layer_diam_mm, y = d15n, 
                                  shape = protein_type, color = layer_type, 
                                  size = protein_type))+
    geom_point()+
    labs(x = "Layer Diameter (mm)", y = 'Delta 15N')+
    scale_color_viridis_d(option = "mako", end = 0.85)+
    scale_size_manual(values = c('whole layer' = 2, "Soluble" = 5, "Insoluble" = 5))+
    facet_grid(sample_desc+sample_id~., labeller = labeller(sample_id = function(x) paste("sample_id", x)))+
    theme_bw()
  
  #CSIAA graph
  
  #combined graph
  loop_fig <- fig_14C + fig_13C + fig_15N + plot_layout(axes = "collect",
                                                        guides = "collect")
  
  ggsave(plot = loop_fig, paste0(getwd(), '/Data_Summaries/Summary/summary_figs/x_variable_diam/specimen_id', spec_list[i], '.png'))
  
  
}

#using layer order
for (i in 1:length(spec_list)){
  loop_dat <- layer_dat %>% 
    filter(specimen_id == spec_list[i])
  loop_title <- paste('specimen_id =', loop_dat$specimen_id[1], loop_dat$species_common_name[1], round(loop_dat$length_cm[1], 0),
                      loop_dat$length_type[1], loop_dat$sex[1])
  #14C graph
  fig_14C <- ggplot(loop_dat, aes(x = layer_order, y = f_modern, shape = protein_type, color = layer_type,
                                  size = protein_type))+
    geom_point()+
    labs(title = loop_title, x = "Layer Order", y = '14C F Modern')+
    scale_size_manual(values = c('whole layer' = 2, "Soluble" = 5, "Insoluble" = 5))+
    scale_x_continuous(limits = c(0, 20))+
    scale_color_viridis_d(option = "mako", end = 0.85)+
    facet_grid(sample_id~.)+
    theme_bw()+
    theme(strip.text = element_blank())
  
  #13C graph
  fig_13C <- ggplot(loop_dat, aes(x = layer_order, y = d13c, shape = protein_type, color = layer_type,
                                  size = protein_type))+
    geom_point()+
    labs(x = "Layer Order", y = 'Delta 13C')+
    scale_color_viridis_d(option = "mako", end = 0.85)+
    scale_size_manual(values = c('whole layer' = 2, "Soluble" = 5, "Insoluble" = 5))+
    scale_x_continuous(limits = c(0, 20))+
    facet_grid(sample_id~.)+
    theme_bw()+
    theme(strip.text = element_blank())
  
  #15N graph
  fig_15N <- ggplot(loop_dat, aes(x = layer_order, y = d15n, 
                                  shape = protein_type, color = layer_type, 
                                  size = protein_type))+
    geom_point()+
    labs(x = "Layer Order", y = 'Delta 15N')+
    scale_color_viridis_d(option = "mako", end = 0.85)+
    scale_size_manual(values = c('whole layer' = 2, "Soluble" = 5, "Insoluble" = 5))+
    scale_x_continuous(limits = c(0, 20))+
    facet_grid(sample_desc+sample_id~., labeller = labeller(sample_id = function(x) paste("sample_id", x)))+
    theme_bw()
  
  #CSIAA graph
  
  #combined graph
  loop_fig <- fig_14C + fig_13C + fig_15N + plot_layout(axes = "collect",
                                                        guides = "collect")
  
  ggsave(plot = loop_fig, paste0(getwd(), '/Data_Summaries/Summary/summary_figs/x_layer_order/specimen_id', spec_list[i], '.png'))
  
  
}

# Protein separation only ----
psdat <- layer_dat %>% 
  filter(protein_type != "whole layer")

ggplot(psdat, aes(x = layer_order, y = f_modern, color = as.factor(sample_id), shape = protein_type))+
  geom_point()+
  geom_line()

psdat_fmod <- layer_dat %>% 
  filter(protein_type != "whole layer") %>% 
  select(sample_id, specimen_id, protein_type, f_modern, layer_order, species_common_name) %>% 
  pivot_wider(names_from = protein_type, values_from = f_modern) %>% 
  mutate(f_modern_ratio = Soluble/Insoluble)

ggplot(psdat_fmod, aes(x = layer_order, y = f_modern_ratio, color = as.factor(sample_id), shape = species_common_name))+
  geom_point()+
  geom_line()+
  facet_grid(species_common_name~.)

#combined 14C----

all14C <- ggplot(layer_dat, aes(x = layer_diam_mm, y = f_modern, color = as.factor(sample_id)))+
  geom_line(show.legend = F)+
  scale_color_viridis_d(option = "viridis", end = 0.85)+
  facet_grid(species_common_name~.)+
  theme_bw()
ggsave(plot = all14C, paste0(getwd(), '/Data_Summaries/Summary/summary_figs/combined14C.png'))

all13C <- ggplot(layer_dat, aes(x = layer_diam_mm, y = d13c, color = as.factor(sample_id)))+
  geom_line(show.legend = F)+
  scale_color_viridis_d(option = "viridis", end = 0.85)+
  facet_grid(species_common_name~.)+
  theme_bw()
ggsave(plot = all13C, paste0(getwd(), '/Data_Summaries/Summary/summary_figs/combined13C.png'))

all15N <- ggplot(layer_dat, aes(x = layer_diam_mm, y = d15n, color = as.factor(sample_id)))+
  geom_line(show.legend = F)+
  scale_color_viridis_d(option = "viridis", end = 0.85)+
  facet_grid(species_common_name~.)+
  theme_bw()
ggsave(plot = all15N, paste0(getwd(), '/Data_Summaries/Summary/summary_figs/combined15N.png'))

csi_dat <- layer_dat %>% 
  filter(!is.na(glu),
         !is.na(layer_diam_mm)) %>% 
  select(sample_id, species_common_name, layer_diam_mm, ala, gly, thr, ser, val, leu, pro, asp, glu, phe, tyr, lys) %>% 
  pivot_longer(!c(sample_id, species_common_name, layer_diam_mm), names_to = 'Amino', values_to = 'value')

allCSIAA <- ggplot(csi_dat, aes(x = layer_diam_mm, y = value, color = as.factor(sample_id)))+
  geom_line(show.legend = F)+
  scale_color_viridis_d(option = "viridis", end = 0.85)+
  facet_grid(species_common_name~Amino, scales = "free")+
  theme_bw()
ggsave(plot = allCSIAA, paste0(getwd(), '/Data_Summaries/Summary/summary_figs/combinedCSIAA.png'))

PSS14cdat <- layer_dat %>% 
  filter(species_common_name == "Pacific Sleeper Shark")
ggplot(PSS14cdat, aes(x = layer_diam_mm, y = f_modern, color = as.factor(sample_id)))+
  geom_line(show.legend = F)+
  scale_color_viridis_d(option = "viridis", end = 0.85)+
  facet_grid(large_marine_ecosystem~.)+
  theme_bw()

