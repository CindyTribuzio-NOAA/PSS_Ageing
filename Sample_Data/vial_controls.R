# Updated Dec 26, 2024 by C. Tribuzio
# code to evaluate the control data from repeated weights
# goal is to determine best practices for weighing dry samples

# Setup ----
libs <- c("tidyverse", "janitor", "googlesheets4")
#, "Hmisc", "RColorBrewer", "gridExtra", "gtable", 
#          "grid", "flextable", "officer", "lubridate", "RODBC", "DBI", "gtable", "patchwork")
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)
'%nin%'<-Negate('%in%') #this is a handy function

# Data----
vial_dat <- read_sheet('1N9O_peedwqVYu0WmLAAh01UJbt8LT-vAdvkBibnJHbM', sheet = "controls") %>% 
  clean_names() %>% 
  select(!time) %>% 
  filter(day !=0) %>% 
  pivot_longer(!c(interval, day), names_to = "vial", values_to = "weight")

# Graphing----

ggplot(vial_dat, aes(x = interval, y = weight, color = vial, shape = vial))+
  geom_point(show.legend = F)+
  geom_line(show.legend = F)+
  facet_grid(vial~day)

# same thing with eye layer data----
layer_dat <- read_sheet('1N9O_peedwqVYu0WmLAAh01UJbt8LT-vAdvkBibnJHbM', sheet = "layers") %>% 
  clean_names() %>% 
  filter(day !=0) %>% 
  select(!date) %>% 
  pivot_longer(!c(interval, day), names_to = "vial", values_to = "weight")

# Graphing----

ggplot(layer_dat, aes(x = interval, y = weight, color = vial, shape = vial))+
  geom_point(show.legend = F)+
  geom_line(show.legend = F)+
  facet_grid(vial~day, scales = "free")
