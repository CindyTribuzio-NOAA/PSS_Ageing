# Updated 7 Jan 2025 by C. Tribuzio
# evaluation of dry layer weight compared to diameter


# To Do ----
# 

# Setup ----
libs <- c("tidyverse", "janitor", "googlesheets4", "readr")
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)
'%nin%'<-Negate('%in%') #this is a handy function

# bring in data----
layer_dat <- read_sheet('1xeHWScrJwWkeN_YV-C6euG_7G4w3u7nHjjew34BoSP0') %>% clean_names() %>% 
  filter(layer_diam_mm > 0)

# plot layer weights----
plot_dat <- layer_dat %>% 
  filter(layer_type %in% c("nucleus", "sequence layer"))
ggplot(plot_dat, aes(x = layer_diam_mm, y = layer_wt_mg))+
  geom_point()+
  geom_hline(yintercept = 2)


# plot AMS results----
pd <- layer_dat %>% 
  filter(delta14c > 0)
ggplot(plot_dat, aes(x = layer_diam_mm, y = delta14c))+
  geom_point()
