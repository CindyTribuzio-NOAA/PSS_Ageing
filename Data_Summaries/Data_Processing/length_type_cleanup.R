# Updated Dec 17, 2024 by C. Tribuzio
# database clean up tests


# To Do ----
# 

# Setup ----
libs <- c("tidyverse", "janitor", "googlesheets4", "readr")
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)
'%nin%'<-Negate('%in%') #this is a handy function

# Unknown Length type----
# try to use length:wt where weight data are available
# Bring in data from google sheets
spec_dat <- read_sheet('1pbSRX_9vj3Xe3_vqK_psvamk18oGSH3Kb-R6NeVSQkc', sheet = 'Specimen_Info') %>% clean_names() %>% 
  filter(!is.na(length_cm)) %>% #can't invent length data, ha!
  filter(length_type %nin% c("Precaudal Length", "Total Length", "Fork Length"),
         !is.na(weight_kg)) 

#bring in parameters
TLW_params <- read_csv(url("https://raw.githubusercontent.com/CindyTribuzio-NOAA/PSS_LW_regression/refs/heads/main/results/PSS_TLLW_regression_params.csv"))
PCLW_params <- read_csv(url("https://raw.githubusercontent.com/CindyTribuzio-NOAA/PSS_LW_regression/refs/heads/main/results/PSS_PCLLW_regression_params.csv"))

sd2 <- spec_dat %>% 
  select(specimen_id, length_cm, weight_kg) %>% 
  mutate(wt_PCL = PCLW_params$estimate[1] * (length_cm ^ PCLW_params$estimate[2]),
         wt_PCLLL = PCLW_params$ll[1] * (length_cm ^ PCLW_params$ll[2]),
         wt_PCLUL = PCLW_params$ul[1] * (length_cm ^ PCLW_params$ul[2]), #don't really need this because of the wt_PCL flag
         wt_TL = TLW_params$estimate[1] * (length_cm ^ TLW_params$estimate[2]),
         wt_TLLL = TLW_params$ll[1] * (length_cm ^ TLW_params$ll[2]),
         wt_TLUL = TLW_params$ul[1] * (length_cm ^ TLW_params$ul[2]),
         below_PCLLL = if_else(weight_kg < wt_PCLLL, "YES", "NO"),
         above_TLUL = if_else(weight_kg > wt_TLUL, "YES", "NO"))

#look at graph and see if any super obvious ones
ggplot(sd2)+
  geom_point(aes(x = length_cm, weight_kg))+
  geom_line(aes(x = length_cm, y = wt_TL), colour = "blue")+
  geom_line(aes(x = length_cm, y = wt_PCL), colour = "red")+
  geom_line(aes(x = length_cm, y = wt_PCLLL), color = "red", linetype="dashed")+
  geom_line(aes(x = length_cm, y = wt_TLUL), color = "blue", linetype="dashed")
  #geom_line(aes(x = length_cm, y = wt_PCLUL), color = "red", linetype="dashed")
  #geom_line(aes(x = length_cm, y = wt_TLLL), color = "blue", linetype="dashed")

# NOT USED, BUT CONSIDERED decision points
# 1) if weight falls above the PCL line, it is likely a PCL length, and if below TL line, then likely a TL
# 2) if weight falls above the PCL CI line, it is likely PCL and vice versa, at alpha = 0.5, should be at least 50% likely to be one over the other

