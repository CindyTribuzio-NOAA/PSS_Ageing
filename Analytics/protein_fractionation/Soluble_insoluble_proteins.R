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
layer_dat <- read_sheet('1xeHWScrJwWkeN_YV-C6euG_7G4w3u7nHjjew34BoSP0', sheet = '14C_layer_results') %>% clean_names()

IS_dat <- layer_dat %>% 
  filter(str_detect(ams_id, 'I|S')) %>% 
  mutate(layer_no = str_remove(ams_id, 'I|S.*')) %>% 
  filter(layer_no != 'P')

nlayers <- IS_dat %>% 
  group_by(layer_no) %>% 
  summarise(nlays = length(ams_id)) %>% 
  filter(nlays > 1) %>% 
  select(layer_no)

#compare soluble and insolube for f_modern
IS_dat2 <- IS_dat %>% 
  filter(layer_no %in% as.vector(nlayers$layer_no)) %>% 
  mutate(ptype = if_else(str_detect(ams_id, 'I'), 'Insoluble', 'Soluble'),
         sample_id = str_remove(layer_no, '_.*')) %>% 
  select(species_common_name, layer_no, ptype, f_modern, layer_type, sample_id, length_cm) %>% 
  pivot_wider(names_from = ptype, values_from = f_modern)

ggplot(IS_dat2, aes(x = Insoluble, y = Soluble, color = layer_type, shape = species_common_name))+
  geom_point(size = 4)+
  geom_abline(intercept = 0, slope = 1)+
  geom_smooth(aes(group = 1), method = "lm", se = TRUE, color = "red", linetype = "dashed", size = 1)+
  theme_bw()

ggplot(IS_dat2, aes(x = Insoluble, y = Soluble, color = layer_type, shape = sample_id))+
  geom_point(size = 4)+
  geom_abline(intercept = 0, slope = 1)+
  geom_smooth(aes(group = 1), method = "lm", se = TRUE, color = "red", linetype = "dashed", size = 1)+
  theme_bw()

# paired t-test
t.test(IS_dat2$Insoluble, IS_dat2$Soluble, paired = TRUE, alternative = "two.sided", conf.level = 0.95)

#data:  IS_dat2$Insoluble and IS_dat2$Soluble
#t = 1.0262, df = 11, p-value = 0.3269
#alternative hypothesis: true mean difference is not equal to 0
#95 percent confidence interval:
#  -0.006783408  0.018633408
#sample estimates:
#  mean difference 
#0.005925 

# slope of the line
model <- lm(Soluble ~ Insoluble, data = IS_dat2)
summary(model)
confint(model)

#Call:
#  lm(formula = Soluble ~ Insoluble, data = IS_dat2)

#Residuals:
#  Min        1Q    Median        3Q       Max 
#-0.025547 -0.003210  0.001010  0.006293  0.023836 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   0.3499     0.1082   3.234 0.008967 ** 
#  Insoluble     0.6413     0.1090   5.884 0.000154 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.01454 on 10 degrees of freedom
#Multiple R-squared:  0.7759,	Adjusted R-squared:  0.7535 
#F-statistic: 34.62 on 1 and 10 DF,  p-value: 0.0001543

#> confint(model)
#2.5 %    97.5 %
#  (Intercept) 0.1087855 0.5909673
#Insoluble   0.3984718 0.8841807
