# CREATION DATE 3 June 2024
# MODIFIED DATE 6 July  2024

# AUTHOR: kitchel@oxy.edu & placensia@oxy.edu

# PURPOSE: Determine how frond biomass varies with depth from both North 1958 and Rassweiler 2018

#############################
##Setup
#############################
library(tidyverse)
library(lubridate)
############################
#LOAD DATA
############################
#open data subset of PVR only
dat_PV_macrocystis_post_construction <- read_csv(file = file.path("data","dat_PV_macrocystis_post_construction.csv"))

#import North et al. 1958 data
North_1958_kelp_frond_biomass <- read_csv(file.path("data","North_1958_kelp_frond_biomass.csv"))

#import Rassweiler et al. 2018 data
Rassweiler_2018_kelpdata <- read_csv(file.path("data","Rassweiler_2018_kelpdata.csv"))

############################
#PVR depths
############################

summary(dat_PV_macrocystis_post_construction$SurveyDepth_m)

#range from 15.2 m to 21.6 m (keep in mind this is dive computer data, so it's rough)

PVR_min_depth <- 15.2
PVR_max_depth <- 21.6
PVR_mean_depth <- 18.42

############################
#North 1958 predictions for this depth range
############################

summary(North_1958_kelp_frond_biomass |>
          filter(Depth_m > 15 & Depth_m < 22))

#For depths between 15.01 and 21.19, weight of fronds ranges from 0.45 to 2.77 with an average of 1.07

#what does this look like visually? (Highlighting depths that match our depths)
ggplot() +
  geom_point(data = North_1958_kelp_frond_biomass, aes(x = Depth_m, y = weight_kg)) +
  geom_point(data = North_1958_kelp_frond_biomass |> filter(Depth_m > 15 & Depth_m < 22), aes(x = Depth_m, y = weight_kg), color = "turquoise") +
  labs(x = "Depth (m)", y = "Frond biomass (kg)") +
  theme_classic()

############################
#Rassweiler 2018 predictions for this depth range
############################

Rassweiler_2018_kelpdata.r <- Rassweiler_2018_kelpdata |>
  filter(complete.cases(DEPTH) & complete.cases(WT_WC) & complete.cases(WT_C)) #only include rows with data for depth, watercolumn weight, and canopy weight

#make new column summing water column weight and canopy weight
Rassweiler_2018_kelpdata.r <- Rassweiler_2018_kelpdata.r |>
  mutate(WT_F = WT_WC + WT_C) |>
  group_by(DEPTH) |>
  mutate(mean_wt_depth = mean(WT_F, na.rm = T))

#run a linear model (predicting weight of frond by depth)
lm_mod <- lm(WT_F ~ DEPTH, data = Rassweiler_2018_kelpdata.r)

#let's look at model results in console
summary(lm_mod)

#it's a significant relationship (Pr(>|t|) or p-value is < 0.05)
#It predicts that Frond weight = 0.117 * DEPTH + 0.010

#Therefore, this model predicts that fronds at the minimum PVR depth = 1.79 kg
#minimum depth PVR * coefficient + intercept
PVR_min_depth * 0.117 + 0.010

#And then, this model predicts that fronds at the maximum PVR depth = 2.5372 kg
#minimum depth PVR * coefficient + intercept
PVR_max_depth * 0.117 + 0.010

#How does Rasweiller data overlap with North data?

#reduce Rasweiller data to depth and biomass
Rassweiler_2018_depth_biomass <- Rassweiler_2018_kelpdata.r |>
  select(DEPTH, WT_F) |>
  mutate(source = "Rassweiller") #add new column identifying data source

#add new column to North data as well
North_1958_kelp_frond_biomass <- North_1958_kelp_frond_biomass |>
  mutate(source = "North")  #add new column identifying data source

#do column names and order match?
colnames(Rassweiler_2018_depth_biomass)
colnames(North_1958_kelp_frond_biomass)

#order yes, but name no, so I will change names of North to match

colnames(North_1958_kelp_frond_biomass) <- colnames(Rassweiler_2018_depth_biomass)

#combine datasets
kelp_frond_biomass_merge <- bind_rows(Rassweiler_2018_depth_biomass, North_1958_kelp_frond_biomass)

kelp_frond_merge_scatterplot <- ggplot() +
  geom_rect(aes(xmin = PVR_min_depth, xmax = PVR_max_depth, ymin = -Inf, ymax = Inf), fill = "lightyellow") + #add depth range of PVR in background
  geom_point(data = kelp_frond_biomass_merge, aes(x = DEPTH, y = WT_F, color = source), size = 2) +
  scale_color_manual(values = c("plum","skyblue")) +
  labs(x = "Depth (m)", y = "Frond biomass (kg)", color = "Data source") +
  geom_smooth(data = kelp_frond_biomass_merge, aes(x = DEPTH, y = WT_F), method = "lm") +
  theme_classic()

#this figure may be helpful for the poster, save it to figures folder
ggsave(kelp_frond_merge_scatterplot, path = file.path("figures"), filename = "kelp_frond_merge_scatterplot.jpg", height = 10, width = 10, units = "in")

#I recommend playing around with this figure to add dates of papers to legend, and make points and figure labels larger
#you can also make other adjustments to your liking!
#It could even be cool to include the linear model you build below on the plot as well as text
#You'd want to share model structure (y = mx+b with actual values for m and b, the R^2 value, and the p value)

#let's build another linear model (predicting weight of frond by depth)
lm_mod_full <- lm(WT_F ~ DEPTH, data = kelp_frond_biomass_merge)

#let's look at model results in console
summary(lm_mod_full)

#it's a significant relationship (Pr(>|t|) or p-value is < 0.05)
#It predicts that Frond weight = 0.072 * DEPTH + 0.320

#Therefore, this model predicts that fronds at the minimum PVR depth = 1.4144 kg
#minimum depth PVR * coefficient + intercept
PVR_min_depth * 0.072 + 0.320

#And then, this model predicts that fronds at the maximum PVR depth = 1.8752 kg
#minimum depth PVR * coefficient + intercept
PVR_max_depth * 0.072 + 0.320

#Let's move forward with the merged datasets for analyses
# We will use PVR mean depth from model for mean frond biomass, but also consider minimum and maximum values as well
PVR_mean_depth * 0.072 + 0.320

#--> 1.64 kg per frond on average for depths between 15.2 m to 21.6 (using both Rassweiller and North as input to a simple linear model)

#save these outputs in a data frame for future scripts
PVR_frond_biomass_model_output <- data.frame(weight_component = c("min","mean","max"), weight_kg = c(PVR_min_depth * 0.072 + 0.320,
                                                                                                     PVR_mean_depth * 0.072 + 0.320,
                                                                                                     PVR_max_depth * 0.072 + 0.320))

write_csv(PVR_frond_biomass_model_output, file = file.path("output","PVR_frond_biomass_model_output.csv"))



