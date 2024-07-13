# CREATION DATE 3 June 2024
# MODIFIED DATE 6 July  2024
# MODIFIED by Katie P. July 10 2024

# AUTHOR: kitchel@oxy.edu & placensia@oxy.edu

# PURPOSE: Determine how frond biomass varies with depth from both North 1958 and Rassweiler 2018
#K.P. Create a graph that shows how giant kelp biomass has increased each year on PVR from 2020 - 2023. Determine how to calculate biomass values on each module from stipe density values (because stipe density is a proxy for frond density).
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
#View data subset
view(dat_PV_macrocystis_post_construction)


#North et al. 1958 conducted a study on M. pyrifera to determine how much giant kelp was present in the Papalote Bed during different months of the year. Due to weathering kelp beds were not present in October and December of 1957, but thick kelp beds were observed from a campsite on shore in May 1957. Then again in May of 1958 aerial surveys revealed that the bed was once again existent. Between May and July 1957 juvenile plants appeared and better growing conditions allowed for the formation of a canopy/
# The North team collected data on giant kelp juveniles at depths from 20 to 25 m, as well as from depths from 4 to 25 m. North 1958 estimated the average weight of a stipes plus attached pneumatocysts and blades for depths of around 18 m; around the depth at which the VRG completed ARM surveys. The data set by North provides the mass of all weighed kelp fronds from this experiment.
#This data set will provide us with the depth and mass of each sampled and collected frond from the North 1958 experiment from June, July, and August of 1958.

#import North et al. 1958 data
North_1958_kelp_frond_biomass <- read_csv(file.path("data","North_1958_kelp_frond_biomass.csv"))
# Lets look at the data set
view(North_1958_kelp_frond_biomass)
# Looks great!

# Rassweiler completed more than a decade long study on determining the relationship between NPP and biomass, and through their dive surveys they were able to create a data set that estimates the seasonal biomass, growth and NPP for the five years covered by the Reed and Rassweiler publication (2002-2006) and with a decade long of additional data (2007 - 2017).
#import Rassweiler et al. 2018 data
Rassweiler_2018_kelpdata <- read_csv(file.path("data","Rassweiler_2018_kelpdata.csv"))

view(Rassweiler_2018_kelpdata)
############################
#PVR depths
############################

summary(dat_PV_macrocystis_post_construction$SurveyDepth_m)

#By looking at the summary of SurveyDepths in m of the dive surveys conducted at PVR we can determine whether the biomass estimates postulated by North and Rassweiler are applicable to our data set. The best way to project the biomass values from Rassweiler and Reed is to make sure that the depths at which they completed their surveys is similar to the depth at which we collected PVR ARM data as well.
# We see that the minimum depth of dive surveys completed by the VRG at PVR was 15.2 m, and the maximum depth was 21.6 m *Zoë note:(keep in mind this is dive computer data, so it's rough)

PVR_min_depth <- 15.2
PVR_max_depth <- 21.6
PVR_mean_depth <- 18.42

############################
#North 1958 predictions for this depth range
############################
# To determine the biomass at the 15.2 - 21.6 depth range we filtered North's data set to only look at fronds collected at this range.

summary(North_1958_kelp_frond_biomass |>
          filter(Depth_m > 15 & Depth_m < 22))

#For depths between 15.01 and 21.19, weight of fronds ranges from 0.45 kg to 2.77 kg with an average of 1.07 kg.

#what does this look like visually? (Highlighting depths that match our depths)
ggplot() +
  geom_point(data = North_1958_kelp_frond_biomass, aes(x = Depth_m, y = weight_kg)) +
  geom_point(data = North_1958_kelp_frond_biomass |> filter(Depth_m > 15 & Depth_m < 22), aes(x = Depth_m, y = weight_kg), color = "turquoise") +
  labs(x = "Depth (m)", y = "Frond biomass (kg)") +
  theme_classic() +
  scale_y_continuous(breaks = seq(0, 3, by = 0.1))
#ggplot shows the frond biomass at which North collected a frond between 15-22 m.

############################
#Rassweiler 2018 predictions for this depth range
############################

Rassweiler_2018_kelpdata.r <- Rassweiler_2018_kelpdata |>
  filter(complete.cases(DEPTH) & complete.cases(WT_WC) & complete.cases(WT_C)) #only include rows with data for depth, watercolumn weight, and canopy weight

# By finding the sum of column weight and canopy weight we can determine the mean weight of all giant kelp fronds
#make new column summing water column weight and canopy weight
Rassweiler_2018_kelpdata.r <- Rassweiler_2018_kelpdata.r |>
  mutate(WT_F = WT_WC + WT_C) |>
  group_by(DEPTH) |>
  mutate(mean_wt_depth = mean(WT_F, na.rm = T))

#Lets look at this column
view(Rassweiler_2018_kelpdata.r)

#run a linear model (predicting weight of frond by depth). The linear model will help describe the relationship between WT_F (sum weight of water column and canopy fronds) and the depth at which the samples were collected by the VRG at PVR (ARM).
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
# Only look at depth and biomass bc those are the two variables we want to compare with North's data
Rassweiler_2018_depth_biomass <- Rassweiler_2018_kelpdata.r |>
  select(DEPTH, WT_F) |>
  mutate(source = "Rassweiller 2018") #add new column identifying data source
view(Rassweiler_2018_depth_biomass)

#add new column to North data as well
North_1958_kelp_frond_biomass <- North_1958_kelp_frond_biomass |>
  mutate(source = "North 1958")  #add new column identifying data source
view(North_1958_kelp_frond_biomass)

#do column names and order match?
colnames(Rassweiler_2018_depth_biomass)
colnames(North_1958_kelp_frond_biomass)

#order yes, but name no, so I will change names of North to match

colnames(North_1958_kelp_frond_biomass) <- colnames(Rassweiler_2018_depth_biomass)

#combine datasets
kelp_frond_biomass_merge <- bind_rows(Rassweiler_2018_depth_biomass, North_1958_kelp_frond_biomass)
view(kelp_frond_biomass_merge)

kelp_frond_merge_scatterplot <- ggplot() +
  geom_rect(aes(xmin = PVR_min_depth, xmax = PVR_max_depth, ymin = -Inf, ymax = Inf), fill = "lightyellow") + #add depth range of PVR in background
  geom_point(data = kelp_frond_biomass_merge, aes(x = DEPTH, y = WT_F, color = source), size = 2) +
  scale_color_manual(values = c("plum","skyblue")) +
  labs(x = "Depth (m)", y = "Frond biomass (kg)", color = "Data source") +
  geom_smooth(data = kelp_frond_biomass_merge, aes(x = DEPTH, y = WT_F), method = "lm") +
  theme_classic()

kelp_frond_merge_scatterplot

# The light yellow area shows the minimum and maximum depths at which the VRG completes PVR surveys. Having this defined area help determine the points at which we can calculate frond biomass values
# The plum points identify all the data taken by North (WT_F ~ DEPTH). The skyblue points show the points at which Rassweiler collected data.
#this figure may be helpful for the poster, save it to figures folder
ggsave(kelp_frond_merge_scatterplot, path = file.path("figures"), filename = "kelp_frond_merge_scatterplot.jpg", height = 10, width = 10, units = "in")

#I recommend playing around with this figure to add dates of papers to legend, and make points and figure labels larger
kelp_frond_merge_scatterplot2 <- ggplot(data = kelp_frond_biomass_merge) +
  ggtitle("Historic Macrocystis pyrifera Biomass and Depth Data") +
  geom_rect(aes(xmin = PVR_min_depth, xmax = PVR_max_depth, ymin = -Inf, ymax = Inf, fill = "Depth of PVR Surveys (2020 - 2023)"), colour = NA, alpha = 0.05) + #add depth range of PVR in background
  geom_point(data = kelp_frond_biomass_merge, aes(x = DEPTH, y = WT_F, color = source), size = 1.5) +
  scale_color_manual(values = c("#549422","#264DFF")) +
  labs(x = "Depth (m)", y = "Frond biomass (kg)", color = "Data Source") +
  geom_smooth(data = kelp_frond_biomass_merge, aes(x = DEPTH, y = WT_F), method = "lm") +
  geom_text() +
  scale_y_continuous(breaks = seq(0, 11, by = 0.5)) +
  scale_x_continuous(breaks = seq(0, 25, by = 1)) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 13)) +
  scale_fill_manual('Depth Range',
                    values = '#B6DBFF',
                    guide = guide_legend(override.aes = list(alpha = 1)))

kelp_frond_merge_scatterplot2


#you can also make other adjustments to your liking!
#It could even be cool to include the linear model you build below on the plot as well as text
#You'd want to share model structure (y = mx+b with actual values for m and b, the R^2 value, and the p value)

#let's build another linear model (predicting weight of frond by depth)
# This new linear model will provide us with a formula to predict kelp biomass within a specific depth in the PVR data set.
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


# Create a new data set that describes biomass based on depth on each module.
view(dat_PV_macrocystis_post_construction)

