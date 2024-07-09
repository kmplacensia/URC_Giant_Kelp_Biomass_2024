# CREATION DATE 3 June 2024
# MODIFIED DATE 6 July  2024

# AUTHOR: kitchel@oxy.edu & placensia@oxy.edu

# PURPOSE: Calculate biomass and carbon content of fronds on PVR by year and by module

#############################
##Setup
#############################
library(tidyverse)
library(lubridate)
library(sf)
library(terra)
############################
#LOAD DATA
############################
#open data subset of PVR only
dat_PV_macrocystis_post_construction <- read_csv(file = file.path("data","dat_PV_macrocystis_post_construction.csv"))

#import data frame of min, max, mean kg per frond from Rassweiler/North model (depth_Frondbiomass_relationships.R)
PVR_frond_biomass_model_output <- read_csv(file = file.path("output","PVR_frond_biomass_model_output.csv"))

#import Rassweiler et al. 2018 data
Rassweiler_2018_kelpdata <- read_csv(file.path("data","Rassweiler_2018_kelpdata.csv"))

#Import PVR module shapefiles
reefs_sf <- st_read(file.path("data","module_shapefiles", "PVR2023_Final_05_JUN_2024.shp"))


############################
#Reduce VRG data to just stipe density per module per year
############################
stipe_density_module_year <- dat_PV_macrocystis_post_construction |>
  mutate(stipe_density_m2 = Abundance/area.m2) |>
  filter(BenthicReefSpecies == "Macrocystis pyrifera stipes") |>
  group_by(SampleYear, Site) |>
  summarise(stipe_density_m2 = mean(stipe_density_m2)) |>
  mutate(Name = substr(Site, 5,7)) #new column with module only (called Name to match df_reefs)

###########################
#Calculate area of modules from shapefiles
###########################
#Access the attribute table of the reefs vector data as a data frame
#This is done through the as.tibble() function
#Then format table to use later
df_reefs <- as_tibble(reefs_sf) |>
  rownames_to_column() |>
  rename(ID = rowname) |>
  mutate(ID = as.numeric(ID))
df_reefs

#Areal Footprint (possibly better to use surface area? good question for Dan or Jonathan?)
df_reefs <- df_reefs |>
  mutate(footprint_m2 = st_area(reefs_sf)
  )

#merge with VRG data
stipe_density_module_year_area <- left_join(stipe_density_module_year, df_reefs, by = "Name")

###########################
#Calculate total number of stipes per module
###########################

stipe_density_module_year_area <- stipe_density_module_year_area |>
  mutate(total_stipes = as.numeric(footprint_m2) * stipe_density_m2)

###########################
#Calculate total biomass of stipes per module
###########################
stipe_density_module_year_area <- stipe_density_module_year_area |>
  mutate(total_frond_biomass = total_stipes * PVR_frond_biomass_model_output[[2,2]])

###########################
#Calculate total carbon from stipes per module
###########################
#From Rassweiler 2018, we find % biomass that = stipe and % that = blades of each frond on average
Rassweiler_2018_kelpdata.r <- Rassweiler_2018_kelpdata |>
  filter(complete.cases(WT_WC) & complete.cases(WT_NB_WC) & WT_WC > WT_NB_WC) #filter to only rows with values for both water column weight and water column weight without blades, AND only include rows where full weight is greater than stipe only weight

#new column for % frond in water column that is stipe only
Rassweiler_2018_kelpdata.r <- Rassweiler_2018_kelpdata.r |>
  mutate(rel_weight_stipe = WT_NB_WC/WT_WC)

#Average value?
summary(Rassweiler_2018_kelpdata.r$rel_weight_stipe) #49%

#standard error to incorporate measure of variance
sample.n <- length(Rassweiler_2018_kelpdata.r$rel_weight_stipe)
sample.sd <- sd(Rassweiler_2018_kelpdata.r$rel_weight_stipe)
sample.se <- sample.sd/sqrt(sample.n)

#SE = 49% +/- 1.2%

#we will say that % carbon of blades is ~34 (making up, will replace with true average value from samples)
#From Rassweiler et al. 2018, we know that then carbon of stipes = 34-12=22%

#new column for total carbon per module in kg
stipe_density_module_year_area <- stipe_density_module_year_area |>
  mutate(total_carbon =  (0.49 * total_frond_biomass * 0.22) + #carbon from stipes (49% of biomass is stipe)
                         (0.51 * total_frond_biomass * 0.34)   #carbon from blades (51% of biomass is blade)
                         )

###########################
#Visualize biomass through time
###########################

ggplot(stipe_density_module_year_area) +
  geom_col(aes(x = SampleYear, y = total_frond_biomass)) +
  facet_wrap(~Name, nrow = 6) +
  theme_classic()

#Can you plot again as summed for all of PV?



###########################
#Visualize carbon through time
###########################

ggplot(stipe_density_module_year_area) +
  geom_col(aes(x = SampleYear, y = total_carbon)) +
  facet_wrap(~Name, nrow = 6) +
  theme_classic()

#Can you plot again as summed for all of PV?


