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
library(ggplot2)
library(dplyr)
############################
#LOAD DATA
############################
#open data subset of PVR only
dat_PV_macrocystis_post_construction <- read_csv(file = file.path("data","dat_PV_macrocystis_post_construction.csv"))

#import data frame of min, max, mean kg per frond from Rassweiler/North model (depth_Frondbiomass_relationships.R)
PVR_frond_biomass_model_output <- read_csv(file = file.path("output","PVR_frond_biomass_model_output.csv"))


#import Rassweiler et al. 2018 data
Rassweiler_2018_kelpdata <- read_csv(file.path("data","Rassweiler_2018_kelpdata.csv"))


#Import PVR module shapefiles... The shape file provides figures of PVR on a map plot.
reefs_sf <- st_read(file.path("data","module_shapefiles", "PVR2023_Final_05_JUN_2024.shp"))

reefs_sf
plot(reefs_sf)
############################
#Reduce VRG data to just stipe density per module per year
############################

stipe_density_module_year <- dat_PV_macrocystis_post_construction |>
  mutate(stipe_density_m2 = Abundance/area.m2) |>
  filter(BenthicReefSpecies == "Macrocystis pyrifera stipes") |>
  group_by(SampleYear, Site) |>
  summarise(stipe_density_m2 = mean(stipe_density_m2)) |>
  mutate(Name = substr(Site, 5,7)) #new column with module only (called Name to match df_reefs)

#area.m2 - transect area
stipe_density_module_year

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
#Aerial Footprint (possibly better to use surface area? good question for Dan or Jonathan?)

df_reefs <- df_reefs |>
  mutate(footprint_m2 = st_area(reefs_sf)
  )

#merge with VRG data. By merging the df_reefs and stipe_density_module_year we can match the area of each module with the stipe density of each module per year.
stipe_density_module_year_area <- left_join(stipe_density_module_year, df_reefs, by = "Name")

###########################
#Calculate total number of stipes per module
###########################
# Total number of stipes per m^2 on each module is equal to the stipe density per m^2 * as.numeric(footprintm2) (surface area of each module)
stipe_density_module_year_area <- stipe_density_module_year_area |>
  mutate(total_stipes = as.numeric(footprint_m2) * stipe_density_m2)

stipe_density_module_year_area
###########################
#Calculate total biomass of stipes per module. Total frond biomass = total_stipes * average weight of frond at 18.42 m = 1.65 kg.
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

Rassweiler_2018_kelpdata.r

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
stipe_density_module_year_area


stipe_density_mean_total_carbon_per_year <- stipe_density_module_year_area |>
  mutate(mean_total_carbon = (mean(total_carbon)/SampleYear)) |>
  dplyr::select(SampleYear, mean_total_carbon, everything())

summary_data_mean_carbon


stipe_density_mean_total_carbon_per_year
###########################
#Visualize biomass through time
###########################

ggplot(stipe_density_module_year_area) +
  geom_col(aes(x = SampleYear, y = total_frond_biomass)) +
  facet_wrap(~Name, nrow = 6) +
  theme_classic()

#Can you plot again as summed for all of PV?

#Open dat_PV_macrocystis_post_construction

dat_PV_macrocystis



#import data frame of min, max, mean kg per frond from Rassweiler/North model (depth_Frondbiomass_relationships.R)

Rassweiler_2018_kelpdata <- read_csv(file.path("data","Rassweiler_2018_kelpdata.csv"))
Rassweiler_2018_kelpdata

###########################
#Visualize carbon through time
###########################

ggplot(stipe_density_module_year_area) +
  geom_col(aes(x = SampleYear, y = total_carbon)) +
  facet_wrap(~Name, nrow = 6) +
  theme_classic()

stipe_density_module_year_area

#Can you plot again as summed for all of PV?

#avg across transects and modules

# I want to plot total carbon storage from each module onto a map


map_plot <- ggplot() +
  geom_sf(data = reefs_sf) +
  theme_classic()

map_plot
summary(map_plot)

map_with_carbon <- ggplot() +
  geom_point(data = stipe_density_module_year_area, aes(x = longitude, y = latitude, size = total_carbon)) +
  scale_size_continuous(name = "Total Carbon")

print(map_with_carbon)




singleyear2020data <- stipe_density_module_year_area %>%
  filter(SampleYear == 2020)
singleyear2020data

map_plot2020 <- map_plot +
  geom_sf(data = singleyear2020data, aes(x = fill = "blue", color = "black", size = 0.2))

map_plot2020

# Create new data set of >
stipe_density_module_year_area_2020 <- stipe_density_module_year_area %>%
  filter(SampleYear == 2020) %>%
  mutate(total_carbon_2020 = total_carbon) %>%
  ungroup() %>%
  select(total_carbon_2020, Name)

# Look at new data set
stipe_density_module_year_area_2020

stipe_density_module_year_area_all_years <- left_join(reefs_sf, stipe_density_module_year_area_2020, by = "Name")

stipe_density_module_year_area_all_years

plot(stipe_density_module_year_area_all_years)

# Add total_carbon from 2021 to reefs_sf
stipe_density_module_year_2021 <- stipe_density_module_year_area %>%
  filter(SampleYear == 2021) %>%
  mutate(total_carbon_2021 = total_carbon) %>%
  ungroup() %>%
  select(total_carbon_2021, Name)

stipe_density_module_year_2021

stipe_density_module_year_area_all_years <- left_join(stipe_density_module_year_area_all_years, stipe_density_module_year_2021, by = "Name")

stipe_density_module_year_area_all_years

plot(stipe_density_module_year_area_all_years)

# Add total_carbon from 2022 to stipe_density_module_year_area_all_years

#Create new data set of stipe_density_module_year_2022
stipe_density_module_year_2022 <- stipe_density_module_year_area %>%
  filter(SampleYear == 2022) %>%
  mutate(total_carbon_2022 = total_carbon) %>%
  ungroup() %>%
  select(total_carbon_2022, Name)

stipe_density_module_year_area_all_years <- left_join(stipe_density_module_year_area_all_years, stipe_density_module_year_2022, by = "Name")

stipe_density_module_year_area_all_years

plot(stipe_density_module_year_area_all_years, max.plot = 10)

# Add total_carbon from 2023 to stipe_density_module_year_area_all_years

stipe_density_module_year_2023 <- stipe_density_module_year_area %>%
  filter(SampleYear == 2023) %>%
  mutate(total_carbon_2023 = total_carbon) %>%
  ungroup() %>%
  select(total_carbon_2023, Name)

stipe_density_module_year_area_all_years <- left_join(stipe_density_module_year_area_all_years, stipe_density_module_year_2023, by = "Name")

stipe_density_module_year_area_all_years

plot(stipe_density_module_year_area_all_years, max.plot = 11)

map_plot_2020 <- ggplot() +
  geom_sf(data = stipe_density_module_year_area_all_years, aes(fill = total_carbon_2020)) +
  theme_classic()

map_plot_2020

map_plot_2021 <- ggplot() +
  geom_sf(data = stipe_density_module_year_area_all_years, aes(fill = total_carbon_2021)) +
  theme_classic()

map_plot_2021

map_plot_2022 <- ggplot() +
  geom_sf(data = stipe_density_module_year_area_all_years, aes(fill = total_carbon_2022)) +
  theme_classic()

map_plot_2022


map_plot_2023 <- ggplot() +
  geom_sf(data = stipe_density_module_year_area_all_years, aes(fill = total_carbon_2023)) +
  theme_classic()

map_plot_2023

map_all_plots_2020_2023 <- plot_grid(map_plot_2020, map_plot_2021, map_plot_2022, map_plot_2023, ncol = 2)
map_all_plots_2020_2023

ggsave(map_all_plots_2020_2023, path = ("figures"), filename = "map_all_plots_2020_2023.jpg", width = 4, height = 8, units = "in", dpi = 300)

view(single_coordinate)




