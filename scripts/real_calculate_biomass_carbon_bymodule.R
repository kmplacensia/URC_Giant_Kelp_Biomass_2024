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

#area.m2 - transect area (60 m)
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
#49% of frond is made of stipes, thus 51% of frond weight is made up of blades

#standard error to incorporate measure of variance
sample.n <- length(Rassweiler_2018_kelpdata.r$rel_weight_stipe)
sample.sd <- sd(Rassweiler_2018_kelpdata.r$rel_weight_stipe)
sample.se <- sample.sd/sqrt(sample.n)

#SE = 49% +/- 1.2%

#####################
#Original from Zoe:
#####################

# We will say that % carbon of blades is ~34 (making up, will replace with true average value from samples)
# From Rassweiler et al. 2018, we know that then carbon of stipes = 34-12=22%

#new column for total carbon per module in kg
stipe_density_module_year_area <- stipe_density_module_year_area |>
  mutate(total_carbon =  (0.49 * total_frond_biomass * 0.22) + #carbon from stipes (49% of biomass is stipe)
                         (0.51 * total_frond_biomass * 0.34)   #carbon from blades (51% of biomass is blade)
                         )
# Calculate mean_total_carbon_per year on each module
stipe_density_mean_total_carbon_per_year2 <- stipe_density_module_year_area |>
  group_by(SampleYear) %>%
  summarise(sum_total_carbon = sum(total_carbon))

#Zoe original
stipe_density_mean_total_carbon_per_year1 <- stipe_density_module_year_area |>
  mutate(mean_total_carbon = (mean(total_carbon)/SampleYear)) |>
  dplyr::select(SampleYear, mean_total_carbon, everything())

summary_data_mean_carbon


#####################
#Katie, from UCSB CHN results
#####################

# Percent carbon of blades from UCSB CHN Analysis is:
avg_percent_carbon_blades_CHN <- CHN_Site_real %>%
  summarise(avg_carbon_blades = mean(C))
#avg_percent_carbon_blades_CHN is 31.6%


# Based on Rassweiler 2018 article, the carbon mass was found to be 12% lower in stipes than in blades, so we subtract 12% from 31.6% to estimate the percent of carbon that makes up the stipes.
# Carbon of stipes = 31.6-12 = 19.6%, and carbon of blades = 31.6%
#new column for total carbon per module in kg
stipe_density_module_year_areaucsb <- stipe_density_module_year_area |>
  mutate(total_carbon =  (0.49 * total_frond_biomass * 0.194) + #carbon from stipes (49% of biomass is stipe)
                         (0.51 * total_frond_biomass * 0.314)   #carbon from blades (51% of biomass is blade)
  )

# Calculate sum_total_carbon_per year based on UCSB CHN Results
stipe_density_mean_total_carbon_per_year2 <- stipe_density_module_year_areaucsb |>
  group_by(SampleYear) %>%
  summarise(sum_total_carbon = sum(total_carbon))

# Calculate mean_total_carbon_per year based on UCSB CHN Results
stipe_density_mean_total_carbon_per_year_UCSB <- stipe_density_module_year_areaucsb |>
  mutate(mean_total_carbon = (mean(total_carbon)/SampleYear))

# Addiitonal piece Zoë added: |> dplyr::select(SampleYear, mean_total_carbon, everything())

###########################
#Visualize biomass through time
###########################
#Original code by Zoë
ggplot(stipe_density_module_year_area) +
  geom_col(aes(x = SampleYear, y = total_frond_biomass)) +
  facet_wrap(~Name, nrow = 6) +
  theme_classic()

#Can you plot again as summed for all of PV?

###########################
#Visualize carbon through time
###########################

ggplot(stipe_density_module_year_areaucsb) +
  geom_col(aes(x = SampleYear, y = total_carbon)) +
  facet_wrap(~Name, nrow = 6) +
  theme_classic()

#Can you plot again as summed for all of PV?

#avg across transects and modules

# I want to plot total carbon storage from each module onto a map


map_plot <- ggplot() +
  geom_sf(data = reefs_sf) +
  theme_classic()


map_with_carbon <- ggplot() +
  geom_point(data = stipe_density_module_year_areaucsb, aes(x = longitude, y = latitude, size = total_carbon)) +
  scale_size_continuous(name = "Total Carbon")

#Original working code to try and plot carbon storage on each module
singleyear2020data <- stipe_density_module_year_areaucsb %>%
  filter(SampleYear == 2020)

singleyear2020data

map_plot2020 <- map_plot +
  geom_sf(data = singleyear2020data, aes(x = fill = "blue", color = "black", size = 0.2))

map_plot2020

# Create new data set of stipe_density_module_year_areaucsb
stipe_density_module_year_area_2020 <- stipe_density_module_year_areaucsb %>%
  filter(SampleYear == 2020) %>%
  mutate(total_carbon_2020 = total_carbon) %>%
  ungroup() %>%
  select(Name, total_carbon_2020)

# Look at carbon storage on all modules in 2020 from the shapefile
stipe_density_module_year_area_all_years <- left_join(reefs_sf, stipe_density_module_year_area_2020, by = "Name")

plot(stipe_density_module_year_area_all_years)

# Add total_carbon from 2021 to reefs_sf
stipe_density_module_year_2021 <- stipe_density_module_year_areaucsb %>%
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
stipe_density_module_year_2022 <- stipe_density_module_year_areaucsb %>%
  filter(SampleYear == 2022) %>%
  mutate(total_carbon_2022 = total_carbon) %>%
  ungroup() %>%
  select(total_carbon_2022, Name)

stipe_density_module_year_area_all_years <- left_join(stipe_density_module_year_area_all_years, stipe_density_module_year_2022, by = "Name")

stipe_density_module_year_area_all_years

plot(stipe_density_module_year_area_all_years, max.plot = 10)

# Add total_carbon from 2023 to stipe_density_module_year_area_all_years

stipe_density_module_year_2023 <- stipe_density_module_year_areaucsb %>%
  filter(SampleYear == 2023) %>%
  mutate(total_carbon_2023 = total_carbon) %>%
  ungroup() %>%
  select(total_carbon_2023, Name)

stipe_density_module_year_area_all_years <- left_join(stipe_density_module_year_area_all_years, stipe_density_module_year_2023, by = "Name")

stipe_density_module_year_area_all_years

plot(stipe_density_module_year_area_all_years, max.plot = 11)

map_plot_2020 <- ggplot() +
  geom_sf(data = stipe_density_module_year_area_all_years, aes(fill = total_carbon_2020)) +
  scale_fill_viridis_c(name = "Total Grams of Carbon \nStored in 2020",  # Legend title
                        limits = c(0, 5500),  # Adjust limits as needed
                        breaks = seq(0, 5500, by = 500),  # Example breaks
                        labels = scales::comma_format()) +
  theme(legend.position = "bottom",
                legend.direction = "horizontal")

# change carbon scale:

map_plot_2021 <- ggplot() +
  geom_sf(data = stipe_density_module_year_area_all_years, aes(fill = total_carbon_2021)) +
  scale_fill_viridis_c(name = "Total Grams of Carbon \nStored in 2021",  # Legend title
                      limits = c(0, 5500),  # Adjust limits as needed
                      breaks = seq(0, 5500, by = 500),  # Example breaks
                      labels = scales::comma_format())+
  theme(legend.position = "bottom",
        legend.direction = "horizontal")
map_plot_2021


# Carbon storage range: 0 - 3000

map_plot_2022 <- ggplot() +
  geom_sf(data = stipe_density_module_year_area_all_years, aes(fill = total_carbon_2022)) +
  scale_fill_viridis_c(name = "Total Grams of Carbon \nStored in 2022",  # Legend title
                       limits = c(0, 5500),  # Adjust limits as needed
                       breaks = seq(0, 5500, by = 500),  # Example breaks
                       labels = scales::comma_format())+
  theme(legend.position = "bottom",
        legend.direction = "horizontal")

map_plot_2022


map_plot_2023 <- ggplot() +
  geom_sf(data = stipe_density_module_year_area_all_years, aes(fill = total_carbon_2023)) +
  scale_fill_viridis_c(name = "Total Grams of Carbon \nStored in 2023",  # Legend title
                       limits = c(0, 5500),  # Adjust limits as needed
                       breaks = seq(0, 5500, by = 500),  # Example breaks
                       labels = scales::comma_format())+
  theme(legend.position = "bottom",
        legend.direction = "horizontal")

map_plot_2023

# Merge all ggplot (2020-2023) into one figure

map_all_plots_2020_2023 <- plot_grid(map_plot_2020, map_plot_2021, map_plot_2022, map_plot_2023, ncol = 2) +
  theme(legend.position = "null")

# Extract the legend from one of the plots
map_carbon_legend <- get_legend(map_plot_2020)
map_carbon_legend


map_all_plots_2020_2023

# Combine the plots without individual legends
combined_plot <- plot_grid(
  map_plot_2020 + theme(legend.position = "none"),
  map_plot_2021 + theme(legend.position = "none"),
  map_plot_2022 + theme(legend.position = "none"),
  map_plot_2023 + theme(legend.position = "none"),
  ncol = 2
)
# Add the combined plot and the extracted legend into a final plot
final_plot <- plot_grid(
  combined_plot,
  map_carbon_legend,
  ncol = 1,
  rel_heights = c(4, 0.5)  # Adjust relative heights as needed
)

ggsave(map_plot_2020, path = ("figures"), filename = "map_all_plots_2020.jpg", width = 6.3, height = 3, units = "in", dpi = 300)
ggsave(map_plot_2021, path = ("figures"), filename = "map_all_plots_2021.jpg", width = 6.3, height = 3, units = "in", dpi = 300)
ggsave(map_plot_2022, path = ("figures"), filename = "map_all_plots_2022.jpg", width = 6.3, height = 3, units = "in", dpi = 300)
ggsave(map_plot_2023, path = ("figures"), filename = "map_all_plots_2023.jpg", width = 6.3, height = 3, units = "in", dpi = 300)



ggsave(map_all_plots_2020_2023, path = ("figures"), filename = "map_all_plots_2020_2023.jpg", width = 6, height = 7, units = "in", dpi = 300)

