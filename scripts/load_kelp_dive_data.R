# CREATION DATE 24 June 2024
# MODIFIED DATE 24 June 2024

# AUTHOR: kitchel@oxy.edu

# PURPOSE: Pull in CRANE data to calculate kelp density across PVR
# NOTE: this script will only work for someone who is connected to VRG Dropbox on their person computer!!

#############################
##Setup
#############################

library(data.table)
library(vegan)
library(dplyr)

############################
#DROPBOX STEPS
############################
#SELECT <- "BOEM_depth_comparison"

#############################
##Load data
#############################

# Read in reference tables (conversions, station names, species names, codes for UPC, ISC, etc.)
source("~/Dropbox/VRG Files/R Code/DataFiles/READ_Sites_Fish_BRS.r")

#Loads up and processes CRANE data
source("~/Dropbox/VRG Files/R Code/Integrated Dive General/CRANE_data_prep.R")

CRANE_data_prep(SELECT = "BOEM_depth_comparison",
                add_0s = "sp_0s", #summed to by Species with 0s added
                )

########################
##Split out kelp
########################
#EVENT data
#dat_event

#KELP (SWATH)
#swath_melt_T_sp, and then %in% "kelp - understory", "kelp - canopy" ,"Sargassum", "green algae", "giant kelp stipes"
#      "giant kelp stipes" included
dat_macrocystis <- swath_melt_T_sp %>%
  filter(BenthicReefSpecies %in% c("Macrocystis pyrifera stipes", "Macrocystis pyrifera"))


########################
##Add Level Orders to Region, Site, DepthZone
########################
source("~/Dropbox/VRG Files/R Code/Integrated Dive General/CRANE_level_orders.R")

dat_macrocystis <- CRANE_level_orders(dat_macrocystis, AR_Region = T, AR_Complex = T, Regions = T,
                                      PVR_Monitor_Cat = T, KOU_SiteType = T, SiteType = T, DepthZones = T, Sites = T,
                                      Eras = T) |>
  droplevels()

########################
##Filtering to PV only
########################

dat_PV_macrocystis <- dat_macrocystis %>%
  filter(Region == "Palos Verdes")

dat_event.r <- dat_event %>%
  filter(Region == "Palos Verdes") %>%
  select(Site, SampleDate, DepthZone, Temperature, Latitude, Longitude) %>%
  group_by(Site, SampleDate, DepthZone) %>%
  summarise(Temperature_c = mean(Temperature, na.rm = T),
            Latitude_d = mean(Latitude, na.rm = T),
            Longitude_d = mean(Longitude, na.rm = T)) %>%
  unique()


########################
##Link event info
########################

dat_PV_macrocystis <- left_join(dat_PV_macrocystis, dat_event.r, by = c("Site","SampleDate","DepthZone"))

########################
##Save this subset as a CSV
########################

write_csv(dat_PV_macrocystis, file = file.path("data","dat_PV_macrocystis.csv"))
