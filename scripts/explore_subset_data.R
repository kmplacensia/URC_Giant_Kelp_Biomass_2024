# CREATION DATE 24 June 2024
# MODIFIED DATE 24 June 2024

# AUTHOR: kitchel@oxy.edu & placensia@oxy.edu

# PURPOSE: Explore the data on kelp at Palos Verdes, and subset to data we need for analysis

#############################
##Setup
#############################

library(tidyverse)
library(lubridate)
############################
#LOAD DATA
############################
dat_PV_macrocystis <- read_csv(file.path("data", "dat_PV_macrocystis.csv"))

#Let's look at it!
View(dat_PV_macrocystis)

#Let's view some summary statistics
summary(dat_PV_macrocystis)

############################
#Only keep artificial reefs at Palos Verdes
############################

#What types of sites did they survey?
unique(dat_PV_macrocystis$DepthZone) #note ARM stands for artificial reefs

#We only want artificial reef observations, no natural reef at PV

dat_PVR_macrocystis <- dat_PV_macrocystis |>
  filter(DepthZone == "ARM") #only include depthzones = ARM

############################
#Narrow down years?
############################

#What years do we have now?

sort(unique(dat_PVR_macrocystis$SampleYear))

#We only want years after installation of Palos Verdes (September of 2020)

dat_PV_macrocystis_post_construction <- dat_PVR_macrocystis |>
  mutate(SampleDate = dmy(SampleDate)) |>
  filter(SampleDate >= "2020-11-13")

#####################
#Filter out data from after September 2020
#####################


############################
#To calculate an overall density for each site and sampling day, we will sum macrocystis stipe count and plant count over 2 transects, and then divide by 120 meters squared (60 m x 2)
############################

#These 'cheat sheets' are always helpful for working through R! https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf

#First, stipe densities!

PV_macrocystis_stipe_densities <- dat_PV_macrocystis_post_construction |>
  group_by(SampleYear, SampleDate, Site) |> #take sum for each site, each sampling day, and each sampling year
  filter(BenthicReefSpecies == "Macrocystis pyrifera stipes") |> #limit to stipes only!
  summarise(stipe_density_m2 = sum(Abundance)/120)

#take a look!
View(PV_macrocystis_stipe_densities)

#Second, plant densities!


PV_macrocystis_plant_densities <- dat_PV_macrocystis_post_construction |>
  group_by(SampleYear, SampleDate, Site) |>
  filter(BenthicReefSpecies == "Macrocystis pyrifera") |>
  summarise(plant_density_m2 = sum(Abundance)/120)

#look at code:
View(PV_macrocystis_plant_densities)
#Try it yourself!! (follow above as a guide)



############################
#How does stipe density vary across years?
############################
ggplot(data = PV_macrocystis_stipe_densities) +
  geom_point(aes(x = factor(SampleYear), y = stipe_density_m2)) +
  theme_classic()

#How does plant density vary across years?

ggplot(data = PV_macrocystis_plant_densities) +
  geom_point(aes(x = factor(SampleYear), y = plant_density_m2)) +
  theme_classic()

#How does plant density compare to stipe density?
p_left <- dplyr::left_join(PV_macrocystis_plant_densities, PV_macrocystis_stipe_densities, by = c("SampleYear", "SampleDate", "Site"))

densityvector <-

ggplot(p_left, aes(x = Year, y = SampleDate)) +
  geom_line()
