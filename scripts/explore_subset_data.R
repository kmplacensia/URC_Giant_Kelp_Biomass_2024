# CREATION DATE 24 June 2024
# MODIFIED DATE 24 June 2024

# AUTHOR: kitchel@oxy.edu & placensia@oxy.edu

# PURPOSE: Explore the data on kelp at Palos Verdes, and subset to data we need for analysis

#############################
##Setup
#############################

library(tidyverse)
library(lubridate)
library(dplyr)
library(reshape2)
############################
#LOAD DATA
############################

dat_PV_macrocystis <- read_csv(file.path("data", "dat_PV_macrocystis.csv"))

#The dat_PV_macrocystis data set is compiled of all data taken by the VRG when surveying the reefs off of the PV Peninsula from 2016 - Present. The entire data set will allow me to narrow down all data taken of the PVR modules from the very first survey completed in November 2020. According to Williams et al. PVR was initially constructed in May 2020.

#Let's look at it!
dat_PV_macrocystis

#Let's view some summary statistics
summary(dat_PV_macrocystis)

############################
#Only keep artificial reefs at Palos Verdes
############################

# The focus of my research project is to determine how much giant kelp biomass has grown on the PV artificial reef constructed by the VRG from 2020 - 2023, as well as the amount of carbon that has been stored by each module as standing biomass from 2020 - 2023.
# To begin I have to narrow down the data set just to look at all data surveys completed on the PVR AR_Complex.

#What types of sites did they survey?
unique(dat_PV_macrocystis$DepthZone) #note ARM stands for artificial reefs

# The VRG surveyed" ""Inner"  "Middle" "Outer"  "Deep"   "ARM (artifical reef)"" sites.


# We only want artificial reef observations, no natural reef at PV
# To only look at artificial reef observations within the PV data set we have to create a new data set "dat_PVR_macrocystis" that just contains surveys at PVR and ARM.
# Our new data set is simply a filter from the dat_PV_macrocystis data set that focuses on only data taken at the "DepthZone: ARM"

dat_PVR_macrocystis <- dat_PV_macrocystis |>
  filter(DepthZone == "ARM") #only include depthzones = ARM

# Lets look at the dat_PV_macrocystis data set
view(dat_PVR_macrocystis)
#Looks great

############################
#Narrow down years?
############################
# I am only interested in looking at giant kelp stipe and plant counts from the VRG ARM surveys after September 2020, post PVR complete construction. To begin this step we will sort the dat_PVR_macrocystis to determine what years ARM surveys were conducted.

sort(unique(dat_PVR_macrocystis$SampleYear))

# What years do we have now?
# 2019 2020 2021 2022 2023

# When was the the earliest dive survey of PVR conducted?
# 2019


#####################
#Filter out data from after first "Post-Construction" survey after September 2020
#####################


# We only want years after complete installation of Palos Verdes Reef (September of 2020)
# To filter the data set I changed the SampleDate format from character to numerical data, mutating "SampleDate" to dmy(SampleDate)"
# Then filtered the data set for post the first survey in November 2020: 11-13-2020

dat_PV_macrocystis_post_construction <- dat_PVR_macrocystis |>
  mutate(SampleDate = dmy(SampleDate)) |>
  filter(SampleDate >= "2020-11-13")
# Lets view the data set
view(dat_PV_macrocystis_post_construction)

#Looks great

##########################
# Now we want to utilize the filtered PVR data set to look at plant and stipe density.
# According to University of Southern Florida: Kelp are made of three main parts: blades, stipes, and holdfasts. Blades are like leave of land plants; photosynthetic factories of the kelp, this is where energy from sunlight and nutrients from the water are combined to produce food.
#.... Blades do not complete nutrient transport functions like the stems of plants, instead provide support for the kelp blades.
# According to North 'Biology of Macrocystis in North America' we can estimate Total biomass in a population by multiplying frond density by the mean wet weight of a frond (i.e. ca 1 kg) (THIS IS THE MAIN GOAL OF DATA CONFIGURATION TO HELP ESTIMATE THE AMOUNT OF CARBON STORED BY GIANT KELP AT EACH MODULE)
# According to Reed et al. 2009 "Density of fronds provides a much better estimator of FSC ("stsanding biomass - mass associated with holdfasts and reproductive blades") than plants as fronds explained nearly 2.5 times more of the observed variability in FSC than plants (r^2 = 0.70 vs 0.34).

# With our interpretation, frond = stipes. So we can utilize stipe density to calculate biomass (eventually).
##########################

############################
#To calculate an overall density for each site and sampling day, we will sum both macrocystis stipe count and plant count over 2 transects (seperately), by dividing stipe count by 120 meters squared (60 m x 2)
############################


#These 'cheat sheets' are always helpful for working through R! https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf

#First, stipe densities! Zoë filtered this data set to only contain the SampleYear, SampleDate, Site, and Density of stipes

PVR_macrocystis_stipe_densities <- dat_PV_macrocystis_post_construction |>
  group_by(SampleYear, SampleDate, Site) |> #take sum for each site, each sampling day, and each sampling year
  filter(BenthicReefSpecies == "Macrocystis pyrifera stipes") |> #limit to stipes only!
  summarise(stipe_density_m2 = sum(Abundance)/120)

#take a look!
PVR_macrocystis_stipe_densities
# Looks great


# Second, plant densities!
# Zoë filtered this data set to only contain the SampleYear, SampleDate, Site, and Density of plants

PVR_macrocystis_plant_densities <- dat_PV_macrocystis_post_construction |>
  group_by(SampleYear, SampleDate, Site) |>
  filter(BenthicReefSpecies == "Macrocystis pyrifera") |>
  summarise(plant_density_m2 = sum(Abundance)/120)

#look at code:
PVR_macrocystis_plant_densities


#Try it yourself!! (follow above as a guide)



############################
#How does stipe density vary across years?
############################


ggplot(data = PVR_macrocystis_stipe_densities) +
  geom_col(aes(x = factor(SampleYear), y = stipe_density_m2)) +
  theme_classic()

############################
#How does plant density vary across years?
############################


ggplot(data = PVR_macrocystis_plant_densities) +
  geom_col(aes(x = factor(SampleYear), y = plant_density_m2)) +
  theme_classic()

#How does plant density compare to stipe density?

# To complete this comparison Zoë helped me create a p_left data set that combined the stipe density values (# stipes/ 120 m^2) and plant density values (#plants/120 m^2)
p_left <- dplyr::left_join(PVR_macrocystis_plant_densities, PVR_macrocystis_stipe_densities, by = c("SampleYear", "SampleDate", "Site"))

#We would like to look at the stipe density and plant density per each sample date between 2020 - 2023

#look at code
View(p_left)

str(p_left)

head(p_left)


# Create data set that is tidy/long instead of wide where we have a column for each of the types of things we measured/recorded in our data. We want each observation/sample date to have both a plant density and stipe density assigned values.
# AKA reshape data from wide (untidy) to long (tidy). Long data is more clear as it shows how many distinct types of variables we have in the data.

# Our goal in comparing plant density and stipe density in p_left is to look at the differences in plant density to stipe density from the entire data set.
# Use the pivot function to go from wide p_left to long

p_left_long <- p_left %>%
  pivot_longer(cols = c(plant_density_m2, stipe_density_m2),
               names_to = "DensityType",
               values_to = "DensityValue")
#View data set
view(p_left_long)

#looks great
#Now that the data set has only two columns describing the DensityType and Density Value, we can plot DensityType as the color variable, SampleYear as the X-variable, and DensityValue as the Y-variable
#plot plant density value vs stipe density value for each sample date

ggplot(data = p_left_long) +
       geom_point(aes(x = SampleDate, y = DensityValue, color = DensityType)) +
  theme_classic()

view(p_left_long)

# This ggplot is not easy to understand.... what about if I plot as a geom_bar with the two types of density side by side
ggplot(data = p_left_long) +
  geom_col(aes(x = Site, y = DensityValue, fill = DensityType)) +
  theme_classic()

#Create sum(DensityValue) per each SampleYear. We want one density value for each year.

#In 2020 what was the average giant kelp stipe density?

# Begin by finding mean value of stipes from 2020 within p_left_long....


p_left_df <- p_left %>%
  group_by(SampleYear) %>%
  summarise(mean_stipe = mean(stipe_density_m2),
            mean_plant = mean(plant_density_m2))

view(p_left_df)

# Plot
ggplot(p_left_df, aes(x = SampleYear, y = mean_stipe)) +
  geom_col() +
  theme_classic()


p_left_df2 <- p_left_long %>%
  group_by(SampleYear, DensityType) %>%
  summarise(mean_density = mean(DensityValue))

#View new data set that just has mean values of each DensityType
view(p_left_df2)


# Now I am creating a ggplot that plots mean plant density and mean stipe density per year in side by side bar plots using the geom_col() function to show how plant density and stipe density have changed every year post construction of PVR.
# Adjusting the names of the DensityType: Plant_density_m2 and stipe_density_m2 to Total plant and Total stipe
# Adjust plot margins

custom_colors <- c("#005000", "#009292")

p_left_df2$DensityType <- ifelse(p_left_df2$DensityType == "stipe_density_m2", "Total stipe",
                                 ifelse(p_left_df2$DensityType == "plant_density_m2", "Total plant",
                                        p_left_df2$DensityType))

Average_plant_and_stipe_density_figure12 <- ggplot(data = p_left_df2, aes(x = SampleYear, y = mean_density, fill = DensityType)) +
  geom_hline(yintercept = seq(0, 3.1, by = 0.5), color = "lightgrey", linetype = "solid") +  # Add lines at 0.5 intervals
  geom_hline(yintercept = 0, color = "black", linetype = "solid") +
  geom_col(position = "dodge") +
  labs(x = "Year of Sample Collection during Fall Season", y = expression("Mean Density (kg/m"^2*")")) +
  scale_y_continuous(breaks = seq(0, 3.1, by = 0.1),
                     labels = function(x) ifelse(x %% 0.5 == 0, as.character(x), ''),
                     expand = c(0,0)) +
  scale_x_continuous(breaks= seq(2020, 2023), expand = c(0,0)) +
  theme(axis.text = element_text(size = 14, color = "black", face = "bold"),
        axis.title = element_text(size = 15, color = "black",face = "bold"),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 13),
        panel.background = element_blank(),
        legend.position = c(0.1, 1),
        legend.justification = c(0, 1)) +
  scale_fill_manual(name = "Plant Component", values = custom_colors)

# I want to graph plant mean density and stipe density at each module from 2020-2023 ("through time")
print(Average_plant_and_stipe_density_figure12)

#Save figure
ggsave(Average_plant_and_stipe_density_figure12, path = ("figures"), filename = "Average_plant_and_stipe_density_figure12.jpg", width = 9, height = 8, units = "in", dpi = 300)

