# CREATION DATE 24 June 2024
# MODIFIED DATE 24 June 2024

# AUTHOR: kitchel@oxy.edu & placensia@oxy.edu

# PURPOSE: Explore the data on kelp at Palos Verdes, and subset to data we need for analysis

#############################
##Setup
#############################

library(tidyverse)

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

#We only want years after installation of Palos Verdes (September of 2020)
dat_PVR_macrocystis <- dat_PV_macrocystis |>
  filter(DepthZone == "ARM") #only include sample years after 2020

############################
#Narrow down years?
############################

#What years do we have now?
sort(unique(dat_PVR_macrocystis$SampleYear))

#We only want years after installation of Palos Verdes (September of 2020)
dat_PV_macrocystis_post_construction <-   #Try it yourself!! (follow above as a guide)



############################
#To calculate an overall density for each site and sampling day, we will sum macrocystis stipe count and plant count over 2 transects, and then divide by 120 meters squared (60 m x 2)
############################

#These 'cheat sheets' are always helpful for working through R! https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf

#First, stipe densities!

PV_macrocystis_stipe_densities <- dat_PV_macrocystis_post_construction |>
  group_by(SampleYear, SampleDate,Site) |> #take sum for each site, each sampling day, and each sampling year
  filter(BenthicReefSpecies == "Macrocystis pyrifera stipes") |> #limit to stipes only!
  summarise(stipe_density_m2 = sum(Abundance)/120)

#take a look!
View(PV_macrocystis_stipe_densities)

#Second, plant densities!


PV_macrocystis_plant_densities <- #Try it yourself!! (follow above as a guide)

############################
#How does density vary across years?
############################
ggplot(data = PV_macrocystis_stipe_densities ) +
  geom_point(aes(x = factor(SampleYear), y = stipe_density_m2)) +
  theme_classic()

#we can save this plot to our figures folder

PV_macrocystis_year_stipedensity <- ggplot(data = PV_macrocystis_stipe_densities ) +
  geom_point(aes(x = factor(SampleYear), y = stipe_density_m2)) +
  theme_classic()

ggsave(PV_macrocystis_year_stipedensity,
       path = "figures", #what folder should it save into
       filename = "PV_macrocystis_year_stipedensity.jpg", #the file name and file type to save
       height = 4, #here and below you edit the dimensions of your plot, this will be useful for your poster!
       width = 5,
       units = "in"
         )

#Take some time to edit the axis labels with variable name and units to make the plot look nicer

############################
#How does density vary across years by site?
############################

PV_macrocystis_year_stipedensity_bysite <- ggplot(data = PV_macrocystis_stipe_densities ) +
  geom_point(aes(x = factor(SampleYear), y = stipe_density_m2)) +
  facet_wrap(~Site) + #this splits the plot by Site
  theme_classic()

ggsave(PV_macrocystis_year_stipedensity,
       path = "figures", #what folder should it save into
       filename = "PV_macrocystis_year_stipedensity.jpg", #the file name and file type to save
       height = 4, #here and below you edit the dimensions of your plot, this will be useful for your poster!
       width = 5,
       units = "in"
)

#What other relationships can you explore or plot?
