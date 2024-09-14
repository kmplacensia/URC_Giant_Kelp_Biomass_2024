Title of project: Increase in Giant Kelp Biomass and Carbon Storage after the Construction of an Artificial Reef in the Southern California Bight


Abstract: As a primary producer, giant kelp (Macrocystis pyrifera) helps mitigate the effects of climate change by absorbing dissolved carbon dioxide (CO2) from the ocean, and therefore the atmosphere, as organic carbon (C) used as plant biomass. In 2020, the Vantuna Research Group (VRG) in collaboration with the Southern California Marine Institute (SCMI), The Bay Foundation, and the National Oceanic and Atmospheric Administration (NOAA) built the Palos Verdes Artificial Reef (PVR) in a shallow subtidal portion of the Palos Verdes (PV) Peninsula in Los Angeles County. The construction of PVR included the development of eighteen artificial reef modules, and by 2021 all modules had recruited rocky-reef associated taxa including giant kelp. Estimating the amount of giant kelp biomass produced on PVR can be used as a method to determine the amount of carbon stored by developing giant kelp forests. In this study we used historic VRG dive data to determine giant kelp stipe (frond) density per each module’s surface area (m2) to then estimate frond biomass using a linear regression model to predict the weight of a single frond based on depth using two historic dive data sets. We found that kelp biomass on PVR increased within a year of installation, and this increase in giant kelp biomass led to an increase in carbon storage. An improved understanding of how kelp forest ecosystems associated with artificial reefs can increase kelp biomass, and therefore local CO2 absorption, can inform coastal communities of the benefits of using artificial reefs to combat the adverse effects of global climate change.


Please contact Kaitlyn Placensia and Zoe Kitchel with questions about this project. The results are partially reproducible from the scripts and data files.

Where do data come from?
We used multiple data sources to create this script including:
1) Andrew Rassweilers 2018 data set including the depth, and wet weight of giant kelp samples collected in Souther Florida; obtained csv file directly from Professor Andrew Rassweiler at Florida State University 
2) A historical data set from North 1958 that provided us with giant kelp wet weight at depths from 0-18m collected in Southern California and Baja California - Occidental Library inner personal loan provided for free
3) "CHN & Site real.xlsx" - Carbon Hydrogen Nitrogen Elemental Analysis Results obtained from collecting giant kelp blades at the PVR sites in July 2024, which allowed us to estimate the average carbon content of a single giant kelp stipe - Results and test conducted the University of California Santa Barbara
4) VRG Historical dive data from 2020-2023 - describing the number of giant kelp plants and stipes accounted for on 2 30-m^2 transects at 18 artifical reef modules - obtained directly from a master file at the Vantuna Research Group Lab
5) VRG shape file - describing the geographic location of each PVR module - obtained directly from a master file at the Vantuna Research Group Lab


What's in this repository?
In this repository there are:
- Scripts - "depth_frondbiomass_relationships.R", "explore_subset_data.R", "load_kelp_data.R", and "real_calculate_biomass_carbon_by_module.R"
- All files not in the "outputs" and "scripts" folders are created from all three scripts
  > figures are made from ggplots, working to estimate the total dry biomass from all the giant kelp stipes accounted for on PVR, and the estimated carbon content from dry standing biomass
- data - contains raw data from other sources

In what order should data be run?
  Data should be run in the order of "explore_subset_data.R", then "depth_frond_biomass_relationships.R", and "real_calculate_biomass_carbon_bymodule.R"

  
