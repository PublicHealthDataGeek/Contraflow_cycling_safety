################################################################################
#                             Get STATS19 data                                 #
#                                                                              #
# This code gets the Road traffic crash (STATS19) data ready for identifying   #
# casualties/crashes that are spatial located to the contraflow segments.      #
#                                                                              #
# It starts with the Crashes data, formats it and then converts it to a        #
# spatial object so that we can identify crashes that occur in Inner London.   #
#                                                                              #
# Then it imports the casualty and vehicle stats19 data, limits it to 1998     #
# - 2019, formats it and saves it as a new Rds.                                #
#                                                                              #
################################################################################

# Load packages
library(stats19)
library(tidyverse)
library(sf)
library(mapview)


################################################################################
#               Get and format crash data 1998-2019                            #
################################################################################

# Load Stats 19 crash data - this is the raw DfT file downloaded from the website
# https://www.data.gov.uk/dataset/cb7ae6f0-4be6-4935-9277-47e5ce24a11f/road-safety-data
crashes1979 = read_delim(file = "./Downloads/1979/dft-road-casualty-statistics-accident-1979-2020.csv", 
                        col_names = TRUE, delim = ",")

# Filter on years we want
years = 1998:2019
crashes1998_2019 = crashes1979 %>%
  filter(accident_year %in% years)
nrow(crashes1998_2019) # 3867332

# Format stat19 data
crashes1998_2019 = format_accidents(crashes1998_2019)

# Save to make easier
saveRDS(crashes1998_2019, file = "./Downloads/1979/dft-road-casualty-statistics-accident-1998-2019.Rds")


##################################################################################
#   Obtain 1998 - 2019 crashes that occur within 150m of Inner London external   #
#            boundary Inner London for linking to contraflow data                #
##################################################################################

# Create buffered boundary for Inner London
lon_lad_2020_bfe = readRDS(file = "./map_data/lon_LAD_boundaries_May_2020_BFE.Rds")
Inner_ons = c("City of London", "Camden", "Greenwich", "Hackney", "Hammersmith & Fulham", 
              "Islington", "Kensington & Chelsea", "Lambeth", "Lewisham", "Newham", "Southwark",  
              "Tower Hamlets", "Wandsworth", "Westminster")
# Inner_stats19 = c("City of London", "Camden", "Greenwich", "Hackney", "Hammersmith and Fulham", 
#                   "Islington", "Kensington and Chelsea", "Lambeth", "Lewisham", "Newham", "Southwark",  
#                   "Tower Hamlets", "Wandsworth", "Westminster")

inner_lon_boroughs = lon_lad_2020_bfe %>%
  filter(BOROUGH %in% Inner_ons)
inner_lon_boroughs_union = inner_lon_boroughs %>% st_union()  # use union as that means any slight gaps get captured
buffer_150_inner_lon_boroughs_union = st_buffer(inner_lon_boroughs_union, dist = 150)

mapview(inner_lon_boroughs_union, color = "red") + 
  mapview(buffer_150_inner_lon_boroughs_union, color = "green") +
  mapview(inner_lon_boroughs)# yep it works 

# Create spatial df of crashes
crashes1998_2019 = readRDS(file = "./Downloads/1979/dft-road-casualty-statistics-accident-1998-2019.Rds")
    # n = 3867332
spatial_data = c("location_easting_osgr", "location_northing_osgr", "longitude", "latitude")
crashes1998_2019_num = crashes1998_2019 %>% 
  mutate_at(spatial_data, as.numeric) # convert these columns to numeric
    # n = 3867332, some NAs introduce by coercion
crashes1998_2019_sf = format_sf(crashes1998_2019_num) # create sf 
    # n = 3862990, 4342 rows removed with no coordinates
# # Examine rows with no coordinates
# crashes1998_2019_sf_dropped = crashes1998_2019_sf %>% st_drop_geometry()
# dropped_no_coord = anti_join(crashes1998_2019_num, crashes1998_2019_sf_dropped) # n= 4342
# dropped_police = dropped_no_coord %>% group_by(police_force) %>% summarise(count = n())
# dropped_pol_lon = dropped_no_coord %>% 
#   filter(police_force == "City of London" | police_force == "Metropolitan Police") 
#   # n = 8, all in 2018, 7 occured in inner London boroughs
# dropped_borough = dropped_no_coord %>% filter(local_authority_district %in% Inner_stats19)
#   # n = 7, all in 2018
# # In summary - 7 crashes in the 1998-2019 dataset that do not have coordinates
# # So this is basically a very tiny proportion and I cant do anything about it anyway!

# Spatially identify all crashes that occurred within the buffered boundary of Inner London
inner_crashes_1998_2019 = st_intersection(crashes1998_2019_sf, buffer_150_inner_lon_boroughs_union)
   # n = 304456

# Save dataset
saveRDS(inner_crashes_1998_2019, file = "./data-processed/stats19/inner_lon_buff_crashes_1998_2019.Rds")


################################################################################
#                      Get and tidy up STATS19 casualty data                   #
################################################################################


# Load Stats 19 crash data
casualties1979 = read_delim(file = "./Downloads/1979/dft-road-casualty-statistics-casualty-1979-2020.csv", 
                         col_names = TRUE, delim = ",")
nrow(casualties1979) #11449312


# Filter on years we want
range(casualties1979$accident_year)

years = 1998:2019
casualties1998_2019 = casualties1979 %>%
  filter(accident_year %in% years)
nrow(casualties1998_2019) # 5222207

# Format stat19 data
casualties1998_2019 = format_casualties(casualties1998_2019)

str(casualties1998_2019)

# Save to make easier
saveRDS(casualties1998_2019, file = "./Downloads/1979/dft-road-casualty-statistics-casualties-1998-2019.Rds")


################################################################################
#                      Get and tidy up STATS19 vehicles data                   #
################################################################################


# Load Stats 19 crash data
vehicles1979 = read_delim(file = "./Downloads/1979/dft-road-casualty-statistics-vehicle-1979-2020.csv", 
                            col_names = TRUE, delim = ",")
nrow(vehicles1979) # 15345829

# Filter on years we want
range(vehicles1979$accident_year)

years = 1998:2019
vehicles1998_2019 = vehicles1979 %>%
  filter(accident_year %in% years)
nrow(vehicles1998_2019) # 7094291

# Format stat19 data
vehicles1998_2019 = format_vehicles(vehicles1998_2019)

str(vehicles1998_2019)

# Save to make easier
saveRDS(vehicles1998_2019, file = "./Downloads/1979/dft-road-casualty-statistics-vehicles-1998-2019.Rds")
