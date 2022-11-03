##############################################################################################
#            Analyse crashes involving pedal cycles within 10m buffer of contraflows         #
#                                                                                            #
# This code file:                                                                            #
# - takes the crashes involving a pedal cycle within 10m buffer of contraflow segments       #
# - only analyses those with a known contraflow start date                                   #
# - tidies up the dataframe eg factoring, removing columns wont use, condense categories     #                                                         #
# - joins the casualties and vehicles involved in each crash to the data frame               #
# - then identifies the crashes that are single bicycle crashes and removes them             #
# - then identifies the self-reported crashes and removes them                               #  
# - creates 'Table 1' - crashes, vehicles, casualties by crash time scale                    #
# - creates 'Table 1' - contraflows by whether crash or not                                  #
# - creates variable that identifies whether crashes are within 10m of a junction (OSM data) #                                                                            #
#                                                                                            #
##############################################################################################


# Load packages
library(tidyverse)
library(sf)
library(mapview)
library(leafsync)
library(lubridate)
library(tableone)

# Load datasets
unbuff_10 = readRDS(file = "data-processed/step_4_S19_joined_to_contraflows/unbuffered_pedal_cycle_crashes_10mcontraflow_30_05_2022.Rds")
unique_tro_df = readRDS(file = "data-processed/unique_tro_df_durations_using_start_date.Rds")

# Factor, relabel and reorder crash time scale
cols_factor = c("crash_time_scale", "crash_time_scale_derived")
unbuff_10 = unbuff_10 %>%
  mutate_at(cols_factor, factor) %>%
  mutate(crash_time_scale = fct_recode(crash_time_scale, Pre_contraflow = "Before", 
                                       Contraflow = "During", Contraflow_removed = "After")) %>%
  mutate(crash_time_scale_derived = fct_recode(crash_time_scale_derived, Pre_contraflow = "Before", 
                                       Contraflow = "During", Contraflow_removed = "After")) %>%
  mutate(crash_time_scale = fct_relevel(crash_time_scale, "Pre_contraflow", "Contraflow", "Contraflow_removed")) %>%
  mutate(crash_time_scale_derived = fct_relevel(crash_time_scale_derived, "Pre_contraflow", "Contraflow", "Contraflow_removed"))

mapview(unbuff_10, zcol = "crash_time_scale") + mapview(unique_tro_df)

# Count crashes by time scale
unbuff_10 %>% st_drop_geometry() %>%
  group_by(crash_time_scale) %>%
  summarise(count = n())
# crash_time_scale   count
# <fct>              <int>
# 1 Pre_contraflow       824
# 2 Contraflow           831
# 3 Contraflow_removed     8
# 4 NA                   111


# Look at number of road segments that have crashes assoc with them based on whether we know or have imputed contraflow start date
unbuff_10 %>% st_drop_geometry() %>%
  filter(!is.na(crash_time_scale)) %>% 
  group_by(unique_contraflow_ID) %>%
  summarise(count = n()) %>%
  nrow()# 317 is the number of unique road_segments that have crashes on where the contraflow start date is known
508-317 # 191 unique road segments segments have no crashes assoc with them in only use contraflows when start date is known 
# this is made up of some road segments that have no crashes on and some that have crashes on but we dont know the start date. 

unbuff_10 %>% st_drop_geometry() %>%
  group_by(unique_contraflow_ID) %>%
  summarise(count = n()) %>%
  nrow()# 336
508-336 # 172 so 172 road segments had no crashes on at all.  
336-317 # 19 road segments without a contraflow start date had crashes on.

# how many roade segments have the contraflow removed?  
unbuff_10 %>% st_drop_geometry() %>%
  filter(crash_time_scale == "Contraflow_removed") %>%
  group_by(unique_contraflow_ID) %>%
  summarise(count = n()) # n = 2 contraflow segments have contraflows removed

# Check road segment status duration so that it adds up to 8035 rather than 8034 days
time_total_check = unbuff_10 %>%
  mutate(time_total_check1 = pre_contraflow_duration + contraflow_duration + post_contraflow_duration) %>%
  mutate(time_total_check2 = pre_contraflow_duration + contraflow_duration) %>%
  mutate(time_total_check = coalesce(time_total_check1, time_total_check2)) %>%
  select(c(time_total_check)) # should all be 8035 days apart from those that are NA because dont have contraflow start date


################################################################################
#         Only keep crashes where we know the start date                       #
################################################################################

# Remove crashes where we dont have contraflow start date
unbuff_10 = unbuff_10 %>% filter(!is.na(contraflow_start_date)) %>%
  select(-c(crash_time_scale_derived)) # n = 1663




################################################################################
#           Obtain duration of contraflow existence in years                   #
################################################################################

# Get dataframe of duration of the different contraflow states
duration = unique_tro_df %>% st_drop_geometry() %>%
  select(c(unique_contraflow_ID, contraflow_start_date, pre_contraflow_duration, contraflow_duration, post_contraflow_duration)) %>%
  rename(Pre_contraflow = pre_contraflow_duration) %>%
  rename(Contraflow = contraflow_duration) %>%
  rename(Contraflow_removed = post_contraflow_duration)

# Pivot the duration_not_imputed df so can join to the num_crashes for crash rate calculation
duration_long = duration %>%
  select(c(unique_contraflow_ID, contraflow_start_date, Pre_contraflow, Contraflow, Contraflow_removed)) %>%
  pivot_longer(cols = Pre_contraflow:Contraflow_removed, names_to = "crash_time_scale", values_to = "duration_days") %>%
  mutate(crash_time_scale = factor(crash_time_scale, levels = c("Pre_contraflow", "Contraflow", "Contraflow_removed")))



################################################################################
#        Join the duration of contraflow status to the unbuff dataframe        #
################################################################################

# Remove contraflow_start_date column from duration_long
duration_long2 = duration_long %>% select(-c("contraflow_start_date"))

# Join using the unique_contraflow_ID and the crash_time_scale
unbuff2 = left_join(unbuff_10, duration_long2, by = c("unique_contraflow_ID", "crash_time_scale"))


################################################################################
#             Characterise crashes where start date is known                   #
################################################################################

# Tidy dataframe by removing columns wont use eg stuff about the TRO date, type etc
unbuff2 = unbuff2 %>%
  select(1:11, 13:17, 19:41, 45:47, 66:68)

# Create some variables that condense data 
cols = c("police_force", "accident_severity", "local_authority_district", "first_road_class",
         "road_type", "junction_detail", "junction_control", "second_road_class", "pedestrian_crossing_human_control",
         "pedestrian_crossing_physical_facilities", "light_conditions", "weather_conditions", "road_surface_conditions",
         "special_conditions_at_site", "carriageway_hazards", "did_police_officer_attend_scene_of_accident")
unbuff3 = unbuff2 %>%
  mutate_at(cols, factor) %>%
  mutate(junction_detail_condensed = fct_collapse(junction_detail,
         junction_roundabout_other = c("Crossroads", "T or staggered junction", "More than 4 arms (not roundabout)", 
                      "Other junction", "Mini-roundabout", "Roundabout", "Slip road", "Private drive or entrance"))) %>%
  mutate(hour = lubridate::hour(time)) %>%
  mutate(day_of_week = factor(day_of_week, levels = c("Monday", "Tuesday", "Wednesday",
                                                      "Thursday", "Friday", "Saturday", "Sunday"))) %>%
  mutate(lights_conditions_condensed = fct_collapse(light_conditions, Darkness = c("Darkness - lights lit", "Darkness - no lighting",
                                                                                   "Darkness - lighting unknown", "Darkness - lights unlit"))) %>%
  mutate(weather_conditions_condensed = fct_collapse(weather_conditions, 
                                                     Fine = c("Fine no high winds", "Fine + high winds"),
                                                     Rain_snow_fog_other = c("Raining no high winds", "Raining + high winds", "Fog or mist", 
                                                                             "Other", "Snowing no high winds"))) %>%
  mutate(road_surface_conditions_condensed = fct_collapse(road_surface_conditions, 
                                                          Wet_ice_mud = c("Frost or ice", "Mud", "Wet or damp"),
                                                          Unknown = c("unknown (self reported)", "Data missing or out of range"))) %>%
  mutate(carriageway_hazards_condensed = fct_collapse(carriageway_hazards, 
                                                      Object_on_road = c("Other object on road", "Pedestrian in carriageway - not injured",
                                                                         "Previous accident", "Vehicle load on road")))

unbuff3$lights_conditions_condensed = fct_infreq(unbuff3$lights_conditions_condensed)
unbuff3$did_police_officer_attend_scene_of_accident = fct_infreq(unbuff3$did_police_officer_attend_scene_of_accident)
unbuff3$road_type = fct_infreq(unbuff3$road_type)
unbuff3$road_surface_conditions_condensed = fct_infreq(unbuff3$road_surface_conditions_condensed)






################################################################################
#                       Identify casualties involved                           #
################################################################################

sum(unbuff3$number_of_casualties) # 1733 casualties in our 1663 crashes

# How many casualties are involved per crash? 
unbuff3 %>% st_drop_geometry() %>%
  group_by(number_of_casualties) %>%
  summarise(count = n())
# number_of_casualties count
# 1                    1  1602
# 2                    2    58
# 3                    3     1
# 4                    4     1
# 5                    8     1

# Load casualty dataset
cas1998_2019 = readRDS(file = "./Downloads/1979/dft-road-casualty-statistics-casualties-1998-2019.Rds")

names(cas1998_2019) 
# [1] "accident_index"                     "accident_year"                     
# [3] "accident_reference"                 "vehicle_reference"                 
# [5] "casualty_reference"                 "casualty_class"                    
# [7] "sex_of_casualty"                    "age_of_casualty"                   
# [9] "age_band_of_casualty"               "casualty_severity"                 
# [11] "pedestrian_location"                "pedestrian_movement"               
# [13] "car_passenger"                      "bus_or_coach_passenger"            
# [15] "pedestrian_road_maintenance_worker" "casualty_type"                     
# [17] "casualty_home_area_type"            "casualty_imd_decile"

# Identify casualties for our crashes 
casualties = cas1998_2019 %>%
  filter(accident_index %in% unbuff3$accident_index) # n = 1733

casualties %>%
  group_by(casualty_class) %>%
  summarise(count = n())
# casualty_class  count
# 1 Driver or rider  1603
# 2 Passenger          14
# 3 Pedestrian        116

casualties %>%
  group_by(casualty_type) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
# casualty_type                                             count
# 1 Cyclist                                                    1582
# 2 Pedestrian                                                  116
# 3 Motorcycle 125cc and under rider or passenger                 9
# 4 Car (including private hire cars) (1979-2004)                 8
# 5 Motorcycle over 125cc (1999-2004)                             5
# 6 Taxi/Private hire car occupant                                3
# 7 Car occupant                                                  2
# 8 Motorcycle over 125cc and up to 500cc rider or  passenger     2
# 9 Motorcycle over 500cc rider or passenger                      2
# 10 Agricultural vehicle occupant                                 1
# 11 Bus or coach occupant (17 or more pass seats)                 1
# 12 Motorcycle (1979-1998)                                        1
# 13 Van / Goods vehicle (3.5 tonnes mgw or under) occupant        1

# create smaller number of categories
casualties = casualties %>%
  mutate(casualty_type_derived = factor(casualty_type)) %>%
  mutate(casualty_type_derived = fct_collapse(casualty_type_derived,
                                             Motorcyclist = c(levels(casualty_type_derived)[startsWith(levels(casualty_type_derived), "Motorcycle")]),
                                             Car = c(levels(casualty_type_derived)[startsWith(levels(casualty_type_derived), "Car")]))) %>%
  mutate(casualty_type_derived = fct_lump_n(casualty_type_derived, n = 4))

casualties %>%
  group_by(casualty_type_derived) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
# casualty_type_derived count
# 1 Cyclist                1582
# 2 Pedestrian              116
# 3 Motorcyclist             19
# 4 Car                      10
# 5 Other                     6

# ped = casualties %>% filter(casualty_class == "Pedestrian")
# sum(ped$casualty_class == ped$casualty_type) # = 116 so all pedestrians are correctly
# # in both casualty class and casualty type

casualties %>%
  group_by(pedestrian_location) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
# pedestrian_location                                   count
# 1 Not a Pedestrian                                       1617
# 2 In carriageway, crossing elsewhere                       48
# 3 Crossing on pedestrian crossing facility                 28
# 4 Crossing elsewhere within 50m. of pedestrian crossing    25
# 5 Unknown or other                                          9
# 6 In carriageway, not crossing                              3
# 7 On footway or verge                                       2
# 8 On refuge, central island or central reservation          1

casualties %>%
  group_by(pedestrian_movement) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
# pedestrian_movement                                          count
# 1 Not a Pedestrian                                                 1617
# 2 Crossing from driver's nearside                                    64
# 3 Unknown or other                                                   24
# 4 Crossing from driver's offside                                     15
# 5 Crossing from nearside - masked by parked or stationary vehicle     7
# 6 Crossing from offside - masked by  parked or stationary vehicle     6

# What happens casualties if I only keep pedestrians and cyclists? 
casualties_c_p = casualties %>%
  filter(casualty_type == "Pedestrian" | casualty_type == "Cyclist") # n = 1698 ie less than our number of crashes(1774)
casualties_c_p %>%
  group_by(accident_index) %>%
  summarise(count = n()) %>%
  group_by(count) %>%
  summarise(number_of_casualties_per_crash = n()) # n = 1651
# count number_of_casualties_per_crash
# 1     1                           1606 crashes had 1 pedestrian or cyclist casualties
# 2     2                             43    "        2      "        "
# 3     3                              2    "        3      "        "
#This may indicates that not all ouf our crashes may have a pedestrian or cyclist casualty. 

# Add column in crash df to indicate whether a pedestrian or cyclist casualty occurred - NB this wont indicate how many of each
cyclist_cas = casualties_c_p %>% filter(casualty_type == "Cyclist") # n = 1582
ped_cas = casualties_c_p %>% filter(casualty_type == "Pedestrian") # 116
unbuff4 = unbuff3 %>%
  mutate(cyclist_casualty = case_when(accident_index %in% cyclist_cas$accident_index ~ TRUE, TRUE ~ FALSE)) %>%
  mutate(pedestrian_casualty = case_when(accident_index %in% ped_cas$accident_index ~ TRUE, TRUE ~ FALSE)) 

# How many crashes had pedestrian, cyclist or both ped and cyclist casualties
unbuff4 %>% st_drop_geometry() %>% filter(cyclist_casualty == TRUE & pedestrian_casualty == FALSE) # n = 1538
unbuff4 %>% st_drop_geometry() %>% filter(cyclist_casualty == FALSE & pedestrian_casualty == TRUE) # n = 82
unbuff4 %>% st_drop_geometry() %>% filter(cyclist_casualty == TRUE & pedestrian_casualty == TRUE) # n = 31

# Do all of our crashes have a cyclist or pedestrian casualty?  
check_casualties = unbuff4 %>% st_drop_geometry() %>%
  filter(cyclist_casualty == FALSE & pedestrian_casualty == FALSE) # n = 12 
# so 12 of our crashes involving pedal cyclists do not have a cyclist or pedestrian casualty
check_cyc_cas = unbuff4 %>% st_drop_geometry() %>%
  filter(cyclist_casualty == FALSE) # n = 94 crashes do not have a cyclist casualty

casualties %>% filter(accident_index %in% check_casualties$accident_index) %>%
  group_by(casualty_type_derived) %>% summarise(count = n()) %>% arrange(desc(count))
# casualty_type_derived count
# <fct>                 <int>
# 1 Motorcyclist              8
# 2 Car                       2
# 3 Other                     2  1 agric vehicle occupant, 1 bus occupant

casualties_c_p %>%
  group_by(casualty_class) %>%
  summarise(count = n())

# casualty_class  count
# 1 Driver or rider  1578
# 2 Passenger           4
# 3 Pedestrian        116

# Create casualty dataframe so can join to our crash dataframe
casualties_c_p_pivot = casualties_c_p %>%
  select(c(1, 5, 4, 10, 11, 19)) %>%
  pivot_wider(names_from = "casualty_reference",
              names_glue = "cas_{casualty_reference}_{.value}",
              values_from = c("vehicle_reference","casualty_type_derived", "casualty_severity", "pedestrian_location")) 
casualties_c_p_pivot = casualties_c_p_pivot %>%
  select(c(1, 2, 6, 10, 14, 3, 7, 11, 15, 5, 9, 13, 17, 4, 8, 12, 16)) # reorder columns so make sense
nrow(casualties_c_p_pivot) # 1651

casualties_c_p %>%
  group_by(accident_index) %>%
  summarise(count = n()) %>%
  group_by(count) %>%
  summarise(number_of_casualties_per_crash = n()) # these total 1651 so that is correct - we have pivoted and got the right number :)
# count number_of_casualties_per_crash
# 1     1                           1606
# 2     2                             43
# 3     3                              2

# Join casualty dataframe to our crash dataset
crashes_casualties = left_join(unbuff4, casualties_c_p_pivot, by = "accident_index") # n = 1663 
# This means I keep the 12 crashes where there is no pedestrian or cyclist casualty

# Save this dataframe
saveRDS(crashes_casualties, file = "data-processed/stats19/pedal_cycle_crashes_with_cyclist_or_ped_casualties_30_05_2022.RDs")


# Identify how many crashes dont have cyclist/pedestrian casualties
crashes_without_ped_cyc_cas = left_join(unbuff4, casualties_c_p) %>% filter(accident_index %in% check_casualties$accident_index) %>% st_drop_geometry()
crashes_without_ped_cyc_cas %>% group_by(crash_time_scale, casualty_type_derived) %>% summarise(n_crashes = n())
# crash_time_scale   casualty_type_derived n_crashes
# <fct>              <fct>                     <int>
# 1 Pre_contraflow     NA                            4
# 2 Contraflow         NA                            7
# 3 Contraflow_removed NA                            1


################################################################################
#                             Join the vehicle dataset                         #
################################################################################

# Load vehicle dataset
veh1998_2019 = readRDS(file = "./Downloads/1979/dft-road-casualty-statistics-vehicles-1998-2019.Rds")

crashes_casualties %>% st_drop_geometry() %>%
  group_by(number_of_vehicles) %>%
  summarise(count = n())
# number_of_vehicles count
# 1                  1   174
# 2                  2  1467
# 3                  3    21
# 4                  4     1

names(vehicles)
# [1] "accident_index"                   "accident_year"                   
# [3] "accident_reference"               "vehicle_reference"               
# [5] "vehicle_type"                     "towing_and_articulation"         
# [7] "vehicle_manoeuvre"                "vehicle_direction_from"          
# [9] "vehicle_direction_to"             "vehicle_location_restricted_lane"
# [11] "junction_location"                "skidding_and_overturning"        
# [13] "hit_object_in_carriageway"        "vehicle_leaving_carriageway"     
# [15] "hit_object_off_carriageway"       "first_point_of_impact"           
# [17] "vehicle_left_hand_drive"          "journey_purpose_of_driver"       
# [19] "sex_of_driver"                    "age_of_driver"                   
# [21] "age_band_of_driver"               "engine_capacity_cc"              
# [23] "propulsion_code"                  "age_of_vehicle"                  
# [25] "generic_make_model"               "driver_imd_decile"               
# [27] "driver_home_area_type" 

# Identify vehicles for our crashes 
vehicles = veh1998_2019 %>%
  filter(accident_index %in% unbuff3$accident_index) # n = 3175

# Tidy up vehicle_type to get fewer categories
vehicles = vehicles %>%
  mutate(vehicle_type_derived = factor(vehicle_type)) %>%
  mutate(vehicle_type_derived = fct_collapse(vehicle_type_derived,
                                             Motorcycle = c(levels(vehicle_type_derived)[startsWith(levels(vehicle_type_derived), "Motorcycle")]),
                                             HGV = c(levels(vehicle_type_derived)[startsWith(levels(vehicle_type_derived), "Goods")]),
                                             LGV = c("Van / Goods 3.5 tonnes mgw or under"),
                                             Car = c(levels(vehicle_type_derived)[startsWith(levels(vehicle_type_derived), "Car")]),
                                             Bus_coach_minibus = c("Bus or coach (17 or more pass seats)", "Minibus (8 - 16 passenger seats)"),
                                             Taxi = c("Taxi (excluding private hire cars) (1979-2004)", "Taxi/Private hire car"),
                                             Other_unknown = c("Agricultural vehicle", "Other vehicle", "Unknown vehicle type (self rep only)")))
#saveRDS(crashes_casualties_vehicles, file = "data-processed/stats19/pedal_cycle_crashes_with_casualties_&_vehicles.RDs")
vehicles%>% group_by(vehicle_type_derived) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
# vehicle_type_derived count
# 1 Pedal cycle           1684
# 2 Car                    873
# 3 LGV                    195
# 4 Taxi                   184
# 5 Bus_coach_minibus       87
# 6 Motorcycle              71
# 7 HGV                     66
# 8 Other_unknown           15


# Create vehicles dataframe so can join to our crash_casualties dataframe
vehicles_pivot = vehicles %>%
  select(c(1, 4, 28, 7, 8, 9, 10)) %>%
  pivot_wider(names_from = "vehicle_reference", 
              names_glue = "veh_{vehicle_reference}_{.value}",
              values_from = c("vehicle_type_derived", "vehicle_manoeuvre", 
                              "vehicle_direction_from", "vehicle_direction_to",
                              "vehicle_location_restricted_lane"))
nrow(vehicles_pivot) # 1633

# Join vehicles dataframe to our crashes_casualties dataset
crashes_casualties_vehicles = left_join(crashes_casualties, vehicles_pivot, by = "accident_index") # n = 1663 
# This means I keep the 12 crashes where there is no pedestrian or cyclist casualty

# Save this dataframe
saveRDS(crashes_casualties_vehicles, file = "data-processed/stats19/pedal_cycle_crashes_with_casualties_&_vehicles_30_05_22.RDs")


################################################################################
#      Now make changes to the crashes_casualties_vehicles df requested        #
#                           during peer review                                 # 
################################################################################
# 1) Remove crashes that are self-reported                                     #
# - this decision was requested following peer review by AAP                   #  
# - rationale is that self-reporting was possible from 2016 onwards.           #
# - as contraflow segments are more likely in 2016, these selfreported crashes #
#   are more likely to have occure on such a segments                          #
# - also people with 'slight injuries' are more likely to have self-reported   #
#   resulting in overrepresentation of these                                   #
################################################################################


crashes_casualties_vehicles %>%
  st_drop_geometry() %>%
  group_by(did_police_officer_attend_scene_of_accident) %>%
  summarise(count = n())
# did_police_officer_attend_scene_of…¹ count
# 1 Yes                                   1103
# 2 No                                     429
# 3 No - accident was reported using a …    66
# 4 Data missing or out of range            65

missing = crashes_casualties_vehicles %>% 
  st_drop_geometry() %>%
  filter(did_police_officer_attend_scene_of_accident == "Data missing or out of range")
# Missing ones are all in 1998

self_report = crashes_casualties_vehicles %>% 
  st_drop_geometry() %>%
  filter(did_police_officer_attend_scene_of_accident == "No - accident was reported using a self completion  form (self rep only)")
 
unique(self_report$accident_year)  # 2016 2019 - so none from 2017 and 2018. Looks like some crashes incorrectly coded

# Identify crashes where unknown self report is used in key crash variables
crashes_casualties_vehicles_filter_unknown = crashes_casualties_vehicles %>%
  filter(junction_detail == "unknown (self reported)" |
           junction_control == "unknown (self reported)" |
           pedestrian_crossing_human_control == "unknown (self reported)" |
           pedestrian_crossing_physical_facilities  == "unknown (self reported)" |
           road_surface_conditions== "unknown (self reported)" |
           special_conditions_at_site == "unknown (self reported)" |
           carriageway_hazards == "unknown (self reported)")
unique(crashes_casualties_vehicles_filter_unknown$accident_year) # 2016 2017 2018 2019 # n = 78
# so therefore remove these observations 
crashes_casualties_vehicles_1 = crashes_casualties_vehicles %>%
  filter(!accident_index %in% crashes_casualties_vehicles_filter_unknown$accident_index)
# n = 1585 ie 1663 - 78 

# now remove any other observations where 'No - accident was reported using a self completion  form'

crashes_casualties_vehicles_nonselfrep = crashes_casualties_vehicles_1 %>%
  filter(did_police_officer_attend_scene_of_accident != "No - accident was reported using a self completion  form (self rep only)")
# n = 1556

# so in total 107 crashes were removed as 'self-reported'. 




################################################################################
# 2) Remove single bicycle crashes                                             #
# - rationale:                                                                 #
# - unlikely that introducing contraflow cycling would affect SBC              #
# - SBC likely to be vastly under-reported in police crash data anyway         #
################################################################################

crashes_casualties_vehicles_nonselfrep %>% 
  st_drop_geometry() %>%
  group_by(number_of_vehicles) %>%
  summarise(count = n())

# number_of_vehicles count
# <dbl> <int>
# 1                  1   161
# 2                  2  1374
# 3                  3    20
# 4                  4     1

sbc = crashes_casualties_vehicles_nonselfrep %>% 
  filter(number_of_vehicles == 1)

sbc %>%
  st_drop_geometry() %>%
  group_by(number_of_casualties, cyclist_casualty, pedestrian_casualty) %>%
  summarise(count = n())

# number_of_casualties count
# <dbl> <int>
#                     1   130
#                     2    30
#                     3     1

# number_of_casualties cyclist_casualty pedestrian_casualty count
#                     1 FALSE            TRUE                   72  1 casualty - pedestrian
#                     1 TRUE             FALSE                  58  1 casualty - cyclist
#                     2 FALSE            TRUE                    1  2 casualties = both pedestrians
#                     2 TRUE             FALSE                   1  2 casualties = both cyclists
#                     2 TRUE             TRUE                   28  2 casualties - 1 cyclist and 1 pedestrian
#                     3 TRUE             TRUE                    1  3 casualties - includes cyclist and pedestrians


sbc %>%
  st_drop_geometry() %>%
  group_by(number_of_casualties, cas_1_casualty_type_derived, cas_2_casualty_type_derived,
           cas_3_casualty_type_derived, cas_4_casualty_type_derived) %>%
  summarise(count = n())
# number_of_casualties cas_1_casualty_type_derived cas_2_casualty_type_derived cas_3_casualty_type_der…¹ cas_4…² count
# 1                    1 Cyclist                     NA                          NA                        NA         58
# 2                    1 Pedestrian                  NA                          NA                        NA         72
# 3                    2 Cyclist                     Cyclist                     NA                        NA          1
# 4                    2 Cyclist                     Pedestrian                  NA                        NA         24
# 5                    2 Pedestrian                  Cyclist                     NA                        NA          4
# 6                    2 Pedestrian                  Pedestrian                  NA                        NA          1
# 7                    3 Cyclist                     Pedestrian                  Pedestrian                NA          1

# There are 58 crashes involving just one vehicle (pedal cycle) where there is only a cyclist casualty.

sbc_cyc_cas = sbc %>%
  filter(number_of_vehicles == 1 & number_of_casualties == 1 & cyclist_casualty == TRUE)
# n = 58

sbc_cyc_cas %>%
  st_drop_geometry() %>%
  group_by(crash_time_scale) %>%
  summarise(count = n())
# crash_time_scale count
# 1 Pre_contraflow      24
# 2 Contraflow          34

sbc_cyc_cas_accident_index = sbc_cyc_cas$accident_index

# Remove presumed SBC
crashes_casualties_vehicles_AAP = crashes_casualties_vehicles_nonselfrep %>%
  filter(!accident_index %in% sbc_cyc_cas_accident_index)
# n = 1498 ie 1556 - 58

crashes_casualties_vehicles_AAP %>%
  st_drop_geometry() %>%
  group_by(number_of_vehicles, cas_1_casualty_type_derived, cas_2_casualty_type_derived,
           cas_3_casualty_type_derived, cas_4_casualty_type_derived) %>%
  summarise(count = n()) 
# number_of_vehicles cas_1_casualty_type_derived cas_2_casualty_type_derived cas_3_casualty_type_derived cas_4_casualty_type_derived count
# 1                  1 Cyclist                     Cyclist                     NA                          NA                              1
# 2                  1 Cyclist                     Pedestrian                  Pedestrian                  NA                              1
# 3                  1 Cyclist                     Pedestrian                  NA                          NA                             24
# 4                  1 Pedestrian                  Cyclist                     NA                          NA                              4
# 5                  1 Pedestrian                  Pedestrian                  NA                          NA                              1
# 6                  1 Pedestrian                  NA                          NA                          NA                             72
# 7                  2 Cyclist                     Cyclist                     NA                          NA                              8
# 8                  2 Cyclist                     NA                          NA                          NA                           1342
# 9                  2 Pedestrian                  Cyclist                     NA                          NA                              1
# 10                  2 Pedestrian                  NA                          NA                          NA                              2
# 11                  2 NA                          Cyclist                     NA                          NA                              9
# 12                  2 NA                          NA                          NA                          NA                             12
# 13                  3 Cyclist                     Cyclist                     NA                          NA                              3
# 14                  3 Cyclist                     NA                          NA                          NA                             15
# 15                  3 NA                          Cyclist                     Pedestrian                  Pedestrian                      1
# 16                  3 NA                          Cyclist                     NA                          NA                              1
# 17                  4 NA                          NA                          NA                          Cyclist                         1

# so can see I have successfully dropped the single bicycle crashes - 1 vehicle and only a cyclist casualty 

# Save this dataframe
saveRDS(crashes_casualties_vehicles_AAP, file = "data-processed/stats19/pedal_cycle_crashes_with_casualties_&_vehicles_AAP_27_09_22.RDs")



################################################################################
#                    Now create the various tables and counts                  #
#                            with this new dataset                             #    
################################################################################

crashes_casualties_vehicles_AAP = readRDS("data-processed/stats19/pedal_cycle_crashes_with_casualties_&_vehicles_AAP_27_09_22.RDs")
# Count crashes by time scale
crashes_casualties_vehicles_AAP %>% st_drop_geometry() %>%
  group_by(crash_time_scale) %>%
  summarise(count = n())

# crash_time_scale   count
# 1 Pre_contraflow       788
# 2 Contraflow           703
# 3 Contraflow_removed     7


# Count number of road segments with crashes on them
crashes_casualties_vehicles_AAP %>% st_drop_geometry() %>%
  group_by(unique_contraflow_ID) %>%
  summarise(count = n()) %>%
  nrow()# 306 is the number of unique road_segments that have crashes on where the contraflow start date is known
508-306 # 20 unique road segments segments have no crashes assoc with them in only use contraflows when start date is known 
# # this is made up of some road segments that have no crashes on and some that have crashes on but we dont know the start date. 


# Create Table 1 of the variables
crashes_casualties_vehicles_AAP_no_geo = crashes_casualties_vehicles_AAP%>% st_drop_geometry() # Drop geometry and contraflow length

dput(names(crashes_casualties_vehicles_AAP_no_geo)) # get list of variables
list_vars = c("number_of_vehicles", "number_of_casualties", "accident_severity",
              "did_police_officer_attend_scene_of_accident",
              "day_of_week", "hour", "speed_limit", "first_road_class", "road_type",
              "junction_detail_condensed", "lights_conditions_condensed",
              "weather_conditions_condensed", "road_surface_conditions_condensed",
              "carriageway_hazards_condensed")
table1 = CreateTableOne(list_vars, strata = "crash_time_scale", data = crashes_casualties_vehicles_AAP_no_geo)
tab1mat = print(table1, quote = FALSE, noSpaces = TRUE, printToggle = FALSE, test = FALSE, showAllLevels = TRUE)
write.csv(tab1mat, file = "output/table_1_crashes_charac_27_09_22.csv")

cyc_ped_cas_data = crashes_casualties_vehicles_AAP_no_geo %>%
  select(c(43, 51, 52))
cyc = cyc_ped_cas_data %>%
  group_by(crash_time_scale, cyclist_casualty) %>%
  summarise(count = n()) %>%
  group_by(crash_time_scale) %>% 
  mutate(total_by_time_scale = sum(count),
         cyc_cas_perc = round(count/total_by_time_scale * 100, digits = 1))
# crash_time_scale   cyclist_casualty count total_by_time_scale cyc_cas_perc
# 1 Pre_contraflow     FALSE               35                 788          4.4
# 2 Pre_contraflow     TRUE               753                 788         95.6
# 3 Contraflow         FALSE               51                 703          7.3
# 4 Contraflow         TRUE               652                 703         92.7
# 5 Contraflow_removed FALSE                1                   7         14.3
# 6 Contraflow_removed TRUE                 6                   7         85.7  

ped = cyc_ped_cas_data %>%
  group_by(crash_time_scale, pedestrian_casualty) %>%
  summarise(count = n()) %>%
  group_by(crash_time_scale) %>% 
  mutate(total_by_time_scale = sum(count),
         ped_cas_perc = round(count/total_by_time_scale * 100, digits = 1))
# crash_time_scale   pedestrian_casualty count total_by_time_scale ped_cas_perc
#  1 Pre_contraflow     FALSE                 746                 788         94.7
# 2 Pre_contraflow     TRUE                   42                 788          5.3
# 3 Contraflow         FALSE                 640                 703         91  
# 4 Contraflow         TRUE                   63                 703          9  
# 5 Contraflow_removed FALSE                   6                   7         85.7
# 6 Contraflow_removed TRUE                    1                   7         14.3


# Table 1 for casualties of the crashes 
casualties_crashes = left_join(crashes_casualties_vehicles_AAP_no_geo, casualties, by = "accident_index") # n = 1567

casualties_crashes %>%
  group_by(number_of_casualties) %>%
  summarise(count = n())
# number_of_casualties count
# 1                    1  1438
# 2                    2   114
# 3                    3     3
# 4                    4     4
# 5                    8     8
1438+114+3+4+8  # n = 1567
unique(casualties_crashes$accident_index) # 1498 unique crashes ie correct

names(casualties_crashes)
casualties_crashes %>%
  group_by(crash_time_scale,casualty_type_derived) %>%
  summarise(count = n())
# crash_time_scale   casualty_type_derived count
# 1 Pre_contraflow     Car                       8
# 2 Pre_contraflow     Cyclist                 755
# 3 Pre_contraflow     Motorcyclist             10
# 4 Pre_contraflow     Pedestrian               44
# 5 Pre_contraflow     Other                     2
# 6 Contraflow         Car                       2
# 7 Contraflow         Cyclist                 662
# 8 Contraflow         Motorcyclist              9
# 9 Contraflow         Pedestrian               64
# 10 Contraflow         Other                     3
# 11 Contraflow_removed Cyclist                   6
# 12 Contraflow_removed Pedestrian                1
# 13 Contraflow_removed Other                     1
cas_crash_t1 = casualties_crashes %>%
  select(c(1, 4, 24, 5, 19, 10, 62, 63))
dput(names(cas_crash_t1)) # get list of variables
cas_vars = c("casualty_type_derived", "casualty_severity")
table1_cas = CreateTableOne(cas_vars, strata = "crash_time_scale", data = casualties_crashes)
tab1casmat = print(table1_cas, noSpaces = TRUE, printToggle = FALSE, test = FALSE, showAllLevels = TRUE)
write.csv(tab1casmat, file = "output/table2_cas_28_09_22.csv")
 
# Look at who was killed etc
ksi_df = casualties_crashes %>% select(c(1, 97, 106, 43)) %>%
  group_by(crash_time_scale, casualty_severity, casualty_type_derived) %>%
  summarise(count = n())

pivot_casualties_ksi = ksi_df %>%
  pivot_wider(names_from = crash_time_scale, values_from = count) %>%
  mutate(casualty_type_derived = fct_relevel(casualty_type_derived, c("Motorcyclist", "Car"), after = 2)) %>%
  arrange(casualty_severity, casualty_type_derived) %>%
  replace_na(list(Pre_contraflow = 0, Contraflow = 0, Contraflow_removed = 0)) %>%
  mutate(Total = Pre_contraflow + Contraflow + Contraflow_removed)
# casualty_severity casualty_type_derived Pre_contraflow Contraflow Contraflow_removed Total
# 1 Fatal             Cyclist                            6          3                  0     9
# 2 Fatal             Pedestrian                         0          1                  0     1
# 3 Serious           Cyclist                           77         81                  1   159
# 4 Serious           Pedestrian                        12         19                  0    31
# 5 Serious           Motorcyclist                       1          1                  0     2
# 6 Serious           Car                                1          0                  0     1
# 7 Slight            Cyclist                          678        606                  6  1290
# 8 Slight            Pedestrian                        33         46                  1    80
# 9 Slight            Motorcyclist                       9          8                  0    17
# 10 Slight            Car                                7          2                  0     9
# 11 Slight            Other                              2          3                  1     6

write_csv(pivot_casualties_ksi, file = "output/table2_ksi_cas_26_09_22.csv")

# Crashes involving cyclist casualities
crashes_casualties_vehicles_AAP_no_geo %>%
  group_by(crash_time_scale, cyclist_casualty) %>%
  summarise(n_crashes_cyc_cas = n()) %>%
  group_by(crash_time_scale) %>%
  mutate(total_crashes_by_time_scale = sum(n_crashes_cyc_cas)) %>%
  ungroup() %>%
  mutate(perc_crashes_cyc_cas = round(n_crashes_cyc_cas/total_crashes_by_time_scale *100, digits = 1)) %>%
  filter(cyclist_casualty == TRUE)

# Crashes involving pedestrian casualities
crashes_casualties_vehicles_AAP_no_geo %>%
  group_by(crash_time_scale, pedestrian_casualty) %>%
  summarise(n_crashes_ped_cas = n()) %>%
  group_by(crash_time_scale) %>%
  mutate(total_crashes_by_time_scale = sum(n_crashes_ped_cas)) %>%
  ungroup() %>%
  mutate(perc_crashes_ped_cas = round(n_crashes_ped_cas/total_crashes_by_time_scale *100, digits = 1)) %>%
  filter(pedestrian_casualty == TRUE)

# Table 1 for vehicles of the crashes
vehicles_crashes = left_join(crashes_casualties_vehicles_AAP_no_geo, vehicles, by = "accident_index") # n = 2915
vehicles_crashes %>%
  group_by(number_of_vehicles) %>%
  summarise(count = n())
# number_of_vehicles count
# <dbl> <int>
# 1                  1   103
# 2                  2  2748
# 3                  3    60
# 4                  4     4

names(vehicles_crashes)
list_vars = c("vehicle_type_derived", "vehicle_location_restricted_lane")
table1_veh = CreateTableOne(list_vars, strata = "crash_time_scale", data = vehicles_crashes)
tab1vehmat = print(table1_veh, noSpaces = TRUE, printToggle = FALSE, test = FALSE, showAllLevels = TRUE)
write.csv(tab1vehmat, file = "output/tables_veh_28_09_22.csv")

# Get data on type of vehicles involved in crashes
veh_type = crashes_casualties_vehicles_AAP_no_geo %>%
  select(c(1, 43, 5, 69:72)) %>%
  group_by(crash_time_scale, veh_1_vehicle_type_derived, veh_2_vehicle_type_derived, veh_3_vehicle_type_derived, veh_4_vehicle_type_derived) %>%
  summarise(num_crashes = n()) %>%
  select(c(1,6, 2:5)) %>%
  arrange(desc(num_crashes)) %>%
  ungroup()
sum(veh_type$num_crashes) # 1498 ie each crash has one vehicle combo

# NB 'Pedal cycle only' involves just one vehicle - a pedal cycle but has involved in either a single pedestrian casualty,
# two pedestrian casualties, two pedal cyclist casualties pedal cyclist and pedestrian casualties - see below  
crashes_casualties_vehicles_AAP_no_geo %>% 
  filter(number_of_vehicles == 1) %>%
  group_by(number_of_vehicles, number_of_casualties, cas_1_casualty_type_derived, cas_2_casualty_type_derived,
           cas_3_casualty_type_derived, cas_4_casualty_type_derived) %>%
  summarise(count = n()) 
# number_of_vehicles number_of_casualties cas_1_casualty_type_derived cas_2_casualty_type_derived cas_3_casualty_type_derived cas_4_casualty_ty…¹ count
# 1                  1                    1 Pedestrian                  NA                          NA                          NA                     72
# 2                  1                    2 Cyclist                     Cyclist                     NA                          NA                      1
# 3                  1                    2 Cyclist                     Pedestrian                  NA                          NA                     24
# 4                  1                    2 Pedestrian                  Cyclist                     NA                          NA                      4
# 5                  1                    2 Pedestrian                  Pedestrian                  NA                          NA                      1
# 6                  1                    3 Cyclist                     Pedestrian                  Pedestrian                  NA                      1


# This code works specifically on this dataset because of the combination of vehicles 
# - it may not work on another dataset if the combination of vehicles is different
veh_type2 = veh_type %>%
  mutate(combo = case_when(
    (veh_1_vehicle_type_derived == "Car" | veh_2_vehicle_type_derived == "Car") &
      (veh_1_vehicle_type_derived == "Pedal cycle" | veh_2_vehicle_type_derived == "Pedal cycle") &
      (veh_3_vehicle_type_derived == "Pedal cycle" | veh_3_vehicle_type_derived == "Car" | is.na(veh_3_vehicle_type_derived)) &
      (veh_4_vehicle_type_derived == "Pedal cycle" | veh_4_vehicle_type_derived == "Car" | is.na(veh_4_vehicle_type_derived)) ~ "Car",
    ((veh_1_vehicle_type_derived == "Car" & veh_2_vehicle_type_derived == "Car" & veh_3_vehicle_type_derived == "Pedal cycle") |
       (veh_1_vehicle_type_derived == "Car" & veh_2_vehicle_type_derived == "Car" & veh_3_vehicle_type_derived == "Car") &
       (veh_4_vehicle_type_derived == "Pedal cycle")) ~ "Car",
    (veh_1_vehicle_type_derived == "Pedal cycle" & is.na(veh_2_vehicle_type_derived) & is.na(veh_3_vehicle_type_derived) & 
       is.na(veh_4_vehicle_type_derived)) ~ "Single pedal cycle",
    (veh_1_vehicle_type_derived == "Pedal cycle" & veh_2_vehicle_type_derived == "Pedal cycle" & veh_3_vehicle_type_derived == "Car") ~ "Car",
    (veh_1_vehicle_type_derived == "Pedal cycle" & veh_2_vehicle_type_derived == "Pedal cycle" & is.na(veh_3_vehicle_type_derived) & 
       is.na(veh_4_vehicle_type_derived)) ~ "Two pedal cycles",
    (veh_1_vehicle_type_derived == "Taxi" | veh_2_vehicle_type_derived == "Taxi") &
      (veh_1_vehicle_type_derived == "Pedal cycle" | veh_2_vehicle_type_derived == "Pedal cycle") ~ "Taxi",
    (veh_1_vehicle_type_derived == "LGV" | veh_2_vehicle_type_derived == "LGV") &
      (veh_1_vehicle_type_derived == "Pedal cycle" | veh_2_vehicle_type_derived == "Pedal cycle") &
      is.na(veh_3_vehicle_type_derived) ~ "LGV",
    (veh_1_vehicle_type_derived == "Pedal cycle" & veh_2_vehicle_type_derived == "LGV" & veh_3_vehicle_type_derived == "Car") ~ "Two motor vehicles",
    (veh_1_vehicle_type_derived == "Pedal cycle" & veh_2_vehicle_type_derived == "Car" & veh_3_vehicle_type_derived == "LGV") ~ "Two motor vehicles",
    (veh_1_vehicle_type_derived == "Pedal cycle" & veh_2_vehicle_type_derived == "LGV" & veh_3_vehicle_type_derived == "Taxi") ~ "Two motor vehicles",
    (veh_1_vehicle_type_derived == "Pedal cycle" & veh_2_vehicle_type_derived == "LGV" & veh_3_vehicle_type_derived == "Motorcycle") ~ "Two motor vehicles",
    (veh_1_vehicle_type_derived == "Bus_coach_minibus" | veh_2_vehicle_type_derived == "Bus_coach_minibus") &
      (veh_1_vehicle_type_derived == "Pedal cycle" | veh_2_vehicle_type_derived == "Pedal cycle") &
      is.na(veh_3_vehicle_type_derived) ~ "Bus, coach or minibus",
    (veh_1_vehicle_type_derived == "HGV" | veh_2_vehicle_type_derived == "HGV") &
      (veh_1_vehicle_type_derived == "Pedal cycle" | veh_2_vehicle_type_derived == "Pedal cycle") &
      is.na(veh_3_vehicle_type_derived) ~ "HGV",
    (veh_1_vehicle_type_derived == "Pedal cycle" ) & (veh_2_vehicle_type_derived == "Pedal cycle") &
      (veh_3_vehicle_type_derived == "HGV") ~ "HGV",
    (veh_1_vehicle_type_derived == "Motorcycle" | veh_2_vehicle_type_derived == "Motorcycle") &
      (veh_1_vehicle_type_derived == "Pedal cycle" | veh_2_vehicle_type_derived == "Pedal cycle") &
      is.na(veh_3_vehicle_type_derived) ~ "Motorcycle",
    (veh_1_vehicle_type_derived == "Other_unknown" | veh_2_vehicle_type_derived == "Other_unknown") &
      (veh_1_vehicle_type_derived == "Pedal cycle" | veh_2_vehicle_type_derived == "Pedal cycle") &
      is.na(veh_3_vehicle_type_derived) ~ "Other",
    (veh_1_vehicle_type_derived == "LGV" & veh_2_vehicle_type_derived == "Pedal cycle" & veh_3_vehicle_type_derived == "Pedal cycle") ~ "LGV",
    (veh_1_vehicle_type_derived == "LGV" & veh_2_vehicle_type_derived == "LGV" & veh_3_vehicle_type_derived == "Pedal cycle") ~ "LGV",
    (veh_1_vehicle_type_derived == "Bus_coach_minibus" & veh_2_vehicle_type_derived == "LGV" & veh_3_vehicle_type_derived == "Pedal cycle") ~ "Two motor vehicles",
    (veh_1_vehicle_type_derived == "Motorcycle" & veh_2_vehicle_type_derived == "Pedal cycle" & veh_3_vehicle_type_derived == "Taxi") ~ "Two motor vehicles",
    (veh_1_vehicle_type_derived == "Pedal cycle" & veh_2_vehicle_type_derived == "Motorcycle" & veh_3_vehicle_type_derived == "Car") ~ "Two motor vehicles",
    (veh_1_vehicle_type_derived == "Taxi" & veh_2_vehicle_type_derived == "Bus_coach_minibus" & veh_3_vehicle_type_derived == "Pedal cycle") ~ "Two motor vehicles",
    (veh_1_vehicle_type_derived == "LGV" & veh_2_vehicle_type_derived == "Motorcycle" & veh_3_vehicle_type_derived == "Pedal cycle") ~ "Two motor vehicles"
  ))

veh_type3 = veh_type2 %>%
  select(c(1,2,7)) %>%
  group_by(crash_time_scale, combo) %>%
  summarise(total_num_crashes = sum(num_crashes)) %>%
  arrange(crash_time_scale, desc(total_num_crashes))

pivot_veh_typ = veh_type3 %>%
  pivot_wider(names_from = "crash_time_scale", values_from = "total_num_crashes")
pivot_veh_typ[is.na(pivot_veh_typ)] = 0 # replace NA with 0

crash_vehicle_type = pivot_veh_typ %>%
  mutate(pre_per = round(Pre_contraflow/sum(Pre_contraflow) * 100, digits = 1),
         contra_per = round(Contraflow/sum(Contraflow) * 100, digits = 1),
         post_per = round(Contraflow_removed/sum(Contraflow_removed) * 100, digits = 1),
         Pre = paste0(Pre_contraflow, " (", pre_per, ")"),
         Contra = paste0(Contraflow, " (", contra_per, ")"),
         Remove = paste0(Contraflow_removed, " (", post_per, ")"))

sum(crash_vehicle_type$Pre_contraflow) # 788
sum(crash_vehicle_type$Contraflow) # 703
sum(crash_vehicle_type$Contraflow_removed) # 7
788 +703 + 7 # 1498 - adds up to correct number of crashes

write.csv(crash_vehicle_type, file = "output/table_1crashes_vehicle_type_28_09_2022.csv")


##########################################################################
#              Identify crashes with 10m of junction                     #
##########################################################################

# identify crashes that are labelled as not with 20m of junction - include these
# identify crashes that are labelled as within 20m of junction but measure at > 10m 
# from junction according to osm

# Obtain junctions in OSM data
library(osmextract)
library(trafficalmr)
gl_pbf19 = "./Documents/PhD/Paper1/data/greater-london-190101.osm.pbf"
gl_osm_lines19 = oe_read(gl_pbf19, quiet = FALSE, layer = "lines") 
gl_osm_lines19 = st_transform(gl_osm_lines19, crs=27700) # PROJCRS["OSGB 1936 / British National Grid"
crashes_casualties_vehicles_AAP = readRDS("data-processed/stats19/pedal_cycle_crashes_with_casualties_&_vehicles_AAP_27_09_22.RDs")
  
# Keep osm lines that are highway categories that can be roads
highways = c("primary", "residential", "trunk", "footway", "service", "unclassified", 
             "tertiary", "secondary", "motorway_link", "motorway", 
             "tertiary_link", "secondary_link", "trunk_link", "pedestrian", "primary_link", 
             "living_street", "road")
osm_lines_roads = gl_osm_lines19 %>% filter(highway %in% highways)

# Get junctions for this osm_lines
junctions = osm_get_junctions(osm_lines_roads)

# Add 10m buffer to junctions
junctions_buff_10 = st_sf(st_buffer(junctions, dist = 10))

# Identify crashes occuring within junction_buff and add label
crashes_10m_junctions = st_join(crashes_casualties_vehicles_AAP, junctions_buff_10, left = FALSE) # n = 1576, some crashes are in > 1 junction buffer

crashes_casualties_vehicles_AAP_2 = crashes_casualties_vehicles_AAP %>%
  mutate(not_within_10m_junction = case_when(accident_index %in% 
                                               crashes_10m_junctions$accident_index ~ FALSE, TRUE ~ TRUE))
# label our crashes by whether they are in 10m or not
  
# Save this new dataframe
saveRDS(crashes_casualties_vehicles_AAP_2,
        file = "data-processed/stats19/pedal_cycle_crashes_with_casualties_&_vehicles_&_junctions_AAP_28_09_22.RDs")
