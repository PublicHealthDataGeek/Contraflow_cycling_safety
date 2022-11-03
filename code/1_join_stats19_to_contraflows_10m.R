###################################################################################
#              Identify crashes within 10m of contraflow segments                 #
#                                                                                 #  
# The inner_crashes_1998_2019 generated in the code file 0_get_stats19.R contains #
# all crashes within 150m of the boundary of Inner London (n = 304456)            #
#                                                                                 #
# NB accident_index is the unique value for each crash that allows joining        #
# vehicles and casualties                                                         #
#                                                                                 #
# This code does the initial buffering of contraflows and then joining unbuffered #
# crashes involving pedal cycles to these buffered contraflows.  Subsequent code  #
# files look at the effect when crashes are buffered.                             #
#                                                                                 #
# Crashes that are spatially joined to more than one contraflow are identified    #
# and then joined to the nearest contraflow.                                      #
#                                                                                 #
# There is some validation and tidying plus then generating new variables that    #
# identify whether a crash happened before, during or after a contraflow was      #
# implemented.                                                                    #
#                                                                                 #
# Code updated on 30/5/2022 so that crashes joined to unique_tro have correct     #
# duration based on the actual start dates.  However have kept the derived time   #
# columns even those these are not used and may now be inaccurate as subsequent   #
# code using subsetting by position so removing these will mess up later code.    #
#                                                                                 #
###################################################################################

# Load packages
library(stats19)
library(tidyverse)
library(sf)
library(mapview)
library(leafsync)
library(lubridate)

# Load datasets 
inner_crashes_1998_2019 = readRDS(file = "./data-processed/stats19/inner_lon_buff_crashes_1998_2019.Rds")
all_tro_df = readRDS(file = "data-processed/all_tro_tidied_11_01_22.Rds")
lon_lad_2020_bfe = readRDS(file = "./map_data/lon_LAD_boundaries_May_2020_BFE.Rds")

# Relabel boroughs for tro and ons so can join to innercrashes
all_tro_df$BOROUGH = fct_recode(all_tro_df$BOROUGH, 
                          "Kensington and Chelsea" = "Kensington & Chelsea",
                          "Hammersmith and Fulham" = "Hammersmith & Fulham")
lon_lad_2020_bfe$BOROUGH = fct_recode(lon_lad_2020_bfe$BOROUGH, 
                          "Kensington and Chelsea" = "Kensington & Chelsea",
                          "Hammersmith and Fulham" = "Hammersmith & Fulham",
                          "Barking and Dagenham" = "Barking & Dagenham")

# Get ONS borough boundaries
Inner_ons = c("City of London", "Camden", "Greenwich", "Hackney", "Hammersmith and Fulham",
          "Islington", "Kensington and Chelsea", "Lambeth", "Lewisham", "Newham", "Southwark",
          "Tower Hamlets", "Wandsworth", "Westminster")
inner_lon_boroughs = lon_lad_2020_bfe %>%
  filter(BOROUGH %in% Inner_ons)

# Check CRS are the same
st_crs(inner_crashes_1998_2019) == st_crs(all_tro_df)  # TRUE

# Create single observation for each unique segment with spatial object for entire unique segments
unique_tro_df = all_tro_df %>%
  group_by(unique_contraflow_ID) %>%
  summarise(across(unique_row_ID:tro_action_3, .fns = first)) %>%
  mutate(contraflow_length = st_length(geometry)) # calculate length based on the summarised geometry
sum(unique_tro_df$contraflow_length) == sum(all_tro_df$contraflow_length) # TRUE

#saveRDS(unique_tro_df, file = "data-processed/unique_tro_df_01_03_2022.Rds")
rm(unique_tro_df)

# Load updated version of unique_tro_df with correct durations ()
unique_tro_df = readRDS(file = "data-processed/unique_tro_df_durations_using_start_date.Rds")

# Visually compare the all_tro_df v unique_tro_df
all = mapview(all_tro_df)
unique = mapview(unique_tro_df)
sync(all, unique)

#### Examine crashes defined by S19 LAD and spatial intersection with ONS boroughs
borough_crashes = inner_crashes_1998_2019 %>%
  st_drop_geometry() %>%
  group_by(local_authority_district) %>%
  summarise(s19_lad = n()) %>%
  rename(BOROUGH = local_authority_district)

borough_intersect_crashes = st_intersection(inner_crashes_1998_2019, lon_lad_2020_bfe)
borough_i_c = borough_intersect_crashes %>%
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(ons_borough = n())

# Create table of these
borough_location_comparison = left_join(borough_crashes, borough_i_c) %>%
  mutate(inner = case_when(BOROUGH %in% Inner_ons == TRUE ~ "TRUE"))

# look at totals - so more crashes have an lad in S19 than are spatially identified as having BOROUGH
sum(borough_location_comparison$s19_lad) # 304456
sum(borough_location_comparison$ons_borough, na.rm = TRUE) # 304456
nrow(inner_crashes_1998_2019) # 304456

# look at totals for inner london boroughs only
inner_table = borough_location_comparison %>%
  filter(inner == TRUE)
sum(inner_table$s19_lad) # 296944
sum(inner_table$ons_borough, na.rm = TRUE) # 296418
sum(inner_table$s19_lad) - sum(inner_table$ons_borough, na.rm = TRUE) # so 526 more crashes identified 
# as occurring in inner london by S19 than by ONS data
# probably unsurprising as had 150m buffer around the Inner london boundary when created 
# the stats19 df.  This will be sorted when do the spatial join to the contraflows.  


# Quick look at contraflows, buffers and crashes using City
city_bound = inner_lon_boroughs %>% filter(BOROUGH == "City of London") # nb not buffered - wouldnt be anyway for this bit
city_crashes = st_intersection(inner_crashes_1998_2019, city_bound) # n = 7515 Nb diff number to that in the table
city_contraflows = unique_tro_df %>% filter(BOROUGH == "City of London") # n= 93
buff_10_city_contraflows = st_buffer(city_contraflows, dist = 10)
mapview(buff_10_city_contraflows, color = "red", alpha.regions = 0.2) + 
  mapview(city_crashes, zcol = "accident_severity") +
  mapview(city_contraflows, color = "red")




################################################################################
#           Examining buffer size around contraflows                          #
################################################################################

# Cycling infrastructure Design Standard:
# Cycling in bus lanes - Lane should be min 3.9m wide but preferably 4.5m  pg 68
# cycling shared with pedestrians - should be 3m (300 cyclist per hour) pr 4.5m (>300 cyclist/hour)pg68
# 
# COntraflow cycling without a dedicated cycling lane min carriageway widths: pg79
# 2.6m with no car parking
# 3.9m base on car passing cycle and no car parking
# 4.6m with car parking on one side
# 6.6m with car parking on both sides

# Cycle lane widths pg 43
# One way Cycle lane (no protection or seg) min 1.5m, desirable min = 2m
# two way cycle lane (with porection eg seg) min 2 (2.5m >300<1000), desirable min - 3m (these are for peak cycle flow < 1000)

# Minium acceptable lane widths pg 78
# Standard UK lane width 3.65m 

# STATS19 original geospatial accuracy was 10m (5 digit long lat) then because 6 digit

# Buffer all contraflows to 10m
buff_10_contraflows = st_buffer(unique_tro_df, dist = 10) # one OSM road in City 



################################################################################
#             Join unbuffered crashes to buffered contraflows                  #
################################################################################

# Use st_join (default st_intersection) to identify unbuffered crashes and buffered contraflows that overlap
unbuff_crashes = st_join(inner_crashes_1998_2019, buff_10_contraflows, left = FALSE) # n = 7458

# Are some crashes associated with more than one contraflow?  YES - will sort 
# these out later once identify whether they involve a bike or not
unbuff_crashes %>%
  st_drop_geometry() %>%
  group_by(accident_index) %>%
  summarise(num_contra_assoc_with_a_crash = n()) %>%
  group_by(num_contra_assoc_with_a_crash) %>%
  summarise(count = n())
# eg 2019480924102 is associated with both city42 and city49 contraflows and therefore this
# accident index has two rows in unbuff_crashes

# num_contra_assoc_with_a_crash count
# 1                             1  6316
# 2                             2   537
# 3                             3    20
# 4                             4     2


################################################################################
#              Identify which crashes involve a bicycle                        #
################################################################################

# vehicles1998_2019 = readRDS(file = "./Downloads/1979/dft-road-casualty-statistics-vehicles-1998-2019.Rds")
# nrow(vehicles1998_2019) # n = 7094291
# names(vehicles1998_2019)
# # [1] "accident_index"                   "accident_year"                   
# # [3] "accident_reference"               "vehicle_reference"               
# # [5] "vehicle_type"                     "towing_and_articulation"         
# # [7] "vehicle_manoeuvre"                "vehicle_direction_from"          
# # [9] "vehicle_direction_to"             "vehicle_location_restricted_lane"
# # [11] "junction_location"                "skidding_and_overturning"        
# # [13] "hit_object_in_carriageway"        "vehicle_leaving_carriageway"     
# # [15] "hit_object_off_carriageway"       "first_point_of_impact"           
# # [17] "vehicle_left_hand_drive"          "journey_purpose_of_driver"       
# # [19] "sex_of_driver"                    "age_of_driver"                   
# # [21] "age_band_of_driver"               "engine_capacity_cc"              
# # [23] "propulsion_code"                  "age_of_vehicle"                  
# # [25] "generic_make_model"               "driver_imd_decile"               
# # [27] "driver_home_area_type" 
# 
# unique(vehicles1998_2019$vehicle_type)
# # [1] "Goods over 3.5 tonnes (1979-1998)"              "Car (including private hire cars) (1979-2004)" 
# # [3] "Van / Goods 3.5 tonnes mgw or under"            "Motorcycle (1979-1998)"                        
# # [5] "Taxi (excluding private hire cars) (1979-2004)" "Bus or coach (17 or more pass seats)"          
# # [7] "Pedal cycle"                                    "Motorcycle 50cc and under"                     
# # [9] "Motorcycle - Scooter (1979-1998)"               "Other vehicle"                                 
# # [11] "Minibus/Motor caravan (1979-1998)"              "Motorcycle - Combination (1979-1998)"          
# # [13] "Data missing or out of range"                   "Goods 7.5 tonnes mgw and over"                 
# # [15] "Goods over 3.5t. and under 7.5t"                "Motorcycle over 125cc (1999-2004)"             
# # [17] "Motorcycle 125cc and under"                     "Agricultural vehicle"                          
# # [19] "Minibus (8 - 16 passenger seats)"               "Ridden horse"                                  
# # [21] "Tram"                                           "Car"                                           
# # [23] "Motorcycle over 500cc"                          "Motorcycle over 125cc and up to 500cc"         
# # [25] "Taxi/Private hire car"                          "Motorcycle - unknown cc"                       
# # [27] "Mobility scooter"                               "Goods vehicle - unknown weight"                
# # [29] "Electric motorcycle"                            "Unknown vehicle type (self rep only)"  
# 
# # Get all crahses involving pedal cycles
# cycles1998_2019 = vehicles1998_2019 %>%
#   filter(vehicle_type == "Pedal cycle")
# # save
# #saveRDS(cycles1998_2019, file = "data/cycles_vehicles1998_2019")

# Read in data
cycles1998_2019 = readRDS(file = "data/cycles_vehicles1998_2019")

# Examine df
nrow(cycles1998_2019) # n = 416631
unique(cycles1998_2019$accident_index) # n = 411544 ie less than nrows 
#? some crashes involved more than one pedal cycle? - Yes!  one involved 12!!!!
cycles1998_2019 %>%
  group_by(accident_index) %>%
  summarise(num_pedal_cycles_involved = n()) %>%
  group_by(num_pedal_cycles_involved) %>%
  summarise(count = n())
# num_pedal_cycles_involved  count
# 1                         1 406899
# 2                         2   4331
# 3                         3    236
# 4                         4     53
# 5                         5     16
# 6                         6      3
# 7                         7      2
# 8                         8      2
# 9                        10      1
# 10                       12      1

# Get df of unique crashes where a pedal cycle is involved
crashes_with_pedal_cycle = cycles1998_2019 %>%
  group_by(accident_index) %>%
  summarise(num_pedal_cycles_involved = n()) %>%
  select(accident_index) # 411544


################################################################################
#         Identify the unbuffered crashes that involve a pedal cycle           #
################################################################################

# Add column to crash/contraflow df to show the ones where pedal cycles are involved
unbuff_crashes2 = unbuff_crashes %>%
  mutate(pedal_cycle_involved = case_when(accident_index %in% crashes_with_pedal_cycle$accident_index ~ TRUE,
                                          TRUE ~ FALSE)) # identify in unbuff_crashes those whose acident index is in crashes_with_pedal_cycle
sum(unbuff_crashes2$pedal_cycle_involved, na.rm = TRUE) # n= 1954

# Visualise crashes by whether a pedal cycle is involved or not
mapview(unbuff_crashes2, zcol = "pedal_cycle_involved") + 
  mapview(buff_11_contraflows, alpha.regions = 0.2)

# Recheck how many contraflows are associated with each crash now have got those that involve a pedal cycle
unbuff_crashes2 %>%
  st_drop_geometry() %>%
  filter(pedal_cycle_involved == TRUE) %>%
  group_by(accident_index) %>%
  summarise(num_contra_assoc_with_a_crash = n()) %>%
  group_by(num_contra_assoc_with_a_crash) %>%
  summarise(count = n())  # so still some to sort out

# num_contra_assoc_with_a_crash count
# 1                             1  1607
# 2                             2   155
# 3                             3    11
# 4                             4     1

# Check to see if any rows are duplicated
cycles_unbuff_crashes2 = unbuff_crashes2 %>%
  filter(pedal_cycle_involved == TRUE) # n = 1954
dedup_cycles = cycles_unbuff_crashes2 %>%
  distinct(.keep_all = TRUE) # n = 1954 so no duplicates
rm(dedup_cycles)
 
# Identify crashes where they are associated with more than one contraflow
multi_contra_crashes_id = cycles_unbuff_crashes2 %>%
  st_drop_geometry() %>%
  group_by(accident_index) %>%
  summarise(num_contra_assoc_with_a_crash = n()) %>%
  arrange(desc(num_contra_assoc_with_a_crash)) %>%
  filter(num_contra_assoc_with_a_crash >1) # n = 167 (155+11+1)

# Pull out crash df
multi_contra_crashes_df = cycles_unbuff_crashes2 %>% filter(accident_index %in% multi_contra_crashes_id$accident_index) %>%
  select(c(1:36, 76)) %>% # 347 (155x2 + 11x3 + 1x4) ie totals up to the correct number of individual rows
  distinct(.keep_all = TRUE) # deduplicate the rows so just have the 167

# Check unique accident_index
unique(multi_contra_crashes_df$accident_index) # n = 167 

#### TEST DUE TO ISSUES WITH NEAREST FEATURE
not_joined_correct_id = c("2017480182386", "2016480049294", "200801CP00297")
not_joined_correct_df = multi_contra_crashes_df %>% filter(accident_index %in% not_joined_correct_id)
test = st_join(not_joined_correct_df, buff_10_contraflows, left = FALSE, join = st_nearest_feature)
test2 = st_join(not_joined_correct_df, unique_cf_tiny, left = FALSE, join = st_nearest_feature)

mapview(test) +mapview(buff_10_contraflows)
mapview(test) +mapview(unique_cf_tiny)

# Rejoin these crashes to the contraflow using nearest neighbour
# Use st_join with st_nearest_feature) to identify non-buffered crashes and buffered contraflows that overlap
join_multi_contra_crash_to_single_contra = st_join(multi_contra_crashes_df, unique_tro_df, left = FALSE, join = st_nearest_feature) %>%   
  relocate(pedal_cycle_involved, .after = last_col())
unique(join_multi_contra_crash_to_single_contra$accident_index) # n = 167

join_multi_contra_crash_to_single_contra %>%  # check just one contraflow associated with each crash
  st_drop_geometry() %>%
  group_by(accident_index) %>%
  summarise(num_contra_assoc_with_a_crash = n()) %>%
  group_by(num_contra_assoc_with_a_crash) %>%
  summarise(count = n())
# num_contra_assoc_with_a_crash count
#   1                            167

# Create dataframe where these crashes are dropped but the rest of those which only joined to one contraflow are kept
drop_multi = cycles_unbuff_crashes2 %>%
  filter(!accident_index %in% join_multi_contra_crash_to_single_contra$accident_index) # n = 1607

# Bind drop_multi with nearest neighbour df
cycles_unbuff_crashes3 = rbind(drop_multi, join_multi_contra_crash_to_single_contra)
unique(cycles_unbuff_crashes3$accident_index) # n = 1774 this matches the number that it should be 
nrow(cycles_unbuff_crashes3) # 1774
unique(cycles_unbuff_crashes3$unique_contraflow_ID) # n = 336
508-336 # 172 contraflow segments have no crashes associated with them.

sum(is.na(cycles_unbuff_crashes3$contraflow_start_date)) # 111 crashes have no start date

################################################################################
#                              Spatial checkin                                 #
################################################################################

# There might be some data that is not correct spatially so to examine for that

# 1) Check local authority district of where crashes occur
unique(cycles_unbuff_crashes3$police_force) # "Metropolitan Police" "City of London"  
 
cycles_unbuff_crashes3 %>% st_drop_geometry() %>% group_by(local_authority_district) %>%
  summarise(count = n())
# local_authority_district    count
# 1 Barking and Dagenham         1   Outer London
# 2 Brent                        1   Outer London
# 3 Camden                     453
# 4 City of London             265
# 5 Greenwich                   15
# 6 Hackney                    116
# 7 Hammersmith and Fulham      48
# 8 Islington                  167
# 9 Kensington and Chelsea      41
# 10 Lambeth                    174
# 11 Lewisham                    52
# 12 Newham                      41
# 13 Southwark                  247
# 14 Tower Hamlets               14
# 15 Wandsworth                  45
# 16 Westminster                 94
  

# Check to see if crashes labelled as local_authority xyz match the contraflow BOROUGH
examine_borough = cycles_unbuff_crashes3 %>%
  filter(local_authority_district != BOROUGH) # n = 25
# View this data
mapview(examine_borough, zcol = "local_authority_district") + 
  mapview(inner_lon_boroughs, alpha.regions = 0.1) +
  mapview(unique_tro_df)
# All bar one are boundary issues - eg the crashes is on a boundary and the contraflow in in the next door neighbourhood
# 200501KF60735 change to from Barking and Dagenham to Newham as this appears to be a mislocation (crash is over 100m into Newham)
# The Brent one is ok to remain labelled as Brent

#View to see if any crashes seem to be in wrong district
clrs <- sf.colors
mapview(cycles_unbuff_crashes3, zcol = "local_authority_district") + 
  mapview(inner_lon_boroughs, zcol = "BOROUGH", alpha.regions = 0.1, col.regions = clrs) +
  mapview(unique_tro_df)
# 	201001BS70521 should be labelled as K&C rather than Westminster

# Examining whether S19 spatial data location is located in the local authority district
map(Inner_ons, ~ggplot() +
      geom_sf(data = inner_lon_boroughs %>% filter(BOROUGH == .x)) +
      geom_sf(data = cycles_unbuff_crashes3 %>% filter(local_authority_district == .x)) +
      labs(title = .x)
)
# These all seem to be ok but double check Southwark as one seemed closer to being outside southern tip - ACTUALLY NO
# southwark = cycles_unbuff_crashes3 %>% filter(local_authority_district == "Southwark")
# mapview(southwark) + mapview(inner_lon_boroughs)


# Corrections based on spatial checking
cycles_unbuff_crashes3$local_authority_district = replace(cycles_unbuff_crashes3$local_authority_district,
                                                          which(cycles_unbuff_crashes3$accident_index == "200501KF60735"),
                                                          values = "Newham")
cycles_unbuff_crashes3$local_authority_district = replace(cycles_unbuff_crashes3$local_authority_district,
                                                          which(cycles_unbuff_crashes3$accident_index == "201001BS70521"),
                                                          values = "Kensington and Chelsea")


# 2) Check road type - redo after have got before and after
cycles_unbuff_crashes3 %>% st_drop_geometry() %>% group_by(road_type) %>% 
  summarise(count = n())  
# NB some of these will be a road type BEFORE it became a one way street so not 
# necessarily incorrect
# road_type                count
# 1 Dual carriageway           122
# 2 One way street             181
# 3 One way street/Slip road   103
# 4 Roundabout                  16
# 5 Single carriageway        1331
# 6 Slip road                    1
# 7 Unknown                     20

mapview(cycles_unbuff_crashes3, zcol = "road_type") + mapview(unique_tro_df)

# 4) Check crashes have been assigned to the nearest contraflow
# Looks like they have
mapview(cycles_unbuff_crashes3, zcol = "unique_contraflow_ID", legend = FALSE) + 
  mapview(unique_cf_tiny, legend = FALSE) # check all contraflows

mapview(join_multi_contra_crash_to_single_contra, zcol = "unique_contraflow_ID", legend = FALSE) + 
  mapview(unique_cf_tiny, legend = FALSE) # check the ones that we joined to the nearest one 
# as they could have joined multiple buffered cfs

################################################################################
#     Identify crashes involving a pedal cycle by whether they occurred        #
#           before, during or after the contraflow was allowed                 #
################################################################################

# Tidy up df first eg drop some columns I wont use (NB all checked to ensure ok to delete for this df)
cycles_unbuff_crashes4 = cycles_unbuff_crashes3 %>%
  select(-c(local_authority_ons_district, local_authority_highway, lsoa_of_accident_location, 
            accident_reference, longitude, latitude, urban_or_rural_area, trunk_road_flag,
            unique_row_ID, pedal_cycle_involved))


# Create interval durations and check the code works correct by matching the duration with that already calculated
cycles_unbuff_crashes5 = cycles_unbuff_crashes4 %>%
  mutate(pre_contraflow_interval = interval(dmy("01-01-1998"), contraflow_start_date)) %>%
  mutate(pre_contraflow_interval_derived = interval(dmy("01-01-1998"), contraflow_start_date_derived)) %>%
  mutate(contraflow_interval = case_when(is.na(contraflow_stop_date) ~ interval(contraflow_start_date, dmy("31-12-2019")),
                                         !is.na(contraflow_stop_date) ~ interval(contraflow_start_date, contraflow_stop_date))) %>%
  mutate(contraflow_interval_derived = case_when(is.na(contraflow_stop_date) ~ interval(contraflow_start_date_derived, dmy("31-12-2019")),
                                                 !is.na(contraflow_stop_date) ~ interval(contraflow_start_date_derived, contraflow_stop_date))) %>%
  mutate(post_contraflow_interval = interval(contraflow_stop_date, dmy("31-12-2019")))

# Identify crashes occuring before during or post contraflow
cycles_unbuff_crashes6 = cycles_unbuff_crashes5 %>%  # right = line 497
  mutate(crash_before_contra = date %within% pre_contraflow_interval) %>% # where contraflow start date is NA then crash_before_contra is NA too
  mutate(crash_before_contra_derived = date %within% pre_contraflow_interval_derived) %>%
  mutate(crash_during_contra = date %within% contraflow_interval) %>%
  mutate(crash_during_contra_derived = date %within% contraflow_interval_derived) %>%
  mutate(crash_after_contra = date %within% post_contraflow_interval)
unique(cycles_unbuff_crashes6$crash_before_contra) # TRUE    NA FALSE
unique(cycles_unbuff_crashes6$crash_before_contra_derived) # TRUE FALSE
unique(cycles_unbuff_crashes6$crash_during_contra) # FALSE    NA  TRUE
unique(cycles_unbuff_crashes6$crash_during_contra_derived) #FALSE  TRUE
unique(cycles_unbuff_crashes6$crash_after_contra) # NA FALSE  TRUE
 
sum(cycles_unbuff_crashes6$crash_before_contra, na.rm = TRUE) # n= 824
sum(cycles_unbuff_crashes6$crash_before_contra_derived, na.rm = TRUE) # n = 884
sum(cycles_unbuff_crashes6$crash_during_contra, na.rm = TRUE) # n = 831
sum(cycles_unbuff_crashes6$crash_during_contra_derived, na.rm = TRUE) # n = 882
sum(cycles_unbuff_crashes6$crash_after_contra, na.rm = TRUE) # n = 8
 
# Add column to indicate whether crash occurred before, during or after contraflow (one for nonderived and one for derived)
cycles_unbuff_crashes7 = cycles_unbuff_crashes6 %>%
  mutate(crash_time_scale = case_when(crash_before_contra == TRUE ~ "Before",
                                      crash_during_contra == TRUE ~ "During",
                                      crash_after_contra == TRUE ~ "After")) %>%
  mutate(crash_time_scale_derived = case_when(crash_before_contra_derived == TRUE ~ "Before",
                                              crash_during_contra_derived == TRUE ~ "During",
                                              crash_after_contra == TRUE ~ "After"))

unique(cycles_unbuff_crashes7$crash_time_scale) # "Before" NA       "During" "After"
unique(cycles_unbuff_crashes7$crash_time_scale_derived) # "Before" "During" "After"

# Examine crashes by time scale
cycles_unbuff_crashes7 %>%
  st_drop_geometry() %>%
  group_by(crash_time_scale, crash_time_scale_derived) %>%
  summarise(count = n())
# crash_time_scale crash_time_scale_derived count
#  1 After            After                        8
# 2 Before           Before                     824
# 3 During           During                     831
# 4 NA               Before                      60
# 5 NA               During                      51


 
#Total time
before = cycles_unbuff_crashes7 %>%
  filter(crash_time_scale == "Before")
sum(before$pre_contraflow_duration) # 4637018
sum(before$contraflow_length) # 140215.4 [m]

during = cycles_unbuff_crashes7 %>%
  filter(crash_time_scale == "During")
sum(during$contraflow_duration) # 3833972
sum(during$contraflow_length) # 158742.3 [m]

after = cycles_unbuff_crashes7 %>%
  filter(crash_time_scale == "After")
sum(after$post_contraflow_duration) # 49413
sum(after$contraflow_length) # 860.5193 [m]

################################################################################
#                                  Save dataframe                              #
################################################################################

# Remove columns no longer required
cycles_unbuff_crashes8 = cycles_unbuff_crashes7 %>%
  select(-c(pre_contraflow_interval, pre_contraflow_interval_derived, 
            contraflow_interval, contraflow_interval_derived,
            post_contraflow_interval, crash_before_contra, crash_before_contra_derived,
            crash_during_contra, crash_during_contra_derived, crash_after_contra))

# So cycles_unbuff_crashes7 is crashes involving a pedal cycle joined to a 10m 
# buffered contraflow (if potentially joined to more than one then joined to the 
# nearest one) where columns have been identified as to whether the crash occurred 
# before or after a contraflow went in.  
# Duration has been updated on 18/5/2022 so total duration is 8035 days
# Uses duration based on the actual contraflow start dates
saveRDS(cycles_unbuff_crashes8, file = "data-processed/step_4_S19_joined_to_contraflows/unbuffered_pedal_cycle_crashes_10mcontraflow_30_05_2022.Rds")


