################################################################################
#      Identifying direction of pedal cycle - with or against contraflow       #
#                                                                              #
# This code originally took all the data pre-AAP peer review where we had      #  
# identified crashes within 10m of junctions. It then identifies whether it is #
# possible to determine the direction the pedal cyclist involved in the crash  #
# was travelling.  Finally it identifies whether the pedal cycle was travelling#
# with or against the flow.                                                    #
#                                                                              #
# BUT following the AAP review, I just filter the new AAP crash data set       #
# on the output of this code file to then get the direction of pedal cycles in #
# the crashes and generate the Table 3 data.                                   #
################################################################################



################################################################################
#                       Create AAP dataset with direction                      #
################################################################################

library(tidyverse)

# Load AAP crash dataset
crashes_casualties_vehicles_AAP_2 = readRDS("data-processed/stats19/pedal_cycle_crashes_with_casualties_&_vehicles_&_junctions_AAP_28_09_22.RDs")
# n = 1498

# Load dataset output from this code
crashes_pc_and_road_seg_direction = readRDS("data-processed/stats19/data_pre_AAP/pedal_cycle_crashes_with_casualties_&_vehicles_&_junctions&cycle_direction_18_05_22.RDs")
# n = 1663

crashes_casualties_vehicles_AAP_direction = crashes_pc_and_road_seg_direction %>%
  filter(accident_index%in% crashes_casualties_vehicles_AAP_2$accident_index)
# n = 1498

# Save new dataset
saveRDS(crashes_casualties_vehicles_AAP_direction, "data-processed/stats19/pedal_cycle_crashes_with_casualties_&_vehicles_&_junctions&cycle_direction_28_09_22.RDs")

crashes_casualties_vehicles_AAP_direction = readRDS("data-processed/stats19/pedal_cycle_crashes_with_casualties_&_vehicles_&_junctions&cycle_direction_28_09_22.RDs")

################################################################################
#             Get pedal cycle direction for crashes for Table 3                #
################################################################################
library(tableone)

# Check that all bus lanes involved in a crash are NEW contraflow bus lanes 
# ie going from two way to a contraflow bus lane
bus_lan_crashes = unique_tro_df %>% 
  filter(unique_contraflow_ID %in% crashes_casualties_vehicles_AAP_direction$unique_contraflow_ID) %>%
  filter(introduces_contraflow_bus_lane == TRUE)
# tro details checked and it appears all are new ones.  

# Get variables
list_vars = c("sig_additional_tro_action", "junction_detail_condensed", "not_within_10m_junction", 
              "cyc_1_direction_when_crashed", "cyc_2_direction_when_crashed")

# A) Crashes that dont occur within 10m of a junction
# Do analysis on two way roads
data_two_way_non10m = crashes_casualties_vehicles_AAP_direction %>% st_drop_geometry() %>%
  filter(sig_additional_tro_action != "Contraflow cycling only") %>%
  filter(not_within_10m_junction == "TRUE")
two_way_table_non10m = CreateTableOne(list_vars, strata = "crash_time_scale", data = data_two_way_non10m)
table_two_way_non10m = print(two_way_table_non10m, quote = FALSE, noSpaces = TRUE, printToggle = FALSE, test = FALSE, showAllLevels = TRUE)
table_two_way_non10m = cbind(Original_road = "Two way", table_two_way_non10m)

# Do analysis on one way roads
data_one_way_non10m = crashes_casualties_vehicles_AAP_direction %>% st_drop_geometry() %>%
  filter(sig_additional_tro_action == "Contraflow cycling only") %>%
  filter(not_within_10m_junction == "TRUE")

one_way_table_non10m = CreateTableOne(list_vars, strata = "crash_time_scale", data = data_one_way_non10m)
table_one_way_non10m = print(one_way_table_non10m, quote = FALSE, noSpaces = TRUE, printToggle = FALSE, test = FALSE, showAllLevels = TRUE)
table_one_way_non10m = cbind(Original_road = "One way", table_one_way_non10m)

# Join these analyses to get table
pc_direction_non10m = rbind(table_one_way_non10m, table_two_way_non10m)
write.csv(pc_direction_non10m, file = "output/pc_direction_non10m_28_09_2022.csv")

# B) Crashes that DO occur within 10m of a junction
# Do analysis on two way roads
data_two_way_10m_JUNC = crashes_casualties_vehicles_AAP_direction %>% st_drop_geometry() %>%
  filter(sig_additional_tro_action != "Contraflow cycling only") %>%
  filter(not_within_10m_junction == "FALSE")
two_way_table_10m_JUNC = CreateTableOne(list_vars, strata = "crash_time_scale", data = data_two_way_10m_JUNC)
table_two_way_10m_JUNC = print(two_way_table_10m_JUNC, quote = FALSE, noSpaces = TRUE, printToggle = FALSE, test = FALSE, showAllLevels = TRUE)
table_two_way_10m_JUNC = cbind(Original_road = "Two way", table_two_way_10m_JUNC)

# Do analysis on one way roads
data_one_way_10m_JUNC = crashes_casualties_vehicles_AAP_direction %>% st_drop_geometry() %>%
  filter(sig_additional_tro_action == "Contraflow cycling only") %>%
  filter(not_within_10m_junction == "FALSE")

one_way_table_10m_JUNC = CreateTableOne(list_vars, strata = "crash_time_scale", data = data_one_way_10m_JUNC)
table_one_way_10m_JUNC = print(one_way_table_10m_JUNC, quote = FALSE, noSpaces = TRUE, printToggle = FALSE, test = FALSE, showAllLevels = TRUE)
table_one_way_10m_JUNC = cbind(Original_road = "One way", table_one_way_10m_JUNC)

# Join these analyses to get table
pc_direction_10m_JUNC = rbind(table_one_way_10m_JUNC, table_two_way_10m_JUNC)
write.csv(pc_direction_10m_JUNC, file = "output/pc_direction_10m_JUNC_28_09_2022.csv")



################################################################################
################################################################################
#                                ORIGINAL CODE                                 #
################################################################################
################################################################################

# load packages
library(tidyverse)
library(mapview)
library(sf)

# Load dataframes
unique_tro_df = readRDS(file = "data-processed/unique_tro_df_durations_using_start_date.Rds")
crashes_casualties_vehicles_2 = readRDS(file = "data-processed/stats19/pedal_cycle_crashes_with_casualties_&_vehicles_&_junctions_18_05_22.RDs")

################################################################################
#       Identify and label crashes that may be near junctions as may not       #
#    be able to determine whether the cyclist was on the contraflow segment    #
################################################################################

# Main issue = crashes occurring near junctions - the pedal cyclist may not be on the contraflow segment

# Look at data - which ones do we want to include?
## If police have labelled it as not within 20m then defo not near a junction
## NB using 2019 data for junctions so some junctions may have been created or removed.

# Look at the those classified as at a junction or not at a junction by STATS19
crashes_casualties_vehicles_2 %>% st_drop_geometry() %>%
  group_by(junction_detail_condensed) %>% summarise(count = n())
# junction_detail_condensed           count
# <fct>                               <int>
# 1 junction_roundabout_other            1524  # some will be included if not within 10m
# 2 Not at junction or within 20 metres   134  # All these are included - even if these are now within 10m it may be that they werent when the crash happened so take this as gold
# 3 unknown (self reported)                 5  # some will be included if not within 10m

# Now look at those classified at or within 10m
crashes_casualties_vehicles_2 %>% st_drop_geometry() %>%
  group_by(junction_detail_condensed, not_within_10m_junction) %>% summarise(count = n())
# junction_detail_condensed           not_within_10m_junction count
# 1 junction_roundabout_other           FALSE                    1150  # Check whether to include these
# 2 junction_roundabout_other           TRUE                      374  # include all these
# 3 Not at junction or within 20 metres FALSE                      49  # Include all these - as these were not near a junction when S19 form was completed
# 4 Not at junction or within 20 metres TRUE                       85  # Include all these
# 5 unknown (self reported)             FALSE                       4  # Check whether to include these
# 6 unknown (self reported)             TRUE                        1  # include all these

# Label each crash as whether it may not be possible to determine
crashes_casualties_vehicles_2 = crashes_casualties_vehicles_2 %>%
  mutate(include_for_direction = case_when((junction_detail_condensed == "junction_roundabout_other" & not_within_10m_junction == FALSE |
                                               junction_detail_condensed == "unknown (self reported)" & not_within_10m_junction == FALSE) ~
                                             "Check", TRUE ~ "Include"))
crashes_casualties_vehicles_2 %>% st_drop_geometry() %>% group_by(include_for_direction) %>% summarise(count = n())
# include_for_direction count
# 1 Check                 1154
# 2 Include                509

################################################################################
#   Create dataframe of pedal cycle directions where vehicles are replaced by  #
#                                   pedal cycle data                           #
################################################################################

# NB none of the crashes have more than two cycles involved.
# pedal cycle exist in all 4 vehicle types so need to go to 4 vehicles

# combinations of crashes with 2 pedal cycles are: veh 1 with 2, veh 1 with 3, veh 2 with 3

# There is: 1 x one vehicle scenario
#           3 x two vehicle scenarios: veh1 = pedcyc OR veh2 = ped cyc OR both veh 1 & 2 are ped cycles
#           ? x three vehicle scenarios - various ones but none have more than 2 cycles
#           ? x four vehicle scenarios - but fortunately we only have 1 crash where there is a pedal cycle involved

test = crashes_casualties_vehicles_2 %>% select(c(1, 5, 27, 44, 46, 90, 91, 70:73, 78:81, 82:85)) %>% st_drop_geometry()

# Manage crashes where there is one vehicle involved
veh_1 = test %>% filter(number_of_vehicles == "1") %>% # these are all pedal cycles
  rename(cyc_1_vehicle_type_derived = veh_1_vehicle_type_derived,
         cyc_1_vehicle_direction_from = veh_1_vehicle_direction_from,
         cyc_1_vehicle_direction_to = veh_1_vehicle_direction_to) %>%
  mutate(num_cycles = 1,
         cyc_2_vehicle_type_derived = NA, cyc_2_vehicle_direction_from = NA, cyc_2_vehicle_direction_to = NA) %>%
  select(c(1:7, 20, 8, 12, 16, 21:23))# 174

# Manage crashes where there are two vehicles involved
veh_2_ped_cyc_1a = test %>% filter(number_of_vehicles == "2" & veh_1_vehicle_type_derived == "Pedal cycle" &
                                     veh_2_vehicle_type_derived != "Pedal cycle") %>%
  rename(cyc_1_vehicle_type_derived = veh_1_vehicle_type_derived,
         cyc_1_vehicle_direction_from = veh_1_vehicle_direction_from,
         cyc_1_vehicle_direction_to = veh_1_vehicle_direction_to) %>%
  mutate(num_cycles = 1,
         cyc_2_vehicle_type_derived = NA, cyc_2_vehicle_direction_from = NA, cyc_2_vehicle_direction_to = NA) %>% 
  select(c(1:7, 20, 8, 12, 16, 21:23))# n = 626

veh_2_ped_cyc_1b = test %>% filter(number_of_vehicles == "2" & veh_2_vehicle_type_derived == "Pedal cycle" &
                                     veh_1_vehicle_type_derived != "Pedal cycle") %>%
  rename(cyc_1_vehicle_type_derived = veh_2_vehicle_type_derived,
         cyc_1_vehicle_direction_from = veh_2_vehicle_direction_from,
         cyc_1_vehicle_direction_to = veh_2_vehicle_direction_to) %>%
  mutate(num_cycles = 1,
         cyc_2_vehicle_type_derived = NA, cyc_2_vehicle_direction_from = NA, cyc_2_vehicle_direction_to = NA) %>%
  select(c(1:7, 20, 9, 13, 17, 21:23))# n = 825
          
veh_2_ped_cycX2 = test %>% filter(number_of_vehicles == "2" & veh_2_vehicle_type_derived == "Pedal cycle" &
                                    veh_1_vehicle_type_derived == "Pedal cycle") %>%
  rename(cyc_1_vehicle_type_derived = veh_1_vehicle_type_derived,
         cyc_1_vehicle_direction_from = veh_1_vehicle_direction_from,
         cyc_1_vehicle_direction_to = veh_1_vehicle_direction_to,
         cyc_2_vehicle_type_derived = veh_2_vehicle_type_derived,
         cyc_2_vehicle_direction_from = veh_2_vehicle_direction_from,
         cyc_2_vehicle_direction_to = veh_2_vehicle_direction_to) %>%
  mutate(num_cycles = 2) %>%
  select(c(1:7, 20, 8, 12, 16, 9, 13, 17))# 16
 

# Manage crashes with 3 vehicles involved
test %>% filter(number_of_vehicles == "3") %>%
  select(1, 8:11) # n = 21 - some have 1 and others have 2 pedal cycles involved

veh_3_testing_1 = test %>% filter(number_of_vehicles == "3" & veh_1_vehicle_type_derived == "Pedal cycle") %>%
  rename(cyc_1_vehicle_type_derived = veh_1_vehicle_type_derived,
         cyc_1_vehicle_direction_from = veh_1_vehicle_direction_from,
         cyc_1_vehicle_direction_to = veh_1_vehicle_direction_to) %>%  # makes veh_1 = cyc_1
  mutate(veh_2_vehicle_type_derived = case_when(veh_2_vehicle_type_derived == "Pedal cycle" ~ "Pedal cycle", 
                                                TRUE ~ NA_character_),
         veh_3_vehicle_type_derived = case_when(veh_3_vehicle_type_derived == "Pedal cycle" ~ "Pedal cycle", 
                                                TRUE ~ NA_character_)) %>% # makes all non-pedal cycles NA so can coalesce 
  rename(cyc_2_vehicle_type_derived = veh_2_vehicle_type_derived,
         cyc_2_vehicle_direction_from = veh_2_vehicle_direction_from,
         cyc_2_vehicle_direction_to = veh_2_vehicle_direction_to,
         cyc_3_vehicle_type_derived = veh_3_vehicle_type_derived,
         cyc_3_vehicle_direction_from = veh_3_vehicle_direction_from,
         cyc_3_vehicle_direction_to = veh_3_vehicle_direction_to) %>% # rename the vehicle colums
  mutate(cyc_2_vehicle_direction_from = case_when(is.na(cyc_2_vehicle_type_derived) ~ NA_character_, 
                                                  TRUE ~ cyc_2_vehicle_direction_from),
         cyc_2_vehicle_direction_to = case_when(is.na(cyc_2_vehicle_type_derived) ~ NA_character_, 
                                                  TRUE ~ cyc_2_vehicle_direction_to),
         cyc_3_vehicle_direction_from = case_when(is.na(cyc_3_vehicle_type_derived) ~ NA_character_, 
                                                  TRUE ~ cyc_3_vehicle_direction_from),
         cyc_3_vehicle_direction_to = case_when(is.na(cyc_3_vehicle_type_derived) ~ NA_character_, 
                                                  TRUE ~ cyc_3_vehicle_direction_to)) %>% # makes all non-pedal cycle directions NA
  mutate(cyc_2_vehicle_type_derived = coalesce(cyc_2_vehicle_type_derived, cyc_3_vehicle_type_derived),
         cyc_2_vehicle_direction_from = coalesce(cyc_2_vehicle_direction_from, cyc_3_vehicle_direction_from),
         cyc_2_vehicle_direction_to = coalesce(cyc_2_vehicle_direction_to, cyc_3_vehicle_direction_to)) %>% # coalesce cyc2 and cyc3
  select(c(1:9, 12, 13, 16, 17)) # drop columns not longer required

veh_3_testing_2 = test %>% filter(number_of_vehicles == "3" & veh_2_vehicle_type_derived == "Pedal cycle") %>%
  filter(!accident_index %in% veh_3_testing_1$accident_index) %>% # drop the rows that have already been sorted in veh_3_testing_1
  rename(cyc_1_vehicle_type_derived = veh_2_vehicle_type_derived,
         cyc_1_vehicle_direction_from = veh_2_vehicle_direction_from,
         cyc_1_vehicle_direction_to = veh_2_vehicle_direction_to) %>%  # makes veh_2 = cyc_1
  mutate(veh_1_vehicle_type_derived = case_when(veh_1_vehicle_type_derived == "Pedal cycle" ~ "Pedal cycle", 
                                                TRUE ~ NA_character_),
         veh_3_vehicle_type_derived = case_when(veh_3_vehicle_type_derived == "Pedal cycle" ~ "Pedal cycle", 
                                                TRUE ~ NA_character_)) %>% # makes all non-pedal cycles NA so can coalesce 
  rename(cyc_2_vehicle_type_derived = veh_3_vehicle_type_derived,
         cyc_2_vehicle_direction_from = veh_3_vehicle_direction_from,
         cyc_2_vehicle_direction_to = veh_3_vehicle_direction_to) %>% # rename the veh_3 columns (dont need to worry about veh_1 as sorted in veh_3_testing_1)
  mutate(cyc_2_vehicle_direction_from = case_when(is.na(cyc_2_vehicle_type_derived) ~ NA_character_, 
                                                  TRUE ~ cyc_2_vehicle_direction_from),
         cyc_2_vehicle_direction_to = case_when(is.na(cyc_2_vehicle_type_derived) ~ NA_character_, 
                                                TRUE ~ cyc_2_vehicle_direction_to)) %>% # makes all non-pedal cycle directions NA
  # mutate(cyc_2_vehicle_type_derived = coalesce(cyc_2_vehicle_type_derived, cyc_3_vehicle_type_derived),
  #        cyc_2_vehicle_direction_from = coalesce(cyc_2_vehicle_direction_from, cyc_3_vehicle_direction_from),
  #        cyc_2_vehicle_direction_to = coalesce(cyc_2_vehicle_direction_to, cyc_3_vehicle_direction_to)) %>% # coalesce cyc2 and cyc3
  select(c(1:7, 9, 10, 13, 14, 17, 18)) # drop columns not longer required

veh_3_testing_3 = test %>% filter(number_of_vehicles == "3" & veh_3_vehicle_type_derived == "Pedal cycle") %>% # n = 11
  filter(!accident_index %in% veh_3_testing_1$accident_index) %>% # drop the rows that have already been sorted in veh_3_testing_1
  filter(!accident_index %in% veh_3_testing_2$accident_index) %>% # drop the rows that have already been sorted in veh_3_testing_2
  rename(cyc_1_vehicle_type_derived = veh_3_vehicle_type_derived,
         cyc_1_vehicle_direction_from = veh_3_vehicle_direction_from,
         cyc_1_vehicle_direction_to = veh_3_vehicle_direction_to) %>%  # makes veh_3 = cyc_1
  mutate(cyc_2_vehicle_type_derived = NA_character_,
         cyc_2_vehicle_direction_from = NA_character_,
         cyc_2_vehicle_direction_to = NA_character_) %>% # create cyc_2 columns that contain NA 
  select(c(1:7, 10, 20, 14, 21, 18, 22)) # drop columns not longer required

veh_3_final = rbind(veh_3_testing_1, veh_3_testing_2, veh_3_testing_3) %>%
  mutate(num_cycles = case_when(cyc_1_vehicle_type_derived == "Pedal cycle" & 
                                  cyc_2_vehicle_type_derived == "Pedal cycle" ~ 2, 
                                TRUE ~ 1)) %>% # add number of cycles involved column
  select(c(1:7, 14, 8, 10, 12, 9, 11, 13)) # reorder cols so can join to other dataframes

# Manage crashes with 4 vehicles involved
veh_4_final = test %>% filter(number_of_vehicles == "4") %>%
  rename(cyc_1_vehicle_type_derived = veh_4_vehicle_type_derived,
         cyc_1_vehicle_direction_from = veh_4_vehicle_direction_from,
         cyc_1_vehicle_direction_to = veh_4_vehicle_direction_to) %>%  # makes veh_4 = cyc_1
  mutate(cyc_2_vehicle_type_derived = NA_character_,
         cyc_2_vehicle_direction_from = NA_character_,
         cyc_2_vehicle_direction_to = NA_character_) %>% # create cyc_2 columns that contain NA 
  mutate(num_cycles = 1) %>% # add num of cycle column
  select(c(1:7, 23, 11, 15, 19:22))  # select and reorder cols so can join to other dataframes

#  Join all these dataframes together
pedal_cyc_direction = rbind(veh_1, veh_2_ped_cyc_1a, veh_2_ped_cyc_1b, veh_2_ped_cycX2, veh_3_final, veh_4_final)

# # Validate that I have got the right total number of cycles per crash
#pedal_cyc_direction %>% group_by(num_cycles) %>% summarise(count = n())
# # num_cycles count
# # 1          1642
# # 2            21
# 
# validate = crashes_casualties_vehicles_2 %>% st_drop_geometry() %>%
#   select(c(1, 69:72)) %>%
#   mutate(cyc_veh_1 = str_count(veh_1_vehicle_type_derived, "Pedal cycle"), 
#          cyc_veh_2 = str_count(veh_2_vehicle_type_derived, "Pedal cycle"),
#          cyc_veh_3 = str_count(veh_3_vehicle_type_derived, "Pedal cycle"), 
#          cyc_veh_4 = str_count(veh_4_vehicle_type_derived, "Pedal cycle")) %>%
#   rowwise() %>%
#   mutate(num_cycles = sum(c_across(cyc_veh_1:cyc_veh_4), na.rm = TRUE))
# 
# validate %>% group_by(num_cycles) %>% summarise(count = n())
# # num_cycles count
# # 1          1642
# # 2            21


# Join pedal cycle direction to the main crashes df
joined_crashes_pc_direction = left_join(crashes_casualties_vehicles_2, select(pedal_cyc_direction, c(1, 8:14)))
#saveRDS(joined_crashes_pc_direction, file = "data-processed/joined_crashes_pc_direction_need_cf_dir_adding.Rds")
saveRDS(joined_crashes_pc_direction, file = "data-processed/joined_crashes_pc_direction_need_cf_dir_adding_18_05_22.Rds")

################################################################################
#              Determine if pedal cycle is moving with or contraflow           #
#                                                                              #
# This is done by examining each unique road segment and identifying which     #
# direction is with or contraflow based on TRO content and OSM. Where the road #
# segment has a 'one way street' introduced the pre-contraflow status is       #
# assumed to be two way and thus pedal cycle direction coded as per this,      #
# otherwise the precontraflow status is assumed to be one way. New variables   #
# cyc_1_cf_direction and cyc_2_cf_direction are created representing this - NB #
# both the 'direction_from' and 'direction_to' must match the contraflow/ with #
# flow direction - this means that pedal cycles travelling on road segments    #
# that are invovled in crashes at junctions are removed (as their directions   # 
# dont match the road segment.                                                  #
# 
# Finally a new column that identifies whether the crash should be included in #
# the calculation of crash rates by direction is created, based on whether we 
# have a pedal cycle direction consistent either with or contaflow - 
# NB this column may not be the final decision for inclusion - will need to examine
# data before deciding.  

joined_crashes_pc_direction = readRDS(file = "data-processed/joined_crashes_pc_direction_need_cf_dir_adding_18_05_22.Rds")

# Make df smaller so can examine pedal cycle directions
min_joined_crashes_pc_direction = joined_crashes_pc_direction %>% select(c(1, 7, 27, 28, 30:39, 42, 44, 46, 91, 92:99))
tro_detail = unique_tro_df %>% select(c(1, 24:38)) %>% st_drop_geometry

min_joined_crashes_pc_direction = left_join(min_joined_crashes_pc_direction, tro_detail) # add on tro details 


                           ##########
                           # Camden #
                           ##########
camden = min_joined_crashes_pc_direction %>% filter(BOROUGH == "Camden")
count = camden %>% st_drop_geometry() %>% group_by(unique_contraflow_ID) %>% summarise(count = n())
# unique_contraflow_ID count
# <fct>                <int>
# 1 cam55                   79
# 2 cam16                   53
# 3 cam1                    30
# 4 cam12                   27
# 5 cam4                    27
# 6 cam6                    24
# 7 cam54                   23
# 8 cam5                    22
# 9 cam61                   19
# 10 cam13                   15
# 11 cam2                    13
# 12 cam34                   12
# 13 cam62                   12
# 14 cam41                   11
# 15 cam28                   10
# 16 cam11                    6
# 17 cam43                    6
# 18 cam18                    4
# 19 cam31                    4
# 20 cam46                    4
# 21 cam51                    4
# 22 cam7                     4
# 23 cam14                    3
# 24 cam17                    3
# 25 cam19                    3
# 26 cam39                    3
# 27 cam20                    2
# 28 cam23                    2
# 29 cam33                    2
# 30 cam44                    2
# 31 cam49                    2
# 32 cam50                    2
# 33 cam57                    2
# 34 cam58                    2
# 35 cam10                    1
# 36 cam21                    1
# 37 cam26                    1
# 38 cam29                    1
# 39 cam3                     1
# 40 cam30                    1
# 41 cam35                    1
# 42 cam36                    1
# 43 cam38                    1
# 44 cam40                    1
# 45 cam52                    1
# 46 cam56                    1



unique(camden$contraflow_stop_date) # All NA so dont need to worry about managing contraflows that have been removed
unique(camden$introduces_one_way_street) # TRUE & FALSE therefore need to check this before assigning direction

# 1) CAM55
## South-westbound contra-flow bus lane introduced, no one way system introduced
## Therefore for both pre-cf and contraflow travelling east to west was contraflow 
cam55 = camden %>% filter(unique_contraflow_ID == "cam55")
mapview(cam55, zcol = "include_for_direction")

# Code those where vehicle direction matches with/contraflow 
cam55_dir = cam55 %>% mutate(
  cyc_1_cf_direction = case_when(
    cyc_1_vehicle_direction_from == "East" & cyc_1_vehicle_direction_to == "West" ~ "Contraflow",
    cyc_1_vehicle_direction_from == "North East" & cyc_1_vehicle_direction_to == "South West" ~ "Contraflow",   
    cyc_1_vehicle_direction_from == "West" & cyc_1_vehicle_direction_to == "East" ~ "With flow",
    cyc_1_vehicle_direction_from == "South West" & cyc_1_vehicle_direction_to == "North East" ~ "With flow",
    cyc_1_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"),
  cyc_2_cf_direction = NA) # add column but there are no cyc 2 for cam55

# Examine those where vehicle direction doesnt match with/contraflow - can some of these be coded?
cam_55_dir_query = cam55_dir %>% filter(is.na(cyc_1_cf_direction)) # n = 15
unique(cam_55_dir_query$cyc_1_vehicle_direction_from) # [1] "North West" "South East" "North"      "South West"
unique(cam_55_dir_query$cyc_1_vehicle_direction_to) # "South East" "North"    
cam_55_dir_query = cam_55_dir_query %>% filter(cyc_1_vehicle_direction_from == "South West") # the only direction that might be relevent
mapview(cam_55_dir_query, zcol = "cyc_1_vehicle_direction_to") # This one going SW to SE so turning off at a junction - dont add direction
# so no more of cam55 can be coded.

# Identify those crashes that definately need to be incldued in the direction crash calculation because we have a direction that matches with flow or contraflow 
cam55_dir_final = cam55_dir %>% 
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", TRUE ~ "Exclude")) %>%
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining
               
# 2) CAM16
## Contra-flow segregated cycle lane introduced, appears to be one way before
## Contraflow direction is north east to south west but need to check values in S19 as it isnt 'straight' NE->SW
## Therefore for both pre-cf and contraflow travelling east to west was contraflow 
cam16 = camden %>% filter(unique_contraflow_ID == "cam16")
mapview(cam16, zcol = "include_for_direction")

# # if vehicle_from north east to south west = contraflow 
 cam16_dir = cam16 %>% mutate(
   cyc_1_cf_direction = case_when(
     cyc_1_vehicle_direction_from == "North East" & cyc_1_vehicle_direction_to == "South West" ~ "Contraflow",
     cyc_1_vehicle_direction_from == "South West" & cyc_1_vehicle_direction_to == "North East" ~ "With flow",
     cyc_1_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"),
   cyc_2_cf_direction = case_when(
     cyc_2_vehicle_direction_from == "North East" & cyc_2_vehicle_direction_to == "South West" ~ "Contraflow",
     cyc_2_vehicle_direction_from == "South West" & cyc_2_vehicle_direction_to == "North East" ~ "With flow",
     cyc_2_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"))

# Examine those where vehicle direction doesnt match with/contraflow - can some of these be coded?
cam_16_dir_query = cam16_dir %>% filter(is.na(cyc_1_cf_direction)) # n = 18
unique(cam_16_dir_query$cyc_1_vehicle_direction_from) # "West"       "East"       "South East" "North"      "North West" "South West"
unique(cam_16_dir_query$cyc_1_vehicle_direction_to) # "East"       "South West" "North"      "South East" "North East" "West"       "North West"
c16veh_dir_from_ok = c("West", "East", "South West")
c16veh_dir_to_ok = c("West", "East", "South West", "North East")

cam_16_dir_query = cam_16_dir_query %>% filter(cyc_1_vehicle_direction_from %in% c16veh_dir_from_ok | 
                                                 cyc_1_vehicle_direction_to %in% c16veh_dir_to_ok) %>% # the only directions that might be relevent,
  select(c(1, 22, 23)) # n = 12
mapview(cam_16_dir_query, zcol = "cyc_1_vehicle_direction_to") # some crashes overlapping so hard to tell
 
# Having checked on map with directions and examined those crashes that are colocated, amend these crashes as follows: 
with_flow_accident_index = c("2018010149532")
contraflow_accident_index = c("2017010050638", "200701EK63818")
cam16_dir$cyc_1_cf_direction = replace(cam16_dir$cyc_1_cf_direction, 
                                     which(cam16_dir$accident_index %in% with_flow_accident_index), values = "With flow")
cam16_dir$cyc_1_cf_direction = replace(cam16_dir$cyc_1_cf_direction, 
                                       which(cam16_dir$accident_index %in% contraflow_accident_index), values = "Contraflow")

# Mark to exclude those where we dont have a direction that doesnt match with flow or contraflow
cam16_dir_final = cam16_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", 
    (cyc_2_cf_direction == "Contraflow" | cyc_2_cf_direction == "With flow") ~ "Include",
    TRUE ~ "Exclude")) %>%
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

# 3) CAM1
# Enable cf cycling in a bus lane, appears to be one way before 
# Contraflow direction is westbound
cam1 = camden %>% filter(unique_contraflow_ID == "cam1")
mapview(cam1, zcol = "include_for_direction")

# if vehicle_from east to west = contraflow, 2 cyc_2 obs 
cam1_dir = cam1 %>% mutate(
  cyc_1_cf_direction = case_when(
    cyc_1_vehicle_direction_from == "East" & cyc_1_vehicle_direction_to == "West" ~ "Contraflow",
    cyc_1_vehicle_direction_from == "West" & cyc_1_vehicle_direction_to == "East" ~ "With flow",
    cyc_1_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"),
  cyc_2_cf_direction = case_when(
    cyc_2_vehicle_direction_from == "East" & cyc_2_vehicle_direction_to == "West" ~ "Contraflow",
    cyc_2_vehicle_direction_from == "West" & cyc_2_vehicle_direction_to == "East" ~ "With flow",
    cyc_2_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"))

# Examine those where vehicle direction doesnt match with/contraflow - can some of these be coded?
cam_1_dir_query = cam1_dir %>% filter(is.na(cyc_1_cf_direction)) # this include the 2nd uncoded cyc2
mapview(cam_1_dir_query, zcol = "cyc_1_vehicle_direction_from")

# # Having checked on map with directions and examined those crashes that are colocated, amend these crashes as follows: 
# 200001EK00604 is south west to south east so does appear to travelling on the road segment
cam1_dir$cyc_1_cf_direction = replace(cam1_dir$cyc_1_cf_direction,
                                       which(cam1_dir$accident_index == "200001EK00604"), values = "With flow")

# Mark to exclude those where we dont have a direction that doesnt match with flow or contraflow
cam1_dir_final = cam1_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", TRUE ~ "Exclude")) %>% # still works even for cyc_2 for this cf seg
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

# 4) cam12
# Introduces cf seg cycle lane, appears to be one way before 
# Contraflow direction is southwestbound
cam12 = camden %>% filter(unique_contraflow_ID == "cam12")
mapview(cam12, zcol = "include_for_direction")

# If vehicle_from north east to southwest = contraflow, 2 cyc_2 obs 
cam12_dir = cam12 %>% mutate(
  cyc_1_cf_direction = case_when(
    cyc_1_vehicle_direction_from == "North East" & cyc_1_vehicle_direction_to == "South West" ~ "Contraflow",
    cyc_1_vehicle_direction_from == "South West" & cyc_1_vehicle_direction_to == "North East" ~ "With flow",
    cyc_1_vehicle_direction_from == "East" & cyc_1_vehicle_direction_to == "West" ~ "Contraflow", # this added in after checking (see below)
    cyc_1_vehicle_direction_from == "West" & cyc_1_vehicle_direction_to == "East" ~ "With flow", # this added in after checking (see below)
    cyc_1_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"),
  cyc_2_cf_direction = case_when(
    cyc_2_vehicle_direction_from == "North East" & cyc_2_vehicle_direction_to == "South West" ~ "Contraflow",
    cyc_2_vehicle_direction_from == "South West" & cyc_2_vehicle_direction_to == "North East" ~ "With flow",
    cyc_2_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"))

# Examine those where vehicle direction doesnt match with/contraflow - can some of these be coded?
cam_12_dir_query = cam12_dir %>% filter(is.na(cyc_1_cf_direction)) # no uncoded cy2 in this df
mapview(cam_12_dir_query, zcol = "cyc_1_vehicle_direction_from")
# Having checked on map with directions and examined those crashes below can be coded so new lines added above covering East and West: 
# 2018010137794 East
# 200801CW10821
# 200701EO40354
# 200801CW10846


# Mark to exclude those where we dont have a direction that doesnt match with flow or contraflow
cam12_dir_final = cam12_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", TRUE ~ "Exclude")) %>% # still works even for cyc_2 for this cf seg
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

# 5) cam4
# Enables cycling in contraflow bus lane, appears to be one way before
# Contraflow direction is southwestbound
cam4 = camden %>% filter(unique_contraflow_ID == "cam4")
mapview(cam4, zcol = "include_for_direction")
 
# If vehicle_from North East to South West = contraflow, there is one observation with a cyc_2 
 cam4_dir = cam4 %>% mutate(
   cyc_1_cf_direction = case_when(
     cyc_1_vehicle_direction_from == "North East" & cyc_1_vehicle_direction_to == "South West" ~ "Contraflow",
     cyc_1_vehicle_direction_from == "South West" & cyc_1_vehicle_direction_to == "North East" ~ "With flow",
     cyc_1_vehicle_direction_from == "East" & cyc_1_vehicle_direction_to == "West" ~ "Contraflow", # this added in after checking (see below)
     cyc_1_vehicle_direction_from == "West" & cyc_1_vehicle_direction_to == "East" ~ "With flow", # this added in after checking (see below)
     cyc_1_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"),
   cyc_2_cf_direction = case_when(
     cyc_2_vehicle_direction_from == "North East" & cyc_2_vehicle_direction_to == "South West" ~ "Contraflow",
     cyc_2_vehicle_direction_from == "South West" & cyc_2_vehicle_direction_to == "North East" ~ "With flow",
     cyc_2_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"))
 
# # # Examine those where vehicle direction doesnt match with/contraflow - can some of these be coded?
cam_4_dir_query = cam4_dir %>% filter(is.na(cyc_1_cf_direction)) # no uncoded cy2 in this df
mapview(cam_4_dir_query, zcol = "cyc_1_vehicle_direction_from")
#  Having checked on map with directions and examined those crashes below can be coded so new lines added above covering East and West: 
# 2018010084899, 2019010175816, 200801CW11677

# Mark to exclude those where we dont have a direction that doesnt match with flow or contraflow
cam4_dir_final = cam4_dir %>%
   mutate(include_for_direction_checked = case_when(
     (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", TRUE ~ "Exclude")) %>% # still works even for cyc_2 for this cf seg
   select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

   
# 6) cam6
# Enables cycling in contraflow bus lane, appears to be one way before
# Contraflow direction is southwestbound
cam6 = camden %>% filter(unique_contraflow_ID == "cam6")
mapview(cam6, zcol = "include_for_direction")

# If vehicle_from north east to southwest = contraflow, no cyc_2 obs
cam6_dir = cam6 %>% mutate(
  cyc_1_cf_direction = case_when(
    cyc_1_vehicle_direction_from == "North East" & cyc_1_vehicle_direction_to == "South West" ~ "Contraflow",
    cyc_1_vehicle_direction_from == "South West" & cyc_1_vehicle_direction_to == "North East" ~ "With flow",
    cyc_1_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"),
  cyc_2_cf_direction = NA) # add column but there are no cyc 2 for cam6

# Examine those where vehicle direction doesnt match with/contraflow - can some of these be coded?
cam_6_dir_query = cam6_dir %>% filter(is.na(cyc_1_cf_direction)) # no uncoded cy2 in this df
mapview(cam_6_dir_query, zcol = "cyc_1_vehicle_direction_from") # Dont include any of these.  

# Mark to exclude those where we dont have a direction that doesnt match with flow or contraflow
cam6_dir_final = cam6_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", TRUE ~ "Exclude")) %>% 
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining


# 7) cam54
# # Enables contraflow cycling 
# # Contraflow direction is westbound (hint of north west bound)
cam54 = camden %>% filter(unique_contraflow_ID == "cam54")
mapview(cam54, zcol = "include_for_direction")
 
# If vehicle_from East to West = contraflow, there no cyc~_2 observations
cam54_dir = cam54 %>% mutate(
  cyc_1_cf_direction = case_when(
    cyc_1_vehicle_direction_from == "East" & cyc_1_vehicle_direction_to == "West" ~ "Contraflow",
    cyc_1_vehicle_direction_from == "West" & cyc_1_vehicle_direction_to == "East" ~ "With flow",
    cyc_1_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"),
  cyc_2_cf_direction = NA) # add column but there are no cyc 2 for cam6

# Examine those where vehicle direction doesnt match with/contraflow - can some of these be coded?
cam_54_dir_query = cam54_dir %>% filter(is.na(cyc_1_cf_direction)) # no uncoded cy2 in this df
mapview(cam_54_dir_query, zcol = "cyc_1_vehicle_direction_from")
#  Having checked on map with directions and examined those crashes below - it appears none travelling on contraflow

# Mark to exclude those where we dont have a direction that doesnt match with flow or contraflow
cam54_dir_final = cam54_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", TRUE ~ "Exclude")) %>% 
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

# 8) cam5
# Enables cycling in contraflow bus lane, appears to be one way before
# Contraflow direction is southwestbound
cam5 = camden %>% filter(unique_contraflow_ID == "cam5")
mapview(cam5, zcol = "include_for_direction")
 
# If vehicle_from North East to South West = contraflow, there is one observation with a cyc_2
cam5_dir = cam5 %>% mutate(
  cyc_1_cf_direction = case_when(
    cyc_1_vehicle_direction_from == "North East" & cyc_1_vehicle_direction_to == "South West" ~ "Contraflow",
    cyc_1_vehicle_direction_from == "South West" & cyc_1_vehicle_direction_to == "North East" ~ "With flow",
    cyc_1_vehicle_direction_from == "West" & cyc_1_vehicle_direction_to == "East" ~ "With flow", # this added in after checking (see below)
    cyc_1_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"),
  cyc_2_cf_direction = case_when(
    cyc_2_vehicle_direction_from == "North East" & cyc_2_vehicle_direction_to == "South West" ~ "Contraflow",
    cyc_2_vehicle_direction_from == "South West" & cyc_2_vehicle_direction_to == "North East" ~ "With flow",
    cyc_2_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"))

# # # # Examine those where vehicle direction doesnt match with/contraflow - can some of these be coded?
cam_5_dir_query = cam5_dir %>% filter(is.na(cyc_1_cf_direction) | cyc_2_vehicle_type_derived == "Pedal cycle") 
mapview(cam_5_dir_query, zcol = "cyc_1_vehicle_direction_from")
# #  Having checked on map with directions and examined those crashes for one direction can be added (West to East) above 

# Mark to exclude those where we dont have a direction that doesnt match with flow or contraflow
 cam5_dir_final = cam5_dir %>%
   mutate(include_for_direction_checked = case_when(
     (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", 
     (cyc_2_cf_direction == "Contraflow" | cyc_2_cf_direction == "With flow") ~ "Include",
     TRUE ~ "Exclude")) %>% 
   select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

# 9) cam61
# ENables contraflow cycling in segregated lane, appears to be one way before
# Contraflow direction is southwestbound but road shape is 'S' shape so needs assessment
cam61 = camden %>% filter(unique_contraflow_ID == "cam61")
mapview(cam61, zcol = "include_for_direction")

 # If vehicle_from north east to southwest = contraflow, one cyc_2 but direction is 'unknown/self reported' so will 
 cam61_dir = cam61 %>% mutate(
   cyc_1_cf_direction = case_when(
     cyc_1_vehicle_direction_from == "North East" & cyc_1_vehicle_direction_to == "South West" ~ "Contraflow",
     cyc_1_vehicle_direction_from == "South West" & cyc_1_vehicle_direction_to == "North East" ~ "With flow",
     cyc_1_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)",
     cyc_1_vehicle_direction_from == "East" & cyc_1_vehicle_direction_to == "West" ~ "Contraflow"), # this added in after checking (see below)),
   cyc_2_cf_direction = case_when(
     cyc_2_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"))
 
 # # Examine those where vehicle direction doesnt match with/contraflow - can some of these be coded?
cam_61_dir_query = cam61_dir %>% filter(is.na(cyc_1_cf_direction)) # no uncoded cy2 in this df
mapview(cam_61_dir_query, zcol = "cyc_1_vehicle_direction_from") # Dont include any of these.  

 # Mark to exclude those where we dont have a direction that doesnt match with flow or contraflow
 cam61_dir_final = cam61_dir %>%
   mutate(include_for_direction_checked = case_when(
     (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", TRUE ~ "Exclude")) %>% # still works even for cyc_2 for this cf seg
   select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

# 10) cam13
 # # ENables contraflow cycling in segregated lane, appears to be one way before
 # # Contraflow direction is southwestbound 
cam13 = camden %>% filter(unique_contraflow_ID == "cam13")
mapview(cam13, zcol = "include_for_direction")

 # If vehicle_from north east to southwest = contraflow, no cyc_2
 cam13_dir = cam13 %>% mutate(
   cyc_1_cf_direction = case_when(
     cyc_1_vehicle_direction_from == "North East" & cyc_1_vehicle_direction_to == "South West" ~ "Contraflow",
     cyc_1_vehicle_direction_from == "South West" & cyc_1_vehicle_direction_to == "North East" ~ "With flow",
     cyc_1_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)",
     cyc_1_vehicle_direction_from == "East" & cyc_1_vehicle_direction_to == "West" ~ "Contraflow"), # this added in after checking (see below)),
   cyc_2_cf_direction = NA)
 
 # Examine those where vehicle direction doesnt match with/contraflow - can some of these be coded?
 cam_13_dir_query = cam13_dir %>% filter(is.na(cyc_1_cf_direction)) # no uncoded cy2 in this df
 mapview(cam_13_dir_query, zcol = "cyc_1_vehicle_direction_from") 

 # Mark to exclude those where we dont have a direction that doesnt match with flow or contraflow
 cam13_dir_final = cam13_dir %>%
   mutate(include_for_direction_checked = case_when(
     (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", TRUE ~ "Exclude")) %>% 
   select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

 # 11) cam2
# Enables contraflow cycling  southbound, not one way before
cam2 = camden %>% filter(unique_contraflow_ID == "cam2")
mapview(cam2, zcol = "include_for_direction")

# If vehicle_from North/NW to South/SE = contraflow, there is one observation with a cyc_2 
cam2_dir = cam2 %>% mutate(
  cyc_1_cf_direction = case_when(
    cyc_1_vehicle_direction_from == "North" & cyc_1_vehicle_direction_to == "South" ~ "Contraflow",
    cyc_1_vehicle_direction_from == "North West" & cyc_1_vehicle_direction_to == "South East" ~ "Contraflow",
    cyc_1_vehicle_direction_from == "North" & cyc_1_vehicle_direction_to == "South East" ~ "Contraflow",
    cyc_1_vehicle_direction_from == "South" & cyc_1_vehicle_direction_to == "North" ~ "With flow",
    cyc_1_vehicle_direction_from == "South East" & cyc_1_vehicle_direction_to == "North West" ~ "With flow",
    cyc_1_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"),
  cyc_2_cf_direction = NA)
 
# Examine those where vehicle direction doesnt match with/contraflow - can some of these be coded?
cam_2_dir_query = cam2_dir %>% filter(is.na(cyc_1_cf_direction)) # no uncoded cy2 in this df
mapview(cam_2_dir_query, zcol = "cyc_1_vehicle_direction_from") # none appear to be travelling on the road segment

# Mark to exclude those where we dont have a direction that doesnt match with flow or contraflow
cam2_dir_final = cam2_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", TRUE ~ "Exclude")) %>% # still works even for cyc_2 for this cf seg
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

#12) cam34
# Enables contraflow cycling  westbound, not one way before
cam34 = camden %>% filter(unique_contraflow_ID == "cam34")
mapview(cam34, zcol = "cyc_1_vehicle_direction_from")

# # If vehicle_from East to West = contraflow, no cyc_2 observations 
cam34_dir = cam34 %>% mutate(
  cyc_1_cf_direction = case_when(
     cyc_1_vehicle_direction_from == "East" & cyc_1_vehicle_direction_to == "West" ~ "Contraflow",
     cyc_1_vehicle_direction_from == "North East" & cyc_1_vehicle_direction_to == "South West" ~ "Contraflow",
     cyc_1_vehicle_direction_from == "West" & cyc_1_vehicle_direction_to == "East" ~ "With flow",
     cyc_1_vehicle_direction_from == "South West" & cyc_1_vehicle_direction_to == "North East" ~ "With flow",
     cyc_1_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"),
   cyc_2_cf_direction = NA)
 
# Examine those where vehicle direction doesnt match with/contraflow - can some of these be coded?
cam_34_dir_query = cam34_dir %>% filter(is.na(cyc_1_cf_direction)) # no uncoded cy2 in this df
mapview(cam_34_dir_query, zcol = "cyc_1_vehicle_direction_from") # none appear to be travelling on the road segment
 
# Mark to exclude those where we dont have a direction that doesnt match with flow or contraflow
cam34_dir_final = cam34_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", TRUE ~ "Exclude")) %>% 
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining
 
# 13) cam62
# ENables contraflow cycling in segregated lane, AND INTRODUCES ONE WAY TRAFFIC FLOW 
# Contraflow direction is north eastbound 
cam62 = camden %>% filter(unique_contraflow_ID == "cam62")
mapview(cam62, zcol = "include_for_direction")
 
# no cyc_2
cam62_dir = cam62 %>% mutate(
  cyc_1_cf_direction = case_when(
    cyc_1_vehicle_direction_from == "North East" & cyc_1_vehicle_direction_to == "South West" ~ "With flow",
    cyc_1_vehicle_direction_from == "South West" & cyc_1_vehicle_direction_to == "North East" ~ "Contraflow",
    cyc_1_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"),
  cyc_2_cf_direction = NA)
 
# Examine those where vehicle direction doesnt match with/contraflow - can some of these be coded?
cam_62_dir_query = cam62_dir %>% filter(is.na(cyc_1_cf_direction)) # no uncoded cy2 in this df
mapview(cam_62_dir_query, zcol = "cyc_1_vehicle_direction_from")

# Mark to exclude those where we dont have a direction that doesnt match with flow or contraflow
cam62_dir_final = cam62_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", TRUE ~ "Exclude")) %>%
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

#14) cam41 
# ENables contraflow cycling on one way street 
# Contraflow direction varies depending on location of crash as one way nature varies
# Axis of street is northwest/south east, no cyc2
cam41 = camden %>% filter(unique_contraflow_ID == "cam41")
mapview(cam41, zcol = "include_for_direction")

# get crashes on the road segment axis - n = 3
cam_41_nw_se = cam41 %>% filter((cyc_1_vehicle_direction_from == "North West" & cyc_1_vehicle_direction_to == "South East") |
  (cyc_1_vehicle_direction_from == "South East" & cyc_1_vehicle_direction_to == "North West"))

mapview(cam_41_nw_se, zcol = "crash_time_scale")
# 200501EO40664 Precontraflow Travelling SE to NW => Contraflow
# 201501EK40579 cant determine as in middle of junction AND whitfield roads either side go in opposite directions for their one way streets
# 201501EK40707 Contraflow, travelling from SE to NW => Contraflow

cam41_dir = cam41 %>% mutate(cyc_1_cf_direction = NA, cyc_2_cf_direction = NA) # create columns
cam41_dir$cyc_1_cf_direction = replace(cam41_dir$cyc_1_cf_direction,
                                      which(cam41_dir$accident_index == "200501EO40664"), 
                                      values = "Contraflow") # change the two values that I can determine
cam41_dir$cyc_1_cf_direction = replace(cam41_dir$cyc_1_cf_direction,
                                       which(cam41_dir$accident_index == "201501EK40707"), 
                                       values = "Contraflow")

# Mark to exclude those where we dont have a direction that doesnt match with flow or contraflow
cam41_dir_final = cam41_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", TRUE ~ "Exclude")) %>% 
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

# 15) cam28
# ENables contraflow cycling on one way street 
# COntraflow direction is NE to SW
cam28 = camden %>% filter(unique_contraflow_ID == "cam28")
mapview(cam28, zcol = "include_for_direction")

# get crashes on the road segment axis - n = 0 so no pedal cycles were going along axis of the road segment
cam_28_ne_sw = cam28 %>% filter((cyc_1_vehicle_direction_from == "North East" & cyc_1_vehicle_direction_to == "South West") |
                                  (cyc_1_vehicle_direction_from == "South West" & cyc_1_vehicle_direction_to == "North East"))

cam_28_dir = cam28 %>% mutate(
  cyc_1_cf_direction = case_when(cyc_1_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"),
  cyc_2_cf_direction = case_when(cyc_1_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"))

cam28_dir_final = cam_28_dir %>% 
  mutate(include_for_direction_checked = case_when(
  (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", TRUE ~ "Exclude")) %>% 
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

#16) cam11
# Introduces seg contraflow cycle lane on existing one way street
# Contraflow direction is East/NE to West/SW
cam11 = camden %>% filter(unique_contraflow_ID == "cam11")
mapview(cam11, zcol = "include_for_direction") + mapview(unique_tro_df)

cam11_dir = cam11 %>% mutate(
  cyc_1_cf_direction = case_when(
    cyc_1_vehicle_direction_from == "East" & cyc_1_vehicle_direction_to == "West" ~ "Contraflow",
    cyc_1_vehicle_direction_from == "North East" & cyc_1_vehicle_direction_to == "South West" ~ "Contraflow",
    cyc_1_vehicle_direction_from == "West" & cyc_1_vehicle_direction_to == "East" ~ "With flow",
    cyc_1_vehicle_direction_from == "South West" & cyc_1_vehicle_direction_to == "North East" ~ "With flow",
    cyc_1_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"),
  cyc_2_cf_direction = NA)

# Examine those where vehicle direction doesnt match with/contraflow - can some of these be coded? n = 0
cam_11_dir_query = cam11_dir %>% filter(is.na(cyc_1_cf_direction)) # no uncoded cy2 in this df 

# Mark to exclude those where we dont have a direction that doesnt match with flow or contraflow
cam11_dir_final = cam11_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", TRUE ~ "Exclude")) %>% 
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

#17) cam43 
# Enables cf cycling
# Contraflow direction is SE to NW
cam43 = camden %>% filter(unique_contraflow_ID == "cam43")
mapview(cam43, zcol = "cyc_1_vehicle_direction_from")

# # # If vehicle_from East to West = contraflow, no cyc_2 observations 
cam43_dir = cam43 %>% mutate(
   cyc_1_cf_direction = case_when(
     cyc_1_vehicle_direction_from == "South East" & cyc_1_vehicle_direction_to == "North West" ~ "Contraflow",
     cyc_1_vehicle_direction_from == "North West" & cyc_1_vehicle_direction_to == "South East" ~ "With flow",
     cyc_1_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"),
   cyc_2_cf_direction = NA)

# Examine those where vehicle direction doesnt match with/contraflow - can some of these be coded?
cam_43_dir_query = cam43_dir %>% filter(is.na(cyc_1_cf_direction)) # no uncoded cy2 in this df
mapview(cam_43_dir_query, zcol = "cyc_1_vehicle_direction_from") # none appear to be travelling on the road segment
# 201201EK40287 not at junction but appears to be planning to turn off road segment later on

cam43_dir$cyc_1_cf_direction = replace(cam43_dir$cyc_1_cf_direction,
                                       which(cam43_dir$accident_index == "201201EK40287"), 
                                       values = "With flow") # change the value that I can determine

# # Mark to exclude those where we dont have a direction that doesnt match with flow or contraflow
cam43_dir_final = cam43_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", TRUE ~ "Exclude")) %>%
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

#18) cam18
# New contraflow cycle lane on existing one way street
# Contraflow direction west/south west to east/north east
cam_18 = camden %>% filter(unique_contraflow_ID == "cam18")
mapview(cam_18, zcol = "cyc_1_vehicle_direction_from")

# If vehicle_from West to East = contraflow, no cyc_2 observations
cam_18_dir = cam_18 %>% mutate(
  cyc_1_cf_direction = case_when(
    cyc_1_vehicle_direction_from == "South West" & cyc_1_vehicle_direction_to == "North East" ~ "Contraflow",
    cyc_1_vehicle_direction_from == "West" & cyc_1_vehicle_direction_to == "East" ~ "Contraflow",
    cyc_1_vehicle_direction_from == "North East" & cyc_1_vehicle_direction_to == "South West" ~ "With flow",
    cyc_1_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"),
  cyc_2_cf_direction = NA)

# Examine those where vehicle direction doesnt match with/contraflow - can some of these be coded?
cam_18_dir_query = cam_18_dir %>% filter(is.na(cyc_1_cf_direction)) # no uncoded cy2 in this df
mapview(cam_18_dir_query, zcol = "cyc_1_vehicle_direction_from") # travelling West to EAst so contraflow

# Mark to exclude those where we dont have a direction that doesnt match with flow or contraflow
cam_18_dir_final = cam_18_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", TRUE ~ "Exclude")) %>% 
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

# 19) cam31
# enables contraflow cycling in existing one way street
# Contraflow direction is SE to NW
cam_31 = camden %>% filter(unique_contraflow_ID == "cam31")
mapview(cam_31, zcol = "cyc_1_vehicle_direction_from")

# If vehicle_from SE to NW = contraflow, no cyc_2 observations
cam_31_dir = cam_31 %>% mutate(
  cyc_1_cf_direction = case_when(
    cyc_1_vehicle_direction_from == "South East" & cyc_1_vehicle_direction_to == "North West" ~ "Contraflow",
    cyc_1_vehicle_direction_from == "North West" & cyc_1_vehicle_direction_to == "South East" ~ "With flow",
    cyc_1_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"),
  cyc_2_cf_direction = NA)

# Examine those where vehicle direction doesnt match with/contraflow - can some of these be coded?
cam_31_dir_query = cam_31_dir %>% filter(is.na(cyc_1_cf_direction)) # no uncoded cy2 in this df
mapview(cam_31_dir_query, zcol = "cyc_1_vehicle_direction_from") # not travelling on road segment

# Mark to exclude those where we dont have a direction that doesnt match with flow or contraflow
cam_31_dir_final = cam_31_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", TRUE ~ "Exclude")) %>% 
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining


# 20) cam46
# Enables cf cycling in existing one way street
# contraflow direction is SE to NW
cam_46 = camden %>% filter(unique_contraflow_ID == "cam46")
mapview(cam_46, zcol = "cyc_1_vehicle_direction_from")

# If vehicle_from SE to NW = contraflow, no cyc_2 observations
cam_46_dir = cam_46 %>% mutate(
  cyc_1_cf_direction = case_when(
    cyc_1_vehicle_direction_from == "South East" & cyc_1_vehicle_direction_to == "North West" ~ "Contraflow",
    cyc_1_vehicle_direction_from == "North West" & cyc_1_vehicle_direction_to == "South East" ~ "With flow",
    cyc_1_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"),
  cyc_2_cf_direction = NA)
 
# Examine those where vehicle direction doesnt match with/contraflow - can some of these be coded?
cam_46_dir_query = cam_46_dir %>% filter(is.na(cyc_1_cf_direction)) # no uncoded cy2 in this df
mapview(cam_46_dir_query, zcol = "cyc_1_vehicle_direction_from") # none travelling on road segment

# Mark to exclude those where we dont have a direction that doesnt match with flow or contraflow
cam_46_dir_final = cam_46_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", TRUE ~ "Exclude")) %>% 
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

# 21) cam51  
# Enables contraflow cycling on existing one way stret
# Contraflow cycling is travelling east to west
cam_51 = camden %>% filter(unique_contraflow_ID == "cam51")
mapview(cam_51, zcol = "cyc_1_vehicle_direction_from")

# If vehicle_from East to West = contraflow, no cyc_2 observations
cam_51_dir = cam_51 %>% mutate(
  cyc_1_cf_direction = case_when(
    cyc_1_vehicle_direction_from == "East" & cyc_1_vehicle_direction_to == "West" ~ "Contraflow",
    cyc_1_vehicle_direction_from == "West" & cyc_1_vehicle_direction_to == "East" ~ "With flow",
    cyc_1_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"),
  cyc_2_cf_direction = NA)

# Examine those where vehicle direction doesnt match with/contraflow - can some of these be coded?
cam_51_dir_query = cam_51_dir %>% filter(is.na(cyc_1_cf_direction)) # no uncoded cy2 in this df
mapview(cam_51_dir_query, zcol = "cyc_1_vehicle_direction_from") # none travelling on road segment

# Mark to exclude those where we dont have a direction that doesnt match with flow or contraflow
cam_51_dir_final = cam_51_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", TRUE ~ "Exclude")) %>% 
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

# 22) cam7 
# enables cf cycling on one way street
# contraflow durection is NW to SE
cam_7 = camden %>% filter(unique_contraflow_ID == "cam7")
mapview(cam_7, zcol = "cyc_1_vehicle_direction_from")

# If vehicle_from NW to SE = contraflow, no cyc_2 observations
cam_7_dir = cam_7 %>% mutate(
  cyc_1_cf_direction = case_when(
    cyc_1_vehicle_direction_from == "North West" & cyc_1_vehicle_direction_to == "South East" ~ "Contraflow",
    cyc_1_vehicle_direction_from == "South East" & cyc_1_vehicle_direction_to == "North West" ~ "With flow",
    cyc_1_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"),
  cyc_2_cf_direction = NA)

# Examine those where vehicle direction doesnt match with/contraflow - can some of these be coded?
cam_7_dir_query = cam_7_dir %>% filter(is.na(cyc_1_cf_direction)) # no uncoded cy2 in this df
mapview(cam_7_dir_query, zcol = "cyc_1_vehicle_direction_from") # none travelling on road segment

# Mark to exclude those where we dont have a direction that doesnt match with flow or contraflow
cam_7_dir_final = cam_7_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", TRUE ~ "Exclude")) %>% 
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

# 23) cam57 - sort this one out as introduces one way stret
#Contraflow direction is from NW to SE
cam_57 = camden %>% filter(unique_contraflow_ID == "cam57")
mapview(cam_57, zcol = "include_for_direction")
# But both crashes involving pedal cycles travelling NE to SW so not on road segment

# Mark to exclude those where we dont have a direction that doesnt match with flow or contraflow
cam_57_dir_final = cam_57 %>%
  mutate(cyc_1_cf_direction = NA, cyc_2_cf_direction = NA) %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", TRUE ~ "Exclude")) %>% 
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

# 24) cam14 - also sort this one as introduces one way street
#Contraflow direction is from NE to SW
cam_14 = camden %>% filter(unique_contraflow_ID == "cam14")
mapview(cam_14, zcol = "include_for_direction") # 

cam_14_dir = cam_14 %>% mutate(
  cyc_1_cf_direction = case_when(
    cyc_1_vehicle_direction_from == "South West" & cyc_1_vehicle_direction_to == "North East" ~ "With flow",
    cyc_1_vehicle_direction_from == "North East" & cyc_1_vehicle_direction_to == "South West" ~ "Contraflow",
    cyc_1_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"),
  cyc_2_cf_direction = NA)

# Examine those where vehicle direction doesnt match with/contraflow - can some of these be coded?
cam_14_dir_query = cam_14_dir %>% filter(is.na(cyc_1_cf_direction)) # no uncoded cy2 in this df
mapview(cam_14_dir_query, zcol = "cyc_1_vehicle_direction_from") # none travelling on road segment

# Mark to exclude those where we dont have a direction that doesnt match with flow or contraflow
cam_14_dir_final = cam_14_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", TRUE ~ "Exclude")) %>% 
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

# Sort out all the remaining camden road segments
cam_already_done = c("cam55", "cam16", "cam1", "cam12", "cam4", "cam6", "cam54", 
                     "cam5", "cam61", "cam13", "cam2", "cam34", "cam62", "cam41", 
                     "cam28", "cam11", "cam43", "cam18", "cam31", "cam46", "cam51",
                     "cam7", "cam57", "cam14")

cam_not_done = camden %>% filter(!unique_contraflow_ID %in% cam_already_done)
unique(cam_not_done$introduces_one_way_street) # FALSE so no need to address these
mapview(cam_not_done, zcol = "cyc_1_vehicle_direction_from") + mapview(unique_tro_df, zcol = "unique_contraflow_ID")

cam_not_done_dir = cam_not_done %>% mutate(
  cyc_1_cf_direction = case_when(
    unique_contraflow_ID == "cam19" & cyc_1_vehicle_direction_from == "North West" ~ "Contraflow",
    unique_contraflow_ID == "cam26" & cyc_1_vehicle_direction_from == "South West" ~ "Contraflow",
    unique_contraflow_ID == "cam3" & cyc_1_vehicle_direction_from == "South West" ~ "Contraflow",
    unique_contraflow_ID == "cam35" & cyc_1_vehicle_direction_from == "South West" ~ "Contraflow",
    unique_contraflow_ID == "cam36" & cyc_1_vehicle_direction_from == "South East" ~ "Contraflow",
    unique_contraflow_ID == "cam40" & cyc_1_vehicle_direction_from == "South East" ~ "With flow",
    unique_contraflow_ID == "cam50" & cyc_1_vehicle_direction_from == "North" ~ "With flow",
    unique_contraflow_ID == "cam56" & cyc_1_vehicle_direction_from == "East" ~ "Contraflow",
    unique_contraflow_ID == "cam58" & cyc_1_vehicle_direction_from == "South West" ~ "Contraflow",
    cyc_1_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"),
  cyc_2_cf_direction = NA)
  
# list of cf where direction not compatible with road segment or 'unknown':
# cam10, cam17, cam20, cam21, cam23, cam29, cam30, cam33, cam38, cam39, cam44, cam49, cam52

# Mark to exclude those where we dont have a direction that doesnt match with flow or contraflow
cam_not_done_dir_final = cam_not_done_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", TRUE ~ "Exclude")) %>% 
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

#  Join all these camden df together

camden_dir_final = rbind(cam55_dir_final, cam16_dir_final, cam1_dir_final, cam12_dir_final, cam4_dir_final, cam6_dir_final, cam54_dir_final, 
                         cam5_dir_final, cam61_dir_final, cam13_dir_final, cam2_dir_final, cam34_dir_final, cam62_dir_final, cam41_dir_final, 
                         cam28_dir_final, cam11_dir_final, cam43_dir_final, cam_18_dir_final, cam_31_dir_final, cam_46_dir_final, cam_51_dir_final,
                         cam_7_dir_final, cam_57_dir_final, cam_14_dir_final, cam_not_done_dir_final)
nrow(camden_dir_final) == nrow(camden) #TRUE

# Save camden
#saveRDS(camden_dir_final, file = "data-processed/pedal_cycle_direction/camden_dir_final.Rds")

# checked - number of unknown matches for cyc 1& 2 with the original min_joined_crashes_pc_direction - see below
# unknown_cyc_1 = min_joined_crashes_pc_direction %>% group_by(BOROUGH, cyc_1_vehicle_direction_from) %>% summarise(count = n()) %>%
#   st_drop_geometry() %>% filter(cyc_1_vehicle_direction_from == "unknown (self reported)")
# 
# unknown_cyc_2 = min_joined_crashes_pc_direction %>% group_by(BOROUGH, cyc_2_vehicle_direction_from) %>% summarise(count = n()) %>%
#   st_drop_geometry() %>% filter(cyc_2_vehicle_direction_from == "unknown (self reported)")


                             ################## 
                             # City of London #
                             ##################

city = min_joined_crashes_pc_direction %>% filter(BOROUGH == "City of London")
count = city %>% st_drop_geometry() %>% group_by(unique_contraflow_ID) %>% summarise(count = n())
# unique_contraflow_ID count
# <fct>                <int>
# 1 city44                  27
# 2 city513                 15
# 3 city1                   14
# 4 city6                   12
# 5 city12                   9
# 6 city19                   9
# 7 city11                   8
# 8 city2                    7
# 9 city527                  7
# 10 city530                  7
# 11 city532                  7
# 12 city8                    7
# 13 city42                   6
# 14 city54                   6
# 15 city50                   5
# 16 city500                  5
# 17 city521                  5
# 18 city528                  5
# 19 city13                   4
# 20 city46                   4
# 21 city508                  4
# 22 city525                  4
# 23 city531                  4
# 24 city7                    4
# 25 city503                  3
# 26 city504                  3  nb removed
# 27 city510                  3
# 28 city511                  3
# 29 city518                  3
# 30 city520                  3
# 31 city529                  3
# 32 city56                   3
# 33 city60                   3
# 34 city14                   2
# 35 city20                   2
# 36 city33                   2
# 37 city37                   2
# 38 city41                   2
# 39 city49                   2
# 40 city514                  2
# 41 city517                  2
# 42 city519                  2
# 43 city58                   2
# 44 city17                   1
# 45 city18                   1
# 46 city22                   1
# 47 city27                   1
# 48 city28                   1
# 49 city3                    1
# 50 city31                   1
# 51 city34                   1
# 52 city4                    1
# 53 city45                   1
# 54 city502                  1
# 55 city505                  1
# 56 city507                  1
# 57 city509                  1
# 58 city51                   1  NB removed
# 59 city512                  1
# 60 city52                   1
# 61 city59                   1
# 62 city9                    1


unique(city$contraflow_stop_date) # 2 diff removal dates so need to handle these city 504 & 51
unique(city$introduces_one_way_street) # TRUE & FALSE therefore need to check this before assigning direction
# no unknwons in City 

# 1) city44 
city44 = city %>% filter(unique_contraflow_ID == "city44")
mapview(city44, zcol = "include_for_direction") + mapview(unique_tro_df)
# impossible to determine which direction is with flow/contraflow as supercomplex junction

city44_dir_final = city44 %>%
  mutate(cyc_1_cf_direction = NA, cyc_2_cf_direction = NA, include_for_direction_checked = "Exclude") %>%  # no unknowns  
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining
  
#2) city513 
city513 = city %>% filter(unique_contraflow_ID == "city513")
mapview(city513, zcol = "cyc_1_vehicle_direction_from") + mapview(unique_tro_df)
# introduces contraflow cycling on one way street, no cyc2
# contraflow is NE to SW

# No pedal cycles are travelling with flow/ contraflow
city513_dir_final = city513 %>%
  mutate(cyc_1_cf_direction = NA, cyc_2_cf_direction = NA, include_for_direction_checked = "Exclude") %>%    # no unknowns
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

# 3) city1
city1 = city %>% filter(unique_contraflow_ID == "city1")
mapview(city1, zcol = "cyc_1_vehicle_direction_from") + mapview(unique_tro_df)
# Introduces one way street (with flow is from west to east)
# There is one cyc_2
# Only one crash involves east/west - and that occured when contraflow was in place
city1 = city1 %>% mutate(cyc_1_cf_direction = NA, cyc_2_cf_direction = NA) # no unknowns
city1_dir_final = city1 %>% 
  mutate(cyc_1_cf_direction = case_when(
    cyc_1_vehicle_direction_from == "East" & cyc_1_vehicle_direction_to == "West" ~ "Contraflow")) %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", TRUE ~ "Exclude")) %>% # still works even for cyc_2 for this cf seg
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining
      
# 4) city6
city6 = city %>% filter(unique_contraflow_ID == "city6")
mapview(city6, zcol = "cyc_1_vehicle_direction_from") + mapview(unique_tro_df) 
# introduces contraflow cycling on one way street, no cyc_"
# contraflow direction is South to north

# Code those where vehicle direction matches with/contraflow
city6_dir = city6 %>% mutate(
  cyc_1_cf_direction = case_when(
    cyc_1_vehicle_direction_from == "South East" & cyc_1_vehicle_direction_to == "North" ~ "Contraflow",
    cyc_1_vehicle_direction_from == "North" & cyc_1_vehicle_direction_to == "South East" ~ "Contraflow",
    cyc_1_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"),
  cyc_2_cf_direction = NA)

city6_dir_final = city6_dir %>% 
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", TRUE ~ "Exclude")) %>% # still works even for cyc_2 for this cf seg
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

# 5) city12
city12 = city %>% filter(unique_contraflow_ID == "city12")
mapview(city12, zcol = "cyc_1_vehicle_direction_from") + mapview(unique_tro_df) 
# introduces cf cycling on a one way street
# no cyc_2
# contraflow is SOuth to north but no pedal cycles have that direction
city12_dir_final = city12 %>%
  mutate(cyc_1_cf_direction = NA, cyc_2_cf_direction = NA, include_for_direction_checked = "Exclude") %>%    
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

#6) city19 
city19 = city %>% filter(unique_contraflow_ID == "city19")
mapview(city19, zcol = "cyc_1_vehicle_direction_from") + mapview(unique_tro_df) 
# enables contraflow cycling in a one way street
# no cyc_2
# contraflow is travelling south to north
city19_north = city19 %>% filter(cyc_1_vehicle_direction_from == "North")
mapview(city19_north) # only one I think can be coded in terms of direction
# 201401CP00391  North to SE with flow
city19_dir = city19 %>%
  mutate(cyc_1_cf_direction = NA, cyc_2_cf_direction = NA) # create empty columns
# replace the crash above 
city19_dir$cyc_1_cf_direction = replace(city19_dir$cyc_1_cf_direction,
                                       which(city19_dir$accident_index == "201401CP00391"), 
                                       values = "With flow")
city19_dir_final = city19_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", TRUE ~ "Exclude")) %>%
    select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

# 7) city11
city11 = city %>% filter(unique_contraflow_ID == "city11")
mapview(city11, zcol = "cyc_1_vehicle_direction_from") + mapview(unique_tro_df) 
# enables contraflow cycling on a one way street
# contraflow cycling is west to east
# only one pedal cycling is travelling on axis of street, no cyc_2

# Code those where vehicle direction matches with/contraflow
city11_dir = city11 %>% mutate(
  cyc_1_cf_direction = case_when(
    cyc_1_vehicle_direction_from == "West" & cyc_1_vehicle_direction_to == "East" ~ "Contraflow"),
  cyc_2_cf_direction = NA) # add column but there are no cyc 2
# Identify those crashes that definately need to be incldued in the direction crash calculation because we have a direction that matches with flow or contraflow
city11_dir_final = city11_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", TRUE ~ "Exclude")) %>%
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

# 8) city2  
city2 = city %>% filter(unique_contraflow_ID == "city2")
mapview(city2, zcol = "cyc_1_vehicle_direction_from") + mapview(unique_tro_df) 
# introduces contraflow bus lane that allows cycling, not removed
# contraflow cycling is SE to NW
# no cyc_2

city2_dir = city2 %>% mutate(
  cyc_1_cf_direction = case_when(
    cyc_1_vehicle_direction_from == "North West" & cyc_1_vehicle_direction_to == "South East" ~ "With flow",
    cyc_1_vehicle_direction_from == "West" & cyc_1_vehicle_direction_to == "East" ~ "With flow",
    cyc_1_vehicle_direction_from == "South East" & cyc_1_vehicle_direction_to == "North West" ~ "Contraflow"),
  cyc_2_cf_direction = NA) # add column but there are no cyc 2

city2_dir_final = city2_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", TRUE ~ "Exclude")) %>%
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

# 9) city527 
city527 = city %>% filter(unique_contraflow_ID == "city527")
mapview(city527, zcol = "cyc_1_vehicle_direction_from") + mapview(unique_tro_df) 
# introduces ONE WAY STREET - but no crashes occured precontraflow 
# not removed
# contraflow cycling is E/SE to W/NW
# no cyc_2

city527_dir = city527 %>% mutate(
  cyc_1_cf_direction = case_when(
    cyc_1_vehicle_direction_from == "East" & cyc_1_vehicle_direction_to == "West" ~ "Contraflow"), # only 1 crash needs labelling
  cyc_2_cf_direction = NA) # add column but there are no cyc 2

city527_dir_final = city527_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", TRUE ~ "Exclude")) %>%
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

#10) city530 
city530 = city %>% filter(unique_contraflow_ID == "city530")
mapview(city530, zcol = "cyc_1_vehicle_direction_from") + mapview(unique_tro_df) 
# enables cf cycling, not removed
# no cyc_"
# contraflow direction - road bends but generally SE to NW apart from one end

city530_dir = city530 %>% mutate(
  cyc_1_cf_direction = case_when(
    cyc_1_vehicle_direction_from == "East" & cyc_1_vehicle_direction_to == "West" ~ "Contraflow",
    cyc_1_vehicle_direction_from == "West" & cyc_1_vehicle_direction_to == "East" ~ "With flow",
    cyc_1_vehicle_direction_from == "South West" & cyc_1_vehicle_direction_to == "North East" ~ "With flow"),
  cyc_2_cf_direction = NA) # add column but there are no cyc 2

city530_dir_final = city530_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", TRUE ~ "Exclude")) %>%
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

#11) city532
city532 = city %>% filter(unique_contraflow_ID == "city532")
mapview(city532, zcol = "cyc_1_vehicle_direction_from") + mapview(unique_tro_df) 
# enables cf cycling, not removed
# no cyc_2
# contraflow direction is East/NE to West/SW

city532_dir = city532 %>% mutate(
  cyc_1_cf_direction = case_when(
    cyc_1_vehicle_direction_from == "East" & cyc_1_vehicle_direction_to == "West" ~ "Contraflow",
    cyc_1_vehicle_direction_from == "North East" & cyc_1_vehicle_direction_to == "South West" ~ "Contraflow",
    cyc_1_vehicle_direction_from == "West" & cyc_1_vehicle_direction_to == "East" ~ "With flow",
    cyc_1_vehicle_direction_from == "South West" & cyc_1_vehicle_direction_to == "North East" ~ "With flow"),
  cyc_2_cf_direction = NA) # add column but there are no cyc 2

city532_dir_final = city532_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", TRUE ~ "Exclude")) %>%
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

# 12) city8
city8 = city %>% filter(unique_contraflow_ID == "city8")
mapview(city8, zcol = "cyc_1_vehicle_direction_from") + mapview(unique_tro_df) 
# enabled cf cycling, no cyc-2
# contraflow is north to south
city8_dir = city8 %>%
  mutate(cyc_1_cf_direction = NA, cyc_2_cf_direction = NA) # create empty columns - no directions areN/S or S/N or even similar
city8_dir_final = city8_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", TRUE ~ "Exclude")) %>%
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

# 13) city42
city42 = city %>% filter(unique_contraflow_ID == "city42")
mapview(city42, zcol = "cyc_1_vehicle_direction_from", map.types = "OpenStreetMap") + mapview(unique_tro_df) 
# enabled cf cycling, no cyc-2
# contraflow is north to south
city42_dir = city42 %>%
  mutate(cyc_1_cf_direction = NA, cyc_2_cf_direction = NA) # create empty columns - no directions areN/S or S/N or even similar
city42_dir_final = city42_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", TRUE ~ "Exclude")) %>%
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

# 14) city54
city54 = city %>% filter(unique_contraflow_ID == "city54")
mapview(city54, zcol = "cyc_1_vehicle_direction_from", map.types = "OpenStreetMap") + mapview(unique_tro_df) 
# enabled cf cycling, no cyc-2
# contraflow is north to south
city54_dir = city54 %>%
  mutate(cyc_1_cf_direction = NA, cyc_2_cf_direction = NA) # create empty columns - no directions areN/S or S/N or even similar
city54_dir_final = city54_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", TRUE ~ "Exclude")) %>%
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

# 15) sort out ones where cf removed
#city504 and city 51
city_cf_removed = city %>% filter(unique_contraflow_ID == "city504" | unique_contraflow_ID == "city51")
mapview(city_cf_removed, zcol = "cyc_1_vehicle_direction_from", map.types = "OpenStreetMap") + mapview(unique_tro_df) 
city_cf_removed_dir = city_cf_removed %>%
  mutate(cyc_1_cf_direction = NA, cyc_2_cf_direction = NA) %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", TRUE ~ "Exclude")) %>%
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining
# unable to determine direction from TRO or OSM (not there for when they were introduced or removed)

# 16) sort out rest of one_way_street ones
city %>% filter(introduces_one_way_street == "TRUE") %>% group_by(unique_contraflow_ID) %>% 
  st_drop_geometry() %>% summarise(count = n()) %>% arrange(desc(count))
# unique_contraflow_ID count
#  1 city1                   14 # sorted
# 2 city527                  7 # sorted
# 3 city50                   5 # checked - doesnt meet direction
# 4 city528                  5 # only one has a direction on street axis
# 5 city529                  3 # checked - doesnt meet direction
# 6 city3                    1 # checked - doesnt meet direction
city_one_way_ID = c("city50", "city528", "city529", "city3")
city_one_way = city %>% filter(unique_contraflow_ID %in% city_one_way_ID)
mapview(city_one_way, zcol = "cyc_1_vehicle_direction_from", map.types = "OpenStreetMap") + mapview(unique_tro_df) 

city_one_way_dir = city_one_way %>%
  mutate(cyc_1_cf_direction = NA, cyc_2_cf_direction = NA)

city_one_way_dir$cyc_1_cf_direction = replace(city_one_way_dir$cyc_1_cf_direction,
                                       which(city_one_way_dir$accident_index == "200401CP00204"), 
                                       values = "With flow") # change the value that I can determine

city_one_way_dir_final = city_one_way_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", TRUE ~ "Exclude")) %>%
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

# Do all the other ones that havent been checked yet
already_done = c("city44", "city513", "city1", "city6", "city12", "city19", "city11", "city2", "city527",
                 "city530", "city532", "city8", "city42", "city54", "city504", "city51", "city50", "city528",
                 "city529", "city3")
need_doing = city %>% filter(!unique_contraflow_ID %in% already_done)
mapview(need_doing, zcol = "cyc_1_vehicle_direction_from", map.types = "OpenStreetMap") + mapview(unique_tro_df) 


need_doing_dir = need_doing %>% mutate(
   cyc_1_cf_direction = case_when(
     unique_contraflow_ID == "city37" & cyc_1_vehicle_direction_from == "East" ~ "Contraflow",
     unique_contraflow_ID == "city502" & cyc_1_vehicle_direction_from == "South West" ~ "Contraflow",
     unique_contraflow_ID == "city505" & cyc_1_vehicle_direction_from == "South West" ~ "With flow",
     accident_index == "200501CP00178" ~ "Contraflow", 
     unique_contraflow_ID == "city7" & cyc_1_vehicle_direction_from == "South East" ~ "With flow",
     unique_contraflow_ID == "city9" & cyc_1_vehicle_direction_from == "South West" ~ "Contraflow"),
   cyc_2_cf_direction = NA)

# list of cf where direction not compatible with road segment or 'unknown':
# city13, city14, city17, city18, city20, city22, city27, city28, city31, city33, city34, city4, city41,
# city 45, city46, city49, city500, city503, city507 , city 508, city509, city510, city511, city512, city514,
# city517, city518, city519, city520, city521, city525, city531, city52, city56, city58, city59, city60

# Mark to exclude those where we dont have a direction that doesnt match with flow or contraflow
need_doing_dir_final = need_doing_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", TRUE ~ "Exclude")) %>% # still works even for cyc_2 for this cf seg
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

# Join all the city df together
city_dir_final = rbind(city44_dir_final, city513_dir_final, city1_dir_final, city6_dir_final,
                       city12_dir_final, city19_dir_final, city11_dir_final, city2_dir_final,
                       city527_dir_final, city530_dir_final, city532_dir_final, city8_dir_final,
                       city42_dir_final, city54_dir_final, city_cf_removed_dir, city_one_way_dir_final,
                       need_doing_dir_final)
nrow(city_dir_final) == nrow(city) #TRUE

# Save city
#saveRDS(city_dir_final, file = "data-processed/pedal_cycle_direction/city_dir_final.Rds")

                            ################## 
                            #    Greenwich   #
                            ##################

green = min_joined_crashes_pc_direction %>% filter(BOROUGH == "Greenwich")
count = green %>% st_drop_geometry() %>% group_by(unique_contraflow_ID) %>% summarise(count = n())
# no cyc_2
unique(green$contraflow_stop_date) # NA
unique(green$introduces_one_way_street) # FALSE


mapview(green, zcol = "cyc_1_vehicle_direction_from", map.types = "OpenStreetMap") + mapview(unique_tro_df) 

# grn13, grn5, grn8 & grn9 - none have anything compatible with road segment axis
green_dir = green %>% mutate(
  cyc_1_cf_direction = case_when(
    unique_contraflow_ID == "grn17" & cyc_1_vehicle_direction_from == "South West" & 
      cyc_1_vehicle_direction_to == "North East" ~ "With flow",
    cyc_1_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"),
  cyc_2_cf_direction = NA) # add column but there are no cyc 2

green_dir_final = green_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", TRUE ~ "Exclude")) %>% # still works even for cyc_2 for this cf seg
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

# Save Greenwich
#saveRDS(green_dir_final, file = "data-processed/pedal_cycle_direction/green_dir_final.Rds")


                                  ################## 
                                  #     Hackney    #
                                  ##################

hack = min_joined_crashes_pc_direction %>% filter(BOROUGH == "Hackney")
count = hack %>% st_drop_geometry() %>% group_by(unique_contraflow_ID) %>% summarise(count = n())
#no cyc_2
unique(hack$contraflow_stop_date) # one stop date
unique(hack$introduces_one_way_street) # TRUE and FALSE so will need to manage

# hac17, 19 and 28 have lots the rest in single digits

# 1) hac17
hac17 = hack %>% filter(unique_contraflow_ID == "hac17")
# introduces one way street, no stop date, no cyc2
# contraflow directions is West to east

hac17_dir = hac17 %>% mutate(
  cyc_1_cf_direction = case_when(
    cyc_1_vehicle_direction_from == "West" & cyc_1_vehicle_direction_to == "East" ~ "Contraflow",
    cyc_1_vehicle_direction_from == "East" & cyc_1_vehicle_direction_to == "West" ~ "With flow",
    cyc_1_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"),
  cyc_2_cf_direction = NA) # add column but there are no cyc 2

hac17_dir_final = hac17_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", TRUE ~ "Exclude")) %>%
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

# 2) hac19
hac19 = hack %>% filter(unique_contraflow_ID == "hac19")
# introduces cf cycling, no stop date, no cyc2
# contraflow directions is East to West

mapview(hac19, zcol = "cyc_1_vehicle_direction_from", map.types = "OpenStreetMap") + mapview(unique_tro_df)
# hac_west = hac19 %>% filter(cyc_1_vehicle_direction_from == "West") # dont include these - not on road segment
# mapview(hac_west)
hac19_dir = hac19 %>% mutate(
  cyc_1_cf_direction = case_when(
    cyc_1_vehicle_direction_from == "West" & cyc_1_vehicle_direction_to == "East" ~ "With flow",
    cyc_1_vehicle_direction_from == "East" & cyc_1_vehicle_direction_to == "West" ~ "Contraflow",
    cyc_1_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"),
  cyc_2_cf_direction = NA) # add column but there are no cyc 2

hac19_dir_final = hac19_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", TRUE ~ "Exclude")) %>%
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

# 3) hac28
hac28 = hack %>% filter(unique_contraflow_ID == "hac28")
mapview(hac28, zcol = "cyc_1_vehicle_direction_from", map.types = "OpenStreetMap") + mapview(unique_tro_df)
# introduces cf cycling, no stop date, no cyc2
# contraflow is N to south
hac28_dir = hac28 %>% mutate(
  cyc_1_cf_direction = case_when(
    cyc_1_vehicle_direction_from == "North" & cyc_1_vehicle_direction_to == "South East" ~ "Contraflow",
    cyc_1_vehicle_direction_from == "North East" & cyc_1_vehicle_direction_to == "South West" ~ "Contraflow",
    cyc_1_vehicle_direction_from == "South East" & cyc_1_vehicle_direction_to == "North" ~ "With flow",
    cyc_1_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"),
  cyc_2_cf_direction = NA) # add column but there are no cyc 2

hac28_dir_final = hac28_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", TRUE ~ "Exclude")) %>%
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

# 4) Hackney stop date
hac_stop = hack %>% filter(!is.na(contraflow_stop_date)) #hac3
unique(hack$contraflow_stop_date) # one stop date
hack_stop_direction_present = hac_stop %>% filter(cyc_1_vehicle_direction_from != "unknown (self reported)")
mapview(hack_stop_direction_present)
# contraflow is west to east, NB crashes occured after contraflow removed
hac_3_dir = hac_stop %>% mutate(
  cyc_1_cf_direction = case_when(
    cyc_1_vehicle_direction_from == "West" & cyc_1_vehicle_direction_to == "East" ~ "Contraflow",
    cyc_1_vehicle_direction_from == "North East" & cyc_1_vehicle_direction_to == "South West" ~ "With flow",
    cyc_1_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"),
  cyc_2_cf_direction = NA) # add column but there are no cyc 2

hac3_dir_final = hac_3_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", TRUE ~ "Exclude")) %>%
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

# 5) Rest of hackney
done = c("hac3", "hac17", "hac19", "hac28")
hac_not_done = hack %>% filter(!unique_contraflow_ID %in%done)
mapview(hac_not_done, zcol = "cyc_1_vehicle_direction_from", map.types = "OpenStreetMap") + mapview(unique_tro_df) 

# checked but direction not compatible
# hac11, hac13, hac14, hac26, hac6
hac_not_done_dir = hac_not_done %>% mutate(
  cyc_1_cf_direction = case_when(
    unique_contraflow_ID == "hac12" & cyc_1_vehicle_direction_from == "East" ~ "Contraflow",
    unique_contraflow_ID == "hac12" & cyc_1_vehicle_direction_from == "North East" ~ "Contraflow",
    unique_contraflow_ID == "hac12" & cyc_1_vehicle_direction_from == "West" ~ "With flow",
    unique_contraflow_ID == "hac18" & cyc_1_vehicle_direction_from == "North" ~ "With flow",
    unique_contraflow_ID == "hac25" & cyc_1_vehicle_direction_from == "East" ~ "Contraflow",
    unique_contraflow_ID == "hac27" & cyc_1_vehicle_direction_from == "North East" ~ "Contraflow",
    unique_contraflow_ID == "hac29" & cyc_1_vehicle_direction_from == "North East" ~ "Contraflow",
    unique_contraflow_ID == "hac30" & cyc_1_vehicle_direction_from == "West" ~ "With flow",
    unique_contraflow_ID == "hac30" & cyc_1_vehicle_direction_from == "South East" ~ "Contraflow",
    unique_contraflow_ID == "hac4" & cyc_1_vehicle_direction_from == "South West" ~ "Contraflow",
    unique_contraflow_ID == "hac7" & cyc_1_vehicle_direction_from == "North West" ~ "Contraflow",
    cyc_1_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"),
  cyc_2_cf_direction = NA)

hac_not_done_dir_final = hac_not_done_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", TRUE ~ "Exclude")) %>% # still works even for cyc_2 for this cf seg
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining


# Join up hackney df

hack_dir_final = rbind(hac17_dir_final, hac19_dir_final, hac28_dir_final, hac3_dir_final, hac_not_done_dir_final)
nrow(hack_dir_final) == nrow(hack) #TRUE

# Save Hackney
#saveRDS(hack_dir_final, file = "data-processed/pedal_cycle_direction/hack_dir_final.Rds")


                                ################## 
                                #    Ham & Ful   #
                                ##################

ham = min_joined_crashes_pc_direction %>% filter(BOROUGH == "Hammersmith and Fulham")
count = ham %>% st_drop_geometry() %>% group_by(unique_contraflow_ID) %>% summarise(count = n())
# only 5 unique cf IDs
# none have cyc_2
# no unknown direction/selfreported

unique(ham$contraflow_stop_date) # NA
unique(ham$introduces_one_way_street) # TRUE and FALSE so will need to manage

mapview(ham, zcol = "cyc_1_vehicle_direction_from", map.types = "OpenStreetMap") + mapview(unique_tro_df) 


# ham2, ham4  - none have anything compatible with road segment axis
# in ham 8 2017010014598, 200801FH10225, 	200001FH00908  - with flow whilst 199901FH00665 = contraflow
ham8_with = c("2017010014598", "200801FH10225", "200001FH00908")
ham_dir = ham %>% mutate(
  cyc_1_cf_direction = case_when(
    unique_contraflow_ID == "ham1" & cyc_1_vehicle_direction_from == "South West" &
      cyc_1_vehicle_direction_to == "North East" ~ "Contraflow",
    unique_contraflow_ID == "ham5" & cyc_1_vehicle_direction_from == "North" &
      cyc_1_vehicle_direction_to == "South East" ~ "With flow",
    accident_index == "199901FH00665" ~ "Contraflow", 
    accident_index %in% ham8_with ~ "With flow"),
  cyc_2_cf_direction = NA) # add column but there are no cyc 2

ham_dir_final = ham_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", TRUE ~ "Exclude")) %>% # still works even for cyc_2 for this cf seg
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

# Save H&F
#saveRDS(ham_dir_final, file = "data-processed/pedal_cycle_direction/ham_dir_final.Rds")


                            ################## 
                            #   Islington    #
                            ##################
isl = min_joined_crashes_pc_direction %>% filter(BOROUGH == "Islington")
count = isl %>% st_drop_geometry() %>% group_by(unique_contraflow_ID) %>% summarise(count = n())
#2 have cyc2 - isl6 and isl5

# unique_contraflow_ID count
# 1 isl1                    72
# 2 isl6                    17
# 3 isl16                   12
# 4 isl5                    12
# 5 isl8                    11
# 6 isl15                    9
# 7 isl2                     9
# 8 isl4                     4
# 9 isl10                    3
# 10 isl9                     3
# 11 isl11                    2
# 12 isl13                    2
# 13 isl3                     2

unique(isl$contraflow_stop_date) # NA
unique(isl$introduces_one_way_street) # TRUE and FALSE so will need to manage

mapview(ham, zcol = "cyc_1_vehicle_direction_from", map.types = "OpenStreetMap") + mapview(unique_tro_df) 

# 1) isl1
# westbound contra-flow bus lane introduced, no one way system introduced
# Therefore for both pre-cf and contraflow travelling east to west was contraflow
# no cyc-2 
isl1 = isl %>% filter(unique_contraflow_ID == "isl1")
mapview(isl1, zcol = "cyc_1_vehicle_direction_from", map.types = "OpenStreetMap")
 
# Code those where vehicle direction matches with/contraflow
isl1_dir = isl1 %>% mutate(
  cyc_1_cf_direction = case_when(
    cyc_1_vehicle_direction_from == "East" & cyc_1_vehicle_direction_to == "West" ~ "Contraflow",
    cyc_1_vehicle_direction_from == "North East" & cyc_1_vehicle_direction_to == "South West" ~ "Contraflow",
    cyc_1_vehicle_direction_from == "West" & cyc_1_vehicle_direction_to == "East" ~ "With flow",
    cyc_1_vehicle_direction_from == "South West" & cyc_1_vehicle_direction_to == "North East" ~ "With flow",
    cyc_1_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"),
  cyc_2_cf_direction = NA) # add column but there are no cyc 2 

# Examine those where vehicle direction doesnt match with/contraflow - can some of these be coded?
isl1_dir_query = isl1_dir %>% filter(is.na(cyc_1_cf_direction)) # n = 9 
# east to sw occur at junction with penton rise so not appropriate to code
mapview(isl1_dir_query, zcol = "cyc_1_vehicle_direction_from") + mapview(unique_tro_df)
#dont think any of these can be coded
 
# Identify those crashes that definately need to be incldued in the direction crash calculation because we have a direction that matches with flow or contraflow 
isl1_dir_final = isl1_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", TRUE ~ "Exclude")) %>%
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

#2) isl6
isl6 = isl %>% filter(unique_contraflow_ID == "isl6")
mapview(isl6, zcol = "cyc_1_vehicle_direction_from", map.types = "OpenStreetMap")
# has one cyc_2
# introduces cf cycle lane, not removed
# contraflow direction is north/NW to s/SE

# Code those where vehicle direction matches with/contraflow
isl6_dir = isl6 %>% mutate(
  cyc_1_cf_direction = case_when(
    cyc_1_vehicle_direction_from == "North West" & cyc_1_vehicle_direction_to == "South East" ~ "Contraflow",
    cyc_1_vehicle_direction_from == "North" & cyc_1_vehicle_direction_to == "South East" ~ "Contraflow",
    cyc_1_vehicle_direction_from == "South East" & cyc_1_vehicle_direction_to == "North West" ~ "With flow",
    cyc_1_vehicle_direction_from == "South East" & cyc_1_vehicle_direction_to == "North" ~ "With flow",
    cyc_1_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"),
  cyc_2_cf_direction = case_when(
    cyc_2_vehicle_direction_from == "North" & cyc_2_vehicle_direction_to == "South East" ~ "Contraflow")) 
# Rest checked and cant be coded

# Identify those crashes that definitely need to be included in the direction crash calculation because we have a direction that matches with flow or contraflow 
isl6_dir_final = isl6_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", 
    (cyc_2_cf_direction == "Contraflow" | cyc_2_cf_direction == "With flow") ~ "Include",
    TRUE ~ "Exclude")) %>%
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

#3) isl16 
isl16 = isl %>% filter(unique_contraflow_ID == "isl16")
mapview(isl16, zcol = "cyc_1_vehicle_direction_from", map.types = "OpenStreetMap") + mapview(unique_tro_df)
# introduces a one way street
# no cyc_2, not removed
# none are on the axis of the road segment
isl16_dir_final = isl16 %>% 
  mutate(cyc_1_cf_direction = case_when(cyc_1_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"),
         cyc_2_cf_direction = NA) %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", TRUE ~ "Exclude")) %>%
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

#4) isl5
isl5 = isl %>% filter(unique_contraflow_ID == "isl5")
mapview(isl5, zcol = "cyc_1_vehicle_direction_from", map.types = "OpenStreetMap")
# has one cyc_2 but direction is unknown
# introduces cf cycle lane, not remove
# contraflow direction is SW to NE

# Code those where vehicle direction matches with/contraflow
isl5_dir = isl5 %>% mutate(
  cyc_1_cf_direction = case_when(
    cyc_1_vehicle_direction_from == "North East" & cyc_1_vehicle_direction_to == "South West" ~ "With flow",
    cyc_1_vehicle_direction_from == "South West" & cyc_1_vehicle_direction_to == "North East" ~ "Contraflow",
    cyc_1_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"),
  cyc_2_cf_direction = case_when(
    cyc_2_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"))
# Rest checked and cant be coded

# Identify those crashes that definitely need to be included in the direction crash calculation because we have a direction that matches with flow or contraflow
isl5_dir_final = isl5_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include",
    (cyc_2_cf_direction == "Contraflow" | cyc_2_cf_direction == "With flow") ~ "Include",
    TRUE ~ "Exclude")) %>%
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

# 5) isl8
isl8 = isl %>% filter(unique_contraflow_ID == "isl8")
mapview(isl8, zcol = "cyc_1_vehicle_direction_from", map.types = "OpenStreetMap")
# introduces a one way street
# contraflow direction is from nw to se but it is on a curve so check all 

# check some of these
check_isl8 = isl8 %>%
  filter((cyc_1_vehicle_direction_from == "North" & cyc_1_vehicle_direction_to == "South East") |
           (cyc_1_vehicle_direction_from == "North West" & cyc_1_vehicle_direction_to == "South East"))
mapview(check_isl8, zcol = "cyc_1_vehicle_direction_from", map.types = "OpenStreetMap")  
# looks like on the one that is North to SE is codable

isl8_dir_final = isl8 %>% mutate(
    cyc_1_cf_direction = case_when(
      cyc_1_vehicle_direction_from == "North West" & cyc_1_vehicle_direction_to == "South East" ~ "Contraflow",
      cyc_1_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"),
    cyc_2_cf_direction = NA) %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", TRUE ~ "Exclude")) %>%
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

# 6) isl15 
isl15 = isl %>% filter(unique_contraflow_ID == "isl15")
mapview(isl15, zcol = "cyc_1_vehicle_direction_from", map.types = "OpenStreetMap") + mapview(unique_tro_df)
# Introduces a one way street
# no cyc_2

# unable to determine direction - contraflow complex and locations not right
isl15_dir_final = isl15 %>% mutate(
  cyc_1_cf_direction = case_when(cyc_1_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"),
  cyc_2_cf_direction = NA) %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", TRUE ~ "Exclude")) %>%
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

# 7) isl2
isl2 = isl %>% filter(unique_contraflow_ID == "isl2")
mapview(isl2, zcol = "cyc_1_vehicle_direction_from", map.types = "OpenStreetMap") + mapview(unique_tro_df)
# Enables cf cycling, not removed
# no cyc_2
# contraflow is North to South but may be a bit of SE
isl2_dir = isl2 %>% mutate(
  cyc_1_cf_direction = case_when(
    cyc_1_vehicle_direction_from == "North" & cyc_1_vehicle_direction_to == "South East" ~ "Contraflow",
    cyc_1_vehicle_direction_from == "North West" & cyc_1_vehicle_direction_to == "South East" ~ "Contraflow",
    cyc_1_vehicle_direction_from == "South East" & cyc_1_vehicle_direction_to == "North" ~ "With flow",
    cyc_1_vehicle_direction_from == "South East" & cyc_1_vehicle_direction_to == "North West" ~ "With flow",
    cyc_1_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"),
  cyc_2_cf_direction = NA)

isl2_dir_final = isl2_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", TRUE ~ "Exclude")) %>%
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

# 8) Sort out rest 
isl_done = c("isl1", "isl6", "isl16", "isl5", "isl8", "isl15", "isl2")
isl_not_done = isl %>% filter(!unique_contraflow_ID %in% isl_done)
mapview(isl_not_done, zcol = "cyc_1_vehicle_direction_from", map.types = "OpenStreetMap") + mapview(unique_tro_df)

# isl road segments that dont match road axis: isl13
isl_not_done_dir = isl_not_done %>% mutate(
  cyc_1_cf_direction = case_when(
    unique_contraflow_ID == "isl9" & cyc_1_vehicle_direction_from == "South East" & cyc_1_vehicle_direction_to == "North West" ~ "With flow",
    unique_contraflow_ID == "isl11" & cyc_1_vehicle_direction_from == "South West" & cyc_1_vehicle_direction_to == "South East" ~ "With flow",
    unique_contraflow_ID == "isl10" & cyc_1_vehicle_direction_from == "West" & cyc_1_vehicle_direction_to == "East" ~ "With flow",
    unique_contraflow_ID == "isl3" & cyc_1_vehicle_direction_from == "West" & cyc_1_vehicle_direction_to == "East" ~ "Contraflow",
    unique_contraflow_ID == "isl4" & cyc_1_vehicle_direction_from == "East" & cyc_1_vehicle_direction_to == "West" ~ "With flow",
    cyc_1_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"),
  cyc_2_cf_direction = NA)

isl_not_done_dir_final = isl_not_done_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", TRUE ~ "Exclude")) %>%
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining


# Join df together
isl_dir_final = rbind(isl1_dir_final, isl6_dir_final, isl16_dir_final, isl5_dir_final,
                      isl8_dir_final, isl15_dir_final, isl2_dir_final, isl_not_done_dir_final)

# Save Islington df
#saveRDS(isl_dir_final, file = "data-processed/pedal_cycle_direction/isl_dir_final.Rds")

                                            ################## 
                                            #   Ken & Chel   #
                                            ##################

ken = min_joined_crashes_pc_direction %>% filter(BOROUGH == "Kensington and Chelsea")
count = ken %>% st_drop_geometry() %>% group_by(unique_contraflow_ID) %>% summarise(count = n()) 
# none have cyc2, most segments only have 1 crash on

unique(ken$contraflow_stop_date) # NA
unique(ken$introduces_one_way_street) # FALSE
unique(ken$introduces_contraflow_bus_lane) # FALSE
mapview(ken, zcol = "cyc_1_vehicle_direction_from", map.types = "OpenStreetMap") + mapview(unique_tro_df) 

# road segments where pedal cycle direciton doesnt match axis: 
# ken27, ken16, ken19, ken22, ken3, ken30, ken32, ken33, ken36, ken37, ken41, ken6, ken9

ken_dir = ken %>% mutate(
  cyc_1_cf_direction = case_when(
    unique_contraflow_ID == "ken31" & cyc_1_vehicle_direction_from == "North East" & cyc_1_vehicle_direction_to == "South West" ~ "With flow",
    unique_contraflow_ID == "ken10" & cyc_1_vehicle_direction_from == "North West" & cyc_1_vehicle_direction_to == "South East" ~ "Contraflow",
    unique_contraflow_ID == "ken12" & cyc_1_vehicle_direction_from == "South East" & cyc_1_vehicle_direction_to == "North West" ~ "Contraflow",
    unique_contraflow_ID == "ken17" & cyc_1_vehicle_direction_from == "South East" & cyc_1_vehicle_direction_to == "North West" ~ "Contraflow",
    unique_contraflow_ID == "ken2" & cyc_1_vehicle_direction_from == "South East" & cyc_1_vehicle_direction_to == "North West" ~ "Contraflow",
    unique_contraflow_ID == "ken21" & cyc_1_vehicle_direction_from == "South West" & cyc_1_vehicle_direction_to == "North East" ~ "Contraflow",
    unique_contraflow_ID == "ken34" & cyc_1_vehicle_direction_from == "North West" & cyc_1_vehicle_direction_to == "South East" ~ "Contraflow",
    unique_contraflow_ID == "ken35" & cyc_1_vehicle_direction_from == "South West" & cyc_1_vehicle_direction_to == "North East" ~ "Contraflow",
    unique_contraflow_ID == "ken39" & cyc_1_vehicle_direction_from == "East" & cyc_1_vehicle_direction_to == "West" ~ "With flow",
    unique_contraflow_ID == "ken8" & cyc_1_vehicle_direction_from == "South West" & cyc_1_vehicle_direction_to == "North East" ~ "Contraflow",
    unique_contraflow_ID == "ken8" & cyc_1_vehicle_direction_from == "North East" & cyc_1_vehicle_direction_to == "South West" ~ "With flow",
    cyc_1_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"),
  cyc_2_cf_direction = NA)

ken_dir_final = ken_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", TRUE ~ "Exclude")) %>%
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

# Save K&C df
#saveRDS(ken_dir_final, file = "data-processed/pedal_cycle_direction/ken_dir_final.Rds")

                                    # #################
                                    #     Lambeth    #
                                    # #################

lam = min_joined_crashes_pc_direction %>% filter(BOROUGH == "Lambeth")
count = lam %>% st_drop_geometry() %>% group_by(unique_contraflow_ID) %>% summarise(count = n()) 
# 3 have cyc_2 that need managing -lam61, lam36 &lam64

# unique_contraflow_ID count
# <fct>                <int>
#   1 lam39                   21
# 2 lam1                    13
# 3 lam31                   11
# 4 lam42                   10
# 5 lam3                     9
# 6 lam21                    8
# 7 lam35                    8
# 8 lam57                    8
# 9 lam14                    6
# 10 lam27                    6
# # … with 31 more rows


unique(lam$contraflow_stop_date) # NA and  stop date  # lam33
unique(lam$introduces_one_way_street) # TRUE & FALSE
unique(lam$introduces_contraflow_bus_lane) # TRUE and FALSE

# 0) Lam33 - the one with the stop date
lam33 = lam %>% filter(unique_contraflow_ID == "lam33")
mapview(lam33, zcol = "cyc_1_vehicle_direction_from", map.types = "OpenStreetMap")
# has one cyc_2 but direction is unknown
# introduces cf cycle lane, not remove
# contraflow direction is SW to NE

# Code those where vehicle direction matches with/contraflow
lam33_dir = lam33 %>% mutate(
  cyc_1_cf_direction = case_when(
    cyc_1_vehicle_direction_from == "South West" & cyc_1_vehicle_direction_to == "North East" ~ "Contraflow"),
  cyc_2_cf_direction = NA)

lam33_dir_final = lam33_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", TRUE ~ "Exclude")) %>%
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

#1) lam39
lam39 = lam %>% filter(unique_contraflow_ID == "lam39")
mapview(lam39, zcol = "cyc_1_vehicle_direction_from", map.types = "OpenStreetMap")
# introduces one way system
# contraflow direction is NW to SE

# Code those where vehicle direction matches with/contraflow
lam39_dir = lam39 %>% mutate(
  cyc_1_cf_direction = case_when(
    cyc_1_vehicle_direction_from == "North West" & cyc_1_vehicle_direction_to == "South East" ~ "Contraflow",  
    cyc_1_vehicle_direction_from == "South East" & cyc_1_vehicle_direction_to == "North West" ~ "With flow",
    cyc_1_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"),
  cyc_2_cf_direction = NA)

lam39_dir_final = lam39_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", TRUE ~ "Exclude")) %>%
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

# 2) lam1 
lam1 = lam %>% filter(unique_contraflow_ID == "lam1")
mapview(lam1, zcol = "cyc_1_vehicle_direction_from", map.types = "OpenStreetMap") + mapview(unique_tro_df)
# Introduces contraflow bus lane
# contraflow direction varies depending on where on South Lambeth place the crash occurred

lam1_south = lam1 %>% filter(accident_index == "200601LX50534" | accident_index == "201101TB00118"  | accident_index == "201401LX50293")
mapview(lam1_south, zcol = "cyc_1_vehicle_direction_from", map.types = "OpenStreetMap")
lam1_south_dir = lam1_south %>% mutate(
  cyc_1_cf_direction = case_when(
    cyc_1_vehicle_direction_from == "North" ~ "With flow",  
    cyc_1_vehicle_direction_from == "South East" ~ "Contraflow"),
  cyc_2_cf_direction = NA)

lam1_north = lam1 %>% filter(!accident_index %in% lam1_south$accident_index)
mapview(lam1_north, zcol = "cyc_1_vehicle_direction_from", map.types = "OpenStreetMap")
# none match the road axis with their crash location and vehicle direction.

lam1_dir =  lam1 %>% mutate(
  cyc_1_cf_direction = case_when(
    accident_index %in% lam1_south$accident_index & cyc_1_vehicle_direction_from == "North" ~ "With flow",  
    accident_index %in% lam1_south$accident_index & cyc_1_vehicle_direction_from == "South East" ~ "Contraflow"),
  cyc_2_cf_direction = NA)

lam1_dir_final = lam1_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", TRUE ~ "Exclude")) %>%
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

# 3) lam31 
lam31 = lam %>% filter(unique_contraflow_ID == "lam31")
mapview(lam31, zcol = "cyc_1_vehicle_direction_from", map.types = "OpenStreetMap") + mapview(unique_tro_df)
# enable cf cycling on existing one way street, not removed, no cyc2
# contraflow direction is NE/N to SW

lam31_dir =  lam31 %>% mutate(
  cyc_1_cf_direction = case_when(
    cyc_1_vehicle_direction_from == "South West" & cyc_1_vehicle_direction_to == "North East" ~ "With flow",  
    cyc_1_vehicle_direction_from == "North East" & cyc_1_vehicle_direction_to == "South West" ~ "Contraflow", ),
  cyc_2_cf_direction = NA)

lam31_dir_final = lam31_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", TRUE ~ "Exclude")) %>%
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

# 4) lam42 
lam42 = lam %>% filter(unique_contraflow_ID == "lam42")
mapview(lam42, zcol = "cyc_1_vehicle_direction_from", map.types = "OpenStreetMap") + mapview(unique_tro_df)
# enables cf cycling, cyc_"
# contraflow is SE to N/NW
lam42_dir =  lam42 %>% mutate(
  cyc_1_cf_direction = case_when(
    cyc_1_vehicle_direction_from == "South East" & cyc_1_vehicle_direction_to == "North" ~ "Contraflow",  
    cyc_1_vehicle_direction_from == "South East" & cyc_1_vehicle_direction_to == "North West" ~ "Contraflow",  
    cyc_1_vehicle_direction_from == "North" & cyc_1_vehicle_direction_to == "South East" ~ "With flow", 
    cyc_1_vehicle_direction_from == "North West" & cyc_1_vehicle_direction_to == "South East" ~ "With flow", 
    cyc_1_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"),
  cyc_2_cf_direction = NA)

lam42_dir_final = lam42_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", TRUE ~ "Exclude")) %>%
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

# 5) lam3
lam3 = lam %>% filter(unique_contraflow_ID == "lam3")
mapview(lam3, zcol = "cyc_1_vehicle_direction_from", map.types = "OpenStreetMap") + mapview(unique_tro_df)
# enables cf cycle lane, no cyc_2, not removed

lam3_dir =  lam3 %>% mutate(
  cyc_1_cf_direction = case_when(
    cyc_1_vehicle_direction_from == "South East" & cyc_1_vehicle_direction_to == "North West" ~ "With flow",  
    cyc_1_vehicle_direction_from == "South East" & cyc_1_vehicle_direction_to == "North" ~ "With flow",  
    cyc_1_vehicle_direction_from == "North West" & cyc_1_vehicle_direction_to == "South East" ~ "Contraflow", 
    cyc_1_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"),
  cyc_2_cf_direction = NA)
lam3_dir_final = lam3_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", TRUE ~ "Exclude")) %>%
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

# 6) Deal with cyc2
#lam61, lam36 &lam64
lam_cyc2 = lam %>% filter(unique_contraflow_ID == "lam61" |unique_contraflow_ID == "lam36" | unique_contraflow_ID == "lam64")
mapview(lam_cyc2, zcol = "cyc_1_vehicle_direction_from", map.types = "OpenStreetMap") + mapview(unique_tro_df)
# none removed, 1 introduces one way street

lam_cyc2_dir = lam_cyc2 %>% mutate(
  cyc_1_cf_direction = case_when(
    unique_contraflow_ID == "lam36" & cyc_1_vehicle_direction_from == "South East" & cyc_1_vehicle_direction_to == "North West" ~ "Contraflow",
    unique_contraflow_ID == "lam61" & cyc_1_vehicle_direction_from == "North" & cyc_1_vehicle_direction_to == "South East" ~ "With flow",
    unique_contraflow_ID == "lam61" & cyc_1_vehicle_direction_from == "South East" & cyc_1_vehicle_direction_to == "North" ~ "Contraflow",
    unique_contraflow_ID == "lam64" & cyc_1_vehicle_direction_from == "North West" & cyc_1_vehicle_direction_to == "South East" ~ "Contraflow",
    unique_contraflow_ID == "lam64" & cyc_1_vehicle_direction_from == "South East" & cyc_1_vehicle_direction_to == "North West" ~ "With flow"),  
  cyc_2_cf_direction = case_when(
    unique_contraflow_ID == "lam36" & cyc_2_vehicle_direction_from == "South East" & cyc_2_vehicle_direction_to == "North West" ~ "Contraflow",
    unique_contraflow_ID == "lam61" & cyc_2_vehicle_direction_from == "South East" & cyc_2_vehicle_direction_to == "North" ~ "Contraflow",
    unique_contraflow_ID == "lam64" & cyc_2_vehicle_direction_from == "North West" & cyc_2_vehicle_direction_to == "South East" ~ "Contraflow"))

lamcyc2_dir_final = lam_cyc2_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", 
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", TRUE ~ "Exclude")) %>%
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

# 7) Deal with rest
done = c("lam33", "lam39", "lam1", "lam31", "lam42", "lam3", "lam36", "lam61", "lam64")
not_done = lam %>% filter(!unique_contraflow_ID %in% done)
mapview(not_done, zcol = "cyc_1_vehicle_direction_from", map.types = "OpenStreetMap") + mapview(unique_tro_df)

# Direction not compatible with road segment axis
#lam14, lam50, lam16, lam17, lam20, lam22, lam23, lam25, lam26, lam27, lam30, lam32, lam4,
# lam44, lam46, lam48, lam49, lam51, lam57, lam58, lam59

lam35sw2ne = not_done %>% filter(unique_contraflow_ID == "lam35" & cyc_1_vehicle_direction_from == "South West")
mapview(lam35sw2ne)
lam_not_done_dir = not_done %>% mutate(
  cyc_1_cf_direction = case_when(
    unique_contraflow_ID == "lam15" & cyc_1_vehicle_direction_from == "East" & cyc_1_vehicle_direction_to == "West" ~ "With flow",
    unique_contraflow_ID == "lam15" & cyc_1_vehicle_direction_from == "West" & cyc_1_vehicle_direction_to == "East" ~ "Contraflow",
    unique_contraflow_ID == "lam21" & cyc_1_vehicle_direction_from == "East" & cyc_1_vehicle_direction_to == "West" ~ "Contraflow",
    unique_contraflow_ID == "lam21" & cyc_1_vehicle_direction_from == "West" & cyc_1_vehicle_direction_to == "East" ~ "With flow",
    unique_contraflow_ID == "lam24" & cyc_1_vehicle_direction_from == "South East" & cyc_1_vehicle_direction_to == "North West" ~ "Contraflow",
    unique_contraflow_ID == "lam35" & cyc_1_vehicle_direction_from == "South West" & cyc_1_vehicle_direction_to == "North East" ~ "Contraflow",
    unique_contraflow_ID == "lam37" & cyc_1_vehicle_direction_from == "West" & cyc_1_vehicle_direction_to == "East" ~ "With flow",
    unique_contraflow_ID == "lam40" & cyc_1_vehicle_direction_from == "North West" & cyc_1_vehicle_direction_to == "South East" ~ "With flow",
    unique_contraflow_ID == "lam40" & cyc_1_vehicle_direction_from == "South East" & cyc_1_vehicle_direction_to == "North" ~ "Contraflow",
    unique_contraflow_ID == "lam41" & cyc_1_vehicle_direction_from == "North West" & cyc_1_vehicle_direction_to == "South East" ~ "With flow",
    unique_contraflow_ID == "lam45" & cyc_1_vehicle_direction_from == "South West" & cyc_1_vehicle_direction_to == "North East" ~ "With flow",
    unique_contraflow_ID == "lam47" & cyc_1_vehicle_direction_from == "West" & cyc_1_vehicle_direction_to == "East" ~ "Contraflow",
    unique_contraflow_ID == "lam54" & cyc_1_vehicle_direction_from == "West" & cyc_1_vehicle_direction_to == "East" ~ "Contraflow",
    unique_contraflow_ID == "lam60" & cyc_1_vehicle_direction_from == "West" & cyc_1_vehicle_direction_to == "East" ~ "With flow",
    cyc_1_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"),
  cyc_2_cf_direction = case_when(
    cyc_2_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"))

lam_not_done_dir_final = lam_not_done_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", TRUE ~ "Exclude")) %>%
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining


# Join dataframes together
lam_dir_final = rbind(lam33_dir_final, lam39_dir_final, lam1_dir_final, lam31_dir_final,
                      lam42_dir_final, lam3_dir_final, lamcyc2_dir_final, lam_not_done_dir_final)

nrow(lam_dir_final) == nrow(lam) # TRUE

# Save lambeth df
saveRDS(lam_dir_final, file = "data-processed/pedal_cycle_direction/lam_dir_final.Rds")

                                      #################
                                      #   Lewisham    #
                                      #################

lew = min_joined_crashes_pc_direction %>% filter(BOROUGH == "Lewisham")
count = lew %>% st_drop_geometry() %>% group_by(unique_contraflow_ID) %>% summarise(count = n()) 
# no cyc2

# unique_contraflow_ID count
# 1 lew19                   12
# 2 lew12                   10
# 3 lew2                     8
# 4 lew18                    7
# 5 lew14                    4
# 6 lew9                     3
# 7 lew7                     2
# 8 lew10                    1
# 9 lew11                    1
# 10 lew16                    1
# 11 lew17                    1
# 12 lew5                     1
# 13 lew6                     1


unique(lew$contraflow_stop_date) # NA
unique(lew$introduces_one_way_street) # TRUE & FALSE
unique(lew$introduces_contraflow_bus_lane) # TRUE and FALSE

# 1) lew19  
lew19 = lew %>% filter(unique_contraflow_ID == "lew19")
mapview(lew19, zcol = "cyc_1_vehicle_direction_from", map.types = "OpenStreetMap") + mapview(unique_tro_df)
# introduces cf and one way street, not removed
# contraflow direction is E to W

# Code those where vehicle direction matches with/contraflow
lew19_dir = lew19 %>% mutate(
  cyc_1_cf_direction = case_when(
    cyc_1_vehicle_direction_from == "West" & cyc_1_vehicle_direction_to == "East" ~ "With flow",
    cyc_1_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"),
  cyc_2_cf_direction = NA)

lew19_dir_final = lew19_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", TRUE ~ "Exclude")) %>%
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

# 2) lew12  
lew12 = lew %>% filter(unique_contraflow_ID == "lew12")
mapview(lew12, zcol = "cyc_1_vehicle_direction_from", map.types = "OpenStreetMap") + mapview(unique_tro_df)
# enables cf cycling in existing bus lane
# unable to figure out contraflow - ? has it been removed or altered.  

lew12_dir_final = lew12 %>%
  mutate(cyc_1_cf_direction = NA, cyc_2_cf_direction = NA, 
    include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", TRUE ~ "Exclude")) %>%
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

# 3) lew2 
lew2 = lew %>% filter(unique_contraflow_ID == "lew2")
mapview(lew2, zcol = "cyc_1_vehicle_direction_from", map.types = "OpenStreetMap") + mapview(unique_tro_df)
# allows cf cycling in a bus lane
# contraflow is south/SW to north/NE
lew2_dir = lew2 %>% mutate(
  cyc_1_cf_direction = case_when(
    cyc_1_vehicle_direction_from == "South West" & cyc_1_vehicle_direction_to == "North East" ~ "Contraflow"),
  cyc_2_cf_direction = NA)

lew2_dir_final = lew2_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", TRUE ~ "Exclude")) %>%
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

# 4) lew18  
lew18 = lew %>% filter(unique_contraflow_ID == "lew18")
mapview(lew18, zcol = "cyc_1_vehicle_direction_from", map.types = "OpenStreetMap") + mapview(unique_tro_df)
# introduces one way street and cf cycling
# contraflow is east to west

lew18_dir = lew18 %>% mutate(
  cyc_1_cf_direction = case_when(
    cyc_1_vehicle_direction_from == "East" & cyc_1_vehicle_direction_to == "West" ~ "Contraflow",
    cyc_1_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"),
  cyc_2_cf_direction = NA)

lew18_dir_final = lew18_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", TRUE ~ "Exclude")) %>%
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

# 5) rest of lewisham
done = c("lew19", "lew12", "lew2", "lew18")
not_done = lew %>% filter(!unique_contraflow_ID %in% done)
mapview(not_done, zcol = "cyc_1_vehicle_direction_from", map.types = "OpenStreetMap") + mapview(unique_tro_df)

# checked but dont match direction of road segment: lew10, lew11, lew14, lew16, lew17, lew6, lew9

lew_not_done_dir = not_done %>% mutate(
  cyc_1_cf_direction = case_when(
    unique_contraflow_ID == "lew5" & cyc_1_vehicle_direction_from == "West" & cyc_1_vehicle_direction_to == "East" ~ "Contraflow",
    unique_contraflow_ID == "lew7" & cyc_1_vehicle_direction_from == "South West" & cyc_1_vehicle_direction_to == "North East" ~ "With flow"),
  cyc_2_cf_direction = NA)

lew_not_done_dir_final = lew_not_done_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", TRUE ~ "Exclude")) %>%
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

# Join dataframes
lew_dir_final = rbind(lew19_dir_final, lew12_dir_final, lew2_dir_final, lew18_dir_final,
                      lew_not_done_dir_final)

nrow(lew_dir_final) == nrow(lew) # TRUE

# Save lewisham df
saveRDS(lew_dir_final, file = "data-processed/pedal_cycle_direction/lew_dir_final.Rds")

                                      #################
                                      #    Newham     #
                                      #################

new = min_joined_crashes_pc_direction %>% filter(BOROUGH == "Newham")
count = new %>% st_drop_geometry() %>% group_by(unique_contraflow_ID) %>% summarise(count = n()) 
# no cyc2

unique(new$contraflow_stop_date) # NA
unique(new$introduces_one_way_street) # TRUE & FALSE
unique(new$introduces_contraflow_bus_lane) # FALSE

# 1) new18 
new18 = new %>% filter(unique_contraflow_ID == "new18")
mapview(new18, zcol = "cyc_1_vehicle_direction_from", map.types = "OpenStreetMap") + mapview(unique_tro_df)
# enables cf cycling
# curved street so hard to define one direction for cf - tro says one way working from the grove to great eastern road

new18east = new18 %>% filter(cyc_1_vehicle_direction_from == "East")
mapview(new18east, zcol = "cyc_1_vehicle_direction_to", map.types = "OpenStreetMap")
# 	200701KF63432 East to West so travelling with flow
new18se = new18 %>% filter(cyc_1_vehicle_direction_from == "South East")
mapview(new18se, zcol = "cyc_1_vehicle_direction_to", map.types = "OpenStreetMap") # so none of these
rest18 = new18 %>% filter(cyc_1_vehicle_direction_from != "East" & cyc_1_vehicle_direction_from != "South East")
mapview(rest18, zcol = "cyc_1_vehicle_direction_from", map.types = "OpenStreetMap") # so none of these

new18_dir_final = new18 %>% mutate(
  cyc_1_cf_direction = case_when(accident_index == "200701KF63432" ~ "With flow"),
  cyc_2_cf_direction = NA) %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", TRUE ~ "Exclude")) %>%
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

# 2) new16
new16 = new %>% filter(unique_contraflow_ID == "new16")
mapview(new16, zcol = "cyc_1_vehicle_direction_from", map.types = "OpenStreetMap") + mapview(unique_tro_df)
# introduces cf cycle lane
# contraflow direction is unable to be determined - doesnt operate all the time

new16_dir_final = new16 %>% mutate(
  cyc_1_cf_direction = NA, cyc_2_cf_direction = NA) %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", TRUE ~ "Exclude")) %>%
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

# 3) Rest of newham
new_not_done = new %>% filter(unique_contraflow_ID != "new18" & unique_contraflow_ID != "new16" )
mapview(new_not_done, zcol = "cyc_1_vehicle_direction_from", map.types = "OpenStreetMap") + mapview(unique_tro_df)
#
# checked but dont match direction of road segment: new17, new19, new2, new21, new22, new23, new26
# new27, new4, new7, new9

new_not_done_dir = new_not_done %>% mutate(
  cyc_1_cf_direction = case_when(
    unique_contraflow_ID == "new28" & cyc_1_vehicle_direction_from == "North East" & cyc_1_vehicle_direction_to == "South West" ~ "Contraflow",
    unique_contraflow_ID == "new29" & cyc_1_vehicle_direction_from == "West" & cyc_1_vehicle_direction_to == "East" ~ "With flow",
    cyc_1_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"),
  cyc_2_cf_direction = NA)

new_not_dir_final = new_not_done_dir %>% 
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", TRUE ~ "Exclude")) %>%
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining


# Join newham dataframes
new_dir_final = rbind(new18_dir_final, new16_dir_final, new_not_dir_final)
nrow(new_dir_final) == nrow(new) # TRUE

# Save Newham df
saveRDS(new_dir_final, file = "data-processed/pedal_cycle_direction/new_dir_final.Rds")


                                  #################
                                  #   Southwark   #
                                  #################

south = min_joined_crashes_pc_direction %>% filter(BOROUGH == "Southwark")
count = south %>% st_drop_geometry() %>% group_by(unique_contraflow_ID) %>% summarise(count = n()) 
# cyc2 - in swk1 and swk2

# unique_contraflow_ID count
# <fct>                <int>
# 1 swk1                    39  has cyc2
# 2 swk81                   29
# 3 swk2                    21 has cyc2
# 4 swk15                   18
# 5 swk24                   18
# 6 swk12                   12
# 7 swk11                   11  has contraflow removed
# 8 swk27                   10
# 9 swk42                   10
# 10 swk85                    9
# # … with 33 more rows

unique(south$contraflow_stop_date) # NA "2017-12-18"  swk11 
unique(south$introduces_one_way_street) # TRUE & FALSE
unique(south$introduces_contraflow_bus_lane) # FALSE & TRUE

# 1) swk1
swk1 = south %>% filter(unique_contraflow_ID == "swk1")
mapview(swk1, zcol = "cyc_1_vehicle_direction_from", map.types = "OpenStreetMap") + mapview(unique_tro_df)
# new contraflow bus lane that allows cycling
# two cyc_2
# contraflow is SE to BW

# Code those where vehicle direction matches with/contraflow
swk1_dir = swk1 %>% mutate(
  cyc_1_cf_direction = case_when(
    cyc_1_vehicle_direction_from == "South East" & cyc_1_vehicle_direction_to == "North West" ~ "Contraflow",
    cyc_1_vehicle_direction_from == "North West" & cyc_1_vehicle_direction_to == "South East" ~ "With flow",
    cyc_1_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"),
  cyc_2_cf_direction = case_when(
    cyc_2_vehicle_direction_from == "South East" & cyc_2_vehicle_direction_to == "North West" ~ "Contraflow",
    cyc_2_vehicle_direction_from == "North West" & cyc_2_vehicle_direction_to == "South East" ~ "With flow",
    cyc_2_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"))

 # Examine those where vehicle direction doesnt match with/contraflow - can some of these be coded?
swk1_dir_query = swk1_dir %>% filter(is.na(cyc_1_cf_direction)) # n = 14
mapview(swk1_dir_query, zcol = "cyc_1_vehicle_direction_to") 
# all seem to be at junctions and thus there directions are compatibile with turning
# so no more of swk1 can be coded.

# Identify those crashes that definately need to be incldued in the direction crash calculation because we have a direction that matches with flow or contraflow
swk1_dir_final = swk1_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include", 
    (cyc_2_cf_direction == "Contraflow" | cyc_2_cf_direction == "With flow") ~ "Include", 
    TRUE ~ "Exclude")) %>%
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

# 2) swk81  
swk81 = south %>% filter(unique_contraflow_ID == "swk81")
mapview(swk81, zcol = "cyc_1_vehicle_direction_from", map.types = "OpenStreetMap") + mapview(unique_tro_df)
# introduces one way street with westbound contraflow that allows cycling
# contraflow is east to west

# # Code those where vehicle direction matches with/contraflow
swk81_dir = swk81 %>% mutate(
  cyc_1_cf_direction = case_when(
    cyc_1_vehicle_direction_from == "East" & cyc_1_vehicle_direction_to == "West" ~ "Contraflow",
    cyc_1_vehicle_direction_from == "West" & cyc_1_vehicle_direction_to == "East" ~ "With flow",
    cyc_1_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"),
  cyc_2_cf_direction = NA)


# Examine those where vehicle direction doesnt match with/contraflow - can some of these be coded?
swk81_dir_query = swk81_dir %>% filter(is.na(cyc_1_cf_direction)) # n = 24
swk81_ne = swk81_dir_query %>% filter(cyc_1_vehicle_direction_from == "North East")
mapview(swk81_ne, zcol = "cyc_1_vehicle_direction_to", map.types = "OpenStreetMap") + mapview(unique_tro_df)
swk81_n = swk81_dir_query %>% filter(cyc_1_vehicle_direction_from == "North")
mapview(swk81_n, zcol = "cyc_1_vehicle_direction_to", map.types = "OpenStreetMap") + mapview(unique_tro_df)
swk81_se = swk81_dir_query %>% filter(cyc_1_vehicle_direction_from == "South East")
mapview(swk81_se, zcol = "cyc_1_vehicle_direction_to", map.types = "OpenStreetMap") + mapview(unique_tro_df)
swk81_sw = swk81_dir_query %>% filter(cyc_1_vehicle_direction_from == "South West")
mapview(swk81_sw, zcol = "cyc_1_vehicle_direction_to", map.types = "OpenStreetMap") + mapview(unique_tro_df)
# None of these appear to be able to be coded

# Identify those crashes that definitely need to be included in the direction crash calculation because we have a direction that matches with flow or contraflow
swk81_dir_final = swk81_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include",
    TRUE ~ "Exclude")) %>%
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

# 3) swk2  
swk2 = south %>% filter(unique_contraflow_ID == "swk2")
mapview(swk2, zcol = "cyc_1_vehicle_direction_from", map.types = "OpenStreetMap") + mapview(unique_tro_df)
# new contraflow bus lane that allows cycling
# one cyc_2 but it direction is unknown
# contraflow is SE to NW

# Code those where vehicle direction matches with/contraflow
swk2_dir = swk2 %>% mutate(
  cyc_1_cf_direction = case_when(
    cyc_1_vehicle_direction_from == "South East" & cyc_1_vehicle_direction_to == "North West" ~ "Contraflow",
    cyc_1_vehicle_direction_from == "North West" & cyc_1_vehicle_direction_to == "South East" ~ "With flow",
    cyc_1_vehicle_direction_from == "West" & cyc_1_vehicle_direction_to == "East" ~ "With flow",
    cyc_1_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"),
  cyc_2_cf_direction = case_when(
    cyc_2_vehicle_direction_from == "South East" & cyc_2_vehicle_direction_to == "North West" ~ "Contraflow",
    cyc_2_vehicle_direction_from == "North West" & cyc_2_vehicle_direction_to == "South East" ~ "With flow",
    cyc_2_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"))
 
# Examine those where vehicle direction doesnt match with/contraflow - can some of these be coded?
swk2_dir_query = swk2_dir %>% filter(is.na(cyc_1_cf_direction)) # n = 5
mapview(swk2_dir_query, zcol = "cyc_1_vehicle_direction_to") 
# only 1 where direction is West to East seems to be codable

# Identify those crashes that definately need to be incldued in the direction crash calculation because we have a direction that matches with flow or contraflow
swk2_dir_final = swk2_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include",
    (cyc_2_cf_direction == "Contraflow" | cyc_2_cf_direction == "With flow") ~ "Include",
    TRUE ~ "Exclude")) %>%
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

# 4) swk15
swk15 = south %>% filter(unique_contraflow_ID == "swk15")
mapview(swk15, zcol = "cyc_1_vehicle_direction_from", map.types = "OpenStreetMap") + mapview(unique_tro_df)
# introduces contraflow cycle lane
# contraflow is east/SE to west/NW - road bends so need to look at most cases individually

swk15_east = swk15 %>% filter(cyc_1_vehicle_direction_from == "East")
mapview(swk15_east) # include
swk15_west = swk15 %>% filter(cyc_1_vehicle_direction_from == "West")
mapview(swk15_west) # include
swk15_nw = swk15 %>% filter(cyc_1_vehicle_direction_from == "North West")
mapview(swk15_nw) #include
swk15_sw = swk15 %>% filter(cyc_1_vehicle_direction_from == "South West")
mapview(swk15_sw, zcol = "cyc_1_vehicle_direction_to", map.types = "OpenStreetMap") + mapview(unique_tro_df) 
# include 	199801MD00223 only

# Code those where vehicle direction matches with/contraflow
swk15_dir = swk15 %>% mutate(
  cyc_1_cf_direction = case_when(
    cyc_1_vehicle_direction_from == "East" & cyc_1_vehicle_direction_to == "West" ~ "Contraflow",
    cyc_1_vehicle_direction_from == "West" & cyc_1_vehicle_direction_to == "East" ~ "With flow",
    cyc_1_vehicle_direction_from == "North West" & cyc_1_vehicle_direction_to == "South East" ~ "With flow",
    cyc_1_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"),
  cyc_2_cf_direction = NA)

swk15_dir$cyc_1_cf_direction = replace(swk15_dir$cyc_1_cf_direction, 
                                       which(swk15_dir$accident_index == "199801MD00223"), values = "With flow")

# Identify those crashes that definitely need to be included in the direction crash calculation because we have a direction that matches with flow or contraflow
swk15_dir_final = swk15_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include",
    TRUE ~ "Exclude")) %>%
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

# 5) swk24  
swk24 = south %>% filter(unique_contraflow_ID == "swk24")
mapview(swk24, zcol = "cyc_1_vehicle_direction_from", map.types = "OpenStreetMap") + mapview(unique_tro_df)
# introduces contraflow cycling
# contraflow is south to north 

# Code those where vehicle direction matches with/contraflow
swk24_dir = swk24 %>% mutate(
  cyc_1_cf_direction = case_when(
    cyc_1_vehicle_direction_from == "South" & cyc_1_vehicle_direction_to == "North" ~ "Contraflow",
    cyc_1_vehicle_direction_from == "North" & cyc_1_vehicle_direction_to == "South" ~ "With flow",
    cyc_1_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"),
  cyc_2_cf_direction = NA)
# NB none appear to be in direction of road axis 

# Identify those crashes that definitely need to be included in the direction crash calculation because we have a direction that matches with flow or contraflow
swk24_dir_final = swk24_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include",
    TRUE ~ "Exclude")) %>%
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

# 6) swk12 
swk12 = south %>% filter(unique_contraflow_ID == "swk12")
mapview(swk12, zcol = "cyc_1_vehicle_direction_from", map.types = "OpenStreetMap") + mapview(unique_tro_df)
# introduces new one way stret with contraflow cycling
# contraflow is north east(e) to south west(w) - although the road bends there are no crashes in the 
# segment where the road bends

# Code those where vehicle direction matches with/contraflow
swk12_dir = swk12 %>% mutate(
  cyc_1_cf_direction = case_when(
    cyc_1_vehicle_direction_from == "North East" & cyc_1_vehicle_direction_to == "South West" ~ "Contraflow",
    cyc_1_vehicle_direction_from == "South West" & cyc_1_vehicle_direction_to == "North East" ~ "With flow",
    cyc_1_vehicle_direction_from == "East" & cyc_1_vehicle_direction_to == "West" ~ "Contraflow",
    cyc_1_vehicle_direction_from == "West" & cyc_1_vehicle_direction_to == "East" ~ "With flow",
    cyc_1_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"),
  cyc_2_cf_direction = NA)
# none of rest appear to be in direction of road axis nor when look at map
 
# Identify those crashes that definitely need to be included in the direction crash calculation because we have a direction that matches with flow or contraflow
swk12_dir_final = swk12_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include",
    TRUE ~ "Exclude")) %>%
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

# 7) swk11 
swk11 = south %>% filter(unique_contraflow_ID == "swk11")
mapview(swk11, zcol = "cyc_1_vehicle_direction_from", map.types = "OpenStreetMap") + mapview(unique_tro_df)
# introduces new one way street with contraflow cycling
# was removed but no crashes occured after removal
# contraflow is east to west

# Code those where vehicle direction matches with/contraflow
swk11_dir = swk11 %>% mutate(
  cyc_1_cf_direction = case_when(
    cyc_1_vehicle_direction_from == "East" & cyc_1_vehicle_direction_to == "West" ~ "Contraflow",
    cyc_1_vehicle_direction_from == "West" & cyc_1_vehicle_direction_to == "East" ~ "With flow",
    cyc_1_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"),
  cyc_2_cf_direction = NA)
# # none of rest appear to be in direction of road axis nor when look at map

# Identify those crashes that definitely need to be included in the direction crash calculation because we have a direction that matches with flow or contraflow
swk11_dir_final = swk11_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include",
    TRUE ~ "Exclude")) %>%
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

# 8) swk27
swk27 = south %>% filter(unique_contraflow_ID == "swk27")
mapview(swk27, zcol = "cyc_1_vehicle_direction_from", map.types = "OpenStreetMap") + mapview(unique_tro_df)
# introduces contraflow cycling
# contraflow is north/NW to south/SE for most of it but then road bends so some might be SW to NE

swk27_north = swk27 %>% filter(cyc_1_vehicle_direction_from == "North")
mapview(swk27_north) # all correct APART from 200801MD69480
swk27_north_include = c("201201MM70897", "201301MM70075", "201301MM70910")
swk27_SE = swk27 %>% filter(cyc_1_vehicle_direction_from == "South East")
mapview(swk27_SE) # include 201201MM70273 only

# Code those where vehicle direction matches with/contraflow
swk27_dir = swk27 %>% mutate(
  cyc_1_cf_direction = case_when(
    accident_index %in% swk27_north_include ~ "Contraflow",
    accident_index == "201201MM70273" ~ 'With flow', 
    cyc_1_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"),
  cyc_2_cf_direction = NA)

# Identify those crashes that definitely need to be included in the direction crash calculation because we have a direction that matches with flow or contraflow
swk27_dir_final = swk27_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include",
    TRUE ~ "Exclude")) %>%
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

#9) swk42 
swk42 = south %>% filter(unique_contraflow_ID == "swk42")
mapview(swk42, zcol = "cyc_1_vehicle_direction_from", map.types = "OpenStreetMap") + mapview(unique_tro_df)
# introduces contraflow cycling
# contraflow is west to east but some complex road bearing

# Code those where vehicle direction matches with/contraflow
swk42_dir = swk42 %>% mutate(
  cyc_1_cf_direction = case_when(
    cyc_1_vehicle_direction_from == "East" & cyc_1_vehicle_direction_to == "West" ~ "With flow",
    cyc_1_vehicle_direction_from == "West" & cyc_1_vehicle_direction_to == "East" ~ "Contraflow",
    cyc_1_vehicle_direction_from == "West" & cyc_1_vehicle_direction_to == "South East" ~ "Contraflow",
    cyc_1_vehicle_direction_from == "North East" & cyc_1_vehicle_direction_to == "South West" ~ "With flow",
    cyc_1_vehicle_direction_from == "South East" & cyc_1_vehicle_direction_to == "West" ~ "With flow",
    cyc_1_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"),
  cyc_2_cf_direction = NA)

# Examine those where vehicle direction doesnt match with/contraflow - can some of these be coded?
swk42_dir_query = swk42_dir %>% filter(is.na(cyc_1_cf_direction)) # n = 5 
# 3 of these can be coded so added above
mapview(swk42_dir_query, zcol = "cyc_1_vehicle_direction_from", map.types = "OpenStreetMap") + mapview(unique_tro_df)

# Identify those crashes that definitely need to be included in the direction crash calculation because we have a direction that matches with flow or contraflow
swk42_dir_final = swk42_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include",
    TRUE ~ "Exclude")) %>%
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

#) 10 swk85
swk85 = south %>% filter(unique_contraflow_ID == "swk85")
mapview(swk85, zcol = "cyc_1_vehicle_direction_from", map.types = "OpenStreetMap") + mapview(unique_tro_df)
# introduces contraflow cycling
# contraflow is west to east (but part of road is SW to NE but no crashes occur on this segment)

# Code those where vehicle direction matches with/contraflow
swk85_dir = swk85 %>% mutate(
  cyc_1_cf_direction = case_when(
    cyc_1_vehicle_direction_from == "East" & cyc_1_vehicle_direction_to == "West" ~ "With flow",
    cyc_1_vehicle_direction_from == "West" & cyc_1_vehicle_direction_to == "East" ~ "Contraflow",
    cyc_1_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"),
  cyc_2_cf_direction = NA)
# rest are incompatible. 

# Identify those crashes that definitely need to be included in the direction crash calculation because we have a direction that matches with flow or contraflow
swk85_dir_final = swk85_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include",
    TRUE ~ "Exclude")) %>%
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

# Manage rest of the swk segments
done = c("swk1", "swk81", "swk2", "swk15", "swk24", "swk12", "swk11", "swk27", "swk42",
         "swk85")
not_done = south %>% filter(!unique_contraflow_ID %in% done)
mapview(not_done, zcol = "cyc_1_vehicle_direction_from", map.types = "OpenStreetMap") + mapview(unique_tro_df)

swk60 = not_done %>% filter(unique_contraflow_ID == "swk60")
mapview(swk60, zcol = "cyc_1_vehicle_direction_from", map.types = "OpenStreetMap") + mapview(unique_tro_df)

# Road segments where ped cyc direction not compatible with axis:
# swk13, swk18, swk22, swk28, swk29, swk30, swk35, swk40, swk50, swk60, swk61, swk62, 
# swk65, swk71, swk75, swk77, swk89, swk90, swk91, swk93, swk94, swk97
not_done_dir = not_done %>% mutate(
  cyc_1_cf_direction = case_when(
    unique_contraflow_ID == "swk17" & cyc_1_vehicle_direction_from == "West" & cyc_1_vehicle_direction_to == "East" ~ "Contraflow",
    unique_contraflow_ID == "swk36" & cyc_1_vehicle_direction_from == "South East" & cyc_1_vehicle_direction_to == "North West" ~ "With flow",
    unique_contraflow_ID == "swk39" & cyc_1_vehicle_direction_from == "South East" & cyc_1_vehicle_direction_to == "North West" ~ "Contraflow",
    unique_contraflow_ID == "swk52" & cyc_1_vehicle_direction_from == "North East" & cyc_1_vehicle_direction_to == "West" ~ "With flow",
    unique_contraflow_ID == "swk58" & cyc_1_vehicle_direction_from == "East" & cyc_1_vehicle_direction_to == "West" ~ "With flow",
    unique_contraflow_ID == "swk67" & cyc_1_vehicle_direction_from == "South West" & cyc_1_vehicle_direction_to == "North East" ~ "Contraflow",
    unique_contraflow_ID == "swk80" & cyc_1_vehicle_direction_from == "West" & cyc_1_vehicle_direction_to == "East" ~ "With flow",
    unique_contraflow_ID == "swk82" & cyc_1_vehicle_direction_from == "South East" & cyc_1_vehicle_direction_to == "North West" ~ "Contraflow",
    unique_contraflow_ID == "swk83" & cyc_1_vehicle_direction_from == "South West" & cyc_1_vehicle_direction_to == "North East" ~ "Contraflow",
    unique_contraflow_ID == "swk95" & cyc_1_vehicle_direction_from == "North" & cyc_1_vehicle_direction_to == "South East" ~ "With flow",
    unique_contraflow_ID == "swk95" & cyc_1_vehicle_direction_from == "South East" & cyc_1_vehicle_direction_to == "North" ~ "Contraflow",
    unique_contraflow_ID == "swk99" & cyc_1_vehicle_direction_from == "East" & cyc_1_vehicle_direction_to == "West" ~ "With flow",
    cyc_1_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"),
  cyc_2_cf_direction = NA)

not_done_dir_final = not_done_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include",
    TRUE ~ "Exclude")) %>%
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining
 
# Join Southwark dataframes
south_dir_final = rbind(swk1_dir_final, swk81_dir_final, swk2_dir_final, swk15_dir_final,
                  swk24_dir_final, swk12_dir_final, swk11_dir_final, swk27_dir_final,
                  swk42_dir_final, swk85_dir_final, not_done_dir_final)

nrow(south_dir_final) == nrow(south) # TRUE

# Save southwark df
saveRDS(south_dir_final, file = "data-processed/pedal_cycle_direction/south_dir_final.Rds")


                              #################
                              # Tower Hamlets #
                              #################

tower = min_joined_crashes_pc_direction %>% filter(BOROUGH == "Tower Hamlets")
count = tower %>% st_drop_geometry() %>% group_by(unique_contraflow_ID) %>% summarise(count = n()) 
# only 2 road segments tow1 and tow 2

unique(tower$contraflow_stop_date) # NA 
unique(tower$introduces_one_way_street) # TRUE
unique(tower$introduces_contraflow_bus_lane) # FALSE 
# no cyc2

# 1) tow1
tow1 = tower %>% filter(unique_contraflow_ID == "tow1")
mapview(tow1, zcol = "cyc_1_vehicle_direction_from", map.types = "OpenStreetMap") + mapview(unique_tro_df)
# new one way system that allows cf cycling
# contraflow is East to West

# Code those where vehicle direction matches with/contraflow
tow1_dir = tow1 %>% mutate(
  cyc_1_cf_direction = case_when(
    cyc_1_vehicle_direction_from == "East" & cyc_1_vehicle_direction_to == "West" ~ "Contraflow",
    cyc_1_vehicle_direction_from == "North East" & cyc_1_vehicle_direction_to == "South West" ~ "Contraflow",
    cyc_1_vehicle_direction_from == "West" & cyc_1_vehicle_direction_to == "East" ~ "With flow",
    cyc_1_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"),
  cyc_2_cf_direction = NA)

# Identify those crashes that definately need to be incldued in the direction crash calculation because we have a direction that matches with flow or contraflow
tow1_dir_final = tow1_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include",
    TRUE ~ "Exclude")) %>%
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

# 2) tow2
tow2 = tower %>% filter(unique_contraflow_ID == "tow2")
mapview(tow2, zcol = "cyc_1_vehicle_direction_from", map.types = "OpenStreetMap") + mapview(unique_tro_df)
# new one way system that allows cf cycling
# contraflow is West East

# Code those where vehicle direction matches with/contraflow
tow2_dir = tow2 %>% mutate(
  cyc_1_cf_direction = case_when(
    cyc_1_vehicle_direction_from == "East" & cyc_1_vehicle_direction_to == "West" ~ "With flow",
    cyc_1_vehicle_direction_from == "West" & cyc_1_vehicle_direction_to == "East" ~ "Contraflow",
    cyc_1_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"),
  cyc_2_cf_direction = NA)


# Identify those crashes that definately need to be incldued in the direction crash calculation because we have a direction that matches with flow or contraflow
tow2_dir_final = tow2_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include",
    TRUE ~ "Exclude")) %>%
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

# join dataframes
tower_dir_final = rbind(tow1_dir_final, tow2_dir_final)
nrow(tower_dir_final) == nrow(tower) # TRUE

# save tower hamlets df
saveRDS(tower_dir_final, file = "data-processed/pedal_cycle_direction/tower_dir_final.Rds")

                                    
                                    #################
                                    #  Wandsworth   #
                                    #################

wand = min_joined_crashes_pc_direction %>% filter(BOROUGH == "Wandsworth")
count = wand %>% st_drop_geometry() %>% group_by(unique_contraflow_ID) %>% summarise(count = n()) 
# only 9 road segments 2 have greater than 10 - wan5 and wan10

unique(wand$contraflow_stop_date) # NA 
unique(wand$introduces_one_way_street) # TRUE & FALSE
unique(wand$introduces_contraflow_bus_lane) # FALSE 
# no cyc2

# 1) wand5
wan5 = wand %>% filter(unique_contraflow_ID == "wan5")
mapview(wan5, zcol = "cyc_1_vehicle_direction_from", map.types = "OpenStreetMap") +mapview(unique_tro_df)
# # new one way system that allows cf cycling
# # contraflow is NW to SE for one segment and SW to NE for the other

wan5_nw_se = wan5 %>% filter((cyc_1_vehicle_direction_from == "North West" & cyc_1_vehicle_direction_to == "South East") | 
                               (cyc_1_vehicle_direction_from == "South East" & cyc_1_vehicle_direction_to == "North West"))
mapview(wan5_nw_se, zcol = "cyc_1_vehicle_direction_from", map.types = "OpenStreetMap") +mapview(unique_tro_df)
# 3 crashes are on the correct axis on the correct segment
include_contraflow = c("200501WW59115", "2016010012510", "2018010120064")

wan5_sw_ne = wan5 %>% filter((cyc_1_vehicle_direction_from == "South West" & cyc_1_vehicle_direction_to == "North East") | 
                               (cyc_1_vehicle_direction_from == "North East" & cyc_1_vehicle_direction_to == "South West"))
mapview(wan5_sw_ne, zcol = "cyc_1_vehicle_direction_from", map.types = "OpenStreetMap") +mapview(unique_tro_df)
#none of these are on the correct segment that goes SW to NE

# Code those where vehicle direction matches with/contraflow
wan5_dir = wan5 %>% mutate(
  cyc_1_cf_direction = case_when(
    accident_index %in% include_contraflow ~ "Contraflow",
    cyc_1_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"), 
  cyc_2_cf_direction = NA)

# Identify those crashes that definately need to be incldued in the direction crash calculation because we have a direction that matches with flow or contraflow
wan5_dir_final = wan5_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include",
    TRUE ~ "Exclude")) %>%
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

# 2) wan10
wan10 = wand %>% filter(unique_contraflow_ID == "wan10")
mapview(wan10, zcol = "cyc_1_vehicle_direction_from", map.types = "OpenStreetMap") +mapview(unique_tro_df)
# # new one way system that allows cf cycling
# # contraflow direction is variable along length but is roughly east west 
# All carahes occur near junction but no directions match correctly. 

# Code those where vehicle direction matches with/contraflow
wan10_dir = wan10 %>% mutate(
  cyc_1_cf_direction = case_when(
    cyc_1_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"), 
  cyc_2_cf_direction = NA)

# Identify those crashes that definately need to be incldued in the direction crash calculation because we have a direction that matches with flow or contraflow
wan10_dir_final = wan10_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include",
    TRUE ~ "Exclude")) %>%
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

# 3) Rest of Wands
done = c("wan5", "wan10")
not_done = wand %>% filter(!unique_contraflow_ID %in% done)
mapview(not_done, zcol = "cyc_1_vehicle_direction_from", map.types = "OpenStreetMap") +mapview(unique_tro_df)

# checked an none in right direction@
# wan13, wan12, wan8, wan3, wan6

not_done_dir = not_done %>% mutate(
  cyc_1_cf_direction = case_when(
    unique_contraflow_ID == "wan1" & cyc_1_vehicle_direction_from == "South West" & cyc_1_vehicle_direction_to == "North East" ~ "With flow",
    unique_contraflow_ID == "wan9" & cyc_1_vehicle_direction_from == "West" & cyc_1_vehicle_direction_to == "East" ~ "With flow",
    cyc_1_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"), 
  cyc_2_cf_direction = NA)

not_done_dir_final = not_done_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include",
    TRUE ~ "Exclude")) %>%
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

# Join wandsworth df together
wan_dir_final = rbind(wan5_dir_final, wan10_dir_final, not_done_dir_final)

#Save dataframe
saveRDS(wan_dir_final, file = "data-processed/pedal_cycle_direction/wands_dir_final.Rds")



#################
#  Westminster  #
#################

west = min_joined_crashes_pc_direction %>% filter(BOROUGH == "Westminster")
count = west %>% st_drop_geometry() %>% group_by(unique_contraflow_ID) %>% summarise(count = n()) 

# unique_contraflow_ID count
# 1 west10                  10
# 2 west28                  10
# 3 west5                   10
# 4 west1                    8
# 5 west6                    7 contraflow removed
# 6 west23                   6
# 7 west26                   6
# 8 west11                   4
# 9 west13                   4
# 10 west24                   4
# # … with 12 more rows

unique(west$contraflow_stop_date) # NA "2000-12-17"
unique(west$introduces_one_way_street) # TRUE & FALSE
unique(west$introduces_contraflow_bus_lane) # FALSE 
# no cyc2

# 1) west10
west10 = west %>% filter(unique_contraflow_ID == "west10")
mapview(west10, zcol = "cyc_1_vehicle_direction_from", map.types = "OpenStreetMap") +mapview(unique_tro_df)
# new one way system that allows cf cycling
# contraflow is NW to SE

# #none of these go NW to SE or vice versa

# Code those where vehicle direction matches with/contraflow
west10_dir = west10 %>% mutate(
  cyc_1_cf_direction = case_when(
    cyc_1_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"),
  cyc_2_cf_direction = NA)
 
# Identify those crashes that definately need to be incldued in the direction crash calculation because we have a direction that matches with flow or contraflow
west10_dir_final = west10_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include",
    TRUE ~ "Exclude")) %>%
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

# 2) west28
west28 = west %>% filter(unique_contraflow_ID == "west28")
mapview(west28, zcol = "cyc_1_vehicle_direction_from", map.types = "OpenStreetMap") +mapview(unique_tro_df)
# new one way system that allows cf cycling
# contraflow is North east bound

# none go in this direction

# Code those where vehicle direction matches with/contraflow
west28_dir = west28 %>% mutate(
  cyc_1_cf_direction = case_when(
    cyc_1_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"),
  cyc_2_cf_direction = NA)

# Identify those crashes that definately need to be incldued in the direction crash calculation because we have a direction that matches with flow or contraflow
west28_dir_final = west28_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include",
    TRUE ~ "Exclude")) %>%
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

#3) west5 
west5 = west %>% filter(unique_contraflow_ID == "west5")
mapview(west5, zcol = "cyc_1_vehicle_direction_from", map.types = "OpenStreetMap") +mapview(unique_tro_df)
# introduces cf cycle lane
# contraflow is from NW to SE

# Code those where vehicle direction matches with/contraflow
west5_dir = west5 %>% mutate(
  cyc_1_cf_direction = case_when(
    cyc_1_vehicle_direction_from == "North West" & cyc_1_vehicle_direction_to == "South East" ~ "Contraflow",
    cyc_1_vehicle_direction_from == "South East" & cyc_1_vehicle_direction_to == "North West" ~ "With flow",
    cyc_1_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"),
  cyc_2_cf_direction = NA)

# Identify those crashes that definately need to be incldued in the direction crash calculation because we have a direction that matches with flow or contraflow
west5_dir_final = west5_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include",
    TRUE ~ "Exclude")) %>%
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

#4) west1 
west1 = west %>% filter(unique_contraflow_ID == "west1")
mapview(west1, zcol = "cyc_1_vehicle_direction_from", map.types = "OpenStreetMap") +mapview(unique_tro_df)
# introduces cf cycle lane
# contraflow is from SW to NE

# Code those where vehicle direction matches with/contraflow
west1_dir = west1 %>% mutate(
  cyc_1_cf_direction = case_when(
    cyc_1_vehicle_direction_from == "South West" & cyc_1_vehicle_direction_to == "North East" ~ "Contraflow",
    cyc_1_vehicle_direction_from == "North East" & cyc_1_vehicle_direction_to == "South West" ~ "With flow",
    cyc_1_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"),
  cyc_2_cf_direction = NA)
 
# Identify those crashes that definately need to be incldued in the direction crash calculation because we have a direction that matches with flow or contraflow
west1_dir_final = west1_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include",
    TRUE ~ "Exclude")) %>%
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

#5) west6 
#has contraflow removed
west6 = west %>% filter(unique_contraflow_ID == "west6")
mapview(west6, zcol = "cyc_1_vehicle_direction_from", map.types = "OpenStreetMap") +mapview(unique_tro_df)
# introduces cf cycle lane
# contraflow is from SW to NE

# Code those where vehicle direction matches with/contraflow
west6_dir = west6 %>% mutate(
  cyc_1_cf_direction = case_when(cyc_1_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"),
  cyc_2_cf_direction = NA)

# Identify those crashes that definately need to be incldued in the direction crash calculation because we have a direction that matches with flow or contraflow
west6_dir_final = west6_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include",
    TRUE ~ "Exclude")) %>%
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

# 6) Rest of Westminster
done = c("west10", "west28", "west5", "west1", "west6")
not_done = west %>% filter(!unique_contraflow_ID %in% done)
mapview(not_done, zcol = "cyc_1_vehicle_direction_from", map.types = "OpenStreetMap") +mapview(unique_tro_df)

# Other segments where direction doesnt match:
# west12, west21, west26, west27, west29, west34, west4
not_done_dir = not_done %>% mutate(
  cyc_1_cf_direction = case_when(
    unique_contraflow_ID == "west11" & cyc_1_vehicle_direction_from == "South West" & cyc_1_vehicle_direction_to == "North East" ~ "With flow",
    unique_contraflow_ID == "west11" & cyc_1_vehicle_direction_from == "North East" & cyc_1_vehicle_direction_to == "South West" ~ "Contraflow",
    unique_contraflow_ID == "west13" & cyc_1_vehicle_direction_from == "West" & cyc_1_vehicle_direction_to == "East" ~ "Contraflow",
    unique_contraflow_ID == "west16" & cyc_1_vehicle_direction_from == "North East" & cyc_1_vehicle_direction_to == "South West" ~ "Contraflow",
    unique_contraflow_ID == "west17" & cyc_1_vehicle_direction_from == "North East" & cyc_1_vehicle_direction_to == "South West" ~ "With flow",
    unique_contraflow_ID == "west18" & cyc_1_vehicle_direction_from == "South East" & cyc_1_vehicle_direction_to == "North" ~ "Contraflow",
    unique_contraflow_ID == "west2" & cyc_1_vehicle_direction_from == "South East" & cyc_1_vehicle_direction_to == "North" ~ "Contraflow",
    unique_contraflow_ID == "west23" & cyc_1_vehicle_direction_from == "North East" & cyc_1_vehicle_direction_to == "South West" ~ "With flow",
    unique_contraflow_ID == "west24" & cyc_1_vehicle_direction_from == "South East" & cyc_1_vehicle_direction_to == "North West" ~ "Contraflow",
    unique_contraflow_ID == "west24" & cyc_1_vehicle_direction_from == "North West" & cyc_1_vehicle_direction_to == "South East" ~ "With flow",
    unique_contraflow_ID == "west25" & cyc_1_vehicle_direction_from == "North West" & cyc_1_vehicle_direction_to == "South East" ~ "With flow",
    unique_contraflow_ID == "west31" & cyc_1_vehicle_direction_from == "North West" & cyc_1_vehicle_direction_to == "South East" ~ "Contraflow",
    cyc_1_vehicle_direction_from == "unknown (self reported)" ~ "unknown (self reported)"),
  cyc_2_cf_direction = NA)

# Identify those crashes that definately need to be incldued in the direction crash calculation because we have a direction that matches with flow or contraflow
not_done_dir_final = not_done_dir %>%
  mutate(include_for_direction_checked = case_when(
    (cyc_1_cf_direction == "Contraflow" | cyc_1_cf_direction == "With flow") ~ "Include",
    TRUE ~ "Exclude")) %>%
  select(c(1, 43:45)) %>% st_drop_geometry() # tidy up df for joining

# Joine west df together
west_dir_final = rbind(west10_dir_final, west28_dir_final, west5_dir_final, west1_dir_final,
                       west6_dir_final, not_done_dir_final)

nrow(west_dir_final) == nrow(west) # tRUE

#Save dataframe
saveRDS(west_dir_final, file = "data-processed/pedal_cycle_direction/west_dir_final.Rds")


################################################################################
#     Join pedal cycle direction to crashes so can analyse rates               #
################################################################################

joined_crashes_pc_direction = readRDS(file = "data-processed/joined_crashes_pc_direction_need_cf_dir_adding_18_05_22.Rds")

# Import pedal cycle borough files
files_to_import <- list.files(path = "data-processed/pedal_cycle_direction", pattern = "*.Rds", 
                              full.names = TRUE) 
all_pedal_cycle_direction <- purrr::map_dfr(files_to_import, readRDS) 

#Tidy this df
all_pedal_cycle_direction = all_pedal_cycle_direction %>%
  mutate(pedal_cyc_direction_exists = case_when(include_for_direction_checked == "Include" ~ TRUE, 
                                                  include_for_direction_checked == "Exclude" ~ FALSE)) %>%
  select(-c(include_for_direction_checked))


# Join pedal cycle direction to crashes
joined = left_join(joined_crashes_pc_direction, all_pedal_cycle_direction, 
                                              by = "accident_index") %>%
  select(-c(include_for_direction))


# Examine contraflow removed road segment
contraflow_removed = joined %>% st_drop_geometry()  %>% 
  group_by(contraflow_stop_date, unique_contraflow_ID, crash_time_scale) %>% summarise(count = n())
# although 6 roda segments had contraflows removed only two (west6 and hac3) had 
# crashes after the contraflow was removed

# west6 cf removed -> TRO revoked the experimental TRO so presumably went to two way (OSM confirms)
# hac3 cf removed -> reverted to two way (Stated in TRO)

# Determine direction that pedal cycles were travelling it when crash happened
# NB this depends on whether the TRO introduces a one way street/contraflow bus lane
# and when the crash happened (pre, during or postcontraflow)

# 1) Do crashes on road segments that were two way pre contraflow
two_way = joined %>% 
  filter(introduces_one_way_street == TRUE | introduces_contraflow_bus_lane == TRUE) 

two_way_added = two_way %>%
  mutate(cyc_1_direction_when_crashed = case_when(
    crash_time_scale == "Pre_contraflow" & cyc_1_cf_direction == "With flow" ~ "With flow",
    crash_time_scale == "Pre_contraflow" & cyc_1_cf_direction == "Contraflow" ~ "With flow (opposite)",
    crash_time_scale == "Pre_contraflow" & cyc_1_cf_direction == "unknown (self reported)" ~ "unknown (self reported)",
    crash_time_scale == "Contraflow" & cyc_1_cf_direction == "With flow" ~ "With flow",
    crash_time_scale == "Contraflow" & cyc_1_cf_direction == "Contraflow" ~ "Contraflow",
    crash_time_scale == "Contraflow" & cyc_1_cf_direction == "unknown (self reported)" ~ "unknown (self reported)",
    crash_time_scale == "Contraflow_removed" & cyc_1_cf_direction == "With flow" ~ "With flow",
    crash_time_scale == "Contraflow_removed" & cyc_1_cf_direction == "Contraflow" ~ "With flow (opposite)",
    crash_time_scale == "Contraflow_removed" & cyc_1_cf_direction == "unknown (self reported)" ~ "unknown (self reported)",
    is.na(cyc_1_cf_direction) ~ "Direction not compatible"),
    cyc_2_direction_when_crashed = case_when(
      crash_time_scale == "Pre_contraflow" & cyc_2_cf_direction == "With flow" ~ "With flow",
      crash_time_scale == "Pre_contraflow" & cyc_2_cf_direction == "Contraflow" ~ "With flow (opposite)",
      crash_time_scale == "Pre_contraflow" & cyc_2_cf_direction == "unknown (self reported)" ~ "unknown (self reported)",
      crash_time_scale == "Contraflow" & cyc_2_cf_direction == "With flow" ~ "With flow",
      crash_time_scale == "Contraflow" & cyc_2_cf_direction == "Contraflow" ~ "Contraflow",
      crash_time_scale == "Contraflow" & cyc_2_cf_direction == "unknown (self reported)" ~ "unknown (self reported)",
      crash_time_scale == "Contraflow_removed" & cyc_2_cf_direction == "With flow" ~ "With flow",
      crash_time_scale == "Contraflow_removed" & cyc_2_cf_direction == "Contraflow" ~ "With flow (opposite)",
      crash_time_scale == "Contraflow_removed" & cyc_2_cf_direction == "unknown (self reported)" ~ "unknown (self reported)",
      cyc_2_vehicle_type_derived == "Pedal cycle" & is.na(cyc_2_cf_direction) ~ "Direction not compatible"))

# 2) Do crashes on road segments that were one way pre contraflow   
one_way = joined %>% 
  filter(introduces_one_way_street == FALSE & introduces_contraflow_bus_lane == FALSE)

one_way_added = one_way %>%
  mutate(cyc_1_direction_when_crashed = case_when(
    crash_time_scale == "Pre_contraflow" & cyc_1_cf_direction == "With flow" ~ "With flow",
    crash_time_scale == "Pre_contraflow" & cyc_1_cf_direction == "Contraflow" ~ "Contraflow (illegal)",
    crash_time_scale == "Pre_contraflow" & cyc_1_cf_direction == "unknown (self reported)" ~ "unknown (self reported)",
    crash_time_scale == "Contraflow" & cyc_1_cf_direction == "With flow" ~ "With flow",
    crash_time_scale == "Contraflow" & cyc_1_cf_direction == "Contraflow" ~ "Contraflow",
    crash_time_scale == "Contraflow" & cyc_1_cf_direction == "unknown (self reported)" ~ "unknown (self reported)",
    crash_time_scale == "Contraflow" & cyc_1_cf_direction == "With flow" ~ "With flow",
    crash_time_scale == "Contraflow_removed" & cyc_1_cf_direction == "With flow" ~ "With flow",
    crash_time_scale == "Contraflow_removed" & cyc_1_cf_direction == "Contraflow" ~ "With flow (opposite)",
    crash_time_scale == "Contraflow_removed" & cyc_1_cf_direction == "unknown (self reported)" ~ "unknown (self reported)",
    is.na(cyc_1_cf_direction) ~ "Direction not compatible"),
    cyc_2_direction_when_crashed = case_when(
      crash_time_scale == "Pre_contraflow" & cyc_2_cf_direction == "With flow" ~ "With flow",
      crash_time_scale == "Pre_contraflow" & cyc_2_cf_direction == "Contraflow" ~ "Contraflow (illegal)",
      crash_time_scale == "Pre_contraflow" & cyc_2_cf_direction == "unknown (self reported)" ~ "unknown (self reported)",
      crash_time_scale == "Contraflow" & cyc_2_cf_direction == "With flow" ~ "With flow",
      crash_time_scale == "Contraflow" & cyc_2_cf_direction == "Contraflow" ~ "Contraflow",
      crash_time_scale == "Contraflow" & cyc_2_cf_direction == "unknown (self reported)" ~ "unknown (self reported)",
      crash_time_scale == "Contraflow_removed" & cyc_2_cf_direction == "With flow" ~ "With flow",
      crash_time_scale == "Contraflow_removed" & cyc_2_cf_direction == "Contraflow" ~ "With flow (opposite)",
      crash_time_scale == "Contraflow_removed" & cyc_2_cf_direction == "unknown (self reported)" ~ "unknown (self reported)",
      cyc_2_vehicle_type_derived == "Pedal cycle" & is.na(cyc_2_cf_direction) ~ "Direction not compatible"))

# Join these two together to get final df
crashes_pc_and_road_seg_direction = rbind(one_way_added, two_way_added)

# Factor pedal cycle direction when crashed
crashes_pc_and_road_seg_direction = crashes_pc_and_road_seg_direction %>%
  mutate(cyc_1_direction_when_crashed = factor(cyc_1_direction_when_crashed, 
                                               levels = c("With flow", "With flow (opposite)",
                                                          "Contraflow", "Contraflow (illegal)",  
                                                          "unknown (self reported)",
                                                          "Direction not compatible")),
         cyc_2_direction_when_crashed = factor(cyc_2_direction_when_crashed, 
                                               levels = c("With flow", "With flow (opposite)",
                                                          "Contraflow", "Contraflow (illegal)", 
                                                          "unknown (self reported)",
                                                          "Direction not compatible")))

################################################################################
#                 Add significant additional action column                     # 
# (code for this was removed when I moved the 'learning effect' stuff to viz)  #
################################################################################

# Add column that has the additional actions of the TRO
crashes_pc_and_road_seg_direction = crashes_pc_and_road_seg_direction %>%
  mutate(sig_additional_tro_action = case_when(introduces_one_way_street == TRUE ~ "One-way street and contraflow cycling", 
                                               introduces_contraflow_bus_lane == TRUE ~ "Contraflow bus lane and contraflow cycling",
                                               TRUE ~ "Contraflow cycling only"))

 

# Save dataframe
saveRDS(crashes_pc_and_road_seg_direction, file = "data-processed/stats19/pedal_cycle_crashes_with_casualties_&_vehicles_&_junctions&cycle_direction_18_05_22.RDs")
#saveRDS(crashes_pc_and_road_seg_direction, file = "data-processed/stats19/pedal_cycle_crashes_with_casualties_&_vehicles_&_junctions&cycle_direction.RDs")


################################################################################
#                     Summarise data pedal cycle direction                     #
################################################################################

library(tableone)

crashes_pc_and_road_seg_direction = readRDS("data-processed/stats19/pedal_cycle_crashes_with_casualties_&_vehicles_&_junctions&cycle_direction_18_05_22.RDs")


# Get variables
list_vars = c("sig_additional_tro_action", "junction_detail_condensed", "not_within_10m_junction", 
              "cyc_1_direction_when_crashed", "cyc_2_direction_when_crashed")


# A) Crashes that dont occur within 10m of a junction
# Do analysis on two way roads
data_two_way_non10m = crashes_pc_and_road_seg_direction %>% st_drop_geometry() %>%
  filter(sig_additional_tro_action != "Contraflow cycling only") %>%
  filter(not_within_10m_junction == "TRUE")
two_way_table_non10m = CreateTableOne(list_vars, strata = "crash_time_scale", data = data_two_way_non10m)
table_two_way_non10m = print(two_way_table_non10m, quote = FALSE, noSpaces = TRUE, printToggle = FALSE, test = FALSE, showAllLevels = TRUE)
table_two_way_non10m = cbind(Original_road = "Two way", table_two_way_non10m)

# Do analysis on one way roads
data_one_way_non10m = crashes_pc_and_road_seg_direction %>% st_drop_geometry() %>%
  filter(sig_additional_tro_action == "Contraflow cycling only") %>%
  filter(not_within_10m_junction == "TRUE")

one_way_table_non10m = CreateTableOne(list_vars, strata = "crash_time_scale", data = data_one_way_non10m)
table_one_way_non10m = print(one_way_table_non10m, quote = FALSE, noSpaces = TRUE, printToggle = FALSE, test = FALSE, showAllLevels = TRUE)
table_one_way_non10m = cbind(Original_road = "One way", table_one_way_non10m)

# Join these analyses to get table
pc_direction_non10m = rbind(table_one_way_non10m, table_two_way_non10m)
write.csv(pc_direction_non10m, file = "output/pc_direction_non10m_10_06_2022.csv")

# B) Crashes that DO occur within 10m of a junction
# Do analysis on two way roads
data_two_way_10m_JUNC = crashes_pc_and_road_seg_direction %>% st_drop_geometry() %>%
  filter(sig_additional_tro_action != "Contraflow cycling only") %>%
  filter(not_within_10m_junction == "FALSE")
two_way_table_10m_JUNC = CreateTableOne(list_vars, strata = "crash_time_scale", data = data_two_way_10m_JUNC)
table_two_way_10m_JUNC = print(two_way_table_10m_JUNC, quote = FALSE, noSpaces = TRUE, printToggle = FALSE, test = FALSE, showAllLevels = TRUE)
table_two_way_10m_JUNC = cbind(Original_road = "Two way", table_two_way_10m_JUNC)

# Do analysis on one way roads
data_one_way_10m_JUNC = crashes_pc_and_road_seg_direction %>% st_drop_geometry() %>%
  filter(sig_additional_tro_action == "Contraflow cycling only") %>%
  filter(not_within_10m_junction == "FALSE")

one_way_table_10m_JUNC = CreateTableOne(list_vars, strata = "crash_time_scale", data = data_one_way_10m_JUNC)
table_one_way_10m_JUNC = print(one_way_table_non10m_JUNC, quote = FALSE, noSpaces = TRUE, printToggle = FALSE, test = FALSE, showAllLevels = TRUE)
table_one_way_10m_JUNC = cbind(Original_road = "One way", table_one_way_10m_JUNC)

# Join these analyses to get table
pc_direction_10m_JUNC = rbind(table_one_way_10m_JUNC, table_two_way_10m_JUNC)
write.csv(pc_direction_10m_JUNC, file = "output/pc_direction_10m_JUNC_10_06_2022.csv")
