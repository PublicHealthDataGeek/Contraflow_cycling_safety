#################################################################################
#                    Prepping data for crash rate analysis                      #
#                                                                               #
# Obtain key data required for crash rate analysis:                             #   
# - bootstrapped crashes dataframe to enable confidence interval calculation    #
# - time duration that road segments spent in different statuses                #
#   ie pre-contraflow, contraflow and post contraflow - required as denominator # 
#   for rate calculation.                                                       #
# - determining whether crashes occurred relative to TFL count cordons which is #
#   required to adjust rates using the appropriate cyclist and traffic volumes  #
#                                                                               #
# Code was rerun using the AAP dataset (where SBC and self-reported crashes     #
# were removed) so total dataset size is 1498 observations (rerun on 28/9/22)   #
# Added additional calculation of numbers from bootstrap by infrastructure sub- #
# type as this additional crash analysis was requested.                         #
#################################################################################

# Load packages
library(tidyverse)
library(sf)
# library(lubridate)
library(rsample) # so can bootstrap

# Set mapview options
mapviewOptions(fgb = FALSE)


# Load dataframes
crashes_casualties_vehicles = readRDS("data-processed/stats19/pedal_cycle_crashes_with_casualties_&_vehicles_&_junctions&cycle_direction_28_09_22.RDs")
unique_tro_df = readRDS(file = "data-processed/unique_tro_df_durations_using_start_date.Rds")

################################################################################
#       Add new variable for infrastructure as requested by AAP reviewer       #
#                       for subgroup crash analysis                            #
################################################################################

crashes_casualties_vehicles %>%
  st_drop_geometry() %>%
  group_by(sig_additional_tro_action, introduces_cf_cyclelane, 
           introduces_cf_cycletrack, introduces_cf_footway,  
           enables_cf_cycling_in_bus_lane, introduces_cf_seg_cyclelane) %>%
  summarise(count = n()) %>%
  select(c(7, 1:6)) %>%
  arrange(sig_additional_tro_action, desc(count))
# count sig_additional_tro_action                    Other actions 
# 1   212 Contraflow bus lane and contraflow cycling enables cf cycling in bus lane                      
# 2   550 Contraflow cycling only                                         
# 3   186 Contraflow cycling only                    introduces a cf cycle lane                                       
# 4   134 Contraflow cycling only                    introduces a cycle lane AND segregated cf cycle lane                       
# 5   106 Contraflow cycling only                    and enables cf cycling in bus lane                      
# 6    20 Contraflow cycling only                    intdouces and cf cycletrack AND contraflow cycling in the footway                       
# 7   124 One-way street and contraflow cycling                        
# 8    96 One-way street and contraflow cycling      introduces a cf cycle lane                  
# 9    38 One-way street and contraflow cycling      introduces a cycle lane AND segregated cf cycle lane                      
# 10    20 One-way street and contraflow cycling     introduces contraflow cycling in the footway                       
# 11    11 One-way street and contraflow cycling     introduces a cf cycle lane AND cf cycle track                    
# 12     1 One-way street and contraflow cycling     introdoces and cf cycletrack AND contraflow cycling in the footway  

crashes_casualties_vehicles_infra = crashes_casualties_vehicles %>%
  mutate(infra = factor
         (case_when((introduces_cf_cyclelane == TRUE & introduces_cf_cycletrack == FALSE & introduces_cf_seg_cyclelane == FALSE) ~ "Cycle lane",
                    introduces_cf_seg_cyclelane == TRUE ~ "Cycle lane with some segregation",
                    (introduces_cf_cycletrack == TRUE & introduces_cf_cyclelane == FALSE & introduces_cf_footway == FALSE) ~ "Cycle track",
                    (introduces_cf_cyclelane == TRUE & introduces_cf_cycletrack == TRUE) ~ "Cycle track and lane",
                    enables_cf_cycling_in_bus_lane == TRUE ~ "Cycling in contraflow bus lane",
                    (introduces_cf_footway == TRUE & introduces_cf_cycletrack == FALSE) ~ "Cycling on footway",
                    (introduces_cf_footway == TRUE & introduces_cf_cycletrack == TRUE) ~ "Cycle track and cycling on footway",
                    TRUE ~ "No additional action"))) 

crashes_casualties_vehicles_infra$unique_contraflow_ID = reorder(crashes_casualties_vehicles_infra$unique_contraflow_ID, crashes_casualties_vehicles_infra$contraflow_start_date)
crashes_casualties_vehicles_infra$sig_additional_tro_action =  fct_infreq(crashes_casualties_vehicles_infra$sig_additional_tro_action)
crashes_casualties_vehicles_infra$infra =  fct_infreq(crashes_casualties_vehicles_infra$infra)                        
check = crashes_casualties_vehicles_infra %>%
  st_drop_geometry() %>%
  group_by(sig_additional_tro_action, introduces_cf_cyclelane, 
           introduces_cf_cycletrack, introduces_cf_footway,  
           enables_cf_cycling_in_bus_lane, introduces_cf_seg_cyclelane, infra) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  select(c(8, 1, 7)) %>%
  arrange(sig_additional_tro_action, desc(count))
# count sig_additional_tro_action                  infra                             
# 1   550 Contraflow cycling only                    No additional action              
# 2   186 Contraflow cycling only                    Cycle lane                        
# 3   134 Contraflow cycling only                    Cycle lane with some segregation  
# 4   106 Contraflow cycling only                    Cycling in contraflow bus lane    
# 5    20 Contraflow cycling only                    Cycle track and cycling on footway
# 6   124 One-way street and contraflow cycling      No additional action              
# 7    96 One-way street and contraflow cycling      Cycle lane                        
# 8    38 One-way street and contraflow cycling      Cycle lane with some segregation  
# 9    20 One-way street and contraflow cycling      Cycling on footway                
# 10    11 One-way street and contraflow cycling      Cycle track and lane              
# 11     1 One-way street and contraflow cycling      Cycle track and cycling on footway
# 12   212 Contraflow bus lane and contraflow cycling Cycling in contraflow bus lane 


saveRDS = crashes_casualties_vehicles = saveRDS(crashes_casualties_vehicles_infra, 
                                                "data-processed/stats19/pedal_cycle_crashes_with_casualties_&_vehicles_&_junctions&cycle_direction&infra_28_09_22.RDs")

################################################################################
#    Identify the road segments that have crashes on by infrastructure type    #
################################################################################

# Identify number of road segments that have each type of infrastructure type introduced on
unique_small = unique_tro_df %>% 
  filter(!is.na(contraflow_start_date)) %>%
  select(c(1, 8:14)) %>%
  st_drop_geometry() %>%
  mutate(sig_additional_tro_action = case_when(introduces_one_way_street == TRUE ~ "One-way street and contraflow cycling", 
                                               introduces_contraflow_bus_lane == TRUE ~ "Contraflow bus lane and contraflow cycling",
                                               TRUE ~ "Contraflow cycling only")) %>%
  mutate(infra = factor
         (case_when((introduces_cf_cyclelane == TRUE & introduces_cf_cycletrack == FALSE & introduces_cf_seg_cyclelane == FALSE) ~ "Cycle lane",
                    introduces_cf_seg_cyclelane == TRUE ~ "Cycle lane with some segregation",
                    (introduces_cf_cycletrack == TRUE & introduces_cf_cyclelane == FALSE & introduces_cf_footway == FALSE) ~ "Cycle track",
                    (introduces_cf_cyclelane == TRUE & introduces_cf_cycletrack == TRUE) ~ "Cycle track and lane",
                    enables_cf_cycling_in_bus_lane == TRUE ~ "Cycling in contraflow bus lane",
                    (introduces_cf_footway == TRUE & introduces_cf_cycletrack == FALSE) ~ "Cycling on footway",
                    (introduces_cf_footway == TRUE & introduces_cf_cycletrack == TRUE) ~ "Cycle track and cycling on footway",
                    TRUE ~ "No additional action"))) 
unique_small$sig_additional_tro_action = factor(unique_small$sig_additional_tro_action,
                                                levels = c("Contraflow cycling only","One-way street and contraflow cycling",
                                                           "Contraflow bus lane and contraflow cycling"))

road_segments = unique_small %>%
  group_by(sig_additional_tro_action, infra) %>%
  summarise(n_segments = n()) %>%
  arrange(sig_additional_tro_action, desc(n_segments))
sum(road_segments$n_segments) # n = 473 this matches the number of road segments that have crashes on


# Identify road segments that have a crash on
crashes_casualties_vehicles_infra = readRDS(file = "data-processed/stats19/pedal_cycle_crashes_with_casualties_&_vehicles_&_junctions&cycle_direction&infra_28_09_22.RDs")

infra_crash_road_segments = crashes_casualties_vehicles_infra %>%
  st_drop_geometry() %>%
  group_by(sig_additional_tro_action, infra, unique_contraflow_ID) %>%
  summarise(n_crashes = n()) %>%
  group_by(sig_additional_tro_action, infra) %>%
  summarise(n_unique_road_segments_with_this_action_that_have_crash = n())
sum(infra_crash_road_segments$n_unique_road_segments_with_this_action_that_have_crash) # n = 306
# sig_additional_tro_action                  infra                              n_unique_…¹
# 1 Contraflow cycling only                    No additional action                       167
# 2 Contraflow cycling only                    Cycling in contraflow bus lane               7
# 3 Contraflow cycling only                    Cycle lane                                  48
# 4 Contraflow cycling only                    Cycle lane with some segregation            12
# 5 Contraflow cycling only                    Cycle track and cycling on footway           2
# 6 One-way street and contraflow cycling      No additional action                        38
# 7 One-way street and contraflow cycling      Cycle lane                                  16
# 8 One-way street and contraflow cycling      Cycle lane with some segregation             3
# 9 One-way street and contraflow cycling      Cycle track and cycling on footway           1
# 10 One-way street and contraflow cycling      Cycling on footway                           2
# 11 One-way street and contraflow cycling      Cycle track and lane                         1
# 12 Contraflow bus lane and contraflow cycling Cycling in contraflow bus lane               9

# Identify number of road segmentst that have a crash on by crash time scale
road_segments_crash_time_scale = crashes_casualties_vehicles_infra %>%
    st_drop_geometry() %>%
    group_by(crash_time_scale, sig_additional_tro_action, infra, unique_contraflow_ID) %>%
    summarise(n_crashes = n()) %>%
    group_by(crash_time_scale, sig_additional_tro_action, infra) %>%
    summarise(n_distinct_road_segments = n_distinct(unique_contraflow_ID)) %>%
    pivot_wider(names_from = crash_time_scale, values_from = n_distinct_road_segments)

road_segments_crash_time_scale$Contraflow[is.na(road_segments_crash_time_scale$Contraflow)] = 0
road_segments_crash_time_scale$Pre_contraflow[is.na(road_segments_crash_time_scale$Pre_contraflow)] = 0
road_segments_crash_time_scale$Contraflow_removed[is.na(road_segments_crash_time_scale$Contraflow_removed)] = 0

# sig_additional_tro_action                  infra                              Pre_contraflow Contraflow Contraflow_removed
# 1 Contraflow cycling only                    No additional action                          137         92                  0
# 2 Contraflow cycling only                    Cycling in contraflow bus lane                  7          6                  0
# 3 Contraflow cycling only                    Cycle lane                                     29         33                  1
# 4 Contraflow cycling only                    Cycle lane with some segregation                8         11                  0
# 5 Contraflow cycling only                    Cycle track and cycling on footway              2          2                  0
# 6 One-way street and contraflow cycling      No additional action                           34         17                  0
# 7 One-way street and contraflow cycling      Cycle lane                                      8         12                  0
# 8 One-way street and contraflow cycling      Cycle lane with some segregation                2          3                  0
# 9 One-way street and contraflow cycling      Cycling on footway                              2          2                  0
# 10 One-way street and contraflow cycling      Cycle track and lane                            1          1                  0
# 11 Contraflow bus lane and contraflow cycling Cycling in contraflow bus lane                  6          9                  0
# 12 One-way street and contraflow cycling      Cycle track and cycling on footway              0          1                  0

# validate above using below dataset
# min = crashes_casualties_vehicles_infra %>%
#   select(c(1, 27, 44, 104, 105)) %>%
#   st_drop_geometry()
# Yes the figures add up 


road_segments_infra_crash = left_join(road_segments, infra_crash_road_segments, by = c("sig_additional_tro_action", "infra")) %>%
  left_join(road_segments_crash_time_scale,by = c("sig_additional_tro_action", "infra")) %>%
  mutate(percent_segments_crash = round(n_unique_road_segments_with_this_action_that_have_crash/n_segments *100, digits = 1),
         percent_segments_crash_pre = round(Pre_contraflow/n_segments *100, digits = 1),
         percent_segments_crash_contra = round(Contraflow/n_segments *100, digits = 1),
         percent_segments_crash_rem = round(Contraflow_removed/n_segments *100, digits = 1),
         road_segments_crash = paste0(n_unique_road_segments_with_this_action_that_have_crash, " (", percent_segments_crash, ")"),
         road_segments_crash_pre = paste0(Pre_contraflow, " (", percent_segments_crash_pre, ")"),
         road_segments_crash_contra = paste0(Contraflow, " (", percent_segments_crash_contra, ")"),
         road_segments_crash_rem = paste0(Contraflow_removed, " (", percent_segments_crash_rem, ")"))
sum(road_segments_infra_crash$n_segments) # 473
sum(road_segments_infra_crash$n_unique_road_segments_with_this_action_that_have_crash) # 306
sum(road_segments_infra_crash$Pre_contraflow) # 236
sum(road_segments_infra_crash$Contraflow) # 189
sum(road_segments_infra_crash$Contraflow_removed) # 1

road_segments_infra_crash = road_segments_infra_crash %>%
  select(c(1:3, 12:15)) # drop extraneous columns

write_csv(road_segments_infra_crash, file = "output/road_segments_infra_crash_status.csv")

 
################################################################################
#         Bootstrap crash data in order to obtain confidence intervals         #
#                                 for crash rates                              #
#                                                                              #
# Purpose - to help quantify the uncertainty in number of crashes between      #
#  years                                                                       #
#                                                                              #  
# bootstrap - all crash data 1000 samples with replacement, each crash         #
# (data row) is an observation, each bootstrap sample has n = 1498             #
#                                                                              #
#                                                                              #
################################################################################

# Reduce crash df to column we want to be able to analyse
cols_keep = c("accident_index", "accident_year", "crash_time_scale", "sig_additional_tro_action", 
              "not_within_10m_junction", "junction_detail_condensed", 
              "cyc_1_direction_when_crashed", "cyc_2_direction_when_crashed", 
              "sig_additional_tro_action", "accident_severity", "cyclist_casualty","pedestrian_casualty",
              "contraflow_length", "infra")
to_boot = crashes_casualties_vehicles_infra %>% 
  select(all_of(cols_keep)) %>%
  st_drop_geometry()


bootstrap_df = bootstraps(to_boot, times=1000, apparent=TRUE) %>%
  mutate(boot = map(splits, analysis)) %>%
  unnest(boot) %>%
  select(-c(splits)) # drop the splits column that contains the nested data

# Save bootstrap df
saveRDS(bootstrap_df, "data-processed/bootstrap_df_28_09_2022.Rds")
# n = 1499498  ie 1001 x the number of crashes (1498)
# 1001 is the original dataset plus the 1000 bootstrapped datasets

################################################################################
#     Get time exposure data for road segments by variables of interest        #
################################################################################

# NB the output of this section of code is not changed by using the AAP dataset
# as it is based on duration of expsoure over the 22 years to infrastructure

# Check that no crashes occur on contraflows where we dont have a start date
nrow(crashes_casualties_vehicles %>% filter(!is.na(contraflow_start_date))) ==
  nrow(crashes_casualties_vehicles) # TRUE

# 1) Get time duration of road segments by status
unique_tro_df_with_start_date = unique_tro_df %>% filter(!is.na(contraflow_start_date)) # n = 473
# The above removes the 35 contraflows where we dont have a start date - these need to be excluded from the
# calculation of the duration as the duration is not accurate (it is based on contraflow_start_date_derived)

Pre_contraflow = sum(unique_tro_df_with_start_date$pre_contraflow_duration)
Contraflow = sum(unique_tro_df_with_start_date$contraflow_duration)
Contraflow_removed = sum(unique_tro_df_with_start_date$post_contraflow_duration, na.rm = TRUE)
total_exposure_duration_days = as.data.frame(t(data.frame(Pre_contraflow, Contraflow, Contraflow_removed))) 

road_segment_exposure = data.frame(crash_time_scale = factor(c("Pre_contraflow", "Contraflow", "Contraflow_removed"),
                                                             levels = c("Pre_contraflow", "Contraflow", "Contraflow_removed")),
                                   total_exposure_duration_days = c(sum(unique_tro_df_with_start_date$pre_contraflow_duration),
                                                                    sum(unique_tro_df_with_start_date$contraflow_duration),
                                                                    sum(unique_tro_df_with_start_date$post_contraflow_duration, na.rm = TRUE)))

# crash_time_scale total_exposure_duration_days
# 1     Pre_contraflow                      2396119
# 2         Contraflow                      1392487
# 3 Contraflow_removed                        11949


# Save dataframe
saveRDS(road_segment_exposure, "data-processed/road_segment_exposure_28_09_2022.Rds")


# 2) Get time duration of road segments by status and action
tro_action = unique_tro_df_with_start_date %>%
  mutate(sig_additional_tro_action = case_when(introduces_one_way_street == TRUE ~ "One-way street and contraflow cycling", 
                                               introduces_contraflow_bus_lane == TRUE ~ "Contraflow bus lane and contraflow cycling",
                                               TRUE ~ "Contraflow cycling only"))
tro_action_durations = tro_action %>% group_by(sig_additional_tro_action) %>%
  summarise(pre_contraflow_duration = sum(pre_contraflow_duration),
            contraflow_duration = sum(contraflow_duration),
            post_contraflow_duration = sum(post_contraflow_duration)) %>%
  st_drop_geometry()

sum(tro_action_durations$pre_contraflow_duration) # 2396119
sum(tro_action_durations$contraflow_duration) # 1392487
sum(tro_action_durations$post_contraflow_duration) # 11949

# Pivot table to get into format for joining to num_crashes for crash rates
pivot_tro_action_durations = tro_action_durations %>%
  pivot_longer(cols = pre_contraflow_duration:post_contraflow_duration, names_to = "crash_time_scale", 
               values_to = "exposure_duration_days") %>%
  mutate(crash_time_scale = factor(crash_time_scale, levels = c("pre_contraflow_duration",
                                                                "contraflow_duration", 
                                                                "post_contraflow_duration"), 
    labels = c("Pre_contraflow", "Contraflow", "Contraflow_removed")))
 
# Save dataframe
saveRDS(pivot_tro_action_durations, "data-processed/road_segment_exposure_tro_action_28_09_2022.Rds")  
  
  


###############################################################################
#  Get TFL cordon data and make dataframe that links accident_index to cordon #
###############################################################################

# Manage TFL cordon spatial data
##Import TfL cordon map (source = TFL counter data spreadsheet fro inner london has map)
tfl_cordon = st_read("map_data/tfl_cordon_3857_v1.shp") 
tfl_cordon_t = tfl_cordon %>% st_transform(27700)

## Manipulate spatial data so have area between central and inner cordon
inner_polygon = tfl_cordon_t %>% filter(type == "inner") %>% select(-c(id)) %>% st_make_valid()
inner_polygon[1,1] = "both"
central_polygon = tfl_cordon_t %>% filter(type == "central") %>% select(-c(id)) %>% st_make_valid()
inner_cordon_area = st_difference(inner_polygon, central_polygon) %>% # obtain polygon of the area between central and inner polygons
  select(-c(type.1))
inner_cordon_area[1,1] = "inner"

# Create df of cordon areas that can be used for maps etc
tfl_cordon_areas = rbind(central_polygon, inner_cordon_area, inner_polygon)
tfl_cordon_areas %>% filter(type != "both") %>% mapview(zcol = "type") #check mapping ok
# saveRDS(tfl_cordon_areas, file = "data-processed/tfl_cordon_areas.Rds")

# Identify which crashes occur within each cordon polygon
crashes_central = st_join(crashes_casualties_vehicles_infra, central_polygon) %>%
  mutate(central_cordon = case_when(type == "central" ~ TRUE, 
                                    TRUE ~ FALSE))
crashes_inner = st_join(crashes_casualties_vehicles_infra, inner_cordon_area) %>%
  mutate(inner_cordon = case_when(type == "inner" ~ TRUE,
                                  TRUE ~ FALSE)) %>%
  st_drop_geometry() %>% # drop this so can do left join
  select(c(accident_index, inner_cordon))

crashes_by_cordon = left_join(crashes_central, crashes_inner, by = "accident_index") %>%
  mutate(cordon_count_location = case_when(central_cordon == TRUE & inner_cordon == FALSE ~ "central", # identifies those in central
                                           central_cordon == FALSE & inner_cordon == TRUE ~ "inner", # identifies those in inner
                                           central_cordon == FALSE & inner_cordon == FALSE ~ "outer")) %>%  # identifies those outside both
  select(c(accident_index, cordon_count_location))

crashes_by_cordon %>% 
  st_drop_geometry() %>%
  group_by(cordon_count_location) %>%
  summarise(number = n())
# cordon_count_location   number
# central                 790
# inner                   622
# outer                    86

# Visual check - the crashes all look to be correctly labelled with the cordon polygon they are in
mapview(crashes_by_cordon, zcol = "cordon_count_location") + mapview(inner_cordon_area)

# # Save dataframe
saveRDS(crashes_by_cordon, "data-processed/crashes_by_cordon_28_09_2022.Rds") 
# This is then used to do visualisation as need crash geometry

# Attach cordon label to crashes
crashes_by_cordon = crashes_by_cordon %>% st_drop_geometry()  #drop geom making it quicker to join
cordon_bootstrap_df = left_join(bootstrap_df, crashes_by_cordon, by = "accident_index") 

# Get number of crashes by year, road segment status and cordon location 
num_crashes_per_year_cordon = cordon_bootstrap_df %>% 
  group_by(id, accident_year, crash_time_scale, cordon_count_location) %>%
  summarise(raw_num_crashes = n()) # num of crashes per year by crash time scale and cordon type for each dataset - apparant +1000 bootstrap

# Get number of crashes by year, road segment status, cordon location and tro_action
num_crashes_per_year_action_cordon = cordon_bootstrap_df %>% 
  group_by(id, accident_year, crash_time_scale, sig_additional_tro_action, cordon_count_location) %>%
  summarise(raw_num_crashes = n()) # 

# Get number of crashes by year, road segment status, cordon location and junction status
num_crashes_per_year_junction_cordon = cordon_bootstrap_df %>%
  group_by(id, accident_year, crash_time_scale, not_within_10m_junction, cordon_count_location) %>%
  summarise(raw_num_crashes = n()) # num of crashes per year by crash time scale and presence of junction

# Get number of crashes by year, road segment status, cordon location and cyclist vehicle flow direction
num_crashes_flow_cyc1_bootstrap = cordon_bootstrap_df %>% 
  group_by(id, accident_year, crash_time_scale, cyc_1_direction_when_crashed, cordon_count_location) %>%
  summarise(num_crashes_cyc1 = n()) # 

num_crashes_flow_cyc2_bootstrap = cordon_bootstrap_df %>% 
  filter(!is.na(cyc_2_direction_when_crashed)) %>% # drop those obs where there is no cyc_2 direction
  group_by(id, accident_year, crash_time_scale, cyc_2_direction_when_crashed, cordon_count_location) %>%
  summarise(num_crashes_cyc2 = n()) # 

num_crashes_per_year_flow = left_join(num_crashes_flow_cyc1_bootstrap, num_crashes_flow_cyc2_bootstrap, 
                  by = c("id","crash_time_scale", "accident_year", "cordon_count_location", 
                         "cyc_1_direction_when_crashed" = "cyc_2_direction_when_crashed")) %>%
  rename(cyc_direction_when_crashed = cyc_1_direction_when_crashed)
num_crashes_per_year_flow$num_crashes_cyc2[is.na(num_crashes_per_year_flow$num_crashes_cyc2)] = 0

num_crashes_per_year_flow = num_crashes_per_year_flow %>% 
  mutate(num_crashes_flow_overall = num_crashes_cyc1 + num_crashes_cyc2) # 

# Get number of crashes by year, road segment status, cordon location and type of infrastructure plus sig add action
num_crashes_per_year_infra_cordon = cordon_bootstrap_df %>% 
  group_by(id, accident_year, crash_time_scale, sig_additional_tro_action, infra, cordon_count_location) %>%
  summarise(raw_num_crashes = n()) # 

# Save dataframes
saveRDS(cordon_bootstrap_df, "data-processed/cordon_bootstrap_df_28_09_2022.Rds")
saveRDS(num_crashes_per_year_cordon, "data-processed/num_crashes_per_year_cordon_28_09_2022.Rds")
saveRDS(num_crashes_per_year_action_cordon, "data-processed/num_crashes_per_year_action_cordon_28_09_2022.Rds")
saveRDS(num_crashes_per_year_junction_cordon, "data-processed/num_crashes_per_year_junction_cordon_28_09_2022.Rds")
saveRDS(num_crashes_per_year_flow, "data-processed/num_crashes_per_year_flow_cordon_28_09_2022.Rds")
saveRDS(num_crashes_per_year_infra_cordon, "data-processed/num_crashes_per_year_infra_cordon_28_09_2022.Rds")
