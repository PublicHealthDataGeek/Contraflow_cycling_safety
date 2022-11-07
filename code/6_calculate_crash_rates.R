################################################################################
#                          Calculating Crash rates                             #
#                                                                              #
# This code file use the crash Rds off all crashes involving a pedal cycle     #
# within 10m buffer of contraflow segments.                                    #
#                                                                              #
# It calculates the following crash rates                                      #
# - Overall crash rate                                                         #
# - Crash rate near/not near junction                                          #
# - crash rate by pedal cycle direction                                        #
# - crash rate by action                                                       #
#                                                                              #
# Crash rates are expressed as raw/unadjusted and then adjusted for change in  #
# cyclist volume                                                               #    
#                                                                              #                           #
################################################################################

# Load packages
library(tidyverse)
library(sf)
library(ggdist)
library(distributional)
library(ggplot2)
library(dplyr) # for relevelling crash_time_scale for visualisations



# Load dataframes
#crashes_casualties_vehicles = readRDS("data-processed/stats19/pedal_cycle_crashes_with_casualties_&_vehicles_&_junctions&cycle_direction_18_05_22.RDs")
#unique_tro_df = readRDS(file = "data-processed/unique_tro_df_durations_using_start_date.Rds")
bootstrap_df = readRDS("data-processed/bootstrap_df_28_09_2022.Rds")
road_segment_exposure = readRDS("data-processed/road_segment_exposure_28_09_2022.Rds")
road_segment_exposure_tro_action = readRDS("data-processed/road_segment_exposure_tro_action_28_09_2022.Rds")
tfl_cordon_cycle_traffic_index = readRDS(file = "data-processed/cycle_volume/tfl_cordon_cycle_traffic_index.Rds")

################################################################################
#                            A) Overall crash rates                            #
################################################################################

#                                                    number of precontraflow crashes
#  Pre contraflow overall crash rate =  ---------------------------------------------------------------------------
#                                       total amount of time precontraflow of all contraflows that we know durations

# Total amount of time precontraflow is the sum of all the time from 1/1/1998
# until a contraflow is put in for all contraflows (which have a start date so we know their
# durations, n = 473) ie irrespective of whether crashes occurs on them or not - they still
# have the potential for a crash to occur on them.

##########
# 1) Raw #
##########

# Get number of crashes
num_crashes = bootstrap_df %>% 
  group_by(id, crash_time_scale) %>%
  summarise(num_crashes = n())
# crash_time_scale   num_crashes (APPARENT)
# 1 Pre_contraflow             788
# 2 Contraflow                 703
# 3 Contraflow_removed           7


# Join number of crashes to length of exposure and calculate rate
raw_overall_crash_rates = left_join(num_crashes, road_segment_exposure) %>%
  mutate(raw_overall_crash_rate_per_100years_of_exposure = round((num_crashes/(total_exposure_duration_days/365)*100), digit = 1)) %>%
  ungroup()

# relabel crash time scale as cant seem to do it in ggplot when using ggdist
levels(raw_overall_crash_rates$crash_time_scale)
raw_overall_crash_rates$crash_time_scale = recode_factor(raw_overall_crash_rates$crash_time_scale,
                                                         Pre_contraflow = "Pre-contraflow",
                                                         Contraflow = "Contraflow",
                                                         Contraflow_removed = "Contraflow removed")
# Apparent crash rate
apparent = raw_overall_crash_rates %>%
  filter(id == "Apparent") 

# Get 95% confidence interval data for crash_rates
conf_int = raw_overall_crash_rates %>%
  filter(id != "Apparent") %>%
  group_by(crash_time_scale) %>%
  summarise(rate_025_percentile = round(quantile(raw_overall_crash_rate_per_100years_of_exposure, prob = 0.025), digit = 3),
            rate_975_percentile = round(quantile(raw_overall_crash_rate_per_100years_of_exposure, prob = 0.975), digit = 3)) 

# Create results table
raw_overall_crash_rates_table = left_join(apparent, conf_int, by = "crash_time_scale") %>%
  mutate(analysis = "raw overall") %>%
  select(c(8, 2:7))
raw_overall_crash_rates_table$rate_95_CI = paste(raw_overall_crash_rates_table$raw_overall_crash_rate_per_100years_of_exposure, 
                                                 "(", raw_overall_crash_rates_table$rate_025_percentile, 
                                                 "-", raw_overall_crash_rates_table$rate_975_percentile, ")")
raw_overall_crash_rates_table %>% select((c(1:4, 8))) %>%
  write_csv("output/crash_rates/tables/raw_overall_crash_rates_table_28_09_22.csv")

# analysis    crash_time_scale   num_crashes total_exposure_duration_days raw_overall_crash_rate_per_100years_of_exposure rate_025_percentile rate_975_percentile rate_95_CI         
# 1 raw overall Pre-contraflow             788                      2396119                                            12                  11.4                12.6 12 ( 11.4 - 12.6 ) 
# 2 raw overall Contraflow                 703                      1392487                                            18.4                17.4                19.4 18.4 ( 17.4 - 19.4…
# 3 raw overall Contraflow removed           7                        11949                                            21.4                 9.2                39.7 21.4 ( 9.2 - 39.7 )

# Save df
saveRDS(raw_overall_crash_rates, "data-processed/rates/raw_overall_crash_rates_28_09_2022.Rds")


############################################
# 2) Adjusted for change in cycling volume #  
############################################

# Load dataframes 
num_crashes_per_year_cordon = readRDS("data-processed/num_crashes_per_year_cordon_28_09_2022.Rds")


# Join number of crashes by year + cordon 
#To start with need to adjust the number of crashese tfl cordon multiplier for cycling and traffic
vol_adj_num_crashes_per_year_cordon = left_join(num_crashes_per_year_cordon,
                                                    tfl_cordon_cycle_traffic_index %>% select(c(1, 8:10)),
                                                    by = c("accident_year" = "year")) %>%
  ungroup() %>%
  mutate(correct_cyc_vol_index = 0) # create blank columns to put the correct multiplier into

# Update the index column (col 9) with the right multiplier 
## (ie put the outer_multipler in for crashes that were outer)
for(i in 1:nrow(vol_adj_num_crashes_per_year_cordon)) {
  if(vol_adj_num_crashes_per_year_cordon[i, 4] == "central") {
    vol_adj_num_crashes_per_year_cordon[i, 9] <- vol_adj_num_crashes_per_year_cordon[i, 6] # col 6  is the cyc central index
  }
  else if(vol_adj_num_crashes_per_year_cordon[i, 4] == "inner") {
    vol_adj_num_crashes_per_year_cordon[i, 9] <- vol_adj_num_crashes_per_year_cordon[i, 7] # col 7 is the cyc inner index
  }
  else if(vol_adj_num_crashes_per_year_cordon[i, 4] == "outer") {
    vol_adj_num_crashes_per_year_cordon[i, 9] <- vol_adj_num_crashes_per_year_cordon[i, 8] # col 8 is the cyc outer index
  }
}

# Drop the no longer needed index columns
vol_adj_num_crashes_per_year_cordon = vol_adj_num_crashes_per_year_cordon %>% select(-c(6:8))
 
# Calculate the adjusted number of crashes using the index 
vol_adj_num_crashes_per_year_cordon = vol_adj_num_crashes_per_year_cordon %>%
  mutate(cyc_vol_cordon_adj_num_crashes_per_year = raw_num_crashes/correct_cyc_vol_index)# raw numbers adjusted by change in cycling vol

# Calculate the total adjusted number of crashes by road segment status
cyc_vol_adj_num_crashes_per_year_cordon_by_time_period = vol_adj_num_crashes_per_year_cordon %>%
  group_by(id, crash_time_scale) %>%
  summarise(total_cyc_vol_cordon_adj_num_crashes = sum(cyc_vol_cordon_adj_num_crashes_per_year))
 
# Calculate the overall adj crash rate taking into account the exposure time
cyc_vol_adj_num_crashes_per_year_cordon_by_time_period = left_join(cyc_vol_adj_num_crashes_per_year_cordon_by_time_period,
                                                                   road_segment_exposure) %>%
  mutate(cyc_vol_cordon_adj_crash_rate_per_100years_of_exposure =
           round((total_cyc_vol_cordon_adj_num_crashes/(total_exposure_duration_days/365)*100), digit = 1)) %>%
  mutate(crash_time_scale = recode(crash_time_scale, Pre_contraflow = "Pre-contraflow",
                                   Contraflow = "Contraflow",
                                   Contraflow_removed = "Contraflow removed"))

# Apparent crash rate
apparent_cyc_adj = cyc_vol_adj_num_crashes_per_year_cordon_by_time_period %>%
  filter(id == "Apparent")

# id       crash_time_scale   total_cyc_vol_cordon_adj_num_crashes total_exposure…¹ cyc_v…²
# 1 Apparent Pre-contraflow                                   591.            2396119     9  
# 2 Apparent Contraflow                                       335.            1392487     8.8
# 3 Apparent Contraflow removed                                 3.54            11949    10.8

 
# Get 95% confidence interval data for crash_rates
conf_int_cyc_adj = cyc_vol_adj_num_crashes_per_year_cordon_by_time_period %>%
  filter(id != "Apparent") %>%
  group_by(crash_time_scale) %>%
  summarise(rate_025_percentile = round(quantile(cyc_vol_cordon_adj_crash_rate_per_100years_of_exposure, prob = 0.025), digit = 3),
            rate_975_percentile = round(quantile(cyc_vol_cordon_adj_crash_rate_per_100years_of_exposure, prob = 0.975), digit = 3)) 

# Create results table
cyc_vol_adj_overall_crash_rates_table = left_join(apparent_cyc_adj, conf_int_cyc_adj, by = "crash_time_scale") %>%
  mutate(analysis = "overall - adjusted for cyc vol") %>%
  select(c(8, 2:7)) %>%
  mutate(total_cyc_vol_cordon_adj_num_crashes = round(total_cyc_vol_cordon_adj_num_crashes, digits = 3))

cyc_vol_adj_overall_crash_rates_table$rate_95_CI = paste(cyc_vol_adj_overall_crash_rates_table$cyc_vol_cordon_adj_crash_rate_per_100years_of_exposure, 
                                                 "(", cyc_vol_adj_overall_crash_rates_table$rate_025_percentile, 
                                                 "-", cyc_vol_adj_overall_crash_rates_table$rate_975_percentile, ")")

cyc_vol_adj_overall_crash_rates_table %>% select((c(2:5, 9))) %>%
  write_csv("output/crash_rates/tables/cyc_vol_adj_overall_crash_rates_table_28_09_22.csv")

# id       analysis                       crash_time_scale   total_cyc_vol_cordon_adj_num_crashes total_exposure_duration_days rate_95_CI             
#   1 Apparent overall - adjusted for cyc vol Pre-contraflow                                   591.                        2396119 9 ( 8.5 - 9.5 )        
# 2 Apparent overall - adjusted for cyc vol Contraflow                                       335.                        1392487 8.8 ( 8.2 - 9.3 )      
# 3 Apparent overall - adjusted for cyc vol Contraflow removed                                 3.54                        11949 10.8 ( 3.795 - 19.905 )

saveRDS(cyc_vol_adj_num_crashes_per_year_cordon_by_time_period, 
        "data-processed/rates/cyc_vol_adj_num_crashes_per_year_cordon_by_time_period_28_09_2022.Rds")

################################################################################
#                     B) Crash rates by presence of junction                   #
################################################################################

################################ 
# 1) Raw crash rates by junction #
################################

# Get number of crashes
num_crashes_10m_junction = bootstrap_df %>% 
  group_by(id, crash_time_scale, not_within_10m_junction) %>%
  summarise(num_crashes = n())
# id            crash_time_scale   not_within_10m_juncti… num_crashes
# 1 Apparent      Pre_contraflow     FALSE                           590
# 2 Apparent      Pre_contraflow     TRUE                            198
# 3 Apparent      Contraflow         FALSE                           496
# 4 Apparent      Contraflow         TRUE                            207
# 5 Apparent      Contraflow_removed FALSE                             3
# 6 Apparent      Contraflow_removed TRUE                              4

# Join number of crashes to length of exposure and calculate rate
raw_crash_rates_by_junction = left_join(num_crashes_10m_junction, road_segment_exposure) %>%
  mutate(raw_crash_rate_by_junction_per_100years_of_exposure = round((num_crashes/(total_exposure_duration_days/365)*100), digit = 1)) %>%
  ungroup() %>%
  mutate(crash_time_scale = recode(crash_time_scale, Pre_contraflow = "Pre-contraflow",
                                   Contraflow = "Contraflow",
                                   Contraflow_removed = "Contraflow removed")) %>%
  mutate(not_within_10m_junction = case_when(not_within_10m_junction == TRUE ~ "No junction",
                                             TRUE ~ "Junction")) #create better labels

# Apparent crash rate
apparent = raw_crash_rates_by_junction %>%
  filter(id == "Apparent")

# Get 95% confidence interval data for crash_rates
conf_int = raw_crash_rates_by_junction %>%
  filter(id != "Apparent") %>%
  group_by(crash_time_scale, not_within_10m_junction) %>%
  summarise(rate_025_percentile = round(quantile(raw_crash_rate_by_junction_per_100years_of_exposure, prob = 0.025), digit = 3),
            rate_975_percentile = round(quantile(raw_crash_rate_by_junction_per_100years_of_exposure, prob = 0.975), digit = 3))

# Create results table
raw_crash_rates_by_junction_table = left_join(apparent, conf_int,
                                                    by = c("crash_time_scale", "not_within_10m_junction")) %>%
  mutate(analysis = "By junction status - raw") %>%
  select(c(9, 2:8)) %>%
  arrange(not_within_10m_junction)

raw_crash_rates_by_junction_table$rate_95_CI = paste(raw_crash_rates_by_junction_table$raw_crash_rate_by_junction_per_100years_of_exposure, 
                                                 "(", raw_crash_rates_by_junction_table$rate_025_percentile, 
                                                 "-", raw_crash_rates_by_junction_table$rate_975_percentile, ")")
raw_crash_rates_by_junction_table %>% select(c(1:5, 9)) %>%
  write_csv("output/crash_rates/tables/raw_crash_rates_by_junction_table_28_09_22.csv")

# Save df
saveRDS(raw_crash_rates_by_junction, "data-processed/rates/raw_crash_rates_by_junction_28_09_2022.Rds")


################################
# 2) Adjusted by cycling volume #
################################

# Join the cycle vol indexes

num_crashes_per_year_junction_cordon = readRDS("data-processed/num_crashes_per_year_junction_cordon_28_09_2022.Rds")

#To start with need to adjust the number of crashese tfl cordon multiplier for cycling and traffic
# Join number of crashes by year + cordon
vol_adj_num_crashes_per_junction = left_join(num_crashes_per_year_junction_cordon,
                                           tfl_cordon_cycle_traffic_index %>% select(c(1, 8:10)),
                                           by = c("accident_year" = "year")) %>%
  ungroup() %>%
  mutate(correct_cyc_vol_index = 0)


# Update the correct_index column (col 10) with the right index
## (ie put the outer_index in for crashes that were outer)
for(i in 1:nrow(vol_adj_num_crashes_per_junction)) {
  if(vol_adj_num_crashes_per_junction[i, 5] == "central") {
    vol_adj_num_crashes_per_junction[i, 10] <-vol_adj_num_crashes_per_junction[i, 7] # col 7  is the cyc central index
  }
  else if(vol_adj_num_crashes_per_junction[i, 5] == "inner") {
    vol_adj_num_crashes_per_junction[i, 10] <- vol_adj_num_crashes_per_junction[i, 8] # col 8 is the cyc inner index
  }
  else if(vol_adj_num_crashes_per_junction[i, 5] == "outer") {
    vol_adj_num_crashes_per_junction[i, 10] <- vol_adj_num_crashes_per_junction[i, 9] # col 9 is the cyc outer index
    }
}

# Drop the no longer needed index columns
vol_adj_num_crashes_per_junction = vol_adj_num_crashes_per_junction %>% select(-c(7:9))

# Calculate the number of crashes adjusted for cyc volume
vol_adj_num_crashes_per_junction = vol_adj_num_crashes_per_junction %>%
  mutate(cyc_adj_num_crashes_per_year_per_junction = raw_num_crashes/correct_cyc_vol_index)

# Calculate the total adjusted number of crashes by road segment status
cyc_adj_num_crashes_per_year_junction_by_time_period = vol_adj_num_crashes_per_junction %>%
  group_by(id, crash_time_scale, not_within_10m_junction) %>%
  summarise(total_junction_cyc_adj_num_crashes = sum(cyc_adj_num_crashes_per_year_per_junction))

# Calculate the overall adj crash rate taking into account the exposure time
cyc_adj_num_crashes_per_year_junction = left_join(cyc_adj_num_crashes_per_year_junction_by_time_period,
                                                road_segment_exposure,
                                                by = c("crash_time_scale")) %>%
  mutate(cyc_adj_crash_rate_by_junction_per_100years_of_exposure =
           round((total_junction_cyc_adj_num_crashes/(total_exposure_duration_days/365)*100), digit = 1)) %>%
  ungroup() %>%
  mutate(crash_time_scale = recode(crash_time_scale, Pre_contraflow = "Pre-contraflow",
                                   Contraflow = "Contraflow",
                                   Contraflow_removed = "Contraflow removed")) %>%
  mutate(not_within_10m_junction = case_when(not_within_10m_junction == TRUE ~ "No junction",
                                             TRUE ~ "Junction")) #create better labels
  
  
# Apparent crash rate
apparent_cyc_adj_junction = cyc_adj_num_crashes_per_year_junction %>%
  filter(id == "Apparent")
 
# Get 95% confidence interval data for crash_rates
conf_int_cyc_adj_junction = cyc_adj_num_crashes_per_year_junction %>%
  filter(id != "Apparent") %>%
  group_by(crash_time_scale, not_within_10m_junction) %>%
  summarise(rate_025_percentile = round(quantile(cyc_adj_crash_rate_by_junction_per_100years_of_exposure, prob = 0.025), digit = 3),
            rate_975_percentile = round(quantile(cyc_adj_crash_rate_by_junction_per_100years_of_exposure, prob = 0.975), digit = 3))
 
cyc_adj_junction_crash_rates_table = left_join(apparent_cyc_adj_junction, conf_int_cyc_adj_junction,
                                             by = c("crash_time_scale", "not_within_10m_junction")) %>%
  mutate(analysis = "By junction - adj cyc vol") %>%
  select(c(9, 2:8)) %>%
  mutate(total_junction_cyc_adj_num_crashes = round(total_junction_cyc_adj_num_crashes, digits = 3)) %>%
  arrange(not_within_10m_junction)

cyc_adj_junction_crash_rates_table$rate_95_CI = paste(cyc_adj_junction_crash_rates_table$cyc_adj_crash_rate_by_junction_per_100years_of_exposure, 
                                                 "(", cyc_adj_junction_crash_rates_table$rate_025_percentile, 
                                                 "-", cyc_adj_junction_crash_rates_table$rate_975_percentile, ")")
cyc_adj_junction_crash_rates_table %>% select(c(1:5, 9)) %>%
  write_csv("output/crash_rates/tables/cyc_adj_crash_rates_by_junction_table_28_09_22.csv")

# Save df
saveRDS(cyc_adj_num_crashes_per_year_junction, "data-processed/rates/cyc_adj_num_crashes_per_year_junction_28_09_2022.Rds")

################################################################################
#                 C) Crash rates by direction of pedal cycle                   #
################################################################################

# NB need to get consistency in wording - flow v direction etc
# so pedal cycle direction is with or against mv flow 

##########
# 1) Raw #
##########

levels(bootstrap_df$cyc_1_direction_when_crashed)
# [1] "With flow"                "With flow (opposite)"
# [3] "Contraflow"               "Contraflow (illegal)"
# [5] "unknown (self reported)"  "Direction not compatible"

# Get number of crashes
num_crashes_flow_cyc1 = bootstrap_df %>%
  group_by(id, crash_time_scale, cyc_1_direction_when_crashed) %>%
  summarise(num_crashes_cyc1 = n())
#   id            crash_time_scale   cyc_1_direction_when_crashed num_crashes_cyc1
# 1 Apparent      Pre_contraflow     With flow                                 124
# 2 Apparent      Pre_contraflow     With flow (opposite)                       52
# 3 Apparent      Pre_contraflow     Contraflow (illegal)                       66
# 4 Apparent      Pre_contraflow     Direction not compatible                  546
# 5 Apparent      Contraflow         With flow                                 170
# 6 Apparent      Contraflow         Contraflow                                166
# 7 Apparent      Contraflow         unknown (self reported)                     9
# 8 Apparent      Contraflow         Direction not compatible                  358
# 9 Apparent      Contraflow_removed Direction not compatible                    7

# Get number of crashes - cyc2
num_crashes_flow_cyc2 = bootstrap_df %>%
  group_by(id, crash_time_scale, cyc_2_direction_when_crashed) %>%
  summarise(num_crashes_cyc2 = n())
 
# # id            crash_time_scale   cyc_2_direction_when_crashed num_crashes
# 1 Apparent      Pre_contraflow     Contraflow (illegal)                        3
# 2 Apparent      Pre_contraflow     Direction not compatible                    2
# 3 Apparent      Pre_contraflow     NA                                        783
# 4 Apparent      Contraflow         With flow                                   1
# 5 Apparent      Contraflow         Contraflow                                  6
# 6 Apparent      Contraflow         unknown (self reported)                     1
# 7 Apparent      Contraflow         Direction not compatible                    4
# 8 Apparent      Contraflow         NA                                        691
# 9 Apparent      Contraflow_removed NA                                          7

# Join cyc1 and cyc2 num of crashes together
num_crashes_flow = left_join(num_crashes_flow_cyc1, num_crashes_flow_cyc2,
                  by = c("id", "crash_time_scale", "cyc_1_direction_when_crashed" = "cyc_2_direction_when_crashed")) %>%
  rename(cyc_direction_when_crashed = cyc_1_direction_when_crashed)
num_crashes_flow$num_crashes_cyc2[is.na(num_crashes_flow$num_crashes_cyc2)] = 0 # replace NA with 0
num_crashes_flow = num_crashes_flow %>%
  mutate(num_crashes_flow_1_2 = num_crashes_cyc1 + num_crashes_cyc2) # get total num of crashes involving each direction
#  will be higher than usual total because of the extra cyc2 direction data
 
# Join number of crashes to length of exposure and calculate rate
raw_crash_rates_by_flow = left_join(num_crashes_flow, road_segment_exposure) %>%
  mutate(raw_crash_rate_by_flow_per_100years_of_exposure = round((num_crashes_flow_1_2/(total_exposure_duration_days/365) * 100), digit = 1)) %>%
  ungroup() %>%
  mutate(crash_time_scale = recode(crash_time_scale, Pre_contraflow = "Pre-contraflow",
                                   Contraflow = "Contraflow",
                                   Contraflow_removed = "Contraflow removed")) # This sorts out these labels for vis
                                             
# This sorts out this label for viz                           
levels(raw_crash_rates_by_flow$cyc_direction_when_crashed)[levels(
  raw_crash_rates_by_flow$cyc_direction_when_crashed) == "unknown (self reported)"] <- "Unknown"

# Apparent crash rate
apparent = raw_crash_rates_by_flow %>%
  filter(id == "Apparent")

# Get 95% confidence interval data for crash_rates
conf_int_flow = raw_crash_rates_by_flow %>%
  filter(id != "Apparent") %>%
  group_by(crash_time_scale, cyc_direction_when_crashed) %>%
  summarise(rate_025_percentile = round(quantile(raw_crash_rate_by_flow_per_100years_of_exposure, prob = 0.025), digit = 3),
            rate_975_percentile = round(quantile(raw_crash_rate_by_flow_per_100years_of_exposure, prob = 0.975), digit = 3))

# Create results table
raw_crash_rates_by_flow_table = left_join(apparent, conf_int_flow,
                                                    by = c("crash_time_scale", "cyc_direction_when_crashed")) %>%
  mutate(analysis = "By pedal cycle direction - raw") %>%
  select(c(11, 2, 3, 6:10))
raw_crash_rates_by_flow_table$rate_95_CI = paste(raw_crash_rates_by_flow_table$raw_crash_rate_by_flow_per_100years_of_exposure, 
                                                 "(", raw_crash_rates_by_flow_table$rate_025_percentile, 
                                                 "-", raw_crash_rates_by_flow_table$rate_975_percentile, ")")
raw_crash_rates_by_flow_table %>% select(c(1:5,9)) %>%
  write_csv("output/crash_rates/tables/raw_crash_rates_by_flow_table_28_09_2022.csv")

# Save df
saveRDS(raw_crash_rates_by_flow, "data-processed/rates/raw_crash_rates_by_flow_28_09_2022.Rds")


#################################
# 2) Adjusted by cycling volume #
#################################

num_crashes_per_year_flow_cordon = readRDS("data-processed/num_crashes_per_year_flow_cordon_28_09_2022.Rds") # this includes cyc1&cy2
 
 
#To start with need to adjust the number of crashese tfl cordon multiplier for cycling and traffic
# Join number of crashes by year + cordon
vol_adj_num_crashes_per_flow = left_join(num_crashes_per_year_flow_cordon,
                                             tfl_cordon_cycle_traffic_index %>% select(c(1, 8:10)),
                                             by = c("accident_year" = "year")) %>%
  ungroup() %>%
  mutate(correct_cyc_vol_index = 0)     # create blank columns to put the correct index

# Update the correct_index column (col 12 for cycle)
## (ie put the outer_index in for crashes that were outer)
for(i in 1:nrow(vol_adj_num_crashes_per_flow)) {
  if(vol_adj_num_crashes_per_flow[i, 5] == "central") {
    vol_adj_num_crashes_per_flow[i, 12] <-vol_adj_num_crashes_per_flow[i, 9] # col 9  is the cyc central index
  }
  else if(vol_adj_num_crashes_per_flow[i, 5] == "inner") {
    vol_adj_num_crashes_per_flow[i, 12] <- vol_adj_num_crashes_per_flow[i, 10] # col 10 is the cyc inner index
  }
  else if(vol_adj_num_crashes_per_flow[i, 5] == "outer") {
    vol_adj_num_crashes_per_flow[i, 12] <- vol_adj_num_crashes_per_flow[i, 11] # col 11 is the cyc outer index
  }
}
 
# Drop the no longer needed index columns
vol_adj_num_crashes_per_flow = vol_adj_num_crashes_per_flow %>% select(-c(6, 7, 9:11))

# Calculate the number of crashes adjusted for cyc and mv volume and pedal cycle direction
vol_adj_num_crashes_per_flow = vol_adj_num_crashes_per_flow %>%
  mutate(cyc_adj_num_crashes_per_year_per_flow = num_crashes_flow_overall/correct_cyc_vol_index)

# Calculate the total adjusted number of crashes by road segment status
cyc_adj_num_crashes_per_year_flow_by_time_period = vol_adj_num_crashes_per_flow %>%
  group_by(id, crash_time_scale, cyc_direction_when_crashed) %>%
  summarise(total_flow_cyc_adj_num_crashes = sum(cyc_adj_num_crashes_per_year_per_flow))
 
# Calculate the overall adj crash rate taking into account the exposure time
cyc_adj_num_crashes_per_year_flow = left_join(cyc_adj_num_crashes_per_year_flow_by_time_period,
                                                  road_segment_exposure,
                                                  by = c("crash_time_scale")) %>%
  mutate(cyc_adj_crash_rate_by_flow_per_100years_of_exposure =
           round((total_flow_cyc_adj_num_crashes/(total_exposure_duration_days/365)*100), digit = 1)) %>%
  mutate(crash_time_scale = recode(crash_time_scale, Pre_contraflow = "Pre-contraflow",
                                   Contraflow = "Contraflow",
                                   Contraflow_removed = "Contraflow removed")) # This sorts out these labels for vis

# This sorts out this label for vix                           
levels(cyc_adj_num_crashes_per_year_flow$cyc_direction_when_crashed)[levels(
  cyc_adj_num_crashes_per_year_flow$cyc_direction_when_crashed) == "unknown (self reported)"] <- "Unknown"

# Apparent crash rate
apparent_cyc_adj_flow = cyc_adj_num_crashes_per_year_flow %>%
  filter(id == "Apparent")

# Get 95% confidence interval data for crash_rates
conf_int_cyc_adj_flow = cyc_adj_num_crashes_per_year_flow %>%
  filter(id != "Apparent") %>%
  group_by(crash_time_scale, cyc_direction_when_crashed) %>%
  summarise(rate_025_percentile = round(quantile(cyc_adj_crash_rate_by_flow_per_100years_of_exposure, prob = 0.025), digit = 3),
            rate_975_percentile = round(quantile(cyc_adj_crash_rate_by_flow_per_100years_of_exposure, prob = 0.975), digit = 3))

cyc_adj_flow_crash_rates_table = left_join(apparent_cyc_adj_flow, conf_int_cyc_adj_flow,
                                               by = c("crash_time_scale", "cyc_direction_when_crashed")) %>%
  mutate(analysis = "By pedal cycle direction - adj cyc vol") %>%
  select(c(9, 2:8)) %>%
  mutate(total_flow_cyc_adj_num_crashes = round(total_flow_cyc_adj_num_crashes, digits = 3)) %>%
  arrange(crash_time_scale) 

cyc_adj_flow_crash_rates_table$rate_95_CI = paste(cyc_adj_flow_crash_rates_table$cyc_adj_crash_rate_by_flow_per_100years_of_exposure, 
                                                "(", cyc_adj_flow_crash_rates_table$rate_025_percentile, 
                                                "-", cyc_adj_flow_crash_rates_table$rate_975_percentile, ")")
cyc_adj_flow_crash_rates_table %>% select((c(1:6, 10))) %>%
  write_csv("output/crash_rates/tables/cyc_adj_crash_rates_by_flow_table_28_09_22.csv")

# Save df
saveRDS(cyc_adj_num_crashes_per_year_flow, "data-processed/rates/cyc_adj_num_crashes_per_year_flow_28_09_2022.Rds")


################################################################################
#         D) Crash rates by whether one or two way to start with                #
################################################################################

################################
# 1) Raw crash rates by action #
################################

# Get number of crashes
num_crashes_action = bootstrap_df %>%
  group_by(id, crash_time_scale, sig_additional_tro_action) %>%
  summarise(num_crashes = n())
# id            crash_time_scale   sig_additional_tro_action                  num_cras…¹
# 1 Apparent      Pre_contraflow     Contraflow cycling only                           532
# 2 Apparent      Pre_contraflow     One-way street and contraflow cycling             176
# 3 Apparent      Pre_contraflow     Contraflow bus lane and contraflow cycling         80
# 4 Apparent      Contraflow         Contraflow cycling only                           457
# 5 Apparent      Contraflow         One-way street and contraflow cycling             114
# 6 Apparent      Contraflow         Contraflow bus lane and contraflow cycling        132
# 7 Apparent      Contraflow_removed Contraflow cycling only                             7

# Join number of crashes to length of exposure and calculate rate
raw_overall_crash_rates_by_action = left_join(num_crashes_action, road_segment_exposure_tro_action) %>%
  mutate(raw_overall_crash_rate_by_action_per_100years_of_exposure = round((num_crashes/(exposure_duration_days/365)*100), digit = 1)) %>%
  ungroup() %>%
  mutate(crash_time_scale = recode(crash_time_scale, Pre_contraflow = "Pre-contraflow",
                                   Contraflow = "Contraflow",
                                   Contraflow_removed = "Contraflow removed")) # need for vis labels

# Apparent crash rate
apparent = raw_overall_crash_rates_by_action %>%
  filter(id == "Apparent")
  
# Get 95% confidence interval data for crash_rates
conf_int = raw_overall_crash_rates_by_action %>%
  filter(id != "Apparent") %>%
  group_by(crash_time_scale, sig_additional_tro_action) %>%
  summarise(rate_025_percentile = round(quantile(raw_overall_crash_rate_by_action_per_100years_of_exposure, prob = 0.025), digit = 3),
            rate_975_percentile = round(quantile(raw_overall_crash_rate_by_action_per_100years_of_exposure, prob = 0.975), digit = 3))

# Create results table
raw_overall_crash_rates_by_action_table = left_join(apparent, conf_int,
                                                    by = c("crash_time_scale", "sig_additional_tro_action")) %>%
  mutate(analysis = "By action - raw") %>%
  select(c(9, 2:8)) %>%
  arrange(sig_additional_tro_action)

raw_overall_crash_rates_by_action_table$rate_95_CI = paste(raw_overall_crash_rates_by_action_table$raw_overall_crash_rate_by_action_per_100years_of_exposure, 
                                                 "(", raw_overall_crash_rates_by_action_table$rate_025_percentile, 
                                                 "-", raw_overall_crash_rates_by_action_table$rate_975_percentile, ")")
raw_overall_crash_rates_by_action_table %>% select((c(1:5, 9))) %>%
  write_csv("output/crash_rates/tables/raw_crash_rates_by_action_table_28_09_22.csv")

#save df
saveRDS(raw_overall_crash_rates_by_action, "data-processed/rates/raw_overall_crash_rates_by_action_28_09_2022.Rds")

##############################################
# 2) Crash rates adjust by cyc vol by action #
##############################################         

num_crashes_per_year_action_cordon = readRDS("data-processed/num_crashes_per_year_action_cordon_28_09_2022.Rds")

#To start with need to adjust the number of crashese tfl cordon multiplier for cycling 
# Join number of crashes by year + cordon
vol_adj_num_crashes_per_action = left_join(num_crashes_per_year_action_cordon,
                                                tfl_cordon_cycle_traffic_index %>% select(c(1, 8:10)),
                                                by = c("accident_year" = "year")) %>%
  ungroup() %>%
  mutate(correct_cyc_vol_index = 0) # create blank column

# Update the correct_multiplier column (col 10 for cycle) with the right multiplier
## (ie put the outer_multipler in for crashes that were outer)
for(i in 1:nrow(vol_adj_num_crashes_per_action)) {
  if(vol_adj_num_crashes_per_action[i, 5] == "central") {
    vol_adj_num_crashes_per_action[i, 10] <-vol_adj_num_crashes_per_action[i, 7] # col 7  is the cyc central multiplier
  }
  else if(vol_adj_num_crashes_per_action[i, 5] == "inner") {
    vol_adj_num_crashes_per_action[i, 10] <- vol_adj_num_crashes_per_action[i, 8] # col 8 is the cyc inner multiplier
  }
  else if(vol_adj_num_crashes_per_action[i, 5] == "outer") {
    vol_adj_num_crashes_per_action[i, 10] <- vol_adj_num_crashes_per_action[i, 9] # col 9 is the cyc outer multiplier
  }
}

# Drop the no longer needed multiplier columns
vol_adj_num_crashes_per_action = vol_adj_num_crashes_per_action %>% select(-c(7:9))

# Calculate the number of crashes adjusted for cyc volume
vol_adj_num_crashes_per_action = vol_adj_num_crashes_per_action %>%
  mutate(cyc_adj_num_crashes_per_year_per_action = raw_num_crashes/correct_cyc_vol_index)

# Calculate the total adjusted number of crashes by road segment status
cyc_adj_num_crashes_per_year_action_by_time_period = vol_adj_num_crashes_per_action %>%
  group_by(id, crash_time_scale, sig_additional_tro_action) %>%
  summarise(total_action_cyc_adj_num_crashes = sum(cyc_adj_num_crashes_per_year_per_action))

# Calculate the overall adj crash rate taking into account the exposure time
cyc_adj_num_crashes_per_year_action = left_join(cyc_adj_num_crashes_per_year_action_by_time_period,
                                                   road_segment_exposure_tro_action,
                                                   by = c("crash_time_scale", "sig_additional_tro_action")) %>%
  mutate(cyc_adj_crash_rate_per_100years_of_exposure =
           round((total_action_cyc_adj_num_crashes/(exposure_duration_days/365) * 100), digit = 1)) %>%
  mutate(crash_time_scale = recode(crash_time_scale, Pre_contraflow = "Pre-contraflow",
                                   Contraflow = "Contraflow",
                                   Contraflow_removed = "Contraflow removed"))

# Apparent crash rate
apparent_cyc_adj_action = cyc_adj_num_crashes_per_year_action %>%
  filter(id == "Apparent")

# Get 95% confidence interval data for crash_rates
conf_int_cyc_adj_action = cyc_adj_num_crashes_per_year_action %>%
  filter(id != "Apparent") %>%
  group_by(crash_time_scale, sig_additional_tro_action) %>%
  summarise(rate_025_percentile = round(quantile(cyc_adj_crash_rate_per_100years_of_exposure, prob = 0.025), digit = 1),
            rate_975_percentile = round(quantile(cyc_adj_crash_rate_per_100years_of_exposure, prob = 0.975), digit = 1))

# Create results table
cyc_adj_action_crash_rates_table = left_join(apparent_cyc_adj_action, conf_int_cyc_adj_action,
                                                by = c("crash_time_scale", "sig_additional_tro_action")) %>%
  mutate(analysis = "By action - adj cyc vol") %>%
  select(c(9, 2:8)) %>%
  mutate(total_action_cyc_adj_num_crashes = round(total_action_cyc_adj_num_crashes, digits = 3)) %>%
  arrange(sig_additional_tro_action)

cyc_adj_action_crash_rates_table$rate_95_CI = paste(cyc_adj_action_crash_rates_table$cyc_adj_crash_rate_per_100years_of_exposure, 
                                                "(", cyc_adj_action_crash_rates_table$rate_025_percentile, 
                                                "-", cyc_adj_action_crash_rates_table$rate_975_percentile, ")")
cyc_adj_action_crash_rates_table %>% select((c(2:6, 10))) %>%
  write_csv("output/crash_rates/tables/cyc_adj_crash_rates_by_action_table_28_09_22.csv")

# Save df
saveRDS(cyc_adj_num_crashes_per_year_action, "data-processed/rates/cyc_adj_num_crashes_per_year_action_28_09_2022.Rds")


################################################################################
# E) Crash rates by whether one or two way to start with AND INFRASTRUCTURE    #
################################################################################

################################
# 1) Raw crash rates by action #
################################

# Get number of crashes
num_crashes_action_infra = bootstrap_df %>%
  group_by(id, crash_time_scale, sig_additional_tro_action, infra) %>%
  summarise(num_crashes = n())
# id            crash_time_scale   sig_additional_tro_action                  infra                              num_crashes
# 1 Apparent      Pre_contraflow     Contraflow cycling only                    No additional action                       346
# 2 Apparent      Pre_contraflow     Contraflow cycling only                    Cycling in contraflow bus lane              64
# 3 Apparent      Pre_contraflow     Contraflow cycling only                    Cycle lane                                  83
# 4 Apparent      Pre_contraflow     Contraflow cycling only                    Cycle lane with some segregation            26
# 5 Apparent      Pre_contraflow     Contraflow cycling only                    Cycle track and cycling on footway          13
# 6 Apparent      Pre_contraflow     One-way street and contraflow cycling      No additional action                        84
# 7 Apparent      Pre_contraflow     One-way street and contraflow cycling      Cycle lane                                  41
# 8 Apparent      Pre_contraflow     One-way street and contraflow cycling      Cycle lane with some segregation            28
# 9 Apparent      Pre_contraflow     One-way street and contraflow cycling      Cycling on footway                          17
# 10 Apparent      Pre_contraflow     One-way street and contraflow cycling      Cycle track and lane                         6
# 11 Apparent      Pre_contraflow     Contraflow bus lane and contraflow cycling Cycling in contraflow bus lane              80
# 12 Apparent      Contraflow         Contraflow cycling only                    No additional action                       204
# 13 Apparent      Contraflow         Contraflow cycling only                    Cycling in contraflow bus lane              42
# 14 Apparent      Contraflow         Contraflow cycling only                    Cycle lane                                  96
# 15 Apparent      Contraflow         Contraflow cycling only                    Cycle lane with some segregation           108
# 16 Apparent      Contraflow         Contraflow cycling only                    Cycle track and cycling on footway           7
# 17 Apparent      Contraflow         One-way street and contraflow cycling      No additional action                        40
# 18 Apparent      Contraflow         One-way street and contraflow cycling      Cycle lane                                  55
# 19 Apparent      Contraflow         One-way street and contraflow cycling      Cycle lane with some segregation            10
# 20 Apparent      Contraflow         One-way street and contraflow cycling      Cycle track and cycling on footway           1
# 21 Apparent      Contraflow         One-way street and contraflow cycling      Cycling on footway                           3
# 22 Apparent      Contraflow         One-way street and contraflow cycling      Cycle track and lane                         5
# 23 Apparent      Contraflow         Contraflow bus lane and contraflow cycling Cycling in contraflow bus lane             132
# 24 Apparent      Contraflow_removed Contraflow cycling only                    Cycle lane                                   7

# Join number of crashes to length of exposure and calculate rate
raw_overall_crash_rates_by_action_infra = left_join(num_crashes_action_infra, road_segment_exposure_tro_action) %>%
  mutate(raw_overall_crash_rate_by_action_infra_per_100years_of_exposure = round((num_crashes/(exposure_duration_days/365)*100), digit = 1)) %>%
  ungroup() %>%
  mutate(crash_time_scale = recode(crash_time_scale, Pre_contraflow = "Pre-contraflow",
                                   Contraflow = "Contraflow",
                                   Contraflow_removed = "Contraflow removed")) # need for vis labels

# Apparent crash rate
apparent = raw_overall_crash_rates_by_action_infra %>%
  filter(id == "Apparent")

# Get 95% confidence interval data for crash_rates
conf_int = raw_overall_crash_rates_by_action_infra %>%
  filter(id != "Apparent") %>%
  group_by(crash_time_scale, sig_additional_tro_action, infra) %>%
  summarise(rate_025_percentile = round(quantile(raw_overall_crash_rate_by_action_infra_per_100years_of_exposure, prob = 0.025), digit = 3),
            rate_975_percentile = round(quantile(raw_overall_crash_rate_by_action_infra_per_100years_of_exposure, prob = 0.975), digit = 3))

# Create results table
raw_overall_crash_rates_by_action_infra_table = left_join(apparent, conf_int,
                                                    by = c("crash_time_scale", "sig_additional_tro_action", "infra")) %>%
  mutate(analysis = "By action and infra - raw") %>%
  select(c(10, 2:9)) %>%
  arrange(sig_additional_tro_action)

raw_overall_crash_rates_by_action_infra_table$rate_95_CI = paste(raw_overall_crash_rates_by_action_infra_table$raw_overall_crash_rate_by_action_infra_per_100years_of_exposure, 
                                                           "(", raw_overall_crash_rates_by_action_infra_table$rate_025_percentile, 
                                                           "-", raw_overall_crash_rates_by_action_infra_table$rate_975_percentile, ")")
raw_overall_crash_rates_by_action_infra_table %>% select((c(1:6, 10))) %>%
  write_csv("output/crash_rates/tables/raw_crash_rates_by_action_infra_table_28_09_22.csv")

#save df
saveRDS(raw_overall_crash_rates_by_action, "data-processed/rates/raw_overall_crash_rates_by_action_infra_28_09_2022.Rds")

# ###################################################################
# # 2) Crash rates adjust by cyc vol by action and additional infra #
# ###################################################################         
# 
num_crashes_per_year_infra_cordon = readRDS("data-processed/num_crashes_per_year_infra_cordon_28_09_2022.Rds")

#To start with need to adjust the number of crashese tfl cordon multiplier for cycling
# Join number of crashes by year + cordon
vol_adj_num_crashes_per_infra = left_join(num_crashes_per_year_infra_cordon,
                                           tfl_cordon_cycle_traffic_index %>% select(c(1, 8:10)),
                                           by = c("accident_year" = "year")) %>%
  ungroup() %>%
  mutate(correct_cyc_vol_index = 0) # create blank column

# Update the correct_multiplier column (col 11 for cycle) with the right multiplier
## (ie put the outer_multipler in for crashes that were outer)
for(i in 1:nrow(vol_adj_num_crashes_per_infra)) {
  if(vol_adj_num_crashes_per_infra[i, 6] == "central") {
    vol_adj_num_crashes_per_infra[i, 11] <-vol_adj_num_crashes_per_infra[i, 8] # col 8  is the cyc central multiplier
  }
  else if(vol_adj_num_crashes_per_infra[i, 6] == "inner") {
    vol_adj_num_crashes_per_infra[i, 11] <- vol_adj_num_crashes_per_infra[i, 9] # col 9 is the cyc inner multiplier
  }
  else if(vol_adj_num_crashes_per_infra[i, 6] == "outer") {
    vol_adj_num_crashes_per_infra[i, 11] <- vol_adj_num_crashes_per_infra[i, 10] # col 10 is the cyc outer multiplier
  }
}

# Drop the no longer needed multiplier columns
vol_adj_num_crashes_per_infra = vol_adj_num_crashes_per_infra %>% select(-c(8:10))

# Calculate the number of crashes adjusted for cyc volume
vol_adj_num_crashes_per_infra = vol_adj_num_crashes_per_infra %>%
  mutate(cyc_adj_num_crashes_per_year_per_infra = raw_num_crashes/correct_cyc_vol_index)
 
# Calculate the total adjusted number of crashes by road segment status
cyc_adj_num_crashes_per_year_infra_by_time_period = vol_adj_num_crashes_per_infra %>%
  group_by(id, crash_time_scale, sig_additional_tro_action, infra) %>%
  summarise(total_infra_cyc_adj_num_crashes = sum(cyc_adj_num_crashes_per_year_per_infra))

# Calculate the overall adj crash rate taking into account the exposure time
cyc_adj_num_crashes_per_year_infra = left_join(cyc_adj_num_crashes_per_year_infra_by_time_period,
                                                road_segment_exposure_tro_action,
                                                by = c("crash_time_scale", "sig_additional_tro_action")) %>%
  mutate(cyc_adj_crash_rate_per_100years_of_exposure_to_tro_action =
           round((total_infra_cyc_adj_num_crashes/(exposure_duration_days/365) * 100), digit = 1)) %>%
  mutate(crash_time_scale = recode(crash_time_scale, Pre_contraflow = "Pre-contraflow",
                                   Contraflow = "Contraflow",
                                   Contraflow_removed = "Contraflow removed"))

# Apparent crash rate
apparent_cyc_adj_infra = cyc_adj_num_crashes_per_year_infra %>%
  filter(id == "Apparent")

# Get 95% confidence interval data for crash_rates
conf_int_cyc_adj_infra = cyc_adj_num_crashes_per_year_infra %>%
  filter(id != "Apparent") %>%
  group_by(crash_time_scale, sig_additional_tro_action, infra) %>%
  summarise(rate_025_percentile = round(quantile(cyc_adj_crash_rate_per_100years_of_exposure_to_tro_action, prob = 0.025), digit = 1),
            rate_975_percentile = round(quantile(cyc_adj_crash_rate_per_100years_of_exposure_to_tro_action, prob = 0.975), digit = 1))

# Create results table
cyc_adj_infra_crash_rates_table = left_join(apparent_cyc_adj_infra, conf_int_cyc_adj_infra,
                                             by = c("crash_time_scale", "sig_additional_tro_action", "infra")) %>%
  mutate(analysis = "By action and infra - adj cyc vol") %>%
  select(c(10, 2:9)) %>%
  mutate(total_infra_cyc_adj_num_crashes = round(total_infra_cyc_adj_num_crashes, digits = 3)) %>%
  arrange(sig_additional_tro_action)

cyc_adj_infra_crash_rates_table$rate_95_CI = paste(cyc_adj_infra_crash_rates_table$cyc_adj_crash_rate_per_100years_of_exposure_to_tro_action,
                                                    "(", cyc_adj_infra_crash_rates_table$rate_025_percentile,
                                                    "-", cyc_adj_infra_crash_rates_table$rate_975_percentile, ")")
cyc_adj_infra_crash_rates_table %>% select((c(2:7, 10, 11))) %>%
  write_csv("output/crash_rates/tables/cyc_adj_crash_rates_by_infra_table_28_09_22.csv")

# Save df
saveRDS(cyc_adj_num_crashes_per_year_infra, "data-processed/rates/cyc_adj_num_crashes_per_year_infra_28_09_2022.Rds")












