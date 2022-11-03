################################################################################
#                           Calculate casualty rates                           #
#                                                                              #
# This code gets the S19 casualty severity adjustment data for the years 2004- #
# 2019.                                                                        #
# As per https://www.gov.uk/government/publications/guide-to-severity-         #
# adjustments-for-reported-road-casualty-statistics/guide-to-severity-         #
# adjustments-for-reported-road-casualties-great-britain                       #
# The severity adjustment probabilities are attached to the correct casualty   #
# and then the dataframe is bootstrapped.                                      # 
#                                                                              #
# This code then calculates the exposure for the casualties before calculating:#
# - unadjusted casualty rates                                                  #
# - casualty rates adjusted for severity                                       #  
# - casualty rates adjusted for severity and change in cycling volume          #
#                                                                              #
# This code was rerun using the AAP crash dataset (this had SBC and self-      #
# reported crashes removed) Date rerun 29/09/2022.                             #
################################################################################



# Load librarys
library(tidyverse)
library(rsample)
library(lubridate)
library(dplyr)
library(sf)



# Pull out adjustment factors that match our crashes - would expect only cashes from Nov 2015 onwards would have one
# then will need to ensure asign to correct casualty where >1 crash

################################################################################
#  Create casualty bootstrap dataframe with correct adjustment probability for #
#   the severity of their injury for years 2005-2019 (As per S19 guidance)     #
################################################################################
                            

# Load datasets
S19_adjust = read_delim(file = "/home/bananafan/Downloads/1979/cas_adjustment_lookup_2019.csv", 
                         col_names = TRUE, delim = ",") # casualty adjustment data
pre_AAP_casualties_crashes = readRDS("data-processed/stats19/data_pre_AAP/casualty_df_joined_to_crashes_16_06_2022.Rds")
crashes_pc_and_road_seg_direction = readRDS("data-processed/stats19/pedal_cycle_crashes_with_casualties_&_vehicles_&_junctions&cycle_direction_28_09_22.RDs")
crashes_by_cordon = readRDS("data-processed/crashes_by_cordon_28_09_2022.Rds")

# Limit casualties to those involved in a AAP crash
APP_casualties_crashes = pre_AAP_casualties_crashes %>%
  filter(accident_index %in% crashes_pc_and_road_seg_direction$accident_index)
# 1498 unique accident indices, 1567 casualties.  

# Examine dfs
names(APP_casualties_crashes)
APP_casualties_crashes %>%
  group_by(casualty_type_derived) %>%
  summarise(count = n())
# casualty_type_derived count
# <fct>                 <int>
# 1 Car                      10
# 2 Cyclist                1423
# 3 Motorcyclist             19
# 4 Pedestrian              109
# 5 Other                     6

APP_casualties_crashes %>%
  group_by(casualty_severity) %>%
  summarise(count = n())
# casualty_severity count
# 1 Fatal                10
# 2 Serious             189
# 3 Slight             1368

# calculate number of casualties per crash
crashes_pc_and_road_seg_direction %>%
  st_drop_geometry() %>%
  group_by(number_of_casualties) %>%
  summarise(count = n()) %>%
  mutate(percent = round(count/1498 *100, digits = 1))
# number_of_casualties count percent
# 1                    1  1438    96  
# 2                    2    57     3.8
# 3                    3     1     0.1
# 4                    4     1     0.1
# 5                    8     1     0.1

names(S19_adjust)
# [1] "accident_index"     "Vehicle_Reference"  "Casualty_Reference"
# [4] "Adjusted_Serious"   "Adjusted_Slight"    "Injury_Based" 
nrow(S19_adjust) # 3314737

# each casualty in a crash has a different adjusted_serious and adjusted_slight value
cyc_casualties_crashes = APP_casualties_crashes %>%
  filter(casualty_type_derived == "Cyclist") # n = 1423
joined_df = left_join(cyc_casualties_crashes %>% select(c(1, 2, 4, 24, 5, 19, 10, 45, 62, 63)), 
                      S19_adjust, by = c("accident_index", "vehicle_reference" = "Vehicle_Reference", 
                                         "casualty_reference" = "Casualty_Reference")) %>%
  rename(accident_year = accident_year.x)

joined_df %>% group_by(accident_year, Adjusted_Serious) %>%
  summarise(count = n()) %>% 
  arrange(accident_year) # so no adjusted values before 2004. 
#NB S19 guidance says to only use adjustment figures from 2005 onwards

# Need to ensure that the correct adjustment probablity is present
# (currently both the serious and slight are there so need to keep the one which matches the casualty severity)
joined_df = joined_df %>%
  mutate(correct_adj_probability = 0)  %>% # add column for this
  filter(accident_year >= 2005) # drop any obs before 2005 as Dft says not to use them
# now n = 1012

# Update the correct_adjust column (col 14) with the right adjustment probability
## (ie put the outer_index in for crashes that were outer)
for(i in 1:nrow(joined_df)) {
  if(joined_df[i, 7] == "Slight") {
    joined_df[i, 14] <-joined_df[i, 12] # col 12  is the adj slight prob
  }
  else if(joined_df[i, 7] == "Serious") {
    joined_df[i, 14] <- joined_df[i, 11] # col 11 is the adj serious prob
  }
  else if(joined_df[i, 7] == "Fatal") {
    joined_df[i, 14] <- 1 # Fatal counts as 1 probability wise
  }
}

# drop no longer required columnes
joined_df = joined_df %>%
  select(-c(11,12)) # drop the adjustment factor columns

# Calculate raw totals
joined_df %>% group_by(casualty_severity) %>%
  summarise(count = n())
# casualty_severity count
# 1 Fatal                 7
# 2 Serious             103
# 3 Slight              902

# Calculate totals using adjustment factors 
joined_df %>% group_by(casualty_severity) %>%
  summarise(count = sum(correct_adj_probability))
# casualty_severity count
# <chr>             <dbl>
# 1 Fatal                7 
# 2 Serious            103 
# 3 Slight             836

# Fiddle with crash df to so can add additional variables to casualties  
crashes_direction = crashes_pc_and_road_seg_direction %>%
  select(c(1, 2, 27, 91, 92, 102:104)) %>%
  filter(accident_year >= 2005)  %>% # n = 1073
  st_drop_geometry()


dir_joined = left_join(joined_df, crashes_direction)
cols_keep = c("accident_index", "accident_year", "crash_time_scale", "sig_additional_tro_action",
              "not_within_10m_junction", 
              "cyc_1_direction_when_crashed", "cyc_2_direction_when_crashed",
              "sig_additional_tro_action", "correct_adj_probability", "casualty_severity",
              "casualty_type_derived")
to_boot = dir_joined %>%
  select(all_of(cols_keep))
 
bootstrap_cas_df = bootstraps(to_boot, times=1000, apparent=TRUE) %>%
  mutate(boot = map(splits, analysis)) %>%
  unnest(boot) %>%
  select(-c(splits)) # drop the splits column that contains the nested data
 
# Attach cordon label to crashes
crashes_by_cordon = crashes_by_cordon %>% st_drop_geometry()  #drop geom making it quicker to join
bootstrap_cas_df = left_join(bootstrap_cas_df, crashes_by_cordon, by = "accident_index")

# Save bootstrap df
saveRDS(bootstrap_cas_df, "data-processed/bootstrap_cas_df_29_09_2022.Rds")


################################################################################
#           Recalculate time duration of exposure for casualty rates           #
################################################################################

# Need to recalculate time duration of exposure as only have severity 
# adjustments from 2005

# load tro df 
unique_tro_df = readRDS(file = "data-processed/unique_tro_df_durations_using_start_date.Rds")
cas_unique_tro_df = unique_tro_df %>% filter(!is.na(contraflow_start_date)) %>% 
  mutate(sig_additional_tro_action = case_when(introduces_one_way_street == TRUE ~ "One-way street and contraflow cycling", 
                                               introduces_contraflow_bus_lane == TRUE ~ "Contraflow bus lane and contraflow cycling",
                                               TRUE ~ "Contraflow cycling only")) %>%
  select(c(1, 18, 20, 41))

  # n = 473

# Create columns for duration pre, during and post contraflow existence
time_min = dmy("01-01-2005")
time_max = dmy("01-01-2020")
cas_unique_tro_df = cas_unique_tro_df %>%
  mutate(pre_contraflow_duration = contraflow_start_date - time_min) %>% # NB those that started on 31/12 now give -1 days (sort later)
  mutate(contraflow_time_stopped = contraflow_stop_date - contraflow_start_date) %>% # for those where contraflow was removed
  mutate(contraflow_time_didntstop = time_max - contraflow_start_date) # for those where contraflow wasnt removed (also creates values for those where contraflow removed)

cas_unique_tro_df$contraflow_time_didntstop = replace(cas_unique_tro_df$contraflow_time_didntstop,
                                                  which(!is.na(cas_unique_tro_df$contraflow_stop_date)),
                                                  values = NA) # Replace these erroneous values with NA

cas_unique_tro_df = cas_unique_tro_df %>%
  mutate(contraflow_duration = coalesce(contraflow_time_stopped, contraflow_time_didntstop)) %>% # create single column with contraflow duration
  mutate(post_contraflow_duration = time_max - contraflow_stop_date) %>% # create column with post-contraflow duration where they are removed
  select(-c(contraflow_time_stopped, contraflow_time_didntstop))

cas_unique_tro_df$post_contraflow_duration[is.na(cas_unique_tro_df$post_contraflow_duration)] = 0  # Replace NA with 0 days

# Check total adds up to what I think
post = cas_unique_tro_df %>% rowwise() %>%
  mutate(post_total = sum(c_across(pre_contraflow_duration:post_contraflow_duration))) %>%
  select(c(unique_contraflow_ID, post_total)) %>% st_drop_geometry()
# those with a contraflow_start_date now have a total of 5478

total_sum = as.numeric(sum(cas_unique_tro_df$pre_contraflow_duration) +
                         sum(cas_unique_tro_df$contraflow_duration) +
                         sum(cas_unique_tro_df$post_contraflow_duration, na.rm = TRUE))
# # 2591094 which is 473 * 5478

# Create df
cas_road_segment_exposure = data.frame(crash_time_scale = factor(c("Pre_contraflow", "Contraflow", "Contraflow_removed"),
                                                             levels = c("Pre_contraflow", "Contraflow", "Contraflow_removed")),
                                   cas_exposure_duration_days = c(as.numeric(sum(cas_unique_tro_df$pre_contraflow_duration)), 
                                                                               as.numeric(sum(cas_unique_tro_df$contraflow_duration)),
                                                                                          as.numeric(sum(cas_unique_tro_df$post_contraflow_duration, na.rm = TRUE))))

# crash_time_scale cas_exposure_duration_days
# 1     Pre_contraflow                    1186657
# 2         Contraflow                    1392488
# 3 Contraflow_removed                      11949

# Save dataframe
saveRDS(cas_road_segment_exposure, "data-processed/cas_road_segment_exposure_29_09_2022.Rds")

# 2) Get time duration of road segments by status and action
cas_tro_action_durations = cas_unique_tro_df %>% group_by(sig_additional_tro_action) %>%
  summarise(pre_contraflow_duration = sum(pre_contraflow_duration),
            contraflow_duration = sum(contraflow_duration),
            post_contraflow_duration = sum(post_contraflow_duration)) %>%
  st_drop_geometry()

# Pivot table to get into format for joining to num_crashes for crash rates
pivot_cas_tro_action_durations = cas_tro_action_durations %>%
  pivot_longer(cols = pre_contraflow_duration:post_contraflow_duration, names_to = "crash_time_scale", 
               values_to = "cas_exposure_duration_days") %>%
  mutate(crash_time_scale = factor(crash_time_scale, levels = c("pre_contraflow_duration",
                                                                "contraflow_duration", 
                                                                "post_contraflow_duration"), 
                                   labels = c("Pre_contraflow", "Contraflow", "Contraflow_removed")))

# Save dataframe
saveRDS(pivot_cas_tro_action_durations, "data-processed/cas_road_segment_exposure_tro_action_29_09_2022.Rds")  







##################################################################################################################################
#                            Calculate casualty rates                                                                            #
#                                                                                                                                #

#                                                                      number of precontraflow cyclist casualties
#  Pre contraflow overall cyclist casualty rate =  ---------------------------------------------------------------------------
#                                                  total amount of time precontraflow of all contraflows that we know durations




################################################################################
#           A) Overall pedal cyclist casualty rates                            #
################################################################################

# Load dataframes
bootstrap_cas_df = readRDS("data-processed/bootstrap_cas_df_29_09_2022.Rds")
cas_road_segment_exposure = readRDS("data-processed/cas_road_segment_exposure_29_09_2022.Rds")



##########
# 1) Raw #
##########
num_crashes_raw = bootstrap_cas_df %>%
  group_by(id, crash_time_scale, casualty_severity) %>%
  summarise(num_crashes = n())
# id              crash_time_scale   casualty_severity num_crashes
# 1 Apparent      Pre_contraflow     Fatal                       4
# 2 Apparent      Pre_contraflow     Serious                    30
# 3 Apparent      Pre_contraflow     Slight                    364
# 4 Apparent      Contraflow         Fatal                       3
# 5 Apparent      Contraflow         Serious                    72
# 6 Apparent      Contraflow         Slight                    535
# 7 Apparent      Contraflow_removed Serious                     1
# 8 Apparent      Contraflow_removed Slight                      3


# Join number of crashes to length of exposure and calculate rate
raw_overall_cas_rates = left_join(num_crashes_raw, cas_road_segment_exposure) %>%
  mutate(raw_overall_cas_rate_per_100years_of_exposure = round((num_crashes/(cas_exposure_duration_days/365) * 100), digit = 1)) %>%
  ungroup()

# relabel crash time scale as cant seem to do it in ggplot when using ggdist
levels(raw_overall_cas_rates$crash_time_scale)
raw_overall_cas_rates$crash_time_scale = recode_factor(raw_overall_cas_rates$crash_time_scale,
                                                         Pre_contraflow = "Pre-contraflow",
                                                         Contraflow = "Contraflow",
                                                         Contraflow_removed = "Contraflow removed")

# Apparent crash rate
apparent_raw = raw_overall_cas_rates %>%
  filter(id == "Apparent")

# Get 95% confidence interval data for crash_rates
conf_int_raw = raw_overall_cas_rates %>%
  filter(id != "Apparent") %>%
  group_by(crash_time_scale, casualty_severity) %>%
  summarise(rate_025_percentile = round(quantile(raw_overall_cas_rate_per_100years_of_exposure, prob = 0.025), digit = 1),
            rate_975_percentile = round(quantile(raw_overall_cas_rate_per_100years_of_exposure, prob = 0.975), digit = 1))

# Create results table
raw_overall_cas_rates_table = left_join(apparent_raw, conf_int_raw, by = c("crash_time_scale", "casualty_severity")) %>%
  mutate(analysis = "raw overall cas") %>%
  select(c(9, 2:8))
raw_overall_cas_rates_table$rate_95_CI = paste(raw_overall_cas_rates_table$raw_overall_cas_rate_per_100years_of_exposure,
                                                 "(", raw_overall_cas_rates_table$rate_025_percentile,
                                                 "-", raw_overall_cas_rates_table$rate_975_percentile, ")")
raw_overall_cas_rates_table %>% select((c(1:5, 9))) %>%
  write_csv("output/cas_rates/tables/raw_overall_cas_rates_table_29_09_22.csv")

# Save df for vis
saveRDS(raw_overall_cas_rates, "data-processed/rates/raw_overall_cas_rates_29_09_2022.Rds")

############################
# 2) Adjusted for severity #
############################

# Get number of crashes
num_cas_adj_sev = bootstrap_cas_df %>%
  group_by(id, crash_time_scale, casualty_severity) %>%
  summarise(sev_adj_num_cas = sum(correct_adj_probability))
 
#   id            crash_time_scale   casualty_severity  count
# 1 Apparent      Pre_contraflow     Fatal                        4   
# 2 Apparent      Pre_contraflow     Serious                     30   
# 3 Apparent      Pre_contraflow     Slight                     332.  
# 4 Apparent      Contraflow         Fatal                        3   
# 5 Apparent      Contraflow         Serious                     72   
# 6 Apparent      Contraflow         Slight                     501.  
# 7 Apparent      Contraflow_removed Serious                      1   
# 8 Apparent      Contraflow_removed Slight                       2.75


# Join number of crashes to length of exposure and calculate rate
sev_adj_overall_cas_rates = left_join(num_cas_adj_sev, cas_road_segment_exposure) %>%
  mutate(sev_adj_overall_cas_rate_per_100years_of_exposure = round((sev_adj_num_cas/(cas_exposure_duration_days/365)*100), digit = 1)) %>%
  ungroup()

# relabel crash time scale as cant seem to do it in ggplot when using ggdist
levels(sev_adj_overall_cas_rates)
sev_adj_overall_cas_rates$crash_time_scale = recode_factor(sev_adj_overall_cas_rates$crash_time_scale,
                                                       Pre_contraflow = "Pre-contraflow",
                                                       Contraflow = "Contraflow",
                                                       Contraflow_removed = "Contraflow removed")

# Apparent crash rate
apparent_sev_adj = sev_adj_overall_cas_rates %>%
  filter(id == "Apparent")

# Get 95% confidence interval data for crash_rates
conf_int_sev_adj = sev_adj_overall_cas_rates %>%
  filter(id != "Apparent") %>%
  group_by(crash_time_scale, casualty_severity) %>%
  summarise(rate_025_percentile = round(quantile(sev_adj_overall_cas_rate_per_100years_of_exposure, prob = 0.025), digit = 1),
            rate_975_percentile = round(quantile(sev_adj_overall_cas_rate_per_100years_of_exposure, prob = 0.975), digit = 1))

# Create results table
sev_adj_overall_cas_rates_table = left_join(apparent_sev_adj, conf_int_sev_adj, by = c("crash_time_scale", "casualty_severity")) %>%
  mutate(analysis = "sev adj overall cas",
         sev_adj_num_cas = round(sev_adj_num_cas, digits = 0)) %>%
  select(c(9, 2:8))
sev_adj_overall_cas_rates_table$rate_95_CI = paste(sev_adj_overall_cas_rates_table$sev_adj_overall_cas_rate_per_100years_of_exposure,
                                               "(", sev_adj_overall_cas_rates_table$rate_025_percentile,
                                               "-", sev_adj_overall_cas_rates_table$rate_975_percentile, ")")
sev_adj_overall_cas_rates_table %>% select((c(1:5, 9))) %>%
  write_csv("output/cas_rates/tables/sev_adj_overall_cas_rates_table_29_09_22.csv")

# Save df for vis
saveRDS(sev_adj_overall_cas_rates, "data-processed/rates/sev_adj_overall_cas_rates_29_09_2022.Rds")



##################################################################
# 3) Adjusted for casualty severity and change in cycling volume #
##################################################################

# load cycle cordon traffic index 
tfl_cordon_cycle_traffic_index = readRDS(file = "data-processed/cycle_volume/tfl_cordon_cycle_traffic_index.Rds")

# Create dataframe of number of casualties per year by crash time scale, cordon location and casualty severity from bootstrap
adj_sev_num_cas_per_year_cordon = bootstrap_cas_df %>% 
  group_by(id, accident_year, crash_time_scale, cordon_count_location, casualty_severity) %>%
  summarise(sev_adj_num_cas = sum(correct_adj_probability))

# Adjust the number of casualties by the change in cycling volume
# a) Join number of casualties by year + cordon
vol_adj_sev_adj_num_cas_per_year_cordon = left_join(adj_sev_num_cas_per_year_cordon,
                                                tfl_cordon_cycle_traffic_index %>% select(c(1, 8:10)),
                                                by = c("accident_year" = "year")) %>%
  ungroup() %>%
  mutate(correct_cyc_vol_index = 0) # create blank columns to put the correct multiplier into
 
# Update the index column (col 10) with the right multiplier
## (ie put the outer_multipler in for casualties from crashes that were outer)
for(i in 1:nrow(vol_adj_sev_adj_num_cas_per_year_cordon)) {
  if(vol_adj_sev_adj_num_cas_per_year_cordon[i, 4] == "central") {
    vol_adj_sev_adj_num_cas_per_year_cordon[i, 10] <- vol_adj_sev_adj_num_cas_per_year_cordon[i, 7] # col 7  is the cyc central index
  }
  else if(vol_adj_sev_adj_num_cas_per_year_cordon[i, 4] == "inner") {
    vol_adj_sev_adj_num_cas_per_year_cordon[i, 10] <- vol_adj_sev_adj_num_cas_per_year_cordon[i, 8] # col 8 is the cyc inner index
  }
  else if(vol_adj_sev_adj_num_cas_per_year_cordon[i, 4] == "outer") {
    vol_adj_sev_adj_num_cas_per_year_cordon[i, 10] <- vol_adj_sev_adj_num_cas_per_year_cordon[i, 9] # col 9 is the cyc outer index
  }
}

# Calculate the volume adjusted number of casualties 
vol_adj_sev_adj_num_cas_per_year_cordon = vol_adj_sev_adj_num_cas_per_year_cordon %>%
  select(-c(7:9)) %>% # Drop the no longer needed index columns
  mutate(cyc_vol_cordon_sev_adj_num_cas_per_year = sev_adj_num_cas/correct_cyc_vol_index)# sev adj numbers adjusted by change in cycling vol

# Calculate the total adjusted (vol and severity) number of cas by road segment status
cyc_vol_sev_adj_num_cas_per_year_cordon_by_time_period = vol_adj_sev_adj_num_cas_per_year_cordon %>%
  group_by(id, crash_time_scale, casualty_severity) %>%
  summarise(total_cyc_vol_cordon_sev_adj_num_cas = sum(cyc_vol_cordon_sev_adj_num_cas_per_year))

# Calculate the overall adj crash rate taking into account the exposure time
cyc_vol_sev_adj_num_cas_per_year_cordon_by_time_period = left_join(cyc_vol_sev_adj_num_cas_per_year_cordon_by_time_period ,
                                                                   cas_road_segment_exposure) %>%
  mutate(cyc_vol_cordon_sev_adj_cas_rate_per_100years_of_exposure =
           round((total_cyc_vol_cordon_sev_adj_num_cas/(cas_exposure_duration_days/365) * 100), digit = 1)) %>%
  mutate(crash_time_scale = recode(crash_time_scale, Pre_contraflow = "Pre-contraflow",
                                   Contraflow = "Contraflow",
                                   Contraflow_removed = "Contraflow removed"))

# # Apparent casualty rate
apparent_cyc_vol_sev_adj = cyc_vol_sev_adj_num_cas_per_year_cordon_by_time_period %>%
  filter(id == "Apparent")

# Get 95% confidence interval data for crash_rates
conf_int_cyc_vol_sev_adj = cyc_vol_sev_adj_num_cas_per_year_cordon_by_time_period %>%
  filter(id != "Apparent") %>%
  group_by(crash_time_scale, casualty_severity) %>%
  summarise(rate_025_percentile = round(quantile(cyc_vol_cordon_sev_adj_cas_rate_per_100years_of_exposure, prob = 0.025), digit = 1),
            rate_975_percentile = round(quantile(cyc_vol_cordon_sev_adj_cas_rate_per_100years_of_exposure, prob = 0.975), digit = 1))

# Create results table
cyc_vol_sev_adj_overall_cas_rates_table = left_join(apparent_cyc_vol_sev_adj, conf_int_cyc_vol_sev_adj, by = c("crash_time_scale", "casualty_severity")) %>%
  mutate(analysis = "overall - adjusted for cyc & sev vol") %>%
  select(c(9, 2:8)) %>%
  mutate(total_cyc_vol_cordon_sev_adj_num_cas = round(total_cyc_vol_cordon_sev_adj_num_cas, digits = 3))
 
cyc_vol_sev_adj_overall_cas_rates_table$rate_95_CI = paste(cyc_vol_sev_adj_overall_cas_rates_table$cyc_vol_cordon_sev_adj_cas_rate_per_100years_of_exposure,
                                                         "(", cyc_vol_sev_adj_overall_cas_rates_table$rate_025_percentile,
                                                         "-", cyc_vol_sev_adj_overall_cas_rates_table$rate_975_percentile, ")")

cyc_vol_sev_adj_overall_cas_rates_table %>% select((c(2:6, 10))) %>%
  write_csv("output/cas_rates/tables/cyc_vol_sev_adj_overall_cas_rates_table_29_09_22.csv")

# save dataframe 
saveRDS(cyc_vol_sev_adj_num_cas_per_year_cordon_by_time_period, 
       "data-processed/rates/cyc_vol_sev_adj_num_cas_per_year_cordon_by_time_period_29_09_2022.Rds")


