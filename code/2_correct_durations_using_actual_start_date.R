################################################################################
#       Update road segment duration status using actual cf start dates        #
#                                                                              #
# Having identified issue with duration calculation where the total was 8034   #
# days instead of the correct 8035 days between 1/1/1998 and 31/12/2019        #
# including the 31/12/2019, this code corrects this in the unique_TRO          #
# dataset using the actual contraflow start dates                              #
#                                                                              #  
# NB for road segments where contraflow start date is unknown the durations    #
# are NA rather than recalculated.                                             #
################################################################################

# Load packages
library(tidyverse)
library(lubridate)
library(sf)

# Load dataset
unique_tro_df = readRDS(file = "data-processed/old_versions/unique_tro_df_01_03_2022.Rds")
str(unique_tro_df)

# Currently duration is numeric column and includes calculation of duration based 
# on derived contraflow start date where this is unavailable

# Pre correction values
pre = unique_tro_df %>% rowwise() %>%
  mutate(pre_total = sum(c_across(pre_contraflow_duration:post_contraflow_duration))) %>%
  select(c(unique_contraflow_ID, pre_total)) %>% st_drop_geometry() # all 8035

sum(unique_tro_df$pre_contraflow_duration) # 2528969
sum(unique_tro_df$contraflow_duration) # 1528333
sum(unique_tro_df$post_contraflow_duration) # 23970

pre_totals_with_start_date = unique_tro_df %>% filter(!is.na(contraflow_start_date))
sum(pre_totals_with_start_date$pre_contraflow_duration) # 2396119
sum(pre_totals_with_start_date$contraflow_duration) # 1392022
sum(pre_totals_with_start_date$post_contraflow_duration) # 11941

# Create columns for duration pre, during and post contraflow existence
time_min = dmy("01-01-1998")
time_max = dmy("01-01-2020")
unique_tro_df = unique_tro_df %>%
  mutate(pre_contraflow_duration = contraflow_start_date - time_min) %>% # NB those that started on 31/12 now give -1 days (sort later)
  mutate(contraflow_time_stopped = contraflow_stop_date - contraflow_start_date) %>% # for those where contraflow was removed
  mutate(contraflow_time_didntstop = time_max - contraflow_start_date) # for those where contraflow wasnt removed (also creates values for those where contraflow removed)

unique_tro_df$contraflow_time_didntstop = replace(unique_tro_df$contraflow_time_didntstop,
                                              which(!is.na(unique_tro_df$contraflow_stop_date)),
                                              values = NA) # Replace these erroneous values with NA

unique_tro_df = unique_tro_df %>%
  mutate(contraflow_duration = coalesce(contraflow_time_stopped, contraflow_time_didntstop)) %>% # create single column with contraflow duration
  mutate(post_contraflow_duration = time_max - contraflow_stop_date) %>% # create column with post-contraflow duration where they are removed
  select(-c(contraflow_time_stopped, contraflow_time_didntstop))

unique_tro_df$post_contraflow_duration[is.na(unique_tro_df$post_contraflow_duration)] = 0  # Replace NA with 0 days

# replace that 1 obs that have a -1 and therefore an incorrect contraflow duration with correct values
change = as_vector(unique_tro_df %>% filter(pre_contraflow_duration == -1) %>% select(c(unique_contraflow_ID)) %>% st_drop_geometry())
unique_tro_df$pre_contraflow_duration = replace(unique_tro_df$pre_contraflow_duration,
                                             which(unique_tro_df$unique_contraflow_ID %in% change), values = 0)
unique_tro_df$contraflow_duration = replace(unique_tro_df$contraflow_duration,
                                         which(unique_tro_df$unique_contraflow_ID %in% change), values = 8035)

# Post correction values
post = unique_tro_df %>% rowwise() %>%
  mutate(post_total = sum(c_across(pre_contraflow_duration:post_contraflow_duration))) %>%
  select(c(unique_contraflow_ID, contraflow_start_date, post_total)) %>% st_drop_geometry() 
# those with a contraflow_start_date now have a total of all 8035

post_totals_with_start_date = unique_tro_df %>% filter(!is.na(contraflow_start_date))
sum(post_totals_with_start_date$pre_contraflow_duration) # was 2396119 and stayed the same
sum(post_totals_with_start_date$contraflow_duration) # was 1392022, now 1392487 
sum(post_totals_with_start_date$post_contraflow_duration, na.rm = TRUE) # was 11941 now 11949 ie the 8 road segments which were removed AND had a start date



total_sum = as.numeric(sum(post_totals_with_start_date$pre_contraflow_duration) + 
                                              sum(post_totals_with_start_date$contraflow_duration) +
                                              sum(post_totals_with_start_date$post_contraflow_duration, na.rm = TRUE))
# 3800555 which is 473 * 8035

# Change to numeric  
unique_tro_df = unique_tro_df %>%
  mutate(pre_contraflow_duration = as.numeric(pre_contraflow_duration),
         contraflow_duration = as.numeric(contraflow_duration),
         post_contraflow_duration = as.numeric(post_contraflow_duration)) # convert to numeric

# Save new file
saveRDS(unique_tro_df, file = "data-processed/unique_tro_df_durations_using_start_date.Rds")
