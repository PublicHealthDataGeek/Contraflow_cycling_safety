################################################################################
#                          Get TFL cycle cordon counts                         #
#                                                                              #
# This code imports the raw TFL cycle cordon counts, tidies it, manages        #
# missing data and then generates an index baselined to 1998.                  #
#                                                                              #
################################################################################

# Load packages
library(tidyverse)
library(sf)
library(readxl)
library(zoo)

# import raw TFL cordon count data
raw_tfl_cycle_volume = as.data.frame(readr::read_delim("data/TFL_Cordon_data_1976_2020.csv", delim = ",", 
                                                       trim_ws = TRUE))
raw_tfl_cycle_volume = raw_tfl_cycle_volume %>%
  rename(year = "...1") %>%
  filter(year >= 1996) # drop years I dont want (keep 1997 as need it to estimate 1998)

# Add data for years with missing counts - use interpolation from zoo package (na.approx)
interpolated_data = as.data.frame(round(na.approx(raw_tfl_cycle_volume), digits = 1)) %>%
  select(c(1:4)) %>%
  rename(central_interpolated = `Central London cordon`, 
         inner_interpolated = `Inner London cordon`,
         outer_interpolated = `London boundary cordon`)

tfl_cycle_volume = left_join(raw_tfl_cycle_volume, interpolated_data) %>%
  select(-c(`Thames screenline`))

# impute data for inner_interpolate 2019 
## central has reduced by 2.3% between 2018 and 2019
## outer has not changed
## so apply half 2.3 ie -1.15% to 70 which is 69.2
tfl_cycle_volume$inner_interpolated = replace(tfl_cycle_volume$inner_interpolated,
                                              which(tfl_cycle_volume$year == "2019"),
                                              values = 69.2)

# Tidy dataframe before getting multiplier
tfl_cycle_volume = tfl_cycle_volume %>%
  filter(year >= 1998 & year <= 2019) %>% # keep only the years I am interested in
  select(c(1, 5:7)) # drop the raw data and keep the interpolated/imputed data

# Get 1998 baseline data
# year central_interpolated inner_interpolated outer_interpolated
# 1998                 53.5               30.7                 10

# Calculate percent change ie index
tfl_cordon_cycle_traffic_index = tfl_cycle_volume %>%
  mutate(central_change_baseline = (central_interpolated/53.5)*100,
         inner_change_baseline = (inner_interpolated/30.7)*100,
         outer_change_baseline = (outer_interpolated/10)*100) %>%
  mutate(cyc_central_crash_index = central_change_baseline/100, 
         cyc_inner_crash_index = inner_change_baseline/100,
         cyc_outer_crash_index = outer_change_baseline/100) %>%
  mutate(year = as.integer(year))

#Save dataframe
saveRDS(tfl_cordon_cycle_traffic_index, file = "data-processed/cycle_volume/tfl_cordon_cycle_traffic_index.Rds")