##################################################################################
#  Create dot visualisation of all crashes by road segment status and TRO action #
##################################################################################


# Load libraries
library(tidyverse)
library(lubridate)
library(sf)
library(cowplot) # for ggsave2

# Load dataframe of pedal cycle crashes joined to casualties and vehicles
crashes_casualties_vehicles = readRDS(file = "data-processed/stats19/pedal_cycle_crashes_with_casualties_&_vehicles_&_junctions_18_05_22.RDs")

# Add column that measure number of weeks/months from contraflow being implemented and crash occurring
crashes_casualties_vehicles = crashes_casualties_vehicles %>%
  mutate(ceiling_weeks_from_cf_intro_to_crash = ceiling(time_length(interval(contraflow_start_date, date), "weeks"))) %>%
  mutate(ceiling_months_from_cf_intro_to_crash = ceiling(time_length(interval(contraflow_start_date, date), "months")))
# ceiling rounds up to nearest whole timeframe - weeks seems reasonable, makes vis simpler and more meaningful as opposed to lots of single crashes

# Add column that has the additional actions of the TRO
crashes_casualties_vehicles = crashes_casualties_vehicles %>%
  mutate(sig_additional_tro_action = case_when(introduces_one_way_street == TRUE ~ "One-way street and contraflow cycling", 
                                               introduces_contraflow_bus_lane == TRUE ~ "Contraflow bus lane and contraflow cycling",
                                               TRUE ~ "Contraflow cycling only"))

# saveRDS(crashes_casualties_vehicles, 
#         file = "data-processed/stats19/pedal_cycle_crashes_with_casualties_&_vehicles_&_junctions.RDs")





learning_df = crashes_casualties_vehicles %>%
  arrange(contraflow_start_date) %>%
  select(c(1, 7, 27, 40, 42, 44, 92:94)) %>%
  st_drop_geometry()
names(learning_df)

n_road_segments = learning_df %>%
  select(c(3, 9)) %>%
  group_by(sig_additional_tro_action, unique_contraflow_ID, ) %>%
  summarise(count = n()) %>%
  group_by(sig_additional_tro_action) %>%
  summarise(n_road_segments = n())

# sig_additional_tro_action                  n_road_segments
# 1 Contraflow cycling only                      244
# 2 One-way street and contraflow cycling         64
# 3 Contraflow bus lane and contraflow cycling     9
# More complex vis - by contraflow ID

n_crashes_action = learning_df %>%
  select(c(3, 9)) %>%
  group_by(sig_additional_tro_action) %>%
  summarise(t_crashes = n())
# sig_additional_tro_action                  count
# 1 Contraflow cycling only                     1098
# 2 One-way street and contraflow cycling        323
# 3 Contraflow bus lane and contraflow cycling   242

n_crashes_action_timescale = learning_df %>%
  select(c(3, 9, 6)) %>%
  group_by(sig_additional_tro_action, crash_time_scale) %>%
  summarise(n_crashes = n()) %>%
  left_join(n_crashes_action) %>%
  mutate(percent = round(n_crashes/t_crashes *100, digit = 1))
#   sig_additional_tro_acti… crash_time_scale n_crashes t_crashes percent
# 1 Contraflow cycling only  Pre_contraflow         556      1098    50.6
# 2 Contraflow cycling only  Contraflow             535      1098    48.7
# 3 Contraflow cycling only  Contraflow_remo…         7      1098     0.6
# 4 One-way street and cont… Pre_contraflow         184       323    57  
# 5 One-way street and cont… Contraflow             138       323    42.7
# 6 One-way street and cont… Contraflow_remo…         1       323     0.3
# 7 Contraflow bus lane and… Pre_contraflow          84       242    34.7
# 8 Contraflow bus lane and… Contraflow             158       242    65.3



learning_df$unique_contraflow_ID = reorder(learning_df$unique_contraflow_ID, learning_df$contraflow_start_date)
learning_df$sig_additional_tro_action =  fct_infreq(learning_df$sig_additional_tro_action)

facet_labels = c("Contraflow cycling only" = "Contraflow cycling 
   (244 road segments,
    1098 crashes)",
                 "One-way street and contraflow cycling" = "...and one-way street 
   (64 road segments, 
    323 crashes)",
                 "Contraflow bus lane and contraflow cycling" = "...and contraflow bus lane 
   (9 road segments, 
    242 crashes)")

# Preferred visualisation of all crashes
fig_2 = learning_df %>%
  ggplot() +
  geom_point(aes(x = date, y = unique_contraflow_ID,
                 color = crash_time_scale)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y", 
               limits = c(as.Date("1998-01-01"), as.Date("2020-01-01")), 
               expand = c(0, 0)) + # this means xaxis is limited
  scale_y_discrete(expand = expansion(add = 5)) +
  labs(x = "Crash date", y = "Unique road segments") +
  theme_classic() +
  geom_vline(xintercept = as.numeric(as.Date("2011-11-01")), linetype = "dashed") +
  theme(axis.text.y = element_blank(), legend.position = "bottom", legend.title = element_blank(),
        axis.ticks = element_line(colour = "black", size = rel(0.5)),
        panel.background = element_rect(color = "black"),
        strip.text.y = element_text(angle = 0, color = "black"),
        strip.background.y = element_rect(fill = "white", color = "white"),
        strip.placement = "outside") +
  facet_grid(sig_additional_tro_action ~., scales = "free", space = "free", 
             labeller = as_labeller(facet_labels)) +
  scale_color_manual(labels = c("Pre-contraflow", "Contraflow", "Contraflow removed"), values = c("#E69F00", "#CC79A7", "#56B4E9")) # wong with pink

ggsave2("output/figures/fig_2_dpi500.jpeg", 
        plot = fig_2, dpi = 500, bg = "white", width = 160, height = 205, units = "mm")

ggsave2("output/figures/fig_2_dpi300.tiff", 
        plot = fig_2, dpi = 300, bg = "white", width = 160, height = 205, units = "mm")



# wong reference https://www.nature.com/articles/nmeth.1618


# https://davidmathlogic.com/colorblind/#%23332288-%23117733-%2344AA99-%2388CCEE-%23DDCC77-%23CC6677-%23AA4499-%23882255  
 # scale_color_manual(labels = c("Pre-contraflow", "Contraflow", "Contraflow removed"), values = c("#CC6677", "#44AA99", "#88CCEE"))  # tol
 #  
 # # scale_color_manual(labels = c("Pre-contraflow", "Contraflow", "Contraflow removed"), values = c("#E69F00", "#009E73", "#56B4E9")) # wong with green
 #  scale_color_manual(labels = c("Pre-contraflow", "Contraflow", "Contraflow removed"), values = c("#E69F00", "#CC79A7", "#56B4E9")) # wong with pink







