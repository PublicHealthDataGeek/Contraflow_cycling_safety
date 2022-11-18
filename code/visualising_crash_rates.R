################################################################################
#                       Create visualisation of crash rates                    #
################################################################################
#### NB some issue with plots 5 data ordered wrong in terms of pre, contraf and cf removed

# Cant use cowplot with the gradient plots as it deletes the grey lines - need to use patchwork


# Load packages
library(tidyverse)
library(sf)
library(ggdist)
library(distributional)
library(ggplot2)
library(dplyr)
library(patchwork)

# Load dataframes - NB rates are now in per 100 years of exposure
raw_overall_crash_rates = readRDS("data-processed/rates/raw_overall_crash_rates_27_07_2022.Rds")
cyc_vol_adj_num_crashes_per_year_cordon_by_time_period = readRDS("data-processed/rates/cyc_vol_adj_num_crashes_per_year_cordon_by_time_period_27_07_2022.Rds")
raw_crash_rates_by_junction = readRDS("data-processed/rates/raw_crash_rates_by_junction_27_07_2022.Rds")
cyc_adj_num_crashes_per_year_junction = readRDS("data-processed/rates/cyc_adj_num_crashes_per_year_junction_27_07_2022.Rds")
raw_overall_crash_rates_by_action = readRDS("data-processed/rates/raw_overall_crash_rates_by_action_27_07_2022.Rds")
cyc_adj_num_crashes_per_year_action = readRDS("data-processed/rates/cyc_adj_num_crashes_per_year_action_27_07_2022.Rds")
raw_crash_rates_by_flow = readRDS("data-processed/rates/raw_crash_rates_by_flow_27_07_2022.Rds")
cyc_adj_num_crashes_per_year_flow = readRDS("data-processed/rates/cyc_adj_num_crashes_per_year_flow_27_07_2022.Rds")

#####  NEED TO CHECK COLUMN LABELS etc as per 100 years

# 1) Raw overall unadjusted
# Visualise
raw_overall_crash_rates$overall = "Overall" # add column so have nice facet label even though plot isnt faceted
raw_overall_unadj = raw_overall_crash_rates %>%
  group_by(crash_time_scale) %>%
  mutate(std_error = sd(raw_overall_crash_rate_per_100years_of_exposure)) %>% # calculates the sd of the rates per road segment status
  filter(id=="Apparent") %>%
  ggplot(aes(x=crash_time_scale, y=raw_overall_crash_rate_per_100years_of_exposure)) +
  stat_gradientinterval(
    aes(dist = dist_normal(mu=raw_overall_crash_rate_per_100years_of_exposure, sigma=std_error)), 
    point_size = 1.5) +
  geom_text(aes(x = crash_time_scale, y = 112.5, label = num_crashes), size = 3) +
  coord_flip()+
  scale_x_discrete(limits = rev) +
  facet_grid(overall ~., scales = "free", space = "free") +
  xlab(element_blank()) +
  ylab(element_blank()) +
  labs(tag = " Raw") +
  scale_y_continuous(limits = c(0, 113), position = "right", breaks = c(0, 25, 50, 75, 100, 112.5),
                     labels = c("0", "25", "50", "75", "100", "n")) +  # keeps the axis and puts it at the top
  theme_classic() +
  geom_hline(yintercept = c(0, 25, 50, 75, 100), linetype = "dashed", color = "#999999") +
  cowplot::panel_border() +
  theme(axis.ticks.x = element_blank(), strip.text.y.right = element_text(angle = 0))

# 2) Adj overall
cyc_vol_adj_num_crashes_per_year_cordon_by_time_period$overall = "Overall" # add column so have nice facet label even though plot isnt faceted
adj_overall = cyc_vol_adj_num_crashes_per_year_cordon_by_time_period %>%
  group_by(crash_time_scale) %>%
  mutate(std_error = sd(cyc_vol_cordon_adj_crash_rate_per_100years_of_exposure)) %>% # calculates the sd of the rates per road segment status
  filter(id=="Apparent") %>%
  ggplot(aes(x=crash_time_scale, y=cyc_vol_cordon_adj_crash_rate_per_100years_of_exposure)) +
  stat_gradientinterval(
    aes(dist = dist_normal(mu=cyc_vol_cordon_adj_crash_rate_per_100years_of_exposure, sigma=std_error)), point_size = 1.5) +
  geom_text(aes(x = crash_time_scale, y = 112.5, label = round(total_cyc_vol_cordon_adj_num_crashes, digits = 0)), size = 3) +
  coord_flip()+  
  scale_x_discrete(limits = rev) +
  facet_grid(overall ~., scales = "free", space = "free") +
  xlab(element_blank()) +
  ylab(element_blank()) +
  labs(tag = "Adjusted") +
  scale_y_continuous(limits = c(0, 113), breaks = NULL) +
  geom_hline(yintercept = c(0, 25, 50, 75, 100), linetype = "dashed", color = "#999999") +
  theme_classic() +
  theme(axis.line.x = element_blank(), strip.text.y.right = element_text(angle = 0)) +
  cowplot::panel_border()


# 3) Raw by junction
raw_junction = raw_crash_rates_by_junction %>%
  group_by(crash_time_scale, not_within_10m_junction) %>%
  mutate(std_error = sd(raw_crash_rate_by_junction_per_100years_of_exposure)) %>% # calculates the sd of the rates per road segment status and tro action
  filter(id=="Apparent") %>%
  ggplot(aes(x=crash_time_scale, y=raw_crash_rate_by_junction_per_100years_of_exposure)) +
  stat_gradientinterval(
    aes(dist = dist_normal(mu=raw_crash_rate_by_junction_per_100years_of_exposure, sigma=std_error)), point_size = 1.5) +
  geom_text(aes(x = crash_time_scale, y = 112.5, label = round(num_crashes, digits = 0)), size = 3) +
  coord_flip() + 
  scale_x_discrete(limits = rev) +
  facet_grid(not_within_10m_junction ~., scales = "free", space = "free",
             labeller = labeller(not_within_10m_junction = label_wrap_gen(10, multi_line = TRUE))) +
  xlab(element_blank()) +
  ylab(element_blank()) +
  labs(tag = "Raw") +
  scale_y_continuous(limits = c(0, 113), breaks = NULL) +
  geom_hline(yintercept = c(0, 25, 50, 75, 100), linetype = "dashed", color = "#999999") +
  theme_classic() +
  theme(strip.text.y.right = element_text(angle = 0), axis.line.x = element_blank()) +
  cowplot::panel_border()

# 4) Adj by junction
adj_junction = cyc_adj_num_crashes_per_year_junction %>%
  group_by(crash_time_scale, not_within_10m_junction) %>%
  mutate(std_error = sd(cyc_adj_crash_rate_by_junction_per_100years_of_exposure)) %>% # calculates the sd of the rates per road segment status
  filter(id=="Apparent") %>%
  ggplot(aes(x=crash_time_scale, y=cyc_adj_crash_rate_by_junction_per_100years_of_exposure)) +
  stat_gradientinterval(
    aes(dist = dist_normal(mu=cyc_adj_crash_rate_by_junction_per_100years_of_exposure, sigma=std_error)), point_size = 1.5) +
  geom_text(aes(x = crash_time_scale, y = 112.5, label = round(total_junction_cyc_adj_num_crashes, digits = 0)), size = 3) +
  coord_flip()+ 
  scale_x_discrete(limits = rev) +
  facet_grid(not_within_10m_junction ~., scales = "free", space = "free", 
             labeller = labeller(not_within_10m_junction = label_wrap_gen(10, multi_line = TRUE))) +
  xlab(element_blank()) +
  ylab(element_blank()) +
  # labs(tag = "Adj") +
  scale_y_continuous(limits = c(0, 113), breaks = NULL) +
  geom_hline(yintercept = c(0, 25, 50, 75, 100), linetype = "dashed", color = "#999999") +
  theme_classic() +
  theme(strip.text.y.right = element_text(angle = 0), axis.line.x = element_blank()) +
  cowplot::panel_border()

# 5) Raw by action
# Visualise
# factor, order and relabel the additional action
raw_overall_crash_rates_by_action = raw_overall_crash_rates_by_action %>%
  mutate(sig_additional_tro_action = factor(sig_additional_tro_action, 
                                            levels = c("Contraflow cycling only", 
                                                       "One-way street and contraflow cycling",
                                                       "Contraflow bus lane and contraflow cycling"),
                                            labels = c("Contraflow cycling", "...and one-way street",
                                                       "... and contraflow bus lane")))

raw_action = raw_overall_crash_rates_by_action %>%
  group_by(crash_time_scale, sig_additional_tro_action) %>%
  mutate(std_error = sd(raw_overall_crash_rate_by_action_per_100years_of_exposure)) %>% # calculates the sd of the rates per road segment status and tro action
  filter(id=="Apparent") %>%
  ggplot(aes(x=crash_time_scale, y=raw_overall_crash_rate_by_action_per_100years_of_exposure)) +
  stat_gradientinterval(
    aes(dist = dist_normal(mu=raw_overall_crash_rate_by_action_per_100years_of_exposure, sigma=std_error)), point_size = 1.5) +
  geom_text(aes(x = crash_time_scale, y = 112.5, label = num_crashes), size = 3) +
  coord_flip()+  
  facet_grid(sig_additional_tro_action ~., scales = "free", space = "free",
             labeller = labeller(sig_additional_tro_action = label_wrap_gen(10, multi_line = TRUE))) +
  xlab(element_blank()) +
  ylab(element_blank()) +
  labs(tag = "Raw") +
  scale_y_continuous(limits = c(0, 113), breaks = NULL) +
  geom_hline(yintercept = c(0, 25, 50, 75, 100), linetype = "dashed", color = "#999999") +
  theme_classic() +
  theme(strip.text.y.right = element_text(angle = 0), axis.line.x = element_blank()) +
  cowplot::panel_border() 


# 6) Adj by action
# factor, order and relabel the additional action
cyc_adj_num_crashes_per_year_action = cyc_adj_num_crashes_per_year_action %>%
  mutate(sig_additional_tro_action = factor(sig_additional_tro_action, 
                                            levels = c("Contraflow cycling only", 
                                                       "One-way street and contraflow cycling",
                                                       "Contraflow bus lane and contraflow cycling"),
                                            labels = c("Contraflow cycling", "...and one-way street",
                                                       "... and contraflow bus lane")))
adj_action = cyc_adj_num_crashes_per_year_action %>%
  group_by(crash_time_scale, sig_additional_tro_action) %>%
  mutate(std_error = sd(cyc_adj_crash_rate_per_100years_of_exposure)) %>% # calculates the sd of the rates per road segment status
  filter(id=="Apparent") %>%
  ggplot(aes(x=crash_time_scale, y=cyc_adj_crash_rate_per_100years_of_exposure)) +
  stat_gradientinterval(
    aes(dist = dist_normal(mu=cyc_adj_crash_rate_per_100years_of_exposure, sigma=std_error)), point_size = 1.5) +
  geom_text(aes(x = crash_time_scale, y = 112.5, label = round(total_action_cyc_adj_num_crashes, digits = 0)), size = 3) +
  coord_flip() + 
  scale_x_discrete(limits = rev) +
  facet_grid(sig_additional_tro_action ~., scales = "free", space = "free", 
             labeller = labeller(sig_additional_tro_action = label_wrap_gen(10, multi_line = TRUE))) +
  xlab(element_blank()) +
  ylab(element_blank()) +
  #labs(tag = "Adj") +
  scale_y_continuous(limits = c(0, 113), breaks = NULL) +
  theme_classic() +
  cowplot::panel_border() +
  theme(strip.text.y.right = element_text(angle = 0), axis.line.x = element_blank()) +
  geom_hline(yintercept = c(0, 25, 50, 75, 100), linetype = "dashed", color = "#999999")


# 7) Raw by pedal cycle direction
raw_flow = raw_crash_rates_by_flow %>%
  group_by(crash_time_scale, cyc_direction_when_crashed) %>%
  arrange(crash_time_scale) %>%
  mutate(std_error = sd(raw_crash_rate_by_flow_per_100years_of_exposure)) %>% # calculates the sd of the rates per road segment status and tro action
  filter(id=="Apparent") %>%
  ggplot(aes(x=cyc_direction_when_crashed, y=raw_crash_rate_by_flow_per_100years_of_exposure)) +
  stat_gradientinterval(
    aes(dist = dist_normal(mu=raw_crash_rate_by_flow_per_100years_of_exposure, sigma=std_error)), point_size = 1.5) +
  geom_text(aes(x = cyc_direction_when_crashed, y = 112.5, label = round(num_crashes_flow_1_2, digits = 0)), size = 3) +
  coord_flip()+
  scale_x_discrete(limits = rev) +
  facet_grid(crash_time_scale ~., scales = "free", space = "free",
             labeller = labeller(crash_time_scale = label_wrap_gen(10, multi_line = TRUE))) +
  xlab(element_blank()) +
  ylab(element_blank()) +
  labs(tag = "Raw") +
  scale_y_continuous(limits = c(0, 113), breaks = NULL) +
  geom_hline(yintercept = c(0, 25, 50, 75, 100), linetype = "dashed", color = "#999999") +
  theme_classic() +
  cowplot::panel_border() +
  theme(strip.text.y.right = element_text(angle = 0), axis.line.x = element_blank()) 

# 8) Adj by pedal cycle direction
adj_flow = cyc_adj_num_crashes_per_year_flow %>%
  group_by(crash_time_scale, cyc_direction_when_crashed) %>%
  arrange(crash_time_scale) %>%
  mutate(std_error = sd(cyc_adj_crash_rate_by_flow_per_100years_of_exposure)) %>%
  filter(id=="Apparent") %>%
  ggplot(aes(x=cyc_direction_when_crashed, y=cyc_adj_crash_rate_by_flow_per_100years_of_exposure)) +
  stat_gradientinterval(
    aes(dist = dist_normal(mu=cyc_adj_crash_rate_by_flow_per_100years_of_exposure, sigma=std_error)), point_size = 1.5) +
  geom_text(aes(x = cyc_direction_when_crashed, y = 112.5, label = round(total_flow_cyc_adj_num_crashes, digits = 0)), size = 3) +
  coord_flip()+
  scale_x_discrete(limits = rev) +
  xlab(element_blank()) +
  ylab(element_blank()) +
  #  labs(tag = "Adj") +
  scale_y_continuous(limits = c(0, 113), breaks = c(0, 25, 50, 75, 100, 112.5),
                     labels = c("0", "25", "50", "75", "100", "n")) +  
  theme_classic() +
  facet_grid(crash_time_scale ~., scales = "free", space = "free", 
             labeller = labeller(crash_time_scale = label_wrap_gen(10, multi_line = TRUE))) +
  geom_hline(yintercept = c(0, 25, 50, 75, 100), linetype = "dashed", color = "#999999") +
  cowplot::panel_border() +
  theme(strip.text.y.right = element_text(angle = 0), axis.ticks.x = element_blank())


################################################################################
#               Join plots together for paper and then save                    #
################################################################################

library(patchwork)
fig_4 = raw_overall_unadj + adj_overall + adj_junction + adj_action + adj_flow +
  plot_layout(ncol = 1, heights =c(0.5, 0.5, 1.1, 1.75, 2.3))


ggsave("output/figures/fig_4_dpi500.jpeg", 
        plot = fig_4, dpi = 500, width = 160, height = 210, units = "mm")
ggsave("output/figures/fig_4_dpi300.tiff", 
        plot = fig_4, dpi = 300, width = 160, height = 210, units = "mm")


# use patchwork to join plots together and Cairo to save so that the gradient appears
# (ggsave doesnt work)

Cairo(2200, 2600, file = "output/figures/fig_4_cairo_test.png", type = "png", bg = "white", dpi = 300)
raw_overall_unadj + adj_overall + adj_junction + adj_action + adj_flow +
  plot_layout(ncol = 1, heights =c(0.5, 0.5, 1.1, 1.75, 2.3))
dev.off()

CairoTIFF(2200, 2600, file = "output/figures/fig_4_cairo_test.tiff", bg = "white", dpi = 300)
raw_overall_unadj + adj_overall + adj_junction + adj_action + adj_flow +
  plot_layout(ncol = 1, heights =c(0.5, 0.5, 1.1, 1.75, 2.3))
dev.off()

