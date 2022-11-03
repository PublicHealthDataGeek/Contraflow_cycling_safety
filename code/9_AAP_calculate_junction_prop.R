#################################################################################
#             Calculate proportion of segment length within 20m of junction     #
#                                                                               #
# Updated on 27/9/2022 following AAP peer-review due to changes made to number  #
# crashes requested by peer-review (remove SBC and self-reported) thereby       #
# reducing number of road segments involved                                     # 
#################################################################################

remotes::install_github("saferactive/trafficalmr")

# Obtain junctions in OSM data
library(osmextract)
library(trafficalmr)
library(sf)
library(tidyverse)
library(mapview)
library(leafsync)

# Load dataframes
gl_pbf19 = "/home/bananafan/Documents/PhD/Paper1/data/greater-london-190101.osm.pbf"
crashes_casualties_vehicles = readRDS("data-processed/stats19/pedal_cycle_crashes_with_casualties_&_vehicles_AAP_27_09_22.RDs")
unique_tro_df = readRDS(file = "data-processed/unique_tro_df_durations_using_start_date.Rds")

# Manipulate OSM data to get junctions
gl_osm_lines19 = oe_read(gl_pbf19, quiet = FALSE, layer = "lines") 
gl_osm_lines19 = st_transform(gl_osm_lines19, crs=27700) # PROJCRS["OSGB 1936 / British National Grid"

highways = c("primary", "residential", "trunk", "footway", "service", "unclassified", 
             "tertiary", "secondary", "motorway_link", "motorway", 
             "tertiary_link", "secondary_link", "trunk_link", "pedestrian", "primary_link", 
             "living_street", "road")
osm_lines_roads = gl_osm_lines19 %>% filter(highway %in% highways)

junctions = osm_get_junctions(osm_lines_roads)
junctions_buff_20 = st_sf(st_buffer(junctions, dist = 20))

# Get road segment that had a crash on it 
ids = unique(crashes_casualties_vehicles$unique_contraflow_ID)
segments = unique_tro_df %>% filter(unique_contraflow_ID %in% ids)
nrow(segments) # 306 road segments had a crash on it

# Get the buffered junctions that are spatially joined to the road segments
segs_junc = st_join(junctions_buff_20, segments, left = FALSE) %>%
  select(c(unique_contraflow_ID)) 

# Create geometrical object of the intersection between the buffered junctions and the road segments
segs_junc_intersection = st_intersection(segments, segs_junc)  

# Create and view maps
segs_map = mapview(segments)
segs_junc_map =mapview(segs_junc)
segs_junc_int_map = mapview(segs_junc_intersection)
leafsync::sync(segs_map, segs_junc_map, segs_junc_int_map)

# Get proportion of length - NB incorrect - see below
sum(st_length(segs_junc_intersection)) /sum(st_length(segments)) * 100 # 121.1762 [1]
sum(st_length(segments)) # 43805.61 [m]
sum(st_length(segs_junc_intersection)) # 53081.98 [m]

# Multiple intersection observations were created as there are so many junctions
## Each of these are included in the st_length whereas actually just need their overall length 
## therefore use group_by and st_union to get correct spatial object
segs_junc_intersection_grouped = segs_junc_intersection %>%
  group_by(unique_contraflow_ID) %>%
  st_union()

sum(st_length(segs_junc_intersection_grouped)) # 27564.44 [m]

# Therefore proportion of road segment length that is within 20m of a junction is: 
sum(st_length(segs_junc_intersection_grouped)) /sum(st_length(segments)) * 100 # 62.92444 [1]


