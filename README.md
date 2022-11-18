# Contraflows and cycling safety: Evidence from 22 years of data involving 508 one-way streets  

This repository contains code and data to support the above journal article. The paper is available at: https://doi.org/10.1016/j.aap.2022.106895 

Please cite as:   

Tait, C., Beecham, R., Lovelace, R., Barber, S., 2023. Contraflows and cycling safety: Evidence from 22 years of data involving 508 one-way streets. Accident Analysis & Prevention 179, 106895. https://doi.org/10.1016/j.aap.2022.106895



### Abstract  

Contraflow cycling on one-way streets is a low cost intervention that research shows can
improve the cycling experience and increase participation. Evidence from several studies
suggest that cyclists on contraflows have a lower crash risk. However, implementing contraflow
cycling is often controversial, including in the United Kingdom (UK). In this paper we examine
whether contraflow cycling on one-way streets alters crash or casualty rates for pedal cyclists.  

Focusing on inner London boroughs between 1998 and 2019, we identified 508 road segments
where contraflow cycling was introduced on one-way streets. We identified road traffic crashes
occurring within 10m of these segments and labelled them as pre-contraflow, contraflow or
contraflow removed crashes. We calculated rates using the number of crashes or casualties
divided by the time exposed and generated 95% confidence intervals using bootstrap
resampling. We adjusted the rates for changes in cordon cycling volume and injury severity
reporting.  

There were 1498 crashes involving pedal cyclists: 788 pre-contraflow, 703 contraflow and 7
following contraflow removal. There was no change in adjusted overall crash rates or pedal
cyclist casualty rates when contraflow cycling was introduced. Proximity to a junction doubled
the crash rate. The crash rate when pedal cyclists were travelling contraflow was the same as
those travelling with flow.  

We have found no evidence that introducing contraflow cycling increases the crash or casualty
rate for pedal cyclists. It is possible that such rates may indeed fall when contraflow cycling is
introduced if more accurate spatio-temporal cycling volume data was available. We recommend
all one-way streets are evaluated for contraflow cycling but encourage judicious junction design and recommend UK legislative change for mandatory two-way cycling on one-way streets unless exceptional circumstances exist.

### Datasets  

#### 1) One-way streets that allow contraflow cycling in Inner London
We collected and collated this primary data from The Gazette. We have made this data available for other researchers to utilise.  
ADD DATASET HERE


#### 2) Road traffic crash data
The UK Road traffic crash data used in this analysis can be obtained from the UK Department of Transport Road Safety Data page:
https://www.data.gov.uk/dataset/cb7ae6f0-4be6-4935-9277-47e5ce24a11f/road-safety-data  


#### 3) London cyclist volume cordon data
The Transport for London (TfL) cycling volume cordon data can be obtained from the TfL Travel in London Report datasets:  

- Travel in London Report 14 data - Figure 5.8.  https://tfl.gov.uk/cdn/static/cms/documents/travel-in-london-report-14-data.xlsx  

- Travel in London Report 12 data - Figure 6.5. https://tfl.gov.uk/cdn/static/cms/documents/travel-in-london-report-12-data.xlsx

A version of this data is available in the data folder in this repo: [TFL cordon count cyclist data](data/TFL_Cordon_data_1976_2020.csv).   

  

#### 4) Other datasets utilised
Open Geography UK data for the geography of London Boroughs can be obtained from the Office of National Statistics Open Geography Portal:
https://geoportal.statistics.gov.uk/

OpenStreetMap can be utilised for examining maps of the one-way streets and contraflows:
https://www.openstreetmap.org  



### Code
  
We have added datasets to the data folder in this repository that are required to run this code. You may need to download your own version of the ONS geography London borough boundaries.

Our code should be run in the following order:  

- [0_get_stats19.R](code/0_get_stats19.R) - This code manipulates the UK Road Traffic Crash data (known as 'STATS19') used in the analysis. You will need to download your own version of the data from the link detailed above under datasets.  

- [0_get_tfl_cycle_cordon_counts.R](code/0_get_tfl_cycle_cordon_counts.R) - This code takes the TFL cordon count dataset and manipulates it to manage missing values and then generates a index of change in cycling volume baselined to 1998 (first year of the study).

- [1_join_stats19_to_contraflows_10m.R](code/1_join_stats19_to_contraflows_10m.R) - This code takes the crash data and identifies the crashes involving pedal cycles that occur within 10m of the one-way streets that allow contraflow cycling.  

- [2_correct_durations_using_actual_start_date.R](code/2_correct_durations_using_actual_start_date.R) - This code corrects an error spotted in the calculation of duration of exposure to the various states (pre-contraflow, contraflow or contraflow removed) of the one-way streets.  

- [3_analyse_contraflow_crashes_using_actual_start_date.R](code/3_analyse_contraflow_crashes_using_actual_start_date.R) - This code takes the crashes involving pedal cycles within 10m of a one-way street that allows contraflow cycling and identifies the vehicles and casualties involved. It removes crashes that are self-reported and single bicycle crashes before tidying the data and developing summary data.  

- [4_withflow_or_contraflow.R](code/4_withflow_or_contraflow.R) - This code determines whether the pedal cycle(s) involved in the crashes is travelling with the motor vehicles flow, contraflow or in some other direction (e.g. turning).  

- [5_prep_data_for_crash_rates.R](code/5_prep_data_for_crash_rates.R) - This code performs a number of actions that are required to subsequently calculate crash rates including bootstrapping the dataset, calculating time duration that road segments spent in different states, determining where crashes occurred relative to TFL cordon counters (required to adjust rates using cyclist volumes) and calculated number of crashes by various variables/states.   

- [6_calculate_crash_rates.R](6_calculate_crash_rates.R) - This code calculates the following crash rates: Overall crash rate, crash rates within/not within 10m of a junction, crash by pedal cycle direction and crash rate by action (i.e. just contraflow cycling, one-way street and contraflow cycling or one-way street with contraflow bus lane and contraflow cycling). Each rate is calculated unadjusted and adjusted for change in cordon count cycling volume.    

- [7_calculate_casualty_rates.R](7_calculate_casualty_rates.R) - This code gets the STATS19 casualty severity adjustment data for the years 2004-2019. This is because the classification of injury severity has changed during the study period and so our casualties need to be adjusted to take into account this change in reporting.  The severity adjustment probabilities are attached to the correct casualty and then the casualty dataframe is bootstrapped. This code then calculates the exposure for the casualties before calculating the following casualty rates: unadjusted; adjusted for severity; and adjusted for severity and change in cordon count cycling volume.  

We have also added code files that create two of the key the visualisations in the paper.

##### Figure 2: Dot visualisation of all crashes involving pedal cycles within 10 m of a road segment by: unique road segment (vertical position); date of crash (horizontal position); crash segment status (colour); and significant change to road segment (pane). The dashed line shows when the traffic sign change was introduced. Colour palette sourced from Wong (2011) to promote visual accessibility.
![](figures/fig2.jpeg)  

[visualising_all_crashes.R](visualising_all_crashes.R)
  
  
##### Figure 4: Crash rates involving pedal cyclists per 100 years of exposure by crash segment status. Rates are presented as raw and adjusted for cordon cycling volume (1998 index) as: overall; by proximity to junctions or roundabouts (within 10 m); by significant change to road segments; and by pedal cycle direction. Visualisation shows point estimates for rates with 95 % confidence intervals generated by bootstrapping. n represents the number of crashes, rounded to the nearest integer for adjusted data.  

![](figures/fig4.jpeg)  

[visualising_crash_rates.R](visualising_crash_rates.R)






### Acknowledgements and Licenses

TfL data: Powered by TfLOpen Data.  Contains OS data © Crown copyright and database rights 2016 and Geomni UK Map data © and database rights [2019].  

The Gazette, Office of National Statistics and UK Road Traffic crash data: Licensed under the Open Government Licence v3.0. http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3  

OpenStreetMap data: © OpenStreetMap contributors and available under Open Database Licence. Contains Ordnance Survey data © Crown copyright and database right 2010-19. https://www.openstreetmap.org/copyright  

ONS data: Contains public sector information licensed under the Open Government Licence v3.0. 
