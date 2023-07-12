################################################################################
### FORMAT DATASET
################################################################################

# create shorthands to make working with the data easier:
## Weights
# weights <- data.list$main %>%
#   group_by(o3_current_hromada) %>% 
#   summarise(n = n()) %>% 
#   rename(strata = "o3_current_hromada") %>% 
#   left_join(sampling, by = "strata")%>% 
#   mutate(weight = (as.numeric(hh_total)/sum(as.numeric(hh_total)))/(n/sum(n))) %>% 
#   select(strata, weight) %>% 
#   rename(w = "strata")

### ADD WEIGHTS, OVERALL AND GROUPING VARIABLES
#
data.list$main <- data.list$main %>% 
  mutate(overall = "overall",
         weight = 1 #as.numeric(weights)
         ) %>%
  relocate(`b17_access_stores/power_outages`, .before = `b17_access_stores/other`) %>%
  relocate(`b17_access_stores/air_alert`, .before = `b17_access_stores/other`)
we <- data.list$main %>% 
  select(weight, uuid) 

### Additional variables are added here

# data.list$main <- data.list$main %>%
#   mutate(`b7_vehicle_fuel/none1` = pmax(`b7_vehicle_fuel/none_vehicles`,`b7_vehicle_fuel/none`,na.rm = TRUE)) %>%
#   select(-c(`b7_vehicle_fuel/none_vehicles`,`b7_vehicle_fuel/none`)) %>%
#   rename(`b7_vehicle_fuel/none` = `b7_vehicle_fuel/none1`)
# 
# data.list$main <- data.list$main %>%
#   mutate(`b7_1_heating_fuel/none1` = pmax(`b7_1_heating_fuel/none_heating`,`b7_1_heating_fuel/none`,na.rm = TRUE)) %>%
#   select(-c(`b7_1_heating_fuel/none_heating`,`b7_1_heating_fuel/none`)) %>%
#   rename(`b7_1_heating_fuel/none` = `b7_1_heating_fuel/none1`)

                                         
