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

if (JMMI_variable == "Customers") {
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

}

if (JMMI_variable == "Retailers") {data.list$main <- data.list$main %>% 
  mutate(overall = "overall",
         weight = 1 #as.numeric(weights)
  ) %>%
  relocate(`z1_food_price_increased/cereal_porridge`, .before = `z1_food_price_increased/all`) %>%
  relocate(`v1_difficulties/storage_during_power_outages`, .before = `v1_difficulties/other`) %>%
  relocate(`w3_access_stores/no_access_due_to_power_outages`, .before = `w3_access_stores/other`) %>%
  relocate(`w3_access_stores/no_access_during_air_alert`, .before = `w3_access_stores/other`)
we <- data.list$main %>% 
  select(weight, uuid) 

### Correcting z1 and z2 questions

z1_cols <- c(
  "z1_food_price_increased/bread",
  "z1_food_price_increased/eggs",
  "z1_food_price_increased/milk",
  "z1_food_price_increased/potatoes",
  "z1_food_price_increased/carrots",
  "z1_food_price_increased/onions",
  "z1_food_price_increased/cabbage",
  "z1_food_price_increased/chicken",
  "z1_food_price_increased/oil",
  "z1_food_price_increased/flour",
  "z1_food_price_increased/rice",
  "z1_food_price_increased/buckwheat",
  "z1_food_price_increased/water",
  "z1_food_price_increased/cereal_porridge"
)

pmax_food <- function(name_food){
  return(pmax(name_food,data.list$main[["z1_food_price_increased/all"]]))
}

data.list$main <- data.list$main %>%
  mutate(across(.cols = z1_cols, .fns = pmax_food))

z2_cols <- c(
  "z2_nfi_price_increased/diapers",
  "z2_nfi_price_increased/body_soap",
  "z2_nfi_price_increased/laundry_soap",
  "z2_nfi_price_increased/powder",
  "z2_nfi_price_increased/toothpaste",
  "z2_nfi_price_increased/pads"
)

pmax_nfi <- function(name_nfi){
  return(pmax(name_nfi,data.list$main[["z2_nfi_price_increased/all"]]))
}

data.list$main <- data.list$main %>%
  mutate(across(.cols = z2_cols, .fns = pmax_nfi))

### Additional variables are added here

data.list$main <- data.list$main %>%
  mutate(food_half_increased = ifelse(is.na(z1_food_price_increased), NA_real_,
                                      ifelse(rowSums(across(z1_cols, .fns=as.numeric), na.rm=TRUE) %_>=_% length(z1_cols)/2, 
                                             "Prices increased for AT LEAST half of food products",
                                             "Prices increased for LESS than half of food products")),
         nfi_half_increased = ifelse(is.na(z2_nfi_price_increased), NA_real_,
                                     ifelse(rowSums(across(z2_cols, .fns=as.numeric), na.rm=TRUE) %_>=_% length(z2_cols)/2, 
                                            "Prices increased for AT LEAST half of NFI products",
                                            "Prices increased for LESS than half of NFI products")),
         all_products_increased = ifelse(is.na(z2_nfi_price_increased) | is.na(z1_food_price_increased), NA_real_,
                                         ifelse(rowSums(across(c(z1_cols, z2_cols), .fns=as.numeric), na.rm=TRUE) %==% (length(z2_cols) + length(z1_cols)), 
                                                "Prices increased for ALL products (food and NFI)",
                                                "Prices increased NOT for all products (food and NFI)")),
         half_food_half_nfi_increased = ifelse(is.na(z2_nfi_price_increased) | is.na(z1_food_price_increased), NA_real_,
                                               ifelse(rowSums(across(z1_cols, .fns=as.numeric), na.rm=TRUE) %_>=_% length(z1_cols)/2 &
                                                        rowSums(across(z2_cols, .fns=as.numeric), na.rm=TRUE) %_>=_% length(z2_cols)/2  , 
                                                      "Prices increased for AT LEAST half of food AND AT LEAST half of NFI products",
                                                      "Prices increased for LESS than half of food OR LESS than half of NFI products")),
  )
}
