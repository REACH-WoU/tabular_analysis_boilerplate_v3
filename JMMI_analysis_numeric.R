################################################################################
###
### PROJECT:      Ukraine JMMI Analysis Script
### PURPOSE:      Use analysed data and output files
### INPUT:        JMMI dataset
### OUTPUT:       XLSX
### LAST MODIFY:  Svitlana Bozhenko
###
################################################################################
# 


# Load the list of oblast <> region
regions <- read.xlsx("resources/Retailers/macroregions.xlsx")

##################################### Formating to numeric #############################################

numeric_cols <- filter(tool.survey, type %in% c("integer","decimal"))$name
final_price_cols <- data.list$main %>%
  select(ends_with("final_price")) %>%
  colnames()
numeric_cols <- c(numeric_cols,final_price_cols)
data.list$main <- data.list$main %>%
  mutate_at(numeric_cols,as.numeric)

food_cols <- c(
  "b3_bread_price_final_price",
  "c3_eggs_price_final_price",
  "d3_milk_price_final_price",
  "e3_potatoes_price_final_price",
  "f3_carrots_price_final_price",
  "g3_onions_price_final_price",
  "h3_cabbage_price_final_price",
  "i3_chicken_price_final_price",
  "j3_oil_price_final_price",
  "k3_flour_price_final_price",
  "l3_rice_price_final_price",
  "m3_buckwheat_price_final_price",
  "n3_water_price_final_price",
  "y3_cereal_porridge_price_final_price"
)

nfi_cols <- c(
  "o3_diapers_price_final_price",
  "p3_body_soap_price_final_price",
  "r3_laundry_soap_price_final_price",
  "q3_powder_price_final_price",
  "s3_toothpaste_price_final_price",
  "t3_pads_price_final_price"
)

##################################### Prices - Retailers #####################################

# Create a new column with region
data.list$main <- data.list$main %>%
  left_join(regions, by = "a5_current_oblast")

# Creating a list of location indicators
location_cols <- c("macroregion","a5_current_oblast", "a6_current_raion", "a7_current_hromada")
geography <- data.list$main %>%
  select(location_cols) %>%
  unique()


#Calculating medians

medians_hromada <- raw_medians(data.list$main,final_price_cols,"a7_current_hromada",c( "macroregion", "a5_current_oblast", "a6_current_raion"))
#medians_raion_test <- raw_medians(data.list$main,final_price_cols,"a6_current_raion", c("macroregion", "a5_current_oblast"))
medians_raion <- raw_medians(filter(medians_hromada, stats == "median"),final_price_cols,"a6_current_raion", c("macroregion", "a5_current_oblast"))
#medians_oblast_test <- raw_medians(filter(medians_raion_test, stats == "median"),final_price_cols,"a5_current_oblast", "macroregion")
#medians_oblast_test2 <- raw_medians(data.list$main,final_price_cols,"a5_current_oblast", "macroregion")
medians_oblast <- raw_medians(filter(medians_raion, stats == "median"),final_price_cols,"a5_current_oblast", "macroregion")
#medians_region_test <- raw_medians(filter(medians_oblast_test2, stats == "median"),final_price_cols,"macroregion")
#medians_region_test2 <- raw_medians(data.list$main,final_price_cols,"macroregion")
medians_region <- raw_medians(filter(medians_oblast, stats == "median"),final_price_cols,"macroregion")
medians_national <- descriptive_stats(rename(filter(medians_region, stats == "median"), "national" = "stats"),"national",final_price_cols)
#medians_national_test <- descriptive_stats(rename(filter(medians_region_test2, stats == "median"), "national" = "stats"),"national",final_price_cols)
#prices_national <- medians_national %>%
#  ungroup() %>%
#  select(-national)

### 
### Impute missing data for basket calculations ###
### 

prices_hromada <- medians_hromada %>%
  filter(stats == "median")
prices_raion <- medians_raion %>%
  filter(stats == "median")
prices_oblast <- medians_oblast %>%
  filter(stats == "median")
prices_region <- medians_region %>%
  filter(stats == "median")


# Creating a dataset with all region medians to impute missing values on oblast level
prices_region_to_update <- prices_oblast[,c("macroregion", "a5_current_oblast")] %>%     
  left_join(prices_region, by = "macroregion")          
# Imputing missing values in prices dataset (only medians)
prices_oblast <- prices_oblast %>%                                                  
  rows_patch(prices_region_to_update, by = c("macroregion","a5_current_oblast","stats"))
# Imputing misiing values in full dataset and calculating the basket
medians_oblast <- medians_oblast %>%
  rows_patch(prices_oblast, by = c("macroregion","a5_current_oblast","stats")) %>%
  basket(food_cols,nfi_cols)

prices_oblast_to_update <- prices_raion[,c("a5_current_oblast", "a6_current_raion")] %>%
  left_join(prices_oblast, by = "a5_current_oblast")
prices_raion <- prices_raion %>%
  rows_patch(prices_oblast_to_update, by = "a6_current_raion")
medians_raion <- medians_raion %>%
  rows_patch(prices_raion, by = c("a6_current_raion","stats")) %>%
  basket(food_cols,nfi_cols)

prices_raion_to_update <- prices_hromada[,c("a6_current_raion", "a7_current_hromada")] %>%
  left_join(prices_raion, by = "a6_current_raion")
prices_hromada <- prices_hromada %>%
  rows_patch(prices_raion_to_update, by = "a7_current_hromada")
medians_hromada <- medians_hromada %>%
  rows_patch(prices_hromada, by = c("a7_current_hromada","stats")) %>%
  basket(food_cols,nfi_cols)

medians_region <- medians_region %>%
  basket(food_cols,nfi_cols)

medians_national <- medians_national %>% 
  basket(food_cols,nfi_cols)

# Putting all together in one dataset
df_prices <- list("medians_hromada" = medians_hromada, 
                  "medians_raion" = medians_raion, 
                  "medians_oblast" = medians_oblast, 
                  "medians_region" = medians_region, 
                  "medians_national" = medians_national)


# Recoding p-codes with names

recode$col <- ".global"

for (i in 1:length(df_prices)){
  df_prices[[i]] <- df_prices[[i]] %>%
    matchmaker::match_df(dictionary = recode, from = "from",               
                         to = "to",                   
                         by = "col") %>%
    filter(!is.na(colnames(df_prices[[i]])[1]))
}


# Writing down to the file 
save.dfs(df_prices, paste0("output/Retailers/", "JMMI UKR Retailers Prices_combined_analysis_", strings['out_date'], ".xlsx"))


###--------Just for a test----------
#prices_hromada <- medians_hromada %>%
#  filter(stats == "median")
#prices_raion_test <- medians_raion_test %>%
#  filter(stats == "median")
#prices_oblast_test <- medians_oblast_test %>%
#  filter(stats == "median")
#prices_region_test <- medians_region_test %>%
#  filter(stats == "median")
#
## Creating a dataset with all region medians to impute missing values on oblast level
#prices_region_test_to_update <- prices_oblast_test[,c("macroregion", "a5_current_oblast")] %>%     
#  left_join(prices_region_test, by = "macroregion")          
## Imputing missing values in prices dataset (only medians)
#prices_oblast_test <- prices_oblast_test %>%                                                  
#  rows_patch(prices_region_test_to_update, by = c("macroregion","a5_current_oblast","stats"))
## Imputing misiing values in full dataset and calculating the basket
#medians_oblast_test <- medians_oblast_test %>%
#  rows_patch(prices_oblast_test, by = c("macroregion","a5_current_oblast","stats")) %>%
#  basket(food_cols,nfi_cols)
#
#prices_oblast_test_to_update <- prices_raion_test[,c("a5_current_oblast", "a6_current_raion")] %>%
#  left_join(prices_oblast_test, by = "a5_current_oblast")
#prices_raion_test <- prices_raion_test %>%
#  rows_patch(prices_oblast_test_to_update, by = "a6_current_raion")
#medians_raion_test <- medians_raion_test %>%
#  rows_patch(prices_raion_test, by = c("a6_current_raion","stats")) %>%
#  basket(food_cols,nfi_cols)
#
#prices_raion_test_to_update <- prices_hromada[,c("a6_current_raion", "a7_current_hromada")] %>%
#  left_join(prices_raion_test, by = "a6_current_raion")
#prices_hromada <- prices_hromada %>%
#  rows_patch(prices_raion_test_to_update, by = "a7_current_hromada")
#medians_hromada <- medians_hromada %>%
#  rows_patch(prices_hromada, by = c("a7_current_hromada","stats")) %>%
#  basket(food_cols,nfi_cols)
#
#medians_region_test <- medians_region_test %>%
#  basket(food_cols,nfi_cols)
#
#medians_national_test <- medians_national_test %>% 
#  basket(food_cols,nfi_cols)
#
#df_prices_test <- list("medians_hromada" = medians_hromada, 
#                       "medians_raion_test" = medians_raion_test, 
#                       "medians_oblast_test" = medians_oblast_test, 
#                       "medians_region_test" = medians_region_test, 
#                       "medians_national_test" = medians_national_test)
#
#for (i in 1:length(df_prices_test)){
#  df_prices_test[[i]] <- df_prices_test[[i]] %>%
#    matchmaker::match_df(dictionary = recode, from = "from",               
#                         to = "to",                   
#                         by = "col") %>%
#    filter(!is.na(colnames(df_prices_test[[i]])[1]))
#}
#
## Writing down to the file 
#save.dfs(df_prices_test, paste0("output/", "JMMI UKR Retailers Prices_combined_analysis_TEST", strings['out_date'], ".xlsx"), "_prices")
#
##################################### Resupply - Retailers #####################################

resupply_cols <- data.list$main %>%
  select(ends_with("resupply_days")) %>%
  colnames()

#Calculating medians

resupply_medians_hromada <- raw_medians(data.list$main,resupply_cols,"a7_current_hromada",c( "macroregion", "a5_current_oblast", "a6_current_raion")) 
resupply_medians_raion <- raw_medians(filter(resupply_medians_hromada,stats=="median"),resupply_cols,"a6_current_raion", c("macroregion", "a5_current_oblast"))
resupply_medians_oblast <- raw_medians(filter(resupply_medians_raion,stats=="median"),resupply_cols,"a5_current_oblast", "macroregion")
resupply_medians_region <- raw_medians(filter(resupply_medians_oblast,stats=="median"),resupply_cols,"macroregion")
resupply_medians_national <- descriptive_stats(rename(filter(resupply_medians_region, stats == "median"), "national" = "stats"),"national",resupply_cols)
#prices_national <- medians_national %>%
#  ungroup() %>%
#  select(-national)

### 
### Impute missing data for basket calculations ###
### 

resupply_hromada <- resupply_medians_hromada %>%
  filter(stats == "median")
resupply_raion <- resupply_medians_raion %>%
  filter(stats == "median")
resupply_oblast <- resupply_medians_oblast %>%
  filter(stats == "median")
resupply_region <- resupply_medians_region %>%
  filter(stats == "median")


# Creating a dataset with all region medians to impute missing values on oblast level
resupply_region_to_update <- resupply_oblast[,c("macroregion", "a5_current_oblast")] %>%     
  left_join(resupply_region, by = "macroregion")          
# Imputing missing values in resupply dataset (only medians)
resupply_oblast <- resupply_oblast %>%                                                  
  rows_patch(resupply_region_to_update, by = c("macroregion","a5_current_oblast","stats"))
# Imputing misiing values in full dataset
resupply_medians_oblast <- resupply_medians_oblast %>%
  rows_patch(resupply_oblast, by = c("macroregion","a5_current_oblast","stats"))

resupply_oblast_to_update <- resupply_raion[,c("a5_current_oblast", "a6_current_raion")] %>%
  left_join(resupply_oblast, by = "a5_current_oblast")
resupply_raion <- resupply_raion %>%
  rows_patch(resupply_oblast_to_update, by = "a6_current_raion")
resupply_medians_raion <- resupply_medians_raion %>%
  rows_patch(resupply_raion, by = c("a6_current_raion","stats"))

resupply_raion_to_update <- resupply_hromada[,c("a6_current_raion", "a7_current_hromada")] %>%
  left_join(resupply_raion, by = "a6_current_raion")
resupply_hromada <- resupply_hromada %>%
  rows_patch(resupply_raion_to_update, by = "a7_current_hromada")
resupply_medians_hromada <- resupply_medians_hromada %>%
  rows_patch(resupply_hromada, by = c("a7_current_hromada","stats"))


# Putting all together in one dataset
df_resupply <- list("resupply_medians_hromada" = resupply_medians_hromada, 
                    "resupply_medians_raion" = resupply_medians_raion, 
                    "resupply_medians_oblast" = resupply_medians_oblast, 
                    "resupply_medians_region" = resupply_medians_region, 
                    "resupply_medians_national" = resupply_medians_national)

# Recoding p-codes with names
for (i in 1:length(df_resupply)){
  df_resupply[[i]] <- df_resupply[[i]] %>%
    matchmaker::match_df(dictionary = recode, from = "from",               
                         to = "to",                   
                         by = "col") %>%
    filter(!is.na(colnames(df_resupply[[i]])[1]))
}

# Writing down to the file 
save.dfs(df_resupply, paste0("output/Retailers/", "JMMI UKR Retailers Resupply_combined_analysis_", strings['out_date'], ".xlsx"))



##################################### Stock days - Retailers #####################################

stock_cols <- data.list$main %>%
  select(ends_with("stock_days")) %>%
  colnames()

# Recoding stock days select_one to numeric
recode_stock <- tool.choices %>%
  filter(list_name == "stock") %>%
  rename("to" = "label::English",
         "from" = "name") %>%
  subset(select = -list_name)
recode_stock$col <- ".global"
recode_stock$to <- c(1, 2.5, 4.5, 6.5, 11, 18, 26.5, 31, 999)

df_stock <- data.list$main %>%
  select(c(location_cols,stock_cols)) %>%
  matchmaker::match_df(dictionary = recode_stock, from = "from",               
                       to = "to",                   
                       by = "col") %>%
  replace_with_na_all(condition = ~.x == "999") %>%
  mutate_at(stock_cols,as.numeric)


stock_medians_hromada <- raw_medians(df_stock,stock_cols,"a7_current_hromada",c( "macroregion", "a5_current_oblast", "a6_current_raion")) 
stock_medians_raion <- raw_medians(filter(stock_medians_hromada,stats=="medians"),stock_cols,"a6_current_raion", c("macroregion", "a5_current_oblast"))
stock_medians_oblast <- raw_medians(filter(stock_medians_raion,stats=="median"),stock_cols,"a5_current_oblast", "macroregion")
stock_medians_region <- raw_medians(filter(stock_medians_oblast,stats=="median"),stock_cols,"macroregion")
stock_medians_national <- descriptive_stats(rename(filter(stock_medians_region, stats == "median"), "national" = "stats"),"national",stock_cols)
#prices_national <- medians_national %>%
#  ungroup() %>%
#  select(-national)

### 
### Impute missing data for basket calculations ###
### 

stock_hromada <- stock_medians_hromada %>%
  filter(stats == "median")
stock_raion <- stock_medians_raion %>%
  filter(stats == "median")
stock_oblast <- stock_medians_oblast %>%
  filter(stats == "median")
stock_region <- stock_medians_region %>%
  filter(stats == "median")


# Creating a dataset with all region medians to impute missing values on oblast level
stock_region_to_update <- stock_oblast[,c("macroregion", "a5_current_oblast")] %>%     
  left_join(stock_region, by = "macroregion")          
# Imputing missing values in stock dataset (only medians)
stock_oblast <- stock_oblast %>%                                                  
  rows_patch(stock_region_to_update, by = c("macroregion","a5_current_oblast","stats"))
# Imputing misiing values in full dataset
stock_medians_oblast <- stock_medians_oblast %>%
  rows_patch(stock_oblast, by = c("macroregion","a5_current_oblast","stats"))

stock_oblast_to_update <- stock_raion[,c("a5_current_oblast", "a6_current_raion")] %>%
  left_join(stock_oblast, by = "a5_current_oblast")
stock_raion <- stock_raion %>%
  rows_patch(stock_oblast_to_update, by = "a6_current_raion")
stock_medians_raion <- stock_medians_raion %>%
  rows_patch(stock_raion, by = c("a6_current_raion","stats"))

stock_raion_to_update <- stock_hromada[,c("a6_current_raion", "a7_current_hromada")] %>%
  left_join(stock_raion, by = "a6_current_raion")
stock_hromada <- stock_hromada %>%
  rows_patch(stock_raion_to_update, by = "a7_current_hromada")
stock_medians_hromada <- stock_medians_hromada %>%
  rows_patch(stock_hromada, by = c("a7_current_hromada","stats"))


# Putting all together in one dataset
df_stock_combined <- list("stock_medians_hromada" = stock_medians_hromada, 
                          "stock_medians_raion" = stock_medians_raion, 
                          "stock_medians_oblast" = stock_medians_oblast, 
                          "stock_medians_region" = stock_medians_region, 
                          "stock_medians_national" = stock_medians_national)

transform_stock <- function(data){
  cat2 <- character()                       
  cat2[data <  1.5] <- "up_to_1_day"                   
  cat2[data >= 1.5 & data < 3.5] <- "2_3_days"
  cat2[data >= 3.5 & data < 5.5] <- "4_5_days"
  cat2[data >= 5.5 & data < 7.5] <- "6_7_days"
  cat2[data >= 7.5 & data < 14.5] <- "8_14_days"
  cat2[data >= 14.5 & data < 21.5] <- "15_21_days"
  cat2[data >= 21.5 & data < 31] <- "22_31_days"
  cat2[data >= 31] <- "31_plus_days"
  return(cat2)
}

# Recoding p-codes with names
for (i in 1:length(df_stock_combined)){
  df_stock_combined[[i]] <- df_stock_combined[[i]] %>%
    matchmaker::match_df(dictionary = recode, from = "from",               
                         to = "to",                   
                         by = "col") %>%
    mutate(across(.cols = stock_cols, .fns = transform_stock))
}


# Writing down to the file 
save.dfs(df_stock_combined, paste0("output/Retailers/", "JMMI UKR Retailers Stock_combined_analysis_", strings['out_date'], ".xlsx"))
##################################### Current Operating - Retailers #####################################

# Select w1 column
operating_cols <- data.list$main %>%
  select("a5_current_oblast","a6_current_raion", "a7_current_hromada", "w1_perc_current_operating") %>%
  left_join(regions, by = "a5_current_oblast") %>% na.omit()
operating_cols <- operating_cols %>% select(macroregion, everything())


# Replace values to the numbers


replace_values_in_w1 <- function(x) {
  x <- gsub("_", "", x)
  x <- ifelse(x == "025" | x == "25", "12.500", x)
  x <- ifelse(x == "2550", "37.500", x)
  x <- ifelse(x == "5075", "62.500", x)
  x <- ifelse(x == "75100" | x == "100" | x == "80", "87.500", x)
  as.numeric(x)
}

# Apply the function to the column
operating_cols$w1_perc_current_operating <- replace_values_in_w1(operating_cols$w1_perc_current_operating)
# Calculate mean per hromada

operating_hromada <- operating_cols %>% group_by(macroregion,a5_current_oblast,a6_current_raion, a7_current_hromada) %>% 
  summarise(w1_perc_current_operating = mean(w1_perc_current_operating)) 
operating_hromada$w1_perc_current_operating <- round(operating_hromada$w1_perc_current_operating,4)
operating_hromada <- left_join(operating_hromada, recode, by = c("a7_current_hromada" = "from"))
names(operating_hromada)[names(operating_hromada) == 'to'] <- 'strata'
w1_operating_hromada <- operating_hromada %>% ungroup() %>% select(2,3,4,6,5) 

# Calculate mean per raion

operating_raion <- operating_cols %>% group_by(macroregion,a5_current_oblast,a6_current_raion) %>% 
  summarise(w1_perc_current_operating = mean(w1_perc_current_operating)) 
operating_raion$w1_perc_current_operating <- round(operating_raion$w1_perc_current_operating,4)
operating_raion <- left_join(operating_raion, recode, by = c("a6_current_raion" = "from"))
names(operating_raion)[names(operating_raion) == 'to'] <- 'strata'
w1_operating_raion <- operating_raion %>% ungroup() %>% select(2,3,5,4) 
# Calculate mean per oblast

operating_oblast <- operating_cols %>% group_by(macroregion,a5_current_oblast) %>% 
  summarise(w1_perc_current_operating = mean(w1_perc_current_operating)) 
operating_oblast$w1_perc_current_operating <- round(operating_oblast$w1_perc_current_operating,4)
operating_oblast <- left_join(operating_oblast, recode, by = c("a5_current_oblast" = "from")) %>% distinct()
names(operating_oblast)[names(operating_oblast) == 'to'] <- 'strata'
w1_operating_oblast <- operating_oblast %>% ungroup() %>% select(2,4,3) 

# Calculate mean per region

operating_region <- operating_cols %>% group_by(macroregion) %>% 
  summarise(w1_perc_current_operating = mean(w1_perc_current_operating)) 
operating_region$w1_perc_current_operating <- round(operating_region$w1_perc_current_operating,4)

w1_operating_region <- operating_region %>% ungroup() 

# Calculate  mean of regions values for national level

operating_national <- w1_operating_region  %>% 
  summarise(w1_perc_current_operating = mean(w1_perc_current_operating)) %>% mutate(national = "national")
w1_operating_national <- operating_national %>% select(national, everything())

# Replace numbers to the categorical values

operating_list <- list("w1_operating_hromada" = w1_operating_hromada, 
                       "w1_operating_raion" = w1_operating_raion, 
                       "w1_operating_oblast" = w1_operating_oblast, 
                       "w1_operating_region" = w1_operating_region, 
                       "w1_operating_national" = w1_operating_national)

# Define a function to replace values
replace_values_in_list <- function(df) {
  df %>%
    mutate(w1_perc_current_operating = case_when(
      w1_perc_current_operating >= 0 & w1_perc_current_operating <= 25 ~ "0_25",
      w1_perc_current_operating > 25 & w1_perc_current_operating <= 50 ~ "25_50",
      w1_perc_current_operating > 50 & w1_perc_current_operating <= 75 ~ "50_75",
      w1_perc_current_operating > 75 & w1_perc_current_operating <= 100 ~ "75_100",
      TRUE ~ as.character(w1_perc_current_operating)
    ))
}

# Apply the function to each data frame in the list
operating_list <- lapply(operating_list, replace_values_in_list)

 # Writing down to the file
 save.dfs(operating_list, paste0("output/Retailers/", "JMMI UKR Retailers Current_Operating_combined_analysis_", strings['out_date'], ".xlsx"))
