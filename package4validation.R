# create a package for validation

# TODO: FINISH THIS
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# provide this one to locate the relevant files

anon_pattern <- paste0(".xlsx")
# packaging data:

filename_data <- list.files("data/", full.names = T)
if(length(filename_data) == 0) stop("Error: couldn't find filename_kobo_raw")
filename_output <- list.files("output/", full.names = T)
if(length(filename_output) == 0) stop("Error: couldn't find filename_data_log")

filename_res <- list.files("resources/",pattern = "*.xlsx", full.names = T)

# combine all cleaning log, except sensitive and already combined files


# source:
filenames_R <- list.files(pattern = "(*.R$)|(*.Rmd$)", recursive = T, full.names = T)
filenames_R <- filenames_R[stringr::str_detect(filenames_R, "(api\\.key)|(validation)", T)]

files_to_zip <- c(filename_data,
                  filename_output,
                  filename_res,
                  filenames_R)

####### WARNING - SENSITIVE ######
pwd <- "REACH_MSNA_UKR_2022"   # <- this is a password to the zip archive, kept in plain text. this means that obviously you don't push this file to git and don't share it
##################################  
  
  
zip(paste0("UKR2214_MSNA_Analysis"), files_to_zip, flags = paste("-P", pwd))

#    D O N E
cat("\n DONE \n")
#    D O N E
