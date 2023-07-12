setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

##Change variable to Retailers or Customers
JMMI_variable <- c("Customers")


## SET FILENAMES AND OTHER STRINGS  --------------------------------------------
strings <- c(
  dataset.name = "[UKR] JMMI Ukraine",      # provide a full name for the title of output documents (e.g. "[POL] Post-Distribution Monitoring")
  dataset.name.short = "JMMI UKR Customers Region",   # provide a short name for filenames of output documents (e.g. "POL_PDM")
  dataset.date = "2023",       # this string is only used for creating titles for output documents
  out_date = stringr::str_sub(stringr::str_remove_all(Sys.Date(), '-'), 3),      # this one is appended to the end of filenames
  filename.data = "data/UKR2203JMMI_Customers_R15_clean_data_07JUN2023.xlsx",      # the filename of your data for analysis
  filename.tool = "resources/Customers/UKR2203_JMMI_Questionnaire_Customers_R15_07JUN2023_SB.xlsx",      # filename of your kobo tool
  filename.daf.tabular = "resources/Customers/DAF_region.xlsx"     # filename of your DAF
)



# ADDITIONAL PARAMETERS WHICH MAY NEED TO BE CHANGED

params  <- c(
    fix_sheet_names_to_match = "data",     # this should be one of "tool", "data", or "none"
    combine_folder = "temp/combine/"
)


source("src/init.R")

## Ananlysis of numeric variables  --------------------------------------------

#source("JMMI_analysis_numeric.R")

## TABULAR  -------------------------------------------------------------------

rmarkdown::render('analysis_tabular_region.Rmd',
                  output_file = paste0("output/Customers/", strings['dataset.name.short'], "_Tabular_Analysis_Region", strings['out_date'],".html"))
cat("\n> tabular analysis completed!")

# ------------------------------------------------------------------------------

