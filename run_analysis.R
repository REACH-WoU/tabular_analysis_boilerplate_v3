# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


## SET FILENAMES AND OTHER STRINGS  --------------------------------------------

strings <- c(

  dataset.name = "???",      # provide a full name for the title of output documents (e.g. "[POL] Post-Distribution Monitoring")
  dataset.name.short = "???",   # provide a short name for filenames of output documents (e.g. "POL_PDM")
  dataset.date = "???",       # this string is only used for creating titles for output documents
  out_date = stringr::str_sub(stringr::str_remove_all(Sys.Date(), '-'), 3),      # this one is appended to the end of filenames
  filename.data = "data/???.xlsx",      # the filename of your data for analysis
  filename.tool = "resources/???.xlsx",      # filename of your kobo tool
  filename.daf.tabular = "resources/???.xlsx",      # filename of your DAF
  
)


# ADDITIONAL PARAMETERS WHICH MAY NEED TO BE CHANGED

params  <- c(
    fix_sheet_names_to_match = "data",     # this should be one of "tool", "data", or "none"
    combine_folder = "temp/combine/"
)

## TABULAR  -------------------------------------------------------------------

rmarkdown::render('analysis_tabular.Rmd',
                  output_file = paste0("output/", strings['dataset.name.short'], "_Tabular_Analysis_", strings['out_date'],".html"))
cat("\n> tabular analysis completed!")

# ------------------------------------------------------------------------------

