library(dplyr) # A Grammar of Data Manipulation
library(readxl) # Read Excel Files

# load custom helper functions
source("functions.R")

files <- c("VH_data", "AC SharePoint data", "CP_Access_data") # list the Excel files we want to read
sheets <- c("1.0 Monthly Summary Report", "A&C SharePoint datafeed", "CP Access") #list the sheet we need from each file
skip_rows = c(2,0,0) # list the rows to skip in each sheet

# save each Excel file's desired sheet as a named list item
participant_data <- purrr::pmap(
  list(..1 = purrr::set_names(files), ..2 = sheets, ..3 = skip_rows,
  .f = ~read_excel(
    path = paste0(get_file_storage_path(), ..1, ".xlsx"),
    sheet = ..2,
    skip = ..3,
    .name_repair = janitor::make_clean_names
  )
)

# read the names table from the CCPFR
ccpfr_names <- read_excel(here::here("data/names.xlsx"))

