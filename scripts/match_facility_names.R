# Setup -------------------------------------------------------------------

# load custom helper functions
source(here::here("scripts/functions.R"))

# check packages are installed, then load them
get_packages(c("dplyr", "stringr"))
library(dplyr) # A Grammar of Data Manipulation
library(stringr) # Simple, Consistent Wrappers for Common String Operations


# Load data ---------------------------------------------------------------

# read the names table from the CCPFR
ccpfr_names <- readxl::read_excel(here::here("data/names.xlsx"))

files <- c("VH_data", "AC SharePoint data", "CP_Access_data") # list the Excel files we want to read
sheets <- c("1.0 Monthly Summary Report", "A&C SharePoint datafeed", "CP Access") #list the sheet we need from each file
rows_to_skip = c(2,0,0) # list the rows to skip in each sheet

# load the desired sheets from the three Excel files as a named list
data <- purrr::pmap(
  list(..1 =  purrr::set_names(files), ..2 = sheets, ..3 = rows_to_skip),
       .f = ~get_excel_data(filename = ..1, sheetname = ..2, skip_rows = ..3)
  )


# Prepare data ------------------------------------------------------------

# separate out facility names and room names for CP Access data
data$CP_Access_data |> 
  mutate(
    facility_name = word(facility_name_room_name, 1, sep = fixed("-")),
    room_name = word(facility_name_room_name, 2, sep = fixed("-"))
    ) |> 
  select(facility_name, room_name)

data$VH_data |> 
  select(facility_name, room_name = sub_facility_name)

data$`AC SharePoint data` |> 
  select(partner_name)
