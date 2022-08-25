# Setup -------------------------------------------------------------------

# load custom helper functions
source("scripts/functions.R")

# uncomment these lines if you don't have devtools and/or awhina
# install.packages("devtools")
# devtools::install_github("lddurbinAC/awhina")
library(awhina)
packages <- c("stringr", "purrr", "lubridate", "dplyr")
get_started(packages)

#create_environment_variable("SHAREPOINT_FILE_STORAGE")

# Functions for this script -----------------------------------------------

# load the desired sheets from the three Excel files as a named list (assuming nobody has these files open!)
get_data_files <- function() {
  files <- c("VH_data", "AC SharePoint data", "CP_Access_data") # list the Excel files we want to read
  sheets <- c("1.0 Monthly Summary Report", "A&C SharePoint datafeed", "CP Access") #list the sheet we need from each file
  rows_to_skip = c(2,0,0) # list the rows to skip in each sheet
  
  pmap(
    list(..1 =  set_names(files), ..2 = sheets, ..3 = rows_to_skip),
    .f = ~get_excel_data(filename = ..1, sheetname = ..2, skip_rows = ..3)
  ) |> saveRDS(here::here("data/vh_cpAccess_AC.rds"))
}

# retrieve a named data source from the list of data sources
get_named_item <- function(name) {
  data <- readRDS(here::here("data/vh_cpAccess_AC.rds"))
  
  data |> 
    pluck(name)
}

# select columns from the data frame
select_columns <- function(df, additional_cols, data_src) {
    df |> 
    mutate(source = data_src) |> 
    select(facility_name, month, quarter, year, source, all_of(additional_cols))
}

# Load data ---------------------------------------------------------------

# *** uncomment the next line if we need to read in the Excel files again ***
# get_data_files()

# Prepare data ------------------------------------------------------------

# prepare the CP Access data
cp_access <- get_named_item("CP_Access_data") |> 
  mutate(
    month = word(reporting_month, 1, sep=fixed("-")),
    month_as_number = match(month, month.abb),
    quarter = paste0("Q", quarter(month_as_number, fiscal_start = 7)),
    year_as_number = word(reporting_month, 2, sep=fixed("-")) |> as.double(),
    year = if_else(
      month_as_number < 7,
      paste0("FY", year_as_number-1, "/", year_as_number),
      paste0("FY", year_as_number, "/", year_as_number+1)
      ),
    facility_name = word(facility_name_room_name, 1, sep = fixed("-")) |> str_trim(),
    room_name = word(facility_name_room_name, 2, sep = fixed("-")) |> str_trim(),
    average_hours_per_week = (booked_hrs/days_in_month(month_as_number))*7,
    utilisation = booked_hrs/(days_in_month(month_as_number)*10),
    ) |> 
  select_columns(c("room_name", "average_hours_per_week", "utilisation"), "CP Access") |> 
  align_names()

cp_access |> filter(is.na(primary_name)) |> distinct(facility_name) # exceptions

# prepare the Venue Hire data
vh <- get_named_item("VH_data") |> 
  filter(!facility_name %in% c("Unknown", "Pakuranga Leisure Centre")) |> 
  mutate(
    utilisation = booking_hours/gross_standard_available_hours,
    month = month_mm,
    year = fin_year,
    room_name = sub_facility_name
  ) |> 
  select_columns(c("room_name", "attendees", "booking_hours", "average_hours_per_week", "utilisation", "source"), "VH") |> 
  align_names()

vh |> filter(is.na(primary_name)) |> distinct(facility_name) # exceptions

# prepare the Arts & Culture data
ac <- get_named_item("AC SharePoint data") |> 
  mutate(month = word(month, 2, sep = fixed(" - ")), facility_name = partner_name) |> 
  select_columns(c("total_attendees_participants"), "AC") |> 
  align_names()

ac |> filter(is.na(primary_name)) |> distinct(facility_name) # exceptions
