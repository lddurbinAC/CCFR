# Setup -------------------------------------------------------------------

# load custom helper functions
source("scripts/functions.R")

# check packages are installed, then load them
packages <- c("dplyr", "stringr", "purrr", "lubridate")
get_started(packages)


# Functions for this script -----------------------------------------------

# load the desired sheets from the three Excel files as a named list (assuming nobody has these files open!)
get_data_files <- function() {
  files <- c("VH_data", "AC SharePoint data", "CP_Access_data") # list the Excel files we want to read
  sheets <- c("1.0 Monthly Summary Report", "A&C SharePoint datafeed", "CP Access") #list the sheet we need from each file
  rows_to_skip = c(2,0,0) # list the rows to skip in each sheet
  
  # problems? We need to sync the SharePoint doc library to our local machine. Maybe write a check_ function for this?
  pmap(
    list(..1 =  set_names(files), ..2 = sheets, ..3 = rows_to_skip),
    .f = ~get_excel_data(filename = ..1, sheetname = ..2, skip_rows = ..3)
  ) |> saveRDS(here::here("data/vh_cpAccess_AC.rds"))
}

# select columns from the data frame
col_selection <- function(df, additional_cols, data_src) {
    df |> 
    mutate(source = data_src) |> 
    select(facility_name, month, quarter, year, source, additional_cols)
}


# Load data ---------------------------------------------------------------

# read the names table from the CCPFR
get_packages(c("readxl", "here"))
ccpfr_names <- readxl::read_excel(here::here("data/ccpfr_data/names.xlsx"))

# *** uncomment the next line if we need to read in the Excel files again ***
# get_data_files()

# read the data as stored in the rds file
data <- readRDS(here::here("data/vh_cpAccess_AC.rds"))


# Prepare data ------------------------------------------------------------

# prepare the CP Access data
cp_access <- data$CP_Access_data |> 
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
  col_selection(c("room_name", "average_hours_per_week", "utilisation"), "CP Access")

# prepare the Venue Hire data
vh <- data$VH_data |> 
  mutate(
    utilisation = booking_hours/gross_standard_available_hours,
    month = month_mm,
    year = fin_year,
    room_name = sub_facility_name
  ) |> 
  col_selection(c("room_name", "attendees", "booking_hours", "average_hours_per_week", "utilisation", "source"), "VH")

# prepare the Arts & Culture data
ac <- data$`AC SharePoint data` |> 
  mutate(month = word(month, 2, sep = fixed(" - ")), facility_name = partner_name) |> 
  col_selection(c("total_attendees_participants"), "AC")
