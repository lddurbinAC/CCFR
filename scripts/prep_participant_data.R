# Setup -------------------------------------------------------------------

# load custom helper functions
source("scripts/functions.R")

# check packages are installed, then load them
packages <- c("dplyr", "stringr", "purrr", "lubridate")
get_started(packages)

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


# Load data ---------------------------------------------------------------

# read the names table from the CCPFR
get_packages(c("readxl", "here"))
ccpfr_names <- readxl::read_excel(here::here("data/ccpfr_data/names.xlsx"))

# uncomment the next line if we need to read in the Excel files again
get_data_files()

# read the data as stored in the rds file
data <- readRDS(here::here("data/vh_cpAccess_AC.rds"))


# Prepare data ------------------------------------------------------------

# separate out facility names and room names for CP Access data
cp_access <- data$CP_Access_data |> 
  mutate(
    month = word(reporting_month, 1, sep=fixed("-")),
    date_placeholder = dmy(paste0("1 ", month, " 2022")),
    days_in_month = days_in_month(date_placeholder),
    month_as_number = month(date_placeholder),
    quarter = case_when(
      month_as_number %in% c(7,8,9) ~ "Q1",
      month_as_number %in% c(10,11,12) ~ "Q2",
      month_as_number %in% c(1,2,3) ~ "Q3",
      month_as_number %in% c(4,5,6) ~ "Q4"
    ),
    year_as_number = word(reporting_month, 2, sep=fixed("-")) |> as.double(),
    previous_year = year_as_number-1,
    next_year = year_as_number+1,
    year = if_else(
      month_as_number < 7,
      paste0("FY", previous_year, "/", year_as_number),
      paste0("FY", year_as_number, "/", next_year)
      ),
    facility_name = word(facility_name_room_name, 1, sep = fixed("-")) |> str_trim(),
    room_name = word(facility_name_room_name, 2, sep = fixed("-")) |> str_trim(),
    average_hours_per_week = (booked_hrs/days_in_month)*7,
    utilisation = booked_hrs/(days_in_month*10)
    ) |> 
  select(facility_name, room_name, month, quarter, year, average_hours_per_week, utilisation)

data$VH_data |> 
  select(facility_name, room_name = sub_facility_name)

data$`AC SharePoint data` |> 
  select(partner_name)

# test <- left_join(cp_access, ccpfr_names, by = c("facility_name" = "value")) |> filter(!is.na(id) & role == "alternate")
# left_join(test, ccpfr_names, by = "facilities_attributes_id") |> filter(role.y == "primary")
