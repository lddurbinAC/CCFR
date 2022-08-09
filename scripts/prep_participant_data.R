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
  ) |> saveRDS(here("data/vh_cpAccess_AC.rds"))
}


# Load data ---------------------------------------------------------------

# read the names table from the CCPFR
get_packages(c("readxl", "here"))
ccpfr_names <- readxl::read_excel(here("data/ccpfr_data/names.xlsx"))

# uncomment the next line if we need to read in the Excel files again
# get_data_files()

# read the data as stored in the rds file
data <- readRDS(here("data/vh_cpAccess_AC.rds"))


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
    utilisation = booked_hrs/(days_in_month*10),
    source = "CP Access"
    ) |> 
  select(
    facility_name,
    room_name,
    month,
    quarter,
    year,
    average_hours_per_week,
    utilisation
    )

data$VH_data |> 
  select(
    facility_name,
    room_name = sub_facility_name,
    month = month_mm,
    quarter,
    year = fin_year,
    attendees,
    booking_hours)

data$`AC SharePoint data` |> 
  mutate(
    month = word(month, 2, sep = fixed(" - "))
  ) |> 
  select(partner_name, month, quarter, year, total_attendees_participants)


# John's requirements...
# 
# -Partner or Facility name:
#   
#   as per the CCFPR matching with the raw data facility columns
# 
# -Local Board:
#   
#   For both VH data and AC Sharepoint data - we'll use CCPFR Local Board data instead for consistency, even though LB is already provided in the raw data.
# 
# 
# -Average Hours Per Week:
#   For VH data -  as per the "Average Hours per week" raw data column, so no further calculations needed
# For AC Sharepoint data - there is NO Average Hours raw data collected, so this column isn't used for AC data
# 
# 
# 
# -Utilisation:
# For VH data - calculated using the  "Booking Hours" raw data column and the "Gross Standard Available Hours" raw data column (using the calculation
# 
# Booking Hours/Gross Standard Available Hours)
# For AC Sharepoint data - NO raw data collected so this column isn't used for AC data
# 
# 
# 
# -Room: eventually we'd use the space data from the CCPFR for consistency but in the meanwhile:
# For VH data - as per the "Sub Facility Name" raw data column
# For AC Sharepoint data - NO raw data collected so this column isn't used for AC data