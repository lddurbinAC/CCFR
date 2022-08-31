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
    .f = ~get_excel_file(filename = ..1, sheetname = ..2, skip_rows = ..3)
  ) |> saveRDS(here::here("data/vh_cpAccess_AC.rds"))
}

# retrieve a named data source from the list of data sources
get_named_item <- function(name) {
  data <- readRDS(here::here("data/vh_cpAccess_AC.rds"))
  
  data |> 
    pluck(name)
}

# select columns from the data frame
select_columns <- function(df, data_src) {
    df |> 
    mutate(source = data_src) |> 
    select(
      source,
      month,
      quarter,
      year,
      facility_name = primary_name,
      local_board,
      funding_level,
      service_delivery_model,
      participants,
      booking_hours,
      average_hours_per_week,
      utilisation,
      room
      )
}

service_delivery_model <- function(df) {
  facilities_attributes <- get_excel_file("facilities_attributes", path = here::here("data/ccpfr_data")) |> 
    select(id, service_delivery_model = delivery_model)
  
  df |> 
    left_join(facilities_attributes, by = c("facilities_attributes_id" = "id"))
}

funding_level <- function(df) {
  regionally_funded <- readr::read_csv(here::here("data/regionally_funded_facilities.csv"), col_types = "c")
  
  df |> 
    mutate(
      funding_level = if_else(
        facility_name %in% (regionally_funded |> pull(facility_name)),
        "regionally funded",
        "Locally funded"
      )
    )
}

# Load data ---------------------------------------------------------------

# *** uncomment the next line if we need to read in the Excel files again ***
# get_data_files()

# Prepare data ------------------------------------------------------------

# prepare the CP Access data
cp_access <- get_named_item("CP_Access_data") |> 
  select(-service_delivery_model) |> # won't be needed when we remove the extraneous columns
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
    room = word(facility_name_room_name, 2, sep = fixed("-")) |> str_trim(),
    average_hours_per_week = (booked_hrs/days_in_month(month_as_number))*7,
    utilisation = booked_hrs/(days_in_month(month_as_number)*10),
    booking_hours = booked_hrs
    ) |> 
  align_names() |> 
  service_delivery_model() |> 
  funding_level() |> 
  select_columns("CP Access") 

# prepare the Venue Hire data
vh <- get_named_item("VH_data") |> 
  filter(!facility_name %in% c("Unknown", "Pakuranga Leisure Centre")) |> 
  mutate(
    utilisation = booking_hours/gross_standard_available_hours,
    month = month_mm,
    year = fin_year,
    room = sub_facility_name
  ) |>
  align_names() |> 
  funding_level() |> 
  select_columns("VH") 

# prepare the Arts & Culture data
ac <- get_named_item("AC SharePoint data") |> 
  mutate(
    month = word(month, 2, sep = fixed(" - ")),
    facility_name = partner_name,
    room = NA_character_,
    average_hours_per_week = NA_real_,
    utilisation = NA_real_,
    booking_hours = NA_real_
    ) |> 
  align_names() |> 
  funding_level() |> 
  select_columns("AC")

consolidated_dataset <- bind_rows(cp_access, vh, ac)
