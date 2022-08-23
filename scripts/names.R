# install.packages("devtools")
# devtools::install_github("lddurbinAC/awhina")
library(awhina)
library(stringr)

source("scripts/functions.R")

get_data <- function(file_name, columns) {
  readxl::read_excel(
    path = paste0(path, "/", file_name),
    .name_repair = janitor::make_clean_names
  ) |> 
    select(facility_name = {{columns}})
}

match_ccpfr <- function(df) {
  df |> 
    align_names() |>
    filter(is.na(facilities_attributes_id)) |> 
    select(-c(facilities_attributes_id, primary_name))
}

packages <- c("dplyr", "readxl")
get_started(packages)
path <- get_file_storage_path("SHAREPOINT_FILE_STORAGE")

CP_facilities_unmatched <- get_data("CP Access_Facilities.xlsx", "facility_name") |> match_ccpfr()
AC_partners_unmatched <- get_data("AC Reporting_Partners.xlsx", "partner_name") |> match_ccpfr()
CP_rooms_unmatched <- get_data("CP Access_Rooms.xlsx", "facility_name") |> 
  mutate(
    facility = word(facility_name, 1, sep = fixed("-")) |> str_squish(),
    facility_name = word(facility_name, 2, sep = fixed("-")) |> str_squish()
    ) |> 
  match_ccpfr()
