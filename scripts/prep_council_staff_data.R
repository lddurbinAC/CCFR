# Setup -------------------------------------------------------------------

# load custom helper functions
source("scripts/functions.R")

# check packages are installed, then load them
packages <- c("dplyr", "readxl", "here", "janitor")
get_started(packages)

data_path <- here("data")


# Load data ---------------------------------------------------------------

council_facilities <- read_excel(
  path = paste0(data_path, "/council_facilities_staff.xlsx"),
  col_types = "text",
  .name_repair = make_clean_names)
venue_for_hire <- read_excel(path = paste0(data_path, "/council_venue_for_hire_verified.xlsx"), .name_repair = make_clean_names)
ccpfr_staff <- read_excel(path = paste0(data_path, "/ccpfr_data/staff.xlsx"), col_types = "text")
ccpfr_staff_bridge_table <- read_excel(path = paste0(data_path, "/ccpfr_data/staff_bridge_table.xlsx"), col_types = "text")


# Prep data ---------------------------------------------------------------

staff_with_emails <- venue_for_hire |>
  filter(!is.na(manager) & !is.na(title)) |> 
  mutate(
    email = "Melissa.Colquhoun@aucklandcouncil.govt.nz",
    id = "13",
    role = "Operations manager",
    facility_type = if_else(stringr::str_starts(title, "A"), "Asset", "Space")
    ) |> 
  select(facility_id = title, facility_type, id, email, role) |> 
  bind_rows(council_facilities)

staff <- staff_with_emails |> 
  distinct(id, email) |> 
  bind_rows(ccpfr_staff) |> 
  unique()

staff_bridge_table <- staff_with_emails |> 
  select(facility_id, facility_type, staff_id = id, role) |> 
  mutate(
    id = as.character(row_number()+160),
    valid_from = NA,
    valid_to = NA,
    notes = NA
    ) |> 
  bind_rows(ccpfr_staff_bridge_table) |> 
  select(id, staff_id, facility_type, facility_id, role, valid_from, valid_to, notes) |> 
  unique()

# writexl::write_xlsx(staff, paste0(data_path, "/ccpfr_data/staff.xlsx"))
# writexl::write_xlsx(staff_bridge_table, paste0(data_path, "/ccpfr_data/staff_bridge_table.xlsx"))
