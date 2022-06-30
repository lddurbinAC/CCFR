library(dplyr) # A Grammar of Data Manipulation
library(stringr) # Simple, Consistent Wrappers for Common String Operations

# read Excel file, select a sheet, clean column names, skip rows
read_file <- function(filename, sheetname, skip_rows) {
  readxl::read_excel(
    path = paste0(path, "/", filename, ".xlsx"),
    sheet = sheetname,
    .name_repair = janitor::make_clean_names,
    skip = skip_rows
  )
}

path <- here::here("data")


# Load data ---------------------------------------------------------------

# which facility names in our data should be changed to match the LTP data?
name_corrections <- readr::read_csv(paste0(path, "/name_corrections.csv"), col_types = "ccc")

# load the LTP data, standardise Designation and Delivery Model terms, add a Commmunity Lease flag
ltp_facilities_list <- read_file("ltp_facilities_original", "FY21 Data reported +FY22 update", 2) |> 
  mutate(
    facility_type = if_else(facility_type == "Arts Facility", "Arts and Culture", facility_type),
    across(starts_with("fy"), str_to_lower),
    fy22_community_lease = if_else(
      !str_detect(notes_fy22, "Community Lease facility") | is.na(notes_fy22),
      FALSE,
      TRUE
    )
  )

# select the columns we need from the LTP data
# manually remove some facilities for the reasons given
ltp_for_comparison <- ltp_facilities_list |> 
  select(name = facility_name, local_board, designation = facility_type, delivery_model = fy21_22, fy22_community_lease) |> 
  filter(!name %in% c(
    "Karaka Hall", # duplicate
    "Mauku Hall", # duplicate
    "Ponsonby Community Centre - Leys Institute Gymnasium", # duplicate & closed
    "Howick Brass Band", # band
    "Papakura Brass Band", # band
    "City of Manukau Pipe Band", # band
    "Manukau Concert Band (MCB)", #band
    "Papakura Pipe Band", #band
    "Friendship House", # out of scope
    "Old Surfdale Post Office", # out of scope
    "Sistema Aotearoa", # out of scope
    "Q Theatre", # regionally funded
    "Te Tuhi", # regionally funded
    "Leys Institute Hall", # closed
    "Buckland Community Centre" # closed
    # Shadbolt House?
  ))

# load our list of community-led facilities, standardise Designation terms
community_facilities <- read_file("facilities_community", "query (15)", 0) |> 
  select(facility_id, name = title, local_board, physical_address, designation = facility_type) |> 
  mutate(designation = if_else(designation == "Arts and Culture Facility", "Arts and Culture", designation))

# load our list of council-led facilities, manually insert two entities
council_facilities <- read_file("facilities_council", "facilities_council", 0) |> 
  add_row(
    facility_id = "E01",
    name = "Franklin Arts Centre",
    local_board = "Franklin",
    physical_address = "12 Massey Avenue, Pukekohe 2120",
    designation = "Arts and Culture",
    facility_type = "Entity"
  ) |> 
  add_row(
    facility_id = "E14",
    name = "Franklin The Centre",
    local_board = "Franklin",
    physical_address = "12 Massey Avenue, Pukekohe 2120",
    designation = "Community Centre",
    facility_type = "Entity"
  ) |> 
  select(-facility_type)


# Prepare data ------------------------------------------------------------

# bind/union the council-led and community-led data into a single data frame
# amend the facility names where required
# manually remove some facilities for the reasons given
facilities <- bind_rows(
  list("community led" = community_facilities, "council led" = council_facilities),
  .id = "delivery_model") |> 
  left_join(name_corrections |> select(-delivery_model), by = c("name" = "our_name")) |> 
  mutate(name = if_else(!is.na(corrected_name), corrected_name, name), .keep = "unused") |> 
  filter(!name %in% c(
    "Waitakere Arts and Cultural Devt Trust", # partner
    "Acacia Court Hall", # out of scope
    "Albany Hall", # out of scope, community leasing
    "North Shore Brass", # out of scope
    "Q Theatre", # regionally funded
    "Te Tuhi", # regionally funded
    "Leys Institute Hall", # closed
    "Buckland Community Centre" # closed
    # Shadbolt House?
  ))

# join the LTP data with our facilities data
combined_facilities_list <- left_join(ltp_for_comparison, facilities, by = c("name", "local_board", "delivery_model"))


# Validate data -------------------------------------------------------------------

# check if designation (a.k.a. facility type) differs between the two lists
combined_facilities_list |> 
  filter(str_to_lower(designation.x) != str_to_lower(designation.y) & paste0(str_to_lower(designation.x), str_to_lower(designation.y)) != "venue for hirerural hall")

# check if service delivery model differs between the two lists - only run if we're not joining on this field
# combined_facilities_list |>
#   filter(str_to_lower(delivery_model.x) != str_replace(delivery_model.y, "_", " "))

# check for any duplicates based on facility name
combined_facilities_list |> group_by(name) |> filter(n() > 1)


# Analyse data ------------------------------------------------------------

# LTP metric by Local Board, excluding Great Barrier
ltp_metric_lb <- combined_facilities_list |> 
  count(local_board, delivery_model) |> 
  with_groups(local_board, mutate, perc = n/sum(n)) |> 
  filter(local_board != "Great Barrier")

# LTP metric for the region, excluding Great Barrier
ltp_metric_regional <- combined_facilities_list |> 
  filter(local_board != "Great Barrier") |> 
  count(delivery_model) |> 
  mutate(perc = n/sum(n)) |> 
  filter(delivery_model == "community led")

# For each facility, compare this year's service delivery model with last year's
ltp_metric_trend <- ltp_facilities_list |> 
  select(name = facility_name, local_board, designation = facility_type, fy21 = fy20_21) |> 
  left_join(combined_facilities_list |> select(name, fy22 = delivery_model), by = "name") |> 
  filter(!is.na(fy21) | !is.na(fy22))

# what's the difference when we remove community lease facilities that aren't Arts & Culture?
ltp_metric_lb_no_leases <- combined_facilities_list |> 
  filter(fy22_community_lease == FALSE | designation.x == "Arts and Culture" | designation.y == "Arts and Culture") |> 
  count(local_board, delivery_model) |> 
  with_groups(local_board, mutate, perc = n/sum(n)) |> 
  filter(delivery_model == "community led" & local_board != "Great Barrier") |> 
  left_join(ltp_metric_lb, by = c("local_board", "delivery_model")) |> 
  mutate(variance_count = n.x - n.y, variance_percent = perc.x - perc.y) |> 
  select(
    local_board,
    all_count = n.y,
    no_community_lease_count = n.x,
    variance_count,
    all_percent = perc.y,
    no_community_lease_percent = perc.x,
    variance_percent
  )
