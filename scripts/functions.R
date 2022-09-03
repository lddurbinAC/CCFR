# **CCPFR PACKAGE**
# join a facilities table with its attributes, returning only facilities
get_attributes <- function(db_table) {
  attributes <- get_excel_data("facilities_attributes", "facilities_attributes", get_file_storage_path()) |> select(-id)
  
  db_table |> 
    left_join(attributes, by = c("id" = "facility_id")) |> 
    filter(!designation %in% c("Room", "Hybrid"))
}

# **CCPFR PACKAGE**
# standardise the facility names using the CCPFR data
align_names <- function(df) {
  # pull in Partners names table too
  ccpfr_names <- awhina::get_excel_file("names")
  
  all_names <- ccpfr_names |> 
    select(facility_name = value, facilities_attributes_id)
  
  primary_names <- ccpfr_names |> 
    filter(role == "primary") |> 
    select(primary_name = value, primary_facility_id = facilities_attributes_id)
  
  df |> 
    left_join(all_names, by = "facility_name") |> 
    left_join(primary_names, by = c("facilities_attributes_id" = "primary_facility_id")) |> 
    # we need to remove this mutate and replace it with a unit test
    mutate(
      primary_name = if_else(is.na(primary_name), facility_name, primary_name)
    )
}