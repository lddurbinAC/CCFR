# **HELPERS PACKAGE: exported function**
# read Excel an file from SharePoint File Storage, optionally name a sheet, skip rows, and change file path
get_excel_data <- function(filename, sheetname = "Sheet1", skip_rows = 0, path = get_file_storage_path("SHAREPOINT_FILE_STORAGE")) {
  readxl::read_excel(
    path = paste0(path, filename, ".xlsx"),
    sheet = sheetname,
    skip = skip_rows,
    .name_repair = janitor::make_clean_names
  )
}

# **CCPFR PACKAGE**
# join a facilities table with its attributes, returning only facilities
get_attributes <- function(db_table) {
  attributes <- get_excel_data("facilities_attributes", "facilities_attributes", get_file_storage_path()) |> select(-id)
  
  db_table |> 
    left_join(attributes, by = c("id" = "facility_id")) |> 
    filter(!designation %in% c("Room", "Hybrid"))
}
