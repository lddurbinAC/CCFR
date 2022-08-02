# load the environment variables
readRenviron(".Renviron")

# safely compose the full path to the SharePoint File Storage document library
get_file_storage_path <- function() {
  file_storage_partial <- Sys.getenv("SHAREPOINT_FILE_STORAGE")
  username <- stringr::str_split(here::here(), "/")[[1]][3]
  
  return(paste0("C:/Users/", username, file_storage_partial))
}

# read Excel file, select a sheet, clean column names, skip rows
read_file <- function(filename, sheetname, path) {
  readxl::read_excel(
    path = paste0(path, "/", filename, ".xlsx"),
    sheet = sheetname,
    .name_repair = janitor::make_clean_names
  )
}

# join a facilities table with its attributes, returning only facilities
get_attributes <- function(db_table) {
  attributes <- read_file("facilities_attributes", "facilities_attributes", get_file_storage_path()) |> select(-id)
  
  db_table |> 
    left_join(attributes, by = c("id" = "facility_id")) |> 
    filter(!designation %in% c("Room", "Hybrid"))
}
