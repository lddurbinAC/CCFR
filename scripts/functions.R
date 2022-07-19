# get path to where the database's exported tables are stored
readRenviron(".Renviron")

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
  attributes <- read_file("facilities_attributes", "facilities_attributes", Sys.getenv("EXTERNAL_PATH")) |> select(-id)
  
  db_table |> 
    left_join(attributes, by = c("id" = "facility_id")) |> 
    filter(!designation %in% c("Room", "Hybrid"))
}
