# load the environment variables
readRenviron(".Renviron")

# install required packages (plus purrr), then load them
get_started <- function(package_names) {
  package_names_with_purrr <- append(package_names, "purrr")
  get_packages(package_names_with_purrr)
  
  for(p in package_names) {
    library(p, character.only = TRUE)
  }
}

# check if the user has their environment variables set up, help them if not
check_environment_variable <- function(variable_names) {
  get_packages("cli")
  
  if(any(Sys.getenv(variable_names) == "")) {
    stop(
      cli::cli_alert_warning(paste0("Environment variables are missing: ", variable_names)),
      cli::cli_alert_warning("Please read set-up vignette to configure your system.")
    )
  }
}

# Install any packages as needed
get_packages <- function(packages) {
  install.packages(
    setdiff(
      packages, 
      rownames(installed.packages())
      )
    )
}


# safely compose the full path to the SharePoint File Storage document library
get_file_storage_path <- function() {
  check_environment_variable(c("SHAREPOINT_FILE_STORAGE"))
  get_packages("stringr")
  get_packages("here")
  
  file_storage_partial <- Sys.getenv("SHAREPOINT_FILE_STORAGE")
  username <- stringr::str_split(here::here(), "/")[[1]][3]
  
  return(paste0("C:/Users/", username, file_storage_partial))
}


# read Excel file, select a sheet, clean column names, skip rows if necessary
get_excel_data <- function(path, filename, sheetname, skip_rows = 0) {
  get_packages("readxl")
  
  readxl::read_excel(
    path = paste0(get_file_storage_path(), filename, ".xlsx"),
    sheet = sheetname,
    skip = skip_rows,
    .name_repair = janitor::make_clean_names
  )
}

# join a facilities table with its attributes, returning only facilities
get_attributes <- function(db_table) {
  attributes <- get_excel_data("facilities_attributes", "facilities_attributes", get_file_storage_path()) |> select(-id)
  
  db_table |> 
    left_join(attributes, by = c("id" = "facility_id")) |> 
    filter(!designation %in% c("Room", "Hybrid"))
}
