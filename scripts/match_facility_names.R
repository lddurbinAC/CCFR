library(dplyr) # A Grammar of Data Manipulation
library(readxl)

# this should extract your username from the R project file path
username <- stringr::str_split(here::here(), "/")[[1]][3]

# make sure you have the SharePoint File Storage sync'd to your machine
sharepoint_path <- paste0("C:/Users/", username, "/Auckland Council/CC Insights & Analysis Team - File Storage/")

# list the Excel files we want to read, and the sheet we need from each file
files <- c("VH_data", "AC SharePoint data", "CP_Access_data")
sheets <- c("1.0 Monthly Summary Report", "A&C SharePoint datafeed", "CP Access")

# save each Excel file's desired sheet as a named list item
participant_data <- purrr::pmap(
  list(..1 = purrr::set_names(files), ..2 = sheets, ..3 = c(2,0,0)),
  .f = ~read_excel(
    path = paste0(sharepoint_path, ..1, ".xlsx"),
    sheet = ..2,
    skip = ..3,
    .name_repair = janitor::make_clean_names
  )
)

ccpfr_names <- read_excel(here::here("data/names.xlsx"))
