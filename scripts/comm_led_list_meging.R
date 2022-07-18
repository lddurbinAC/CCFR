library(dplyr) # A Grammar of Data Manipulation
library(stringr) # Simple, Consistent Wrappers for Common String Operations

source(here::here("scripts/functions.R"))

local_path <- here::here("data")
external_path <- "C:/Users/durbinl/Auckland Council/CC Insights & Analysis Team - File Storage"

nina_fua_list <- read_file("facilities_community", "query (15)", local_path)
assets <- read_file("assets", "Sheet1", external_path)
spaces <- read_file("spaces", "Sheet1", external_path)

assets_with_attributes <- assets |> get_attributes()
space_with_attributes <- spaces |> get_attributes()

nina_fua_list |> filter(is.na(facility_id)) # any newly-added facilities?
