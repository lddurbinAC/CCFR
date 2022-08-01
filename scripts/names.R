library(dplyr) # A Grammar of Data Manipulation
library(stringr) # Simple, Consistent Wrappers for Common String Operations
library(lubridate) # Make Dealing with Dates a Little Easier
library(tidyr) # Tidy Messy Data

source(here::here("scripts/functions.R"))

prep_data <- function(df, type) {
 df |> 
    select(facility_id = id, name) |> 
    mutate(facility_type := {{type}})
}

local_path <- here::here("data")
external_path <- "C:/Users/durbinl/Auckland Council/CC Insights & Analysis Team - File Storage"

john_data <- readxl::read_excel(
  path = paste0(external_path, "/Facility_Partner_Names.xlsx"),
  sheet = "CCPFR list")

# get tables exported from Access database
assets <- read_file("assets", "assets", external_path) |> prep_data("asset")
spaces <- read_file("spaces", "spaces", external_path) |> prep_data("space")
entities <- read_file("entities", "entities", external_path) |> prep_data("entity")

facilities <- bind_rows(assets, spaces, entities)

alternate_names <- facilities |> 
  left_join(john_data, by = "name") |> 
  pivot_longer(4:5, names_to = "alternate_name_number", values_to = "alternate_name") |> 
  filter(!is.na(alternate_name)) |> 
  select(facility_id, facility_type, name = alternate_name) |> 
  mutate(role = "alternate") 

all_names <- alternate_names |> 
  bind_rows(facilities) |> 
  mutate(role = if_else(is.na(role), "primary", role))

new_facilities <- john_data |> 
  left_join(facilities, by = "name") |> 
  filter(is.na(facility_id)) |> 
  pivot_longer(2:3, names_to = "alternate_name_number", values_to = "alternate_name") |> 
  select(name, alternate_name) |> 
  mutate(alternate_name = if_else(is.na(alternate_name), name, alternate_name)) |> 
  distinct(alternate_name, .keep_all = TRUE) |> 
  filter(name == alternate_name)
