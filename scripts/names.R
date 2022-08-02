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
  
# Make sure primary names in John's list match CCPFR so we can obtain aliases
john_data_amended <- john_data |> 
  mutate(name = case_when(
    name == "Freeman’s Bay Community Hall" ~ "Freeman's Bay Community Hall", # the apostrophe is the difference
    name == "Glen Innes Community  Hall" ~ "Glen Innes Community Hall", # has double spacing after Community,
    name == "Panmure Community  Hall" ~ "Panmure Community Hall", # has double spacing after Community,
    name == "Athol Syms Centre" ~ "Athol Syms Hall", # name is the alias
    name == "Bays Community Centre - St Annes Hall" ~ "St Annes Hall", # name is the alias
    name == "Hobsonville HQ" ~ "Headquarters, Hobsonville", # name is the alias
    name == "Kerr St Artspace (Depot)" ~ "The Depot Artspace", # name is the alias
    name == "Metro Theatre (Māngere East Hall)" ~ "Metro Theatre", # name is the alias
    name == "Old Central School Hall" ~ "Papakura Old Central School Hall", # name is the alias
    name == "Paturoa Bay Hall" ~ "Titirangi Beach Hall (Paturoa Bay)", # name is the alias
    name == "Playhouse Theatre" ~ "Glen Eden Playhouse Theatre", # name is the alias
    name == "Point Chevalier Community Centre" ~ "Pt Chevalier Community Centre", # name is the alias
    name == "Shadbolt House (not operational)" ~ "Shadbolt House", # name is the alias
    name == "St Heliers Community Centre - Glendowie Community Hall" ~ "St Heliers Church & Community Centre", # name is the alias
    name == "Sunnynook Community Centre - Kennedy Park" ~ "Sunnynook Community Centre", # name is the alias
    name == "Takaanini" ~ "Takaanini Community Hub", # name is the alias
    name == "TSB Bank Wallace Arts Centre (Pah Homestead)" ~ "Pah Homestead Wallace Arts Centre", # name is the alias
    name == "Waitakere Central Community Arts Council (CEAC)" ~ "Corban Estate Arts Centre (CEAC)", # name is the alias
    name == "Western Springs Garden Community Hall/s" ~ "Western Springs Garden Community Hall", # name is the alias
    name == "Whare Koa - Māngere Community House" ~ "Whare Koa Māngere Community House", # name is the alias (just the hyphen)
    name == "Mangere Central Community Hall" ~ "Māngere Central Community Hall", # macron
    name == "Mangere Old School Hall" ~ "Māngere Old School Hall", # macron
    name == "Mangere War Memorial Hall" ~ "Māngere War Memorial Hall", # macron
    name == "Mary Thomas Centre - Crossland Room" ~ "Mary Thomas Centre", # this is a room
    name == "Meadowbank Community Centre - Tahapa Hall" ~ "Meadowbank Community Centre", # this is a room
    name == "Onehunga Community Centre - Pearce Street Hall" ~ "Onehunga Community Centre", # this is a room
    TRUE ~ name
  ))

# get tables exported from Access database
assets <- read_file("assets", "assets", external_path) |> prep_data("asset")
spaces <- read_file("spaces", "spaces", external_path) |> prep_data("space")
entities <- read_file("entities", "entities", external_path) |> prep_data("entity")

# Union CCPFR facilities tables
facilities <- bind_rows(assets, spaces, entities)

# Get all facilities in CCPFR that are in John's list, extract aliases
alternate_names <- facilities |> 
  left_join(john_data_amended, by = "name") |> 
  pivot_longer(4:5, names_to = "alternate_name_number", values_to = "alternate_name") |> 
  filter(!is.na(alternate_name)) |> 
  select(facility_id, facility_type, name = alternate_name) |> 
  mutate(role = "alternate") 

all_names <- alternate_names |> 
  bind_rows(facilities) |> 
  mutate(
    role = if_else(is.na(role), "primary", role)
    )

new_facilities <- john_data_amended |> 
  left_join(facilities, by = "name") |> 
  filter(is.na(facility_id)) |> 
  pivot_longer(2:3, names_to = "alternate_name_number", values_to = "alternate_name") |> 
  select(name, alternate_name, notes) |> 
  mutate(alternate_name = if_else(is.na(alternate_name), name, alternate_name)) |> 
  distinct(alternate_name, .keep_all = TRUE) |> 
  filter(name == alternate_name)
