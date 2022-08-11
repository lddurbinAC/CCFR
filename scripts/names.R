# Setup -------------------------------------------------------------------

# load custom helper functions
source("scripts/functions.R")

get_started(c("here","dplyr", "stringr", "lubridate", "tidyr"))

local_data <- paste0(here::here("data/ccpfr_data"), "/")


# Functions for this script -----------------------------------------------

prep_data <- function(df, type) {
 df |> 
    select(facility_id = id, name) |> 
    mutate(facility_type := {{type}})
}


# Load data ---------------------------------------------------------------

john_data <- get_excel_data("Facility_Partner_Names", "CCPFR list")

# get tables exported from Access database
assets <- get_excel_data("assets", "assets", path = local_data) |> prep_data("asset")
spaces <- get_excel_data("spaces", "spaces", path = local_data) |> prep_data("space")
entities <- get_excel_data("entities", "entities", path = local_data) |> prep_data("entity")
facilities_attributes <- get_excel_data("facilities_attributes", "facilities_attributes", path = local_data) |> 
  mutate(facility_type = str_to_lower(facility_type)) |> 
  select(id, facility_id, facility_type)


# Prep data ---------------------------------------------------------------

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

# Union CCPFR facilities tables
facilities <- bind_rows(assets, spaces, entities)

# Get all facilities in CCPFR that are in John's list, extract aliases
alternate_names <- facilities |> 
  left_join(john_data_amended, by = "name") |> 
  pivot_longer(4:5, names_to = "alternate_name_number", values_to = "alternate_name") |> 
  filter(!is.na(alternate_name)) |> 
  select(facility_id, facility_type, name = alternate_name) |> 
  mutate(role = "alternate") 

names <- alternate_names |> 
  bind_rows(facilities) |> 
  left_join(facilities_attributes, by = c("facility_type", "facility_id")) |> 
  filter(!is.na(id)) |> 
  mutate(
    facilities_attributes_id = id,
    id = row_number(),
    role = if_else(is.na(role), "primary", role)
    ) |> 
  select(
    id,
    value = name,
    role,
    facilities_attributes_id
  )

new_facilities <- john_data_amended |> 
  left_join(facilities, by = "name") |> 
  filter(is.na(facility_id)) |> 
  pivot_longer(2:3, names_to = "alternate_name_number", values_to = "alternate_name") |> 
  select(name, alternate_name, notes) |> 
  mutate(alternate_name = if_else(is.na(alternate_name), name, alternate_name)) |> 
  distinct(alternate_name, .keep_all = TRUE) |> 
  filter(name == alternate_name)
