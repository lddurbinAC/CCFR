library(dplyr) # A Grammar of Data Manipulation
library(stringr) # Simple, Consistent Wrappers for Common String Operations
library(lubridate) # Make Dealing with Dates a Little Easier
library(tidyr) # Tidy Messy Data

source(here::here("scripts/functions.R"))

# determines whether the facility is an asset or a space based on first letter in its ID
assign_facility_type <- function(df) {
    df |> mutate(facility_type = case_when(
      str_starts(facility_id, "A") ~ "Asset",
      str_starts(facility_id, "S") ~ "Space",
      TRUE ~ "Other"
    ))
}

# assign role to contact, re-name columns
prepare_contacts_data <- function(contact_role) {
  verified_contacts |> 
    select(partner_id, starts_with(contact_role)) |> 
    mutate(role = str_to_sentence(contact_role) |> str_replace("_", " ")) |> 
    rename(name = 2, email_address = 3, phone_number = 4)
}


local_path <- here::here("data")
external_path <- "C:/Users/durbinl/Auckland Council/CC Insights & Analysis Team - File Storage"

# get export of MS List holding data verified by Fua
nina_fua_list <- read_file("facilities_community", "query (15)", local_path)

# get tables exported from Access database
assets <- read_file("assets", "assets", external_path)
spaces <- read_file("spaces", "spaces", external_path)
entities <- read_file("entities", "entities", external_path)

# merge tables with facilities_attributes
assets_with_attributes <- assets |> get_attributes()
space_with_attributes <- spaces |> get_attributes()
entities_with_attributes <- entities |> get_attributes()

# nina_fua_list |> filter(is.na(facility_id)) # any newly-added facilities?


# Attributes: add staffed and leased fields to facilities_attributes ------------------

existing_attributes <- read_file("facilities_attributes", "facilities_attributes", Sys.getenv("EXTERNAL_PATH")) |> 
  select(-c(staffed, partner_id)) |> 
  filter(delivery_model == "Community led" & !designation %in% c("Room", "Hybrid"))

new_attributes <- nina_fua_list |> 
  select(facility_id, leased = community_lease, staffed)

attributes <- left_join(existing_attributes, new_attributes) |> 
  mutate(
    closed = as.logical(closed),
    across(closed:staffed, ~replace_na(.x, FALSE))
  )

writexl::write_xlsx(attributes, paste0(local_path, "/facilities_attributes.xlsx"))

# Partners: create partners table and partners_bridge_table ---------------------------------------------------

multi_site_partners <- nina_fua_list |> 
  filter(!is.na(provider_name)) |> 
  select(facility_id, title, provider_name, provider_type) |> 
  assign_facility_type() |> 
  group_by(provider_name) |> 
  filter(n() > 1) |> 
  arrange(provider_name) |> 
  mutate(
    num = cur_group_id(),
    id = paste0("P", num),
    .before = "facility_id"
  ) |> 
  ungroup()

single_site_partners <- nina_fua_list |> 
  filter(
    !is.na(provider_name) & !provider_name %in% pull(multi_site_partners, provider_name)
  ) |> 
  select(facility_id, title, provider_name, provider_type) |>
  assign_facility_type() |> 
  mutate(
    id = paste0("P", row_number() + max(multi_site_partners$num))
  )

partners <- multi_site_partners |> 
  distinct(across(id:facility_type)) |> 
  bind_rows(single_site_partners)

writexl::write_xlsx(partners, paste0(local_path, "/partners.xlsx"))

partners_bridge_table <- multi_site_partners |> 
  select(partner_id = id, facility_type, facility_id) |> 
  bind_rows(
    single_site_partners |> select(partner_id = id, facility_type, facility_id)
  ) |> 
  mutate(
    id = paste0("PB", row_number()),
    .before = "partner_id"
  )

writexl::write_xlsx(partners_bridge_table, paste0(local_path, "/partners_bridge_table.xlsx"))


# Contacts: crate contacts table ----------------------------------------------------

verified_contacts <- nina_fua_list |> 
  filter(!is.na(provider_name) & !is.na(facility_id)) |> 
  select(facility_id, title, provider_name:reporting_provider_phone_number ) |> 
  assign_facility_type() |> 
  mutate(
    across(where(is.double), ~if_else(!is.na(.x), paste0("0", .x), NA_character_))
  ) |> 
  left_join(
    partners_bridge_table |> select(-id),
    by = c("facility_id", "facility_type")
  ) |> 
  select(-c(title, provider_name, provider_type, facility_id, facility_type))

contacts <- purrr::map_dfr(
  c("site_contact", "chairperson", "reporting_provider"),
  prepare_contacts_data
) |> 
  unique()

writexl::write_xlsx(contacts, paste0(local_path, "/contacts.xlsx"))


# Agreements: create agreements table -------------------------------------------------

agreements_db <- assets_with_attributes |> 
  filter(delivery_model == "Community led") |> 
  select(agreement_type, facility_type, facility_id = id)

agreements_verified <- nina_fua_list |> 
  select(facility_id, agreement_term, valid_from = agreement_commencement_date) |> 
  mutate(valid_to = valid_from + years(agreement_term)-days(1))

agreements <- left_join(agreements_db, agreements_verified) |> 
  mutate(id = row_number(), .before = "agreement_type") |> 
  left_join(
    partners |> select(partner_id = id, facility_id, facility_type),
    by = c("facility_type", "facility_id")
  )

writexl::write_xlsx(agreements, paste0(local_path, "/agreements.xlsx"))


# Funding: create funding table ----------------------------------------------------

funding <- nina_fua_list |> 
  select(facility_id, abs_funding_total, ldi_funding_total) |> 
  assign_facility_type() |> 
  left_join(
    agreements |> select(agreement_id = id, facility_type, facility_id),
    by = c("facility_type", "facility_id")
  ) |> 
  select(-c(facility_id, facility_type)) |> 
  pivot_longer(1:2, names_to = "funding_type", values_to = "amount") |> 
  mutate(service_level = case_when(
    amount < 25000 ~ "Access",
    amount < 75000 ~ "Activation",
    amount >= 75000 ~ "Intervention",
  ))

writexl::write_xlsx(funding, paste0(local_path, "/funding.xlsx"))


# Staff: create staff table and staff_bridge_table -------------------------------

emails <- readr::read_csv(paste0(local_path, "/staff_emails.csv"), col_types = "cc")

staff_data <- nina_fua_list |> 
  select(facility_id, relationship_manager) |> 
  assign_facility_type() |> 
  left_join(
    emails,
    by = c("relationship_manager" = "staff_name")
  ) |> 
  select(everything(), email = staff_email, -relationship_manager) |> 
  mutate(id = row_number())

staff <- staff_data |> 
  distinct(email) |> 
  filter(!is.na(email)) |> 
  mutate(id = row_number())

writexl::write_xlsx(staff, paste0(local_path, "/staff.xlsx"))

staff_bridge_table <- staff_data |> 
  left_join(
    staff |> select(staff_id = id, email),
    by = "email"
    )

writexl::write_xlsx(staff_bridge_table, paste0(local_path, "/staff_bridge_table.xlsx"))
