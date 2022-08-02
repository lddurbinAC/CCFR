library(dplyr)
library(readxl)

# provide your username here
username <- stringr::str_split(here::here(), "/")[[1]][3]

# make sure you have the SharePoint File Storage sync'd to your machine
sharepoint_path <- paste0("C:/Users/", username, "/Auckland Council/CC Insights & Analysis Team - File Storage")

vh <- read_excel(path = paste0(sharepoint_path, "/VH_data.xlsx"))
ac <- read_excel(path = paste0(sharepoint_path, "/AC SharePoint data.xlsx"), sheet = "A&C SharePoint datafeed")
cp <- read_excel(path = paste0(sharepoint_path, "/CP_Access_data.xlsx"), sheet = "CP Access")


