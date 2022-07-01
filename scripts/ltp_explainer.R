library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(dplyr) # A Grammar of Data Manipulation

# filter by Local Board and service delivery model, then plot coordinates for the visual
choose_facilities <- function(local_board, delivery_model) {
  facilities |> 
    filter(local_board == {{local_board}} & delivery_model == {{delivery_model}}) |> 
    arrange(name) |> 
    mutate(coord_x = (row_number()-1) %% 6, coord_y = floor((row_number()/6)-0.1))
}

path <- here::here("data")

facilities <- readRDS(fs::dir_ls(path, glob = "*.rds")) |> filter(local_board != "Great Barrier")

local_boards <- distinct(facilities, local_board) |> pull()

facilities_blocks <- choose_facilities(local_boards[[2]], "community led")

ggplot(data = facilities_blocks, aes(x = coord_x, y = -coord_y)) +
  geom_point(
    shape=15,
    size=35,
    colour = if_else(facilities_blocks$delivery_model == "council led", "#00304B", "#6E963C")
    ) +
  geom_text(aes(label = stringr::str_wrap(name, 11)), colour = "white", size = 3, fontface = "bold") +
  scale_x_continuous(limits = c(-0.5,5.5)) +
  scale_y_continuous(limits = c(-3.5,0.25)) +
  theme_void() +
  theme(plot.title = element_text(margin = margin(5,0,10,0))) +
  ggtitle(paste0("There are XX community-led facilities in the ", facilities_blocks$local_board, " Local Board, listed below"))

